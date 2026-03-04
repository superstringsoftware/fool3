{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards, DataKinds #-}

{-
This has a Compiler State Monad and converts STG program to intermediary DotNet types.
Actual code gen happens elsewhere (CSharpGen for C#, eventually something else for IL).
-}

module Compiler where

import GHC

import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Driver.Ppr (showPprUnsafe)
import GHC.Driver.Env
import GHC.CoreToStg.Prep
import GHC.CoreToStg
import GHC.Stg.Pipeline
import GHC.Types.Literal

import GHC.Core
import GHC.Stg.Syntax
import GHC.Core.TyCon

import GHC.Types.Var
import GHC.Types.Id.Info (idDetails)

import GHC.Core.TyCo.Rep -- Type data type
import GHC.Core.DataCon

import GHC.Builtin.PrimOps
import GHC.Types.ForeignCall

import GHC.Types.Var.Set (dVarSetElems)

import Control.Monad.State
import Data.List.Index (imapM, ifind)
import Data.Foldable (foldlM)

-- FIRST, SOME GHC HELPERS
showGhc :: (Outputable a) => a -> String
showGhc = showPprUnsafe

showVarType :: Var -> String
showVarType v = showGhc $ varType v

-- NOW OUR COMPILER MONAD
data CompilerState = CompilerState {
    stringAlign :: Int,
    isTopLevel :: Bool,
    -- when we are creating FUN / THUNKs with free vars, need this to convert names to field numbers in the C# class
    currentFreeVars :: Args,
    -- counter for generating unique __freeVars variable names
    freeVarsCounter :: Int,
    -- the current freeVars variable name (for nested scoping)
    currentFreeVarsName :: String
}

-- state monad for code generation
-- most code gen actions happen in Expr -> SM String actions
type SM = State CompilerState

initialCompilerState = CompilerState {
    stringAlign = 0,
    isTopLevel = True,
    currentFreeVars = [],
    freeVarsCounter = 0,
    currentFreeVarsName = "__freeVars"
}

checkTopLevel :: SM Bool
checkTopLevel = get >>= \s -> pure $ (stringAlign s) == 0

pushFreeVars :: Args -> SM ()
pushFreeVars fv = modify (\s -> s {currentFreeVars = fv})

popFreeVars :: SM ()
popFreeVars = modify (\s -> s {currentFreeVars = []})

readFreeVars :: SM Args
readFreeVars = get >>= pure . currentFreeVars

-- Get a fresh unique name for __freeVars (to avoid duplicate var declarations in same scope)
freshFreeVarsName :: SM String
freshFreeVarsName = do
    s <- get
    let n = freeVarsCounter s
    let name = "__fv" ++ show n
    put s { freeVarsCounter = n + 1, currentFreeVarsName = name }
    return name

-- Get the current freeVars variable name
getFreeVarsName :: SM String
getFreeVarsName = get >>= pure . currentFreeVarsName

-- Reset freeVars name to default (for top-level)
resetFreeVarsName :: SM ()
resetFreeVarsName = modify (\s -> s { currentFreeVarsName = "__freeVars" })

-- | Bracket pattern: set free vars name and push free vars for scope,
-- execute action, then restore previous state
withFreeVars :: String -> Args -> SM a -> SM a
withFreeVars fvName fv action = do
    savedFvName <- getFreeVarsName
    modify (\s -> s { currentFreeVarsName = fvName })
    pushFreeVars fv
    result <- action
    popFreeVars
    modify (\s -> s { currentFreeVarsName = savedFvName })
    return result

incTab :: SM ()
incTab = modify (\s -> s { stringAlign = (stringAlign s) + 4 } )

decTab :: SM ()
decTab = modify (\s -> s { stringAlign = (stringAlign s) - 4 } )

tab :: SM String
tab = do
    st <- get
    return (replicate (stringAlign st) ' ')

-- helper method that returns a string aligned by current alignment
stab :: String -> SM String
stab s = do
    st <- get
    let t = replicate (stringAlign st) ' '
    return (t ++ s)

------------------------------------------------------------------------------------------------
-- Now for the interesting part, conversion from STG into internal codegen rep
------------------------------------------------------------------------------------------------
-- Filtering out some bindings / rhs we don't want:
-- Top level literals - don't look like we need them in the compilation now
isTopLevelLiteral :: CgStgTopBinding -> Bool
isTopLevelLiteral (StgTopStringLit _ _) = True
isTopLevelLiteral _ = False
-- top level constructor applications to what looks like some helpful type / kind manipulation stuff which
-- we also probably don't need, at least initially
isHelperConApp :: CgStgTopBinding -> Bool
isHelperConApp (StgTopLifted (StgNonRec bndr _)) = elem (showVarType bndr) helperCons
    where helperCons = ["KindRep", "TrName", "TyCon"]
isHelperConApp _ = False
-- all filters combined
allStgTopBindingFilters b = (isTopLevelLiteral b) || (isHelperConApp b)

-- We are stripping going down the GenStgTopBinding --> GenStgBinding hierarchy,
-- then killing the difference between REC and NONREC (may need it in the future, but not now???)
-- and simply representing the program as the list of bindings from Var to GenStgRhs
type BareStgProgram = [(Var, CgStgRhs)]
-- function that does this simplification, then we can simply call stgProcessGenericBinding to create the program
simplifyStgToBare :: [CgStgTopBinding] -> BareStgProgram
simplifyStgToBare [] = []
simplifyStgToBare (x:xs) = if (allStgTopBindingFilters x)
        then simplifyStgToBare xs else (convertTopBinding x) ++ (simplifyStgToBare xs)

convertTopBinding :: CgStgTopBinding -> BareStgProgram
convertTopBinding (StgTopLifted x) = convertStgBinding x

-- need this separately as this is used in let expressions!
convertStgBinding :: CgStgBinding -> BareStgProgram
convertStgBinding (StgNonRec bndr rhs) = [(bndr, rhs)]
convertStgBinding (StgRec ls) = ls

-- we want to store all op applications together,
-- thus adding some tags to process StgOpApp below
type IdName = (String, Maybe Type)
type Args = [Var]

var2IdName v = (showGhc v ++ extractVarDetails v, Just $ varType v)

extractVarDetails :: Var -> String
extractVarDetails v = showGhc (idDetails v)

-- Types for handling code generation later on
-- Heap objects, loosely following standard STG operational semantics
-- 'name' is always the variable to which we are binding the expression
data DotNetObj =
    -- function object
      FUN   { name :: IdName, freeVars :: Args, args :: Args, code :: DotNetExpr}
    -- updatable thunk
    | THUNK { name :: IdName, freeVars :: Args, args :: Args, code :: DotNetExpr}
    -- thunk that only gets called once and can be garbage collected afterwards
    | ONCE  { name :: IdName, freeVars :: Args, args :: Args, code :: DotNetExpr}
     -- constructor application, always saturated
    | CON   { name :: IdName, conName :: String, conTag :: Int, conArgs :: [StgArg]}

-- following BareStgProgram
type DotNetProgram = [DotNetObj]

-- converting closures to heap object representation while stripping unneeded info
-- In GHC 9.6: StgRhsClosure ext ccs updateFlag binders body
-- For CodeGen pass, ext :: DIdSet (free vars)
stgBinding2DotNet :: (Var, CgStgRhs) -> DotNetObj
-- this is probably a function
stgBinding2DotNet (v, (StgRhsClosure fvSet ccs ReEntrant args expr)) =
    FUN   (var2IdName v) (dVarSetElems fvSet) args (stgExpr2DotNetExpr expr)
stgBinding2DotNet (v, (StgRhsClosure fvSet ccs SingleEntry args expr)) =
    ONCE  (var2IdName v) (dVarSetElems fvSet) args (stgExpr2DotNetExpr expr)
stgBinding2DotNet (v, (StgRhsClosure fvSet ccs _flag args expr)) =
    THUNK (var2IdName v) (dVarSetElems fvSet) args (stgExpr2DotNetExpr expr)
stgBinding2DotNet (v, (StgRhsCon ccs dcon _cn _ticks args)) = CON (var2IdName v) (showGhc dcon) (dataConTag dcon) args

stgProgram2DotNetProgram :: BareStgProgram -> DotNetProgram
stgProgram2DotNetProgram p = map stgBinding2DotNet p

-- Expression representation
data DotNetExpr = RAWSTG CgStgExpr -- non implemented conversion yet
    | VAR IdName -- lone Var not being applied to anything, corresponds to App Var [] in Stg
    | LITERAL Literal
    -- various calls
    | FUNCALL IdName [StgArg]
    | PAPCALL IdName [StgArg] -- partial function application (if we know it at compile time)
    | CONCALL IdName Int [StgArg] -- constructor application with tag
    | PRIMOP  IdName [StgArg]
    | PRIMCALL IdName [StgArg]
    | FOREIGNCALL IdName [StgArg]
    -- let bindings as in effect they create a separate isolated piece of code
    | LET DotNetObj DotNetExpr -- let (binding) in (expr)
    | LETREC DotNetProgram DotNetExpr -- same but letrec
    -- Case: only 1 default option, so simple evaluation and return
    -- varName - variable to bind expr to and evaluate (CASE drives evaluation!)
    -- retExpr - expression to return
    | CASEDEFAULT { bndrName :: String, expr :: DotNetExpr, retExpr :: DotNetExpr, altType :: AltType }
    -- casesimple - only 1 case, this means it is used only for pattern matching but there's
    -- in fact no branching needed in the code, so also - evaluation, pattern match, return
    | CASESIMPLE { bndrName :: String, expr :: DotNetExpr, singleCase :: (AltCon, [String], DotNetExpr), altType :: AltType }
    | CASE {bndrName :: String, expr :: DotNetExpr,
            defaultCase :: Maybe DotNetExpr,
            cases :: [(AltCon, [String], DotNetExpr)], altType :: AltType }

-- helper cons checks we need to drive code generation
isACase CASESIMPLE {..} = True
isACase CASEDEFAULT {..} = True
isACase CASE {..} = True
isACase _ = False

isALet (LET _ _) = True
isALet (LETREC _ _) = True
isALet _ = False

isACaseOrLet e = (isACase e) || (isALet e)

stgExpr2DotNetExpr :: CgStgExpr -> DotNetExpr
stgExpr2DotNetExpr (StgLit lit) = LITERAL lit
stgExpr2DotNetExpr (StgApp occ []) = VAR (var2IdName occ)
stgExpr2DotNetExpr (StgApp occ args) = FUNCALL (var2IdName occ) args -- no PAP analysis now!
-- constructor application (gained ConstructorNumber in 9.6)
stgExpr2DotNetExpr (StgConApp dcon _cn args _tps) = CONCALL (showGhc dcon, Nothing) (dataConTag dcon) args
-- primops and foreign calls
stgExpr2DotNetExpr (StgOpApp (StgPrimOp pop) args _tp) = PRIMOP (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgPrimCallOp pop) args _tp) = PRIMCALL (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgFCallOp pop _) args _tp) = FOREIGNCALL (showGhc pop, Nothing) args
-- let bindings (gained extension field in 9.6)
stgExpr2DotNetExpr (StgLet _ext (StgNonRec bndr rhs) expr) = LET (stgBinding2DotNet (bndr,rhs)) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLet _ext (StgRec ls) expr) = LETREC (stgProgram2DotNetProgram ls) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLetNoEscape _ext (StgNonRec bndr rhs) expr) = LET (stgBinding2DotNet (bndr,rhs)) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLetNoEscape _ext (StgRec ls) expr) = LETREC (stgProgram2DotNetProgram ls) (stgExpr2DotNetExpr expr)
-- case - 1 default option (alts are now records in 9.6)
stgExpr2DotNetExpr (StgCase ex bndr altType (GenStgAlt{alt_con=DEFAULT, alt_bndrs=bndrs, alt_rhs=ex1}:[])) =
    CASEDEFAULT (showGhc bndr) (stgExpr2DotNetExpr ex) (stgExpr2DotNetExpr ex1) altType
-- case - 1 option, so driving eval and pattern match but no branching
stgExpr2DotNetExpr (StgCase ex bndr altType (GenStgAlt{alt_con=con, alt_bndrs=bndrs, alt_rhs=expr}:[])) =
    CASESIMPLE (showGhc bndr) (stgExpr2DotNetExpr ex) (con, map showGhc bndrs, stgExpr2DotNetExpr expr) altType
-- general case
stgExpr2DotNetExpr (StgCase ex bndr altType alts) =
    CASE (showGhc bndr) (stgExpr2DotNetExpr ex) (maybeDefault alts) (convertAlts alts) altType
    where maybeDefault (GenStgAlt{alt_con=DEFAULT, alt_rhs=ex}:_) = Just (stgExpr2DotNetExpr ex)
          maybeDefault _ = Nothing
          convertAlts (GenStgAlt{alt_con=DEFAULT}:xs) = convertAlts xs
          convertAlts [] = []
          convertAlts (GenStgAlt{alt_con=con, alt_bndrs=bndrs, alt_rhs=ex}:xs) =
              (con, map showGhc bndrs, stgExpr2DotNetExpr ex):(convertAlts xs)
stgExpr2DotNetExpr e = RAWSTG e



stg2DotNet :: [CgStgTopBinding] -> DotNetProgram
stg2DotNet = stgProgram2DotNetProgram . simplifyStgToBare




--------------------------------------------------------
-- simple show instances converting to text - LEGACY
--------------------------------------------------------
instance Show DotNetObj where
    show (CON v n tag ar) = fst v ++ " = new CON(" ++ show tag ++ ", " ++ showGhc ar ++ ")\n"
    show e@(FUN _ _ _ _) = _s e "FUN"
    show e@(ONCE _ _ _ _) = _s e "THUNK_ONCE"
    show e@(THUNK _ _ _ _) = _s e "THUNK"
-- helper function
_s o con = (fst $ name o) ++ " = new "
            ++ con ++ "("
            ++ showGhc (freeVars o) ++ ", "
            ++ "(" ++ showGhc (args o) ++ ")=> {\n"
            ++ show (code o) ++ "})\n"

instance Show DotNetExpr where
    show (RAWSTG e) = "[NOT IMPLEMENTED] " ++ "(" ++ showGhc e ++ ")\n"
    show (VAR v) = fst v
    show (LITERAL l) = showGhc l
    show (FUNCALL (n,_) args) = n ++ ".CALL" ++ showGhc args ++ "\n"
    show (PAPCALL (n,_) args) = "[PAP]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    show (CONCALL (n,_) tag args) = "new CON(" ++ show tag ++ ", " ++ showGhc args ++ ")\n"
    show (PRIMOP (n,_) args) = "[PRIMOP]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    show (PRIMCALL (n,_) args) = "[PRIMCALL]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    show (FOREIGNCALL (n,_) args) = "[FOREIGN]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    show (LET o e@(LET _ _)) = show o ++ show e
    show (LET o e) = show o ++ "return " ++ show e ++ "\n"
    show (LETREC p e) = "[REC]" ++ show p ++ "return " ++ show e ++ "\n"
    show (CASEDEFAULT n e re _) = "[CASEDEFAULT] " ++ n ++ " = " ++ show e ++ " in " ++ show re
    show (CASESIMPLE n e _ _) = "[CASESIMPLE] " ++ n ++ " = " ++ show e
    show (CASE n e _ cs _) = "[CASE] " ++ n ++ " = " ++ show e ++ " {" ++ show (length cs) ++ " alts}"

type TextProgram = [String]
