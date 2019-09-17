{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards #-}

{-
This has a Compiler State Monad and converts STG program to intermediary DotNet types.
Actual code gen happens elsewhere (CSharpGen for C#, eventually something else for IL).

Compilation sequence goes in phases:
https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/ghc-8.6.3/src/DriverPipeline.html

Actual Haskell compilation happens here:
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (RealPhase (Hsc src_flavour)) input_fn dflags0

there's a lot of interesting initializations that's worth understanding and then
    actual compilation happens in hscIncrementalCompile:
    http://hackage.haskell.org/package/ghc-8.6.5/docs/src/HscMain.html


-}

module Compiler where

import GHC

import DynFlags
import Outputable
import HscTypes
import CorePrep
import CoreToStg
import SimplStg
import Literal

import CoreSyn
import StgSyn
import TyCon

import Var
import Name (nameStableString)
import Kind
import IdInfo

import TyCoRep -- Type data type (??)

--import UniqDSet
import DataCon

import PrimOp
import ForeignCall

import Control.Monad.State
import Data.List.Index (imapM, ifind)
import Data.Foldable (foldlM)

-- FIRST, SOME GHC HELPERS
showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

showVarType v = (showGhc $ varType v)
showVarName v = showGhc (varName v)

-- http://hackage.haskell.org/package/ghc-8.6.5/docs/DataCon.html#t:DataCon
-- read deconstruction for better data con representation
showDataCon dcon = showGhc dcon -- (showGhc $ getName dcon) -- ++ "`" ++ (showGhc $ dataConTag dcon)   
showDataConList [] = "{}"
showDataConList (dc:dcs) = "{" ++ showDataCon dc ++ 
    (foldl (\acc d -> acc ++ ", " ++ showDataCon d) "" dcs) ++ "}"
stgShowDCon = showDataCon -- legacy

-- showing vars
showVar v = showGhc v ++ showVarDetails v -- showGhc (varName v) ++ " : " ++ showGhc (varType v)
showVarDetails v = showGhc $ idDetails v

-- tyConName :: TyCon -> Name
    -- tyConKind :: TyCon -> Kind
    -- tyConTyVars :: TyCon -> [TyVar]
    -- tyConDataCons :: TyCon -> [DataCon]
-- TODO: http://hackage.haskell.org/package/ghc-8.6.5/docs/TyCon.html#
showTyCon tc = showGhc (tyConName tc) ++ "(" ++ showGhc (tyConTyVars tc) ++ ") : " 
    ++ showGhc (tyConKind tc) ++
    " = { " ++ showDataConList (tyConDataCons tc) ++ " }"

-- NOW OUR COMPILER MONAD
data CompilerState = CompilerState {
    stringAlign :: Int,
    isTopLevel :: Bool,
    -- when we are creating FUN / THUNKs with free vars, need this to convert names to field numbers in the C# class
    currentFreeVars :: Args 
}

-- state monad for code generation
-- most code gen actions happen in Expr -> SM String actions
type SM = State CompilerState

initialCompilerState = CompilerState {
    stringAlign = 0,
    isTopLevel = True,
    currentFreeVars = []
}

checkTopLevel :: SM Bool
checkTopLevel = get >>= \s -> pure $ (stringAlign s) == 0

pushFreeVars :: Args -> SM ()
pushFreeVars fv = modify (\s -> s {currentFreeVars = fv})

popFreeVars :: SM ()
popFreeVars = modify (\s -> s {currentFreeVars = []})

readFreeVars :: SM Args
readFreeVars = get >>= pure . currentFreeVars

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
-- Top level literals - don't loook like we need them in the compilation now
isTopLevelLiteral :: GenStgTopBinding Var Var -> Bool
isTopLevelLiteral (StgTopStringLit _ _) = True
isTopLevelLiteral _ = False
-- top level constructor applications to what looks like some helpful type / kind manipulation stuff which 
-- we also probably don't need, at least initially
isHelperConApp :: GenStgTopBinding Var Var -> Bool
isHelperConApp (StgTopLifted (StgNonRec bndr _)) = elem (showVarType bndr) helperCons
    where helperCons = ["KindRep", "TrName", "TyCon"]
isHelperConApp _ = False
-- all filters combined
allStgTopBindingFilters b = (isTopLevelLiteral b) || (isHelperConApp b)

-- We are stripping going down the GenStgTopBinding --> GenStgBinding hierarchy,
-- then killing the difference between REC and NONREC (may need it in the future, but not now???)
-- and simply representing the program as the list of bindings from Var to GenStgRhs
type BareStgProgram = [(Var, GenStgRhs Var Var)]
-- function that does this simplification, then we can simply call stgProcessGenericBinding to create the program
simplifyStgToBare :: [GenStgTopBinding Var Var] -> BareStgProgram
simplifyStgToBare [] = []
simplifyStgToBare (x:xs) = if (allStgTopBindingFilters x)
        then simplifyStgToBare xs else (convertTopBinding x) ++ (simplifyStgToBare xs)
        
convertTopBinding :: GenStgTopBinding Var Var -> BareStgProgram
convertTopBinding (StgTopLifted x) = convertStgBinding x

-- need this separately as this is used in let expressions!
convertStgBinding :: GenStgBinding Var Var -> BareStgProgram
convertStgBinding (StgNonRec bndr rhs) = [(bndr, rhs)]
convertStgBinding (StgRec ls) = ls

-- we want to store all op applications together, 
-- thus adding some tags to process StgOpApp below
-- data VarType = GHCType Type | DATACON | PRIMOP | PRIMCALL | FOREIGN              
type IdName = (String, Maybe Type)
type Args = [Var]

-- var2IdName v = (showVarName v, Just $ varType v)
var2IdName v = (showGhc v ++ extractVarDetails v, Just $ varType v)

extractVarInfo v = showGhc $ arityInfo (idInfo v)
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
    | CON   { name :: IdName, conName :: String, conArgs :: [GenStgArg Var]}
    -- deriving (Eq)

-- following BareStgProgram
type DotNetProgram = [DotNetObj]

-- converting closures to heap object representation while stripping unneeded info
stgBinding2DotNet :: (Var, GenStgRhs Var Var) -> DotNetObj 
-- this is probably a function 
stgBinding2DotNet (v, (StgRhsClosure ccs binfo freeVars ReEntrant args expr)) = 
    FUN   (var2IdName v) freeVars args (stgExpr2DotNetExpr expr)
stgBinding2DotNet (v, (StgRhsClosure ccs binfo freeVars SingleEntry args expr)) = 
    ONCE  (var2IdName v) freeVars args (stgExpr2DotNetExpr expr)    
stgBinding2DotNet (v, (StgRhsClosure ccs binfo freeVars flag args expr)) = 
    THUNK (var2IdName v) freeVars args (stgExpr2DotNetExpr expr)    
stgBinding2DotNet (v, (StgRhsCon ccs dcon args)) = CON (var2IdName v) (stgShowDCon dcon) args

stgProgram2DotNetProgram :: BareStgProgram -> DotNetProgram
stgProgram2DotNetProgram p = map stgBinding2DotNet p

-- Expression representation
{-
StgCase (GenStgExpr bndr occ) bndr AltType [GenStgAlt bndr occ]	 
StgLet (GenStgBinding bndr occ) (GenStgExpr bndr occ)	 
StgLetNoEscape (GenStgBinding bndr occ) (GenStgExpr bndr occ)	 
StgTick (Tickish bndr) (GenStgExpr bndr occ)
-}
data DotNetExpr = RAWSTG (GenStgExpr Var Var) -- non implemented conversion yet
    | VAR IdName -- lone Var not being applied to anything, corresponds to App Var [] in Stg
    | LITERAL Literal
    -- various calls
    | FUNCALL IdName [GenStgArg Var] 
    | PAPCALL IdName [GenStgArg Var] -- partial function application (if we know it at compile time)
    | CONCALL IdName [GenStgArg Var] -- constructor application, more or less equal to CON in Heap Objects
    | PRIMOP  IdName [GenStgArg Var]
    | PRIMCALL IdName [GenStgArg Var]
    | FOREIGNCALL IdName [GenStgArg Var]
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

stgExpr2DotNetExpr :: GenStgExpr Var Var -> DotNetExpr
stgExpr2DotNetExpr (StgLit lit) = LITERAL lit
stgExpr2DotNetExpr (StgApp occ []) = VAR (var2IdName occ)
stgExpr2DotNetExpr (StgApp occ args) = FUNCALL (var2IdName occ) args -- no PAP analysis now!
-- constructor application
stgExpr2DotNetExpr (StgConApp dcon args tp) = CONCALL (stgShowDCon dcon, Nothing) args
-- primops and foreign calls
stgExpr2DotNetExpr (StgOpApp (StgPrimOp pop) args tp) = PRIMOP (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgPrimCallOp pop) args tp) = PRIMCALL (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgFCallOp pop _) args tp) = FOREIGNCALL (showGhc pop, Nothing) args
-- let bindings
stgExpr2DotNetExpr (StgLet (StgNonRec bndr rhs) expr) = LET (stgBinding2DotNet (bndr,rhs)) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLet (StgRec ls) expr) = LETREC (stgProgram2DotNetProgram ls) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLetNoEscape (StgNonRec bndr rhs) expr) = LET (stgBinding2DotNet (bndr,rhs)) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLetNoEscape (StgRec ls) expr) = LETREC (stgProgram2DotNetProgram ls) (stgExpr2DotNetExpr expr)
-- case - 1 default option
stgExpr2DotNetExpr (StgCase ex bndr altType ((DEFAULT, bndrs, ex1):[]) ) = 
    CASEDEFAULT (showGhc bndr) (stgExpr2DotNetExpr ex) (stgExpr2DotNetExpr ex1) altType
-- case - 1 option, so driving eval and pattern match but no branching
stgExpr2DotNetExpr (StgCase ex bndr altType ((con, bndrs, expr):[]) ) = 
    CASESIMPLE (showGhc bndr) (stgExpr2DotNetExpr ex) (con, map showGhc bndrs, stgExpr2DotNetExpr expr) altType
-- general case
stgExpr2DotNetExpr (StgCase ex bndr altType alts) = 
    CASE (showGhc bndr) (stgExpr2DotNetExpr ex) (maybeDefault alts) (convertAlts alts) altType
    where maybeDefault ((DEFAULT, bndrs, ex):_) = Just (stgExpr2DotNetExpr ex)
          maybeDefault _ = Nothing
          convertAlts ((DEFAULT, bndrs, ex):xs) = convertAlts xs
          convertAlts [] = []
          convertAlts ((con,bndrs, ex):xs) = (con, map showGhc bndrs, stgExpr2DotNetExpr ex):(convertAlts xs)
stgExpr2DotNetExpr e = RAWSTG e



stg2DotNet :: [GenStgTopBinding Var Var] -> DotNetProgram
stg2DotNet = stgProgram2DotNetProgram . simplifyStgToBare

    


--------------------------------------------------------
-- simple show instances converting to text - LEGACY
--------------------------------------------------------
instance Show DotNetObj where
    show (CON v n ar) = fst v ++ " = new " ++ n ++ showGhc ar ++ "\n"
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
    show (CONCALL (n,_) args) = "new " ++ n ++ showGhc args ++ "\n"
    show (PRIMOP (n,_) args) = "[PRIMOP]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    show (PRIMCALL (n,_) args) = "[PRIMCALL]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    show (FOREIGNCALL (n,_) args) = "[FOREIGN]" ++ n ++ ".CALL" ++ showGhc args ++ "\n"
    -- let .. in let - is a separate case, don't need "return" there!!!
    show (LET o e@(LET _ _)) = show o ++ show e 
    show (LET o e) = show o ++ "return " ++ show e ++ "\n"
    show (LETREC p e) = "[REC]" ++ show p ++ "return " ++ show e ++ "\n"

type TextProgram = [String]     

