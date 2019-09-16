{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards #-}

{-
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

data CompilerState = CompilerState {
    stringAlign :: Int,
    isTopLevel :: Bool
}

-- state monad for code generation
-- most code gen actions happen in Expr -> SM String actions
type SM = State CompilerState

initialCompilerState = CompilerState {
    stringAlign = 0,
    isTopLevel = True
}

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

var2IdName v = (showVarName v, Just $ varType v)

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
    -- Program - used to desconstruct let bindings as in effect they create a separate isolated piece of code
    | LET DotNetObj DotNetExpr -- let (binding) in (expr)  
    | LETREC DotNetProgram DotNetExpr -- same but letrec
    -- the (relatively) easy part is done, now we need to deconstruct and convert CASE and LET, which get represented
    -- by different low-level constructs

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

stgExpr2DotNetExpr :: GenStgExpr Var Var -> DotNetExpr
stgExpr2DotNetExpr (StgLit lit) = LITERAL lit
stgExpr2DotNetExpr (StgApp occ []) = VAR (var2IdName occ)
stgExpr2DotNetExpr (StgApp occ args) = FUNCALL (var2IdName occ) args -- no PAP analysis now!
stgExpr2DotNetExpr (StgConApp dcon args tp) = CONCALL (stgShowDCon dcon, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgPrimOp pop) args tp) = PRIMOP (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgPrimCallOp pop) args tp) = PRIMCALL (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgOpApp (StgFCallOp pop _) args tp) = FOREIGNCALL (showGhc pop, Nothing) args
stgExpr2DotNetExpr (StgLet (StgNonRec bndr rhs) expr) = LET (stgBinding2DotNet (bndr,rhs)) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLet (StgRec ls) expr) = LETREC (stgProgram2DotNetProgram ls) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLetNoEscape (StgNonRec bndr rhs) expr) = LET (stgBinding2DotNet (bndr,rhs)) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr (StgLetNoEscape (StgRec ls) expr) = LETREC (stgProgram2DotNetProgram ls) (stgExpr2DotNetExpr expr)
stgExpr2DotNetExpr e = RAWSTG e

stg2DotNet :: [GenStgTopBinding Var Var] -> DotNetProgram
stg2DotNet = stgProgram2DotNetProgram . simplifyStgToBare

-- silly textual representation for testing purposes mostly (OLD)
type TextProgram = [String]              
-- convert bare to text
bareToTextProgram :: BareStgProgram -> SM TextProgram
bareToTextProgram bsp = mapM (\(v, rhs)-> stgProcessGenericBinding v rhs) bsp
-- convert incoming stg to text
stgToText :: [GenStgTopBinding Var Var] -> TextProgram
stgToText stgp = evalState ((bareToTextProgram . simplifyStgToBare) stgp) initialCompilerState

---------------------------------------------------------------
-- OLDER CONVERT TO TEXT STUFF USED FOR UNDERSTANDING TYPES ETC
---------------------------------------------------------------

stgProcessBind :: GenStgTopBinding Var Var -> SM String
stgProcessBind e@(StgTopStringLit bndr bs) = 
    modify (\s -> s {isTopLevel = True}) >> 
    return ("// [TOP STRING LITERAL] " ++ showVarWithType bndr ++ "\n"
                  ++ (showGhc $ varName bndr) ++ " = " ++ show bs)
    -- return ("STG Top String Literal: " ++ showGhc e)
-- stgProcessBind (StgTopLifted bn) = stgProcessGenBinding bn ++ "\n"
stgProcessBind (StgTopLifted bn) = 
    modify (\s -> s {isTopLevel = True} ) >> stgProcessGenBinding bn
-- stgProcessBind (StgTopLifted bn) = stgProcessGenBindingFiltered filterOutCons bn ++ "\n"

{-
StgNonRec bndr (GenStgRhs bndr occ)	 
StgRec [(bndr, GenStgRhs bndr occ)]
bndr are Id here which is an alias for Var, occ is as well???
-}
stgProcessGenBinding :: GenStgBinding Var Var -> SM String
stgProcessGenBinding (StgNonRec bndr rhs) = stgProcessGenericBinding bndr rhs
stgProcessGenBinding (StgRec ls) = do
    st <- get
    let fn acc (b,rhs) = acc ++ evalState (stgProcessGenericBinding b rhs) st
    return (foldl fn "[REC]\n" ls ++ "[ENDREC]")
    

-- stgProcessGenericBinding :: GenStgBinding Var Var -> SM String
stgProcessGenericBinding bndr rhs = do 
    rhsRes <- stgProcessRHS rhs
    s1 <- stab ("// " ++ showVarWithType bndr ++ "\n")
    s2 <- stab("var " ++ (showGhc $ varName bndr) ++ " = " ++ rhsRes
                ++ "\n")
    return (s1 ++ s2)

{-
DataCon: https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/ghc-8.6.3/DataCon.html#t:DataCon
 -- opaque time, follow the link to get deconstruction functions

data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Literal

  occ is now Id which is Var

 data GenStgRhs bndr occ
  = StgRhsClosure
        CostCentreStack         -- CCS to be attached (default is CurrentCCS)
        StgBinderInfo           -- Info about how this binder is used (see below)
        [occ]                   -- non-global free vars; a list, rather than
                                -- a set, because order is important
        !UpdateFlag             -- ReEntrant | Updatable | SingleEntry
        [bndr]                  -- arguments; if empty, then not a function;
                                -- as above, order is important.
        (GenStgExpr bndr occ)   -- body

  | StgRhsCon
        CostCentreStack  -- CCS to be attached (default is CurrentCCS).
                         -- Top-level (static) ones will end up with
                         -- DontCareCCS, because we don't count static
                         -- data in heap profiles, and we don't set CCCS
                         -- from static closure.
        DataCon          -- Constructor. Never an unboxed tuple or sum, as those
                         -- are not allocated.
        [GenStgArg occ]  -- Args

    with constructors it's easy, simply application of the constructor to it's arguments.
    
For closures, we don't care about cost center (do we?), binder info is interesting:
data StgBinderInfo
  = NoStgBinderInfo
  | SatCallsOnly        -- All occurrences are *saturated* *function* calls
                        -- This means we don't need to build an info table and
                        -- slow entry code for the thing
                        -- Thunks never get this value
    ^^^ maybe helpful to optimize code but can ignore for now
    For update flags:
    A @ReEntrant@ closure may be entered multiple times, but should not be
updated or blackholed. An @Updatable@ closure should be updated after
evaluation (and may be blackholed during evaluation). A @SingleEntry@
closure will only be entered once, and so need not be updated but may
safely be blackholed.
data UpdateFlag = ReEntrant | Updatable | SingleEntry
-}
-- ReEntrant closure is most likely a function
stgProcessRHS :: GenStgRhs Var Var -> SM String
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars ReEntrant args expr) = do 
    exprRes <- stgProcessExpr expr
    return (
        "\n/* free Vars: " 
        ++ showVarList freeVars ++ " */\n"
        ++ showVarList args
        ++ " => {\n" ++ exprRes ++ "} ")
-- this would be a THUNK
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars Updatable args expr) = do
    exprRes <- stgProcessExpr expr
    return ("new THUNK (" 
        ++ showVarList freeVars ++ ", "
        ++ showVarList args
        ++ " => {\n" ++ exprRes ++ "} );")
    -- keeping other closures as is for now
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars flag args expr) = do
    exprRes <- stgProcessExpr expr
    return ("[CLOSURE]" ++ 
        showVarList freeVars ++ show flag ++ showVarList args 
        ++ " . " ++ exprRes)
stgProcessRHS e@(StgRhsCon ccs dcon args) = return ("new " ++ stgShowDCon dcon ++ processGenStgArgs args)
    -- ++ "\n[CostCenter for above:]" ++ showGhc ccs

filterOutCons (StgRhsClosure ccs binfo freeVars flag args expr) = True
filterOutCons (StgRhsCon ccs dcon args) = False

instance Show PrimOp where show = showGhc
instance Show PrimCall where show = showGhc
instance Show ForeignCall where show = showGhc
deriving instance Show StgOp



-- function application
stgProcessExpr ::  GenStgExpr Var Var -> SM String
stgProcessExpr (StgApp oc args) = do 
    let ret = showVar oc ++ processGenStgArgs args
    return ret
stgProcessExpr (StgLit l) = return $ showGhc l
-- constructor application
stgProcessExpr (StgConApp dcon args types) = return (
    "new " ++ stgShowDCon dcon ++ processGenStgArgs args) -- ++ " [TYPES]" ++ showGhc types
-- operator application
stgProcessExpr e@(StgOpApp op args tp) = return $ showGhc e -- (show op ++ processGenStgArgs args)
-- apparently this is only used during transformation, so safely to ignore?
-- stgProcessExpr e@(StgLam bndrs ex) = return ("[Lam]" ++ showGhc e) -- bndrs ++ " = " ++ stgProcessExpr ex
-- case forces evaluation!!!
-- (altCon, bndrs, ex)
-- special case of only one default alternative - we DO NOT need switch statement in this case
stgProcessExpr (StgCase ex bndr altType ((DEFAULT, bndrs, ex1):[]) ) = do
    exprRes <- stgProcessExpr ex
    exprRes1 <- stgProcessExpr ex1
    s1 <- stab ("// ONE DEFAULT CASE: ALT TYPE: " ++ showGhc altType ++ "\n")
    s2 <- stab ("var " ++ showGhc bndr ++ " = " ++ "EVAL (" ++ exprRes ++ ");\n")
    s3 <- stab ("return " ++ exprRes1 ++ ";")
    return (s1 ++ s2 ++ s3)
stgProcessExpr (StgCase ex bndr altType alts) = do
    exprRes <- stgProcessExpr ex
    s1 <- stab ("// CASE: ALT TYPE: " ++ showGhc altType ++ "\n")
    s2 <- stab ("var " ++ showGhc bndr ++ " = " ++ "EVAL (" ++ exprRes ++ ");\n")
    s3 <- stab ("switch (" ++ showGhc bndr ++ ") {\n")
    s4 <- stab "}"
    incTab
    altsRes <- processCaseAlts alts
    decTab
    return (s1 ++ s2 ++ s3 ++ altsRes ++ "\n" ++ s4)
stgProcessExpr e@(StgLet binding expr) = do
    exprRes <- stgProcessExpr expr
    modify (\s -> s {isTopLevel = False} )
    bRes <- stgProcessGenBinding binding
    return ("\n" ++ bRes ++ "\nreturn " ++ exprRes)
stgProcessExpr e = return ("[NOT IMPLEMENTED]" ++ showGhc e)

processCaseAlts alts = foldM processCaseAlt "" alts
processCaseAlt acc (altCon, bndrs, ex) = do
    s1 <- stab (processAltCon altCon bndrs ++ ":\n")
    incTab
    exprRes <- stgProcessExpr ex
    decTab
    s2 <- stab (exprRes ++ "\n")
    return (acc ++ s1 ++ s2)


{-
data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Literal
-}
processGenStgArgs [] = ""
processGenStgArgs (x:[]) = ".CALL(" ++ processGenStgArg x ++ ")"
processGenStgArgs (x:xs) = ".CALL(" ++ processGenStgArg x ++
    foldl (\acc v -> acc ++ ", " ++ processGenStgArg v) "" xs ++ ")"

processGenStgArg (StgLitArg  lit) = showGhc lit
processGenStgArg (StgVarArg  occ) = showVar occ

processAltCon DEFAULT _ = "default"
processAltCon (LitAlt lit) _ = "case " ++ showGhc lit
processAltCon (DataAlt dc) bndrs = "case " ++ showGhc dc ++ " " ++ showVarList bndrs


showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

instance Show UpdateFlag where
    show ReEntrant = "\\r" 
    show Updatable = "\\u" 
    show SingleEntry = "\\s"

{-
Var Type is Kind which is Type which is here:
http://hackage.haskell.org/package/ghc-8.6.5/docs/Type.html#t:Type (opaque type)
data Type
  -- See Note [Non-trivial definitional equality]
  = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a coercion variable)

  | AppTy
        Type
        Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --     See Note [Respecting definitional equality] (EQ1) about the
                        --     no 'CastTy' requirement
                        --
                        --  2) Argument type

  | TyConApp
        TyCon
        [KindOrType]    -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
                        -- Invariant: saturated applications of 'FunTyCon' must
                        -- use 'FunTy' and saturated synonyms must use their own
                        -- constructors. However, /unsaturated/ 'FunTyCon's
                        -- do appear as 'TyConApp's.
                        -- Parameters:
                        --
                        -- 1) Type constructor being applied to.
                        --
                        -- 2) Type arguments. Might not have enough type arguments
                        --    here to saturate the constructor.
                        --    Even type synonyms are not necessarily saturated;
                        --    for example unsaturated type synonyms
                        --    can appear as the right hand side of a type synonym.

  | ForAllTy
        !TyVarBinder
        Type            -- ^ A Π type.

  | FunTy Type Type     -- ^ t1 -> t2   Very common, so an important special case

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  | CastTy
        Type
        KindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never refl.
                      -- INVARIANT: The Type is not a CastTy (use TransCo instead)
                      -- See Note [Respecting definitional equality] (EQ2) and (EQ3)

  | CoercionTy
        Coercion    -- ^ Injection of a Coercion into a type
                    -- This should only ever be used in the RHS of an AppTy,
                    -- in the list of a TyConApp, when applying a promoted
                    -- GADT data constructor
-}

showType :: Type -> String
showType (TyConApp tc kots) = "[TyConApp]" ++ showGhc tc ++ " " ++ showGhc kots
showType t = "[NOT IMPLEMENTED] " ++ showGhc t

-- showing vars
showVar v = showGhc v ++ showVarDetails v -- showGhc (varName v) ++ " : " ++ showGhc (varType v)
showVarDetails v = showGhc $ idDetails v

showVarType v = (showGhc $ varType v)
showVarName v = showGhc (varName v)

showVarWithType bndr = (showGhc $ varName bndr) ++ " :: " ++ (showGhc $ varType bndr)
-- showVarWithType v = (showGhc $ varName v) ++ " :: " ++ (showType $ varType v)

showVarList [] = "()"
--showVarList (v:[]) = "[" ++ showVar v ++ "]"
showVarList (v:vs) = "(" ++ showVar v ++ (foldl (\acc v1 -> acc ++ ", " ++ showVar v1) "" vs) ++ ")"

-- Mapping TyThings
processTyThing e@(ATyCon tc) = "TyCon: " ++ showGhc e ++
    "\nVars: " ++ showGhc (tyConTyVars tc) ++
    "\nConstructors: " ++ showGhc (tyConDataCons tc)
    ++ "\n"
processTyThing e@(AnId var) = "Var: " ++ showVar var
processTyThing e = showGhc e

-- Mapping STG
-- http://hackage.haskell.org/package/ghc-lib-8.8.1/docs/StgSyn.html#t:GenStgRhs
{-
stg2stg is the final pass that gives us a list of
type StgTopBinding = GenStgTopBinding Vanilla -- vanilla is simply a pass tag

but then we have annTopBindingsFreeVars :: [StgTopBinding] -> [CgStgTopBinding] - should we call it?
it's in /compiler/stgSyn/StgFVs.hs


-}

{-
-- | A top-level binding.
data GenStgTopBinding pass
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted (GenStgBinding pass)
  | StgTopStringLit Id ByteString

data GenStgBinding pass
  = StgNonRec (BinderP pass) (GenStgRhs pass)
  | StgRec    [(BinderP pass, GenStgRhs pass)]
-}
{-

https://downloads.haskell.org/~ghc/8.6.5/docs/html/libraries/ghc-8.6.5/StgSyn.html
StgApp occ [GenStgArg occ]	 
StgLit Literal	 
StgConApp DataCon [GenStgArg occ] [Type]	 
StgOpApp StgOp [GenStgArg occ] Type	 
StgLam (NonEmpty bndr) StgExpr	 
StgCase (GenStgExpr bndr occ) bndr AltType [GenStgAlt bndr occ]	 
StgLet (GenStgBinding bndr occ) (GenStgExpr bndr occ)	 
StgLetNoEscape (GenStgBinding bndr occ) (GenStgExpr bndr occ)	 
StgTick (Tickish bndr) (GenStgExpr bndr occ)

type GenStgAlt bndr occ = (AltCon, [bndr], GenStgExpr bndr occ) Source#

data AltCon Source# =
    DataAlt DataCon	 
    LitAlt Literal	-- A literal: case e of { 1 -> ... } Invariant: always an *unlifted* literal See Note [Literal alternatives]
    DEFAULT	-- Trivial alternative: case e of { _ -> ... }

-- important for handling CASE statements!!!
data AltType
  = PolyAlt             -- Polymorphic (a lifted type variable)
  | MultiValAlt Int     -- Multi value of this arity (unboxed tuple or sum)
                        -- the arity could indeed be 1 for unary unboxed tuple
                        -- or enum-like unboxed sums
  | AlgAlt      TyCon   -- Algebraic data type; the AltCons will be DataAlts
  | PrimAlt     PrimRep -- Primitive data type; the AltCons (if any) will be LitAlts


Important types
http://hackage.haskell.org/package/ghc-prim-0.5.3/docs/src/GHC.Types.html

-- Show instance for TyCon found in GHC.Show
data TyCon = TyCon WORD64_TY WORD64_TY   -- Fingerprint
                   Module                -- Module in which this is defined
                   TrName                -- Type constructor name
                   Int#                  -- How many kind variables do we accept?
                   KindRep               -- A representation of the type's kind

data TrName
  = TrNameS Addr#  -- Static
  | TrNameD [Char] -- Dynamic
  
-- | The representation produced by GHC for conjuring up the kind of a
-- 'TypeRep'.  See Note [Representing TyCon kinds: KindRep] in TcTypeable.
data KindRep = KindRepTyConApp TyCon [KindRep]
             | KindRepVar !KindBndr
             | KindRepApp KindRep KindRep
             | KindRepFun KindRep KindRep
             | KindRepTYPE !RuntimeRep
             | KindRepTypeLitS TypeLitSort Addr#
             | KindRepTypeLitD TypeLitSort [Char] 

-- | A de Bruijn index for a binder within a 'KindRep'.
type KindBndr = Int             

data TypeLitSort = TypeLitSymbol
                 | TypeLitNat

data Module = Module
                TrName   -- Package name
                TrName   -- Module name                 
-}

    -- tyConName :: TyCon -> Name
    -- tyConKind :: TyCon -> Kind
    -- tyConTyVars :: TyCon -> [TyVar]
    -- tyConDataCons :: TyCon -> [DataCon]
-- TODO: http://hackage.haskell.org/package/ghc-8.6.5/docs/TyCon.html#
showTyCon tc = showGhc (tyConName tc) ++ "(" ++ showGhc (tyConTyVars tc) ++ ") : " 
    ++ showGhc (tyConKind tc) ++
    " = { " ++ showDataConList (tyConDataCons tc) ++ " }"

-- http://hackage.haskell.org/package/ghc-8.6.5/docs/DataCon.html#t:DataCon
-- read deconstruction for better data con representation
showDataCon dcon = (showGhc $ getName dcon) -- ++ "`" ++ (showGhc $ dataConTag dcon)   
showDataConList [] = "{}"
showDataConList (dc:dcs) = "{" ++ showDataCon dc ++ 
    (foldl (\acc d -> acc ++ ", " ++ showDataCon d) "" dcs) ++ "}"
stgShowDCon = showDataCon -- legacy