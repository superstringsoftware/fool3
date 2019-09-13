{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving #-}

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

module CompilerText where

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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

instance Show UpdateFlag where
    show ReEntrant = "\\r" 
    show Updatable = "\\u" 
    show SingleEntry = "\\s"

-- showing vars
showVar v = showGhc v ++ showVarDetails v -- showGhc (varName v) ++ " : " ++ showGhc (varType v)
showVarDetails v = showGhc $ idDetails v

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
stgProcessBind :: GenStgTopBinding Var Var -> SM String
stgProcessBind e@(StgTopStringLit bndr bs) = 
    modify (\s -> s {isTopLevel = True}) >> 
    return ("// " ++ (showGhc $ varName bndr) ++ " :: " 
                  ++ (showGhc $ varType bndr) ++ "\n"
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
    s1 <- stab ("// " ++ (showGhc $ varName bndr) ++ " :: " 
                ++ (showGhc $ varType bndr) ++ "\n")
    s2 <- stab("var " ++ (showGhc $ varName bndr) ++ " = " ++ rhsRes
                ++ "\n")
    return (s1 ++ s2)

-- filtering out some rhs we don't want
{-
stgProcessGenBindingFiltered filt (StgNonRec bndr rhs) = stgProcessGenericBindingFiltered filt bndr rhs
stgProcessGenBindingFiltered filt (StgRec ls) = foldl fn "[REC]\n" ls ++ "[ENDREC]"
    where fn acc (b,rhs) = acc ++ stgProcessGenericBindingFiltered filt b rhs ++ "\n"
stgProcessGenericBindingFiltered filt bndr rhs = if (filt rhs) then stgProcessGenericBinding bndr rhs else ""
-}

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
    
    for closures, we don't care about cost center (do we?), binder info is interesting:
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

type IdName = String
-- Type for handling code generation later on
data DotNetExpr = DNLit Literal -- literal
    | FUN IdName [GenStgArg Var]
    | PAP IdName [GenStgArg Var]
    | CON IdName [GenStgArg Var] -- constructor application to a list of 
    | THUNK 
    | NOTIMPLEMENTED (GenStgExpr Var Var)
    -- deriving (Eq)

stgExpr2dnExpr :: GenStgExpr Var Var -> SM DotNetExpr
-- application: now simply converting to FUN, but have to detect PAPs where possible
-- as well as oversaturated applications (?)
stgExpr2dnExpr (StgApp oc args) = pure $ FUN (showGhc $ varName oc) args
-- literals go in as is
stgExpr2dnExpr (StgLit l) = pure $ DNLit l
-- constructor applications are guaranteed to be saturated so this is easy
stgExpr2dnExpr (StgConApp dcon args types) = pure $ CON (showGhc $ dataConName dcon) args
stgExpr2dnExpr e = return (NOTIMPLEMENTED e)

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
stgProcessExpr (StgOpApp op args tp) = return (show op ++ processGenStgArgs args)
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