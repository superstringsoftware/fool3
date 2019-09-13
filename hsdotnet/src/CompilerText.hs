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

Original module attempting to convert STG directly to pseudo-dotnet.
More of a learning experience.
Moved to intermediate types.
-}

module CompilerText where

import GHC

import DynFlags
import Outputable
import HscTypes
import CorePrep
import CoreToStg
import SimplStg

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
-- stgProcessBind :: GenStgTopBinding b o -> String
stgProcessBind (StgTopStringLit _ bs) = "STG Top String Literal: " ++ show bs
-- stgProcessBind (StgTopLifted bn) = stgProcessGenBinding bn ++ "\n"
stgProcessBind (StgTopLifted bn) = stgProcessGenBinding bn ++ "\n"
-- stgProcessBind (StgTopLifted bn) = stgProcessGenBindingFiltered filterOutCons bn ++ "\n"

{-
StgNonRec bndr (GenStgRhs bndr occ)	 
StgRec [(bndr, GenStgRhs bndr occ)]
bndr are Id here which is an alias for Var, occ is as well???
-}
stgProcessGenBinding (StgNonRec bndr rhs) = stgProcessGenericBinding bndr rhs
stgProcessGenBinding (StgRec ls) = foldl fn "[REC]\n" ls ++ "[ENDREC]"
    where fn acc (b,rhs) = acc ++ stgProcessGenericBinding b rhs ++ "\n"
stgProcessGenericBinding bndr rhs = "// " ++ (showGhc $ varName bndr) ++ " :: " 
    ++ (showGhc $ varType bndr) ++ "\n"
    ++ "var " ++ (showGhc $ varName bndr) ++ " = " ++ stgProcessRHS rhs

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
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars ReEntrant args expr) = "\n/* free Vars: " 
    ++ showVarList freeVars ++ " */\n"
    ++ showVarList args
    ++ " => {\n" ++ stgProcessExpr expr ++ "} "
-- this would be a THUNK
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars Updatable args expr) = 
    "new THUNK (" 
    ++ showVarList freeVars ++ ", "
    ++ showVarList args
    ++ " => {\n" ++ stgProcessExpr expr ++ "} );"
    -- keeping other closures as is for now
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars flag args expr) = "[CLOSURE]" ++ 
    showVarList freeVars ++ show flag ++ showVarList args ++ " . " ++ stgProcessExpr expr
stgProcessRHS e@(StgRhsCon ccs dcon args) = "new " ++ stgShowDCon dcon ++ processGenStgArgs args
    -- ++ "\n[CostCenter for above:]" ++ showGhc ccs

filterOutCons (StgRhsClosure ccs binfo freeVars flag args expr) = True
filterOutCons (StgRhsCon ccs dcon args) = False

instance Show PrimOp where show = showGhc
instance Show PrimCall where show = showGhc
instance Show ForeignCall where show = showGhc
deriving instance Show StgOp

-- function application
stgProcessExpr (StgApp oc args) = showVar oc ++ " " ++ processGenStgArgs args
stgProcessExpr (StgLit l) = showGhc l
-- constructor application
stgProcessExpr (StgConApp dcon args types) = "new " ++ stgShowDCon dcon ++ processGenStgArgs args -- ++ " [TYPES]" ++ showGhc types
-- operator application
stgProcessExpr (StgOpApp op args tp) = show op ++ processGenStgArgs args
-- apparently this is only used during transformation, so safely to ignore?
stgProcessExpr e@(StgLam bndrs ex) = "[Lam]" ++ showGhc e -- bndrs ++ " = " ++ stgProcessExpr ex
-- case forces evaluation!!!
stgProcessExpr (StgCase ex bndr altType alts) = "var " ++ showGhc bndr ++ " = " 
    ++ "EVAL (" ++ stgProcessExpr ex ++ ");\n"
    ++ "switch (" ++ showGhc bndr ++ ")"
    ++ processCaseAlts alts
stgProcessExpr e@(StgLet binding expr) = "[Let]" ++ stgProcessGenBinding binding
    ++ " [IN] " ++ stgProcessExpr expr
stgProcessExpr e = "NOT IMPLEMENTED" -- ++ showGhc e

processCaseAlts alts = foldl processCaseAlt "{\n" alts ++ "}"
processCaseAlt acc (altCon, bndrs, ex) = acc
    ++ processAltCon altCon bndrs ++ ":\n"
    ++ stgProcessExpr ex ++ "\n"


{-
data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Literal
-}
processGenStgArgs [] = "()"
processGenStgArgs (x:[]) = "(" ++ processGenStgArg x ++ ")"
processGenStgArgs (x:xs) = "(" ++ processGenStgArg x ++
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