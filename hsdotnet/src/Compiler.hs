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

module Compiler where

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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

instance Show UpdateFlag where
    show ReEntrant = "\\r" 
    show Updatable = "\\u" 
    show SingleEntry = "\\s"

-- showing vars
showVar v = showGhc v ++ showVarDetails v -- showGhc (varName v) ++ " : " ++ showGhc (varType v)
showVarDetails v = showGhc $ idDetails v

showVarList [] = "[]"
--showVarList (v:[]) = "[" ++ showVar v ++ "]"
showVarList (v:vs) = "[" ++ showVar v ++ (foldl (\acc v1 -> acc ++ ", " ++ showVar v1) "" vs) ++ "]"

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
stgProcessBind (StgTopLifted bn) = "EXPR STG Top Lifted:\n" ++ stgProcessGenBinding bn ++ "\nEND EXPR"

{-
StgNonRec bndr (GenStgRhs bndr occ)	 
StgRec [(bndr, GenStgRhs bndr occ)]
bndr are Id here which is an alias for Var, occ is as well???
-}
stgProcessGenBinding (StgNonRec bndr rhs) = stgProcessGenericBinding bndr rhs
stgProcessGenBinding (StgRec ls) = foldl fn "" ls
    where fn acc (b,rhs) = acc ++ stgProcessGenericBinding b rhs ++ "\n"
stgProcessGenericBinding bndr rhs = "[Binder:] " ++ (showGhc $ varName bndr)
    ++ ":" ++ (showGhc $ varType bndr)
    ++ "\n[Expr:]" ++ stgProcessRHS rhs

{-
DataCon: https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/ghc-8.6.3/DataCon.html#t:DataCon
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
stgProcessRHS e@(StgRhsClosure ccs binfo freeVars flag args expr) = "[StgRhsClosure:]\n" 
    ++ "[SHOW GHC]" ++ showGhc e -- stgProcessExpr expr
    ++ "\n[OURS]" ++ showVarList freeVars ++ show flag ++ showVarList args ++ " . " ++ stgProcessExpr expr
stgProcessRHS e@(StgRhsCon ccs dcon args) = "[StgRhsCon:]\n" 
    ++ "[SHOW GHC]"++ showGhc e
    ++ "\n[OURS]" ++ (showGhc $ getName dcon) ++ "(" ++ ") " ++ showGhc args
    -- ++ "\n[CostCenter for above:]" ++ showGhc ccs


stgProcessExpr e@(StgApp oc args) = "App " ++ showGhc e
stgProcessExpr (StgLit l) = showGhc l
stgProcessExpr e@(StgConApp dcon args types) = "ConApp " ++ showGhc e
stgProcessExpr e@(StgOpApp op args tp) = "OpApp " ++ showGhc e
stgProcessExpr e@(StgLam bndrs ex) = "Lam " ++ showGhc e -- bndrs ++ " = " ++ stgProcessExpr ex
stgProcessExpr e = "NOT IMPLEMENTED: " ++ showGhc e
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