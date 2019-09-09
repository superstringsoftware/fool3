{-# LANGUAGE ScopedTypeVariables #-}

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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- showing vars
showVar v = showGhc v ++ showVarDetails v -- showGhc (varName v) ++ " : " ++ showGhc (varType v)
showVarDetails v = showGhc $ idDetails v

-- Mapping TyThings
processTyThing e@(ATyCon tc) = "TyCon: " ++ showGhc e ++
    "\nVars: " ++ showGhc (tyConTyVars tc) ++
    "\nConstructors: " ++ showGhc (tyConDataCons tc)
    ++ "\n"
processTyThing e@(AnId var) = "Var: " ++ showVar var
processTyThing e = showGhc e

-- Mapping STG

{-
StgTopLifted (GenStgBinding bndr occ)	 
StgTopStringLit bndr ByteString
-}
-- stgProcessBind :: GenStgTopBinding b o -> String
stgProcessBind (StgTopStringLit _ bs) = "STG Top String Literal: " ++ show bs
stgProcessBind (StgTopLifted bn) = "STG Top Lifted:\n" ++ stgProcessGenBinding bn

{-
StgNonRec bndr (GenStgRhs bndr occ)	 
StgRec [(bndr, GenStgRhs bndr occ)]
bndr are Id here which is an alias for Var
-}
stgProcessGenBinding (StgNonRec bndr rhs) = "[Binder:]" ++ (showGhc $ varName bndr)
     ++ ":" ++ (showGhc $ varType bndr)
     ++ "\n[Expr:]\n" ++ stgProcessRHS rhs
-- stgProcessGenBinding (StgRec bndr rhs) = stgProcessRHS rhs

{-
StgRhsClosure CostCentreStack StgBinderInfo [occ] !UpdateFlag [bndr] (GenStgExpr bndr occ)	 
StgRhsCon CostCentreStack DataCon [GenStgArg occ]
DataCon: https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/ghc-8.6.3/DataCon.html#t:DataCon
data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Literal

  occ is now Id which is Var
-}
stgProcessRHS e@(StgRhsClosure ccs bi occs flag bndrs expr) = "[StgRhsClosure:]\n" ++ showGhc e -- stgProcessExpr expr
stgProcessRHS (StgRhsCon ccs dcon args) = "StgRhsCon: " ++ showGhc dcon ++ showGhc args
    -- ++ "\n[CostCenter for above:]" ++ showGhc ccs

stgProcessExpr e@(StgApp oc args) = "App " ++ showGhc e
stgProcessExpr (StgLit l) = showGhc l
stgProcessExpr e@(StgConApp dcon args types) = "ConApp " ++ showGhc e
stgProcessExpr e@(StgOpApp op args tp) = "OpApp " ++ showGhc e
stgProcessExpr e@(StgLam bndrs ex) = "Lam " ++ showGhc e -- bndrs ++ " = " ++ stgProcessExpr ex
stgProcessExpr e = "NOT IMPLEMENTED: " ++ showGhc e
{-
StgApp occ [GenStgArg occ]	 
StgLit Literal	 
StgConApp DataCon [GenStgArg occ] [Type]	 
StgOpApp StgOp [GenStgArg occ] Type	 
StgLam (NonEmpty bndr) StgExpr	 
StgCase (GenStgExpr bndr occ) bndr AltType [GenStgAlt bndr occ]	 
StgLet (GenStgBinding bndr occ) (GenStgExpr bndr occ)	 
StgLetNoEscape (GenStgBinding bndr occ) (GenStgExpr bndr occ)	 
StgTick (Tickish bndr) (GenStgExpr bndr occ)
-}