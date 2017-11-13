{-# LANGUAGE OverloadedStrings #-}

module Core where

import TermColors

type Name = String

-- Intermediate explicity typed IR
data DExpr
  = DLit Literal
  | DVar Name -- for bound variables and functions???
  | DLam Var Type DExpr
  | DApp DExpr DExpr
    deriving (Eq, Ord, Show)

data Literal = LInt !Int | LFloat !Double | LChar !Char deriving (Eq, Ord, Show)

data Var
  = Id Name Type
  | TyVar Name Kind
  deriving (Show, Eq, Ord)

getVarName (Id n _) = n
getVarName (TyVar n _) = n

data Type
  = TVar Name -- TVar
  | TCon Name -- TyCon
  | TApp Type Type
  | TArr Type Type
  | TForall [Pred] [TVar] Type
  | ToDerive -- added it to handle initial parsing
  | TStar -- what if we are doing fully dependent types - then there should be no difference between Kinds and types,
    -- we will represent KStar as TStar, then * -> * would be TApp TStar TStar, but at the same time we can build different
    -- crazy functions that can take types, values etc arguments and construct any kind of stuff.
  deriving (Show, Eq, Ord)

data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name
  deriving (Show, Eq, Ord)

data TyCon
  = AlgTyCon { tyId :: Name }
  | PrimTyCon { tyId :: Name }
  deriving (Show, Eq, Ord)

data Pred
  = IsIn Name Type
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Type Variables
-------------------------------------------------------------------------------

data TVar = TV
  { tvName   :: Name
  } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- Alpha Equivalence
-------------------------------------------------------------------------------

class Alpha a where
  aeq :: a -> a -> Bool

instance Alpha TVar where
  aeq _ _ = True

instance Alpha Type where
  aeq (TVar _) (TVar _)     = True
  aeq (TApp a b) (TApp c d) = aeq a c && aeq b d
  aeq (TArr a b) (TArr c d) = aeq a c && aeq b d
  aeq (TCon a) (TCon b)     = a == b
  aeq _ _                   = False

instance Alpha Kind where
  aeq KStar KStar = True
  aeq KPrim KPrim = True
  aeq (KArr a b) (KArr c d) = aeq a c && aeq b d
  aeq _ _ = False

-------------------------------------------------------------------------------
-- Pretty Print typeclass
-------------------------------------------------------------------------------
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Type where
  prettyPrint (TVar nm) = nm
  prettyPrint (TCon nm) = as [yellow, bold] nm
  prettyPrint ToDerive  = as [dgray, bold] "?"
  prettyPrint (TApp t1 t2) = prettyPrint t1 ++ " " ++ prettyPrint t2
  prettyPrint e = show e
{-
  | TArr Type Type
  | TForall [Pred] [TVar] Type
-}

instance PrettyPrint Var where
  prettyPrint (Id nm tp)   = nm ++ ":" ++ prettyPrint tp
  prettyPrint (TyVar nm k) = nm ++ ":" ++ prettyPrint k

instance PrettyPrint Kind where
  prettyPrint KStar = "*"
  prettyPrint k = show k
