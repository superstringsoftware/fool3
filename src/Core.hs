{-# LANGUAGE OverloadedStrings #-}

module Core where

type Name = String

data EExpr
  = EInt !Int
  | EVar Name -- for bound variables and functions???
  | ELam Var EExpr
  | EApp EExpr EExpr
    deriving (Eq, Ord, Show)

tInt = TCon "Int" -- PrimTyCon {tyId = "Int"}

-- * x x
simple = (EApp (EApp (EVar "primMult") (EInt 2)) (EInt 2))
square = ELam (Id "x" tInt)  (EApp (EApp (EVar "primMult") (EVar "x")) (EVar "x"))

program = EApp (EVar "square") (EInt 4)


eval :: EExpr -> EExpr
eval (EInt x) = EInt x -- int can't be evaluated further
eval (EVar "square") = square -- substituting call to square to actual expression
eval (EApp (EApp (EVar "primMult") (EInt x)) (EInt y)) = EInt (x*y) -- calculating primitive multiplication on ints
eval (EApp e1 e2) = EApp (eval e1) (eval e2)

eval (EApp (ELam (Id n _) e) s) = s -- this is the trickiest - need to traverse the tree and build a new tree where each occurence of n in e is substituted for s

--fact = ELam (Id "n" tInt) (EApp (EVar "primMult") (EInt 4))

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
