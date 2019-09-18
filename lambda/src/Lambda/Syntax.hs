{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module Lambda.Syntax
where

-- testing different lambda machinery - based on currying and based on tuples
type Name = String
type ConsTag = String

data Var = Var Name Type deriving (Show, Eq)
type Binding = (Var, Expr) -- binding of Expr to a Variable Var

data Pred
  = Exists Type -- TApp (TCon name) (TVar name) - e.g., Num a --> TApp (TCon "Num") (TVar "a")
  | Unconstrained -- variables can be anything
  deriving (Show, Eq)

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Name -- TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
  | HighVar Name [Pred] -- for rank-n polymorphism, predicates for a specific type variable
  | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the constructor
  | TApp Type Type -- Constructor application - Maybe Int, List a etc
  | TArr Type Type -- Function sig - Maybe a -> String etc
  | TExpr Expr -- since we will support dependent types, type constructors can be applied to quite complicated expressions
  -- | TForall [Pred] [TVar] Type
  | ToDerive -- added it to handle initial parsing
  -- | TClass Name -- for initial typeclass parsing, might change this
  -- | InsType Expr -- this is probably a workaround - when we are beta-reducing Type lambdas for variables like \a. x:a
  -- "a" needs to be able to become any kind of expression (since we are applying lambdas to expressions).
  -- Type checking etc will fix this.
  deriving (Show, Eq)

data Expr = 
    VarId Name
  | Lit Literal
  | Lam [Var] Expr Type [Pred] -- predicates here apply to the whole function, for rank-n need to use HighVar in Type
  | App Expr Expr
  | Tuple ConsTag [Expr] Type -- polymorphic tuple. 
  | Let [Binding] Expr -- bindings "in" Expr; top level function definitions go here as well
  | PatternMatch Expr Expr -- 1 occurence of pattern match
  | BinaryOp Name Expr Expr
  | ResBinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | EMPTY
  deriving (Show, Eq)

data Literal = LInt !Int | LFloat !Double | LChar !Char |
               LString !String | LList [Expr] | LVec [Expr]
               deriving (Eq, Show)

