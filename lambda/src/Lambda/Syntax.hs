{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module Lambda.Syntax
where

-- testing different lambda machinery - based on currying and based on tuples
type Name = String
type ConsTag = String

data Var = Var Name Type deriving (Show, Eq)
type Binding = (Var, Expr) -- binding of Expr to a Variable Var

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Name -- TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
  | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the constructor
  | TApp Type Type -- Constructor application - Maybe Int, List a etc
  | TArr Type Type -- Function sig - Maybe a -> String etc
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
  | Lam [Var] Expr Type -- tuple based machinery
  | App Expr Expr
  | Tuple ConsTag [Expr] Type -- polymorphic tuple. 
  | Let [Binding] Expr -- bindings "in" Expr; top level function definitions go here as well
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | EMPTY
  deriving (Show, Eq)


data Literal = LInt !Int | LFloat !Double | LChar !Char |
               LString !String | LList [Expr] | LVec [Expr]
               deriving (Eq, Show)

{-

-- Surfance language AST type to handle both lazy and strict hopefully a bit more efficiently
-- So, no currying
data Expr 
    = Lit Literal
    | VarId Name
    | Type Name [Var] [Expr] -- sum type built from product constructors, which are Lambdas themselves!
    | BinaryOp Name Expr Expr
    | UnaryOp  Name Expr
    | Tuple Name [Expr] -- polymorphic tuple. 
    | Record Name [(Name, Expr)] -- polymorphic record
    | NewRecord Name [Var] -- polymorphic record
    | App Expr Expr
    | Case Expr [(Expr, Expr)] -- case: which expr we are inspecting, alternatives
    | Lam Name [Var] Expr Type -- typed function (or any typed expression for that matter)
    -- class 
    | Typeclass     Name [Pred] [Var] [Expr] -- Expr here can only be Lam as it's a list of functions basically, where interface is just an empty expression
    | Typeinstance  Name [Pred] Type  [Expr] -- for parsing typelcass instances - Name is typeclass name, Type is the type and [Expr] are all Lambdas
    | EMPTY
    deriving (Eq, Ord, Show)
-}