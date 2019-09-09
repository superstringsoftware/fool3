{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module Lambda.Syntax
where

-- testing different lambda machinery - based on currying and based on tuples
type Name = String
type Type = String
type ConsTag = String

data Var = Var Name Type

data Expr = 
    VarId Name
  | Lam    Var Expr Type -- currying based machinery
  | TLam [Var] Expr Type -- tuple based machinery
  | App Expr Expr
  | Tuple ConsTag [Expr] Type -- polymorphic tuple. 
  | Let Var Expr -- binding a var / symbol to an expression

-- Type is an expression itself?? It's merely constructor applications

{-
data Literal = LInt !Int | LFloat !Double | LChar !Char |
               LString !String | LBool !Bool | LList [Expr] | LVec [Expr]
               deriving (Eq, Ord, Show)


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