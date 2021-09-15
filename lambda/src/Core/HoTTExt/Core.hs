{-# LANGUAGE OverloadedStrings, NamedFieldPuns, GADTs #-}

-- This is our PRIMARY CORE LANGUAGE, based on HoTT with some changes and extensions, notably - 
-- we are using N-tuples explicitly, but there's more.

module Core.HoTTExt.Core
where

type Name = String

data Field = Field {
    name :: Name,
    typ :: Expr,
    val :: Expr
} deriving (Show, Eq)

type Tuple a = [a]
type Record = [Field]

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] deriving (Eq, Show)

data Expr =
    UNDEFINED -- used basically instead of Nothing in default values etc
  | Id Name
  | Lit Literal
  | U Int
  | Pi Record Expr 
  -- ^^^ generalization of Pi-types - mapping a tuple of name:type to a type. Each subsequent argument
  -- type may depend on the previous variable here! Of course, "val" is ignored here since we are interested in types only.
  | Lam Record Expr -- same, defining a function by abstracting a bunch of variables in a tuple
  -- Now, application is a bit tricky, we distinguish between anonymous application and named:
  | AApp Expr (Tuple Expr)  -- anonymous application
  | NApp Expr (Tuple Field) -- named application
    deriving (Show, Eq)
