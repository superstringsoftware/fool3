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
  | U Int -- universe hierarchy
  | Pi Record Expr 
  -- ^^^ generalization of Pi-types - mapping a tuple of name:type to a type. Each subsequent argument
  -- type may depend on the previous variable here! Of course, "val" is ignored here since we are interested in types only.
  | Lam Record Expr -- same, defining a function by abstracting a bunch of variables in a tuple
  | App Expr Record  -- in case of anonymous application, `name` fields will be empty or index; typ is calculated for type checking
  | Sigma Record -- generalization of a Sigma type
    deriving (Show, Eq)


{- 
We probably won't use Lambda in Core, since we'll desugar impl and expl into one record?

data Lambda = Lambda {
    implParams :: Record, -- [a1:to1, ...] - optional parameters, usually types
    explParams :: Record -- (x1:t1 = v1, ..., xn:tn = vn) - supporting default values
  , body       :: Expr -- whatever our lambda is bound to
  , lamType    :: Expr -- return type of the function, used in type checking
} deriving (Show, Eq)

-}