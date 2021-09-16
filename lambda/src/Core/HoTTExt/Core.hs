{-# LANGUAGE OverloadedStrings, NamedFieldPuns, GADTs #-}

-- This is our PRIMARY CORE LANGUAGE, based on HoTT with some changes and extensions, notably - 
-- we are using N-tuples explicitly, but there's more.

module Core.HoTTExt.Core
where

type Name = String

data Var = Var {
    name :: Name,
    typ :: Expr,
    val :: Expr
} deriving (Show, Eq)

type Tuple a = [a]
type Record = [Var]

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] deriving (Eq, Show)

-- Lambda - represents EVERYTHING pretty much (see README in HoTT folder). Its type signature is
-- also obvious from the definition, so it encodes Pi types already!
data Lambda = Lambda {
    implParams :: Record, -- [a1:to1, ...] - optional parameters, usually types
    explParams :: Record -- (x1:t1 = v1, ..., xn:tn = vn) - supporting default values
  , body       :: Expr -- whatever our lambda is bound to
  , lamType    :: Expr -- return type of the function, used in type checking: 
   -- Type for type constructors, Sigma for typeclasses etc, specific type for type constructors and functions etc
} deriving (Show, Eq)

data Expr =
    UNDEFINED -- used basically instead of Nothing in default values etc
  | Id Name
  | Typed Expr Expr -- expression with type
  | Lit Literal
  | U Int -- universe hierarchy
  | Universe -- biggest universe, when we want to refer to any type, kind, etc - see e.g. pifte function!
  | Type -- U 0 synonim
  | Sigma -- for sigma types (typeclasses etc)
  | Pi Lambda -- we need to distinguish between Pi type signature and actual lambdas. So here - ignore Expr.
  | Lam Lambda -- same, defining a function by abstracting a bunch of variables in a tuple
  | App Expr Record Record  
  -- ^^^ in case of anonymous application, `name` fields will be empty or index; 
  -- typ is calculated for type checking. Optional + Explicit params. 
  -- SHOULD WE MAKE ALL PARAMS EXPLICIT IN CORE?!?!?!?!
  | Tuple [Expr] -- a general tuple of expressions
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