{-# LANGUAGE OverloadedStrings, NamedFieldPuns, GADTs #-}

-- This is our PRIMARY CORE LANGUAGE, based on HoTT with some changes and extensions, notably - 
-- we are using N-tuples explicitly, but there's more.

module Core
where

import Util.PrettyPrinting

type Name = String

data Var = Var {
    name :: Name,
    typ :: Expr,
    val :: Expr
} deriving (Show, Eq)

-- type Tuple a = [a]
type Record = [Var]

-- data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] deriving (Eq, Show)

-- Lambda - represents EVERYTHING pretty much (see README in HoTT folder). Its type signature is
-- also obvious from the definition, so it encodes Pi types already!
data Lambda = Lambda {
    lamName    :: Name
  , params     :: Record -- (x1:t1 = v1, ..., xn:tn = vn) - supporting default values
  , body       :: Expr -- whatever our lambda is bound to
  , lamType    :: Expr -- return type of the function, used in type checking: 
   -- Type for type constructors, Sigma for typeclasses etc, specific type for type constructors and functions etc
} deriving (Show, Eq)



data Expr =
    UNDEFINED -- used basically instead of Nothing in default values etc
  | Id Name
  | Typed Expr Expr -- expression with type
  | Binding Var -- Var contains both the name and the expression to be bound to, used inside Actions etc
  | Function Lambda -- defining a function by abstracting a bunch of variables in a tuple
  | Action Lambda -- Action is simply a list of expressions in order
  | App Expr [Expr] -- application
  | PatternMatch Expr Expr -- pattern match
  | PatternMatches [Expr] -- only PatternMatch is allowed, need to distinquish with generic tuple
  | Tuple [Expr] -- any tuple { ... , ... }, 
  | SumType Lambda -- sum type definition, which is also basically a lambda with 
  -- body expression being a tuple of Lambdas which are constructors
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | Type -- U 0 synonim

  -- | U Int -- universe hierarchy
  -- | Universe -- biggest universe, when we want to refer to any type, kind, etc - see e.g. pifte function!
  
  
  -- ^^^ in case of anonymous application, `name` fields will be empty or index; 
  -- typ is calculated for type checking. Optional + Explicit params. 
    deriving (Show, Eq)



-- --------------------------------- PRETTY PRINTING --------------------------------------------

instance PrettyPrint Lambda where
  ppr (Lambda name params body sig) = name ++ " " 
    ++ showListRoBr ppr params 
    ++ pprTyp sig
    ++ if (body == UNDEFINED) then "" else " = " ++ ppr body

pprTyp ex = if (ex == UNDEFINED) then "" else ":" ++ ppr ex

instance PrettyPrint Var where
  ppr (Var n t _) = as [bold] n ++ if (t == UNDEFINED) then "" else ":" ++ ppr t

instance PrettyPrint Expr where
  ppr UNDEFINED = ""
  ppr (Id v) = as [bold] v
  ppr (PatternMatch e1 e2) = ppr e1 ++ " -> " ++ ppr e2
  ppr (PatternMatches ps) = showListCuBr ppr ps
  ppr (App e ex) = (ppr e) ++ showListRoBr ppr ex
  ppr (Tuple ex) = showListCuBr ppr ex
  ppr (Binding (Var nm tp val)) = as [bold] nm ++ pprTyp tp ++ " = " ++ ppr val 
  ppr e = show e
  -- λ  