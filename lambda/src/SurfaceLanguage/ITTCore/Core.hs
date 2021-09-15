{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

-- Intuitionistic type theory based core language, extended from pairs in Sigma and Pi to n-tuples.

module SurfaceLanguage.ITTCore.Core where

type Name = String
type ConsTag = (String, Int)

-- First, starting with expressions of the classical Calculus of Constructions:
-- ε = V | Kind | Type | (εε) | 𝜆V:ε.ε | ∏V:ε.ε
-- We'll need to extend it to sigma types from ML theory, add inductive types, Sum types etc.
{-
data CoCExpr = 
    CVar Name
  | CKind
  | CType
  | CApp CoCExpr CoCExpr
  | CLam (Name, CoCExpr) CoCExpr
  | CPi (Name, CoCExpr) CoCExpr

-- extending it to "classic" ML:

data MLExpr = 
    MLVar Name
  | MLVoid
  | MLUnit
  | MLU Int -- instead of kind and type
  | MLApp MLExpr MLExpr
  | MLLam (Name, MLExpr) MLExpr
  | MLPi (Name, MLExpr) MLExpr
  | MLSigma (Name, MLExpr) MLExpr
-}
{- 
Based on the above, let's extend it to support tuples and inductive / sum types. Not as clean, but closer 
to reality:

ε = V -- variables and symbols
  | U Nat -- universe hierarchy
  | (εε) -- application
  | (ε1, ...) - N-tuple
  | 𝜆V:ε.ε -- lambda abstraction
  | ∏V:ε.ε -- pi types (generalization of -> )
  | ∑V:ε.ε -- sigma types (generalization of product; ehm, so can THIS be a tuple??? No, because we can't put values - which are applications of constructor functions - in place of v:E)

-}

-- a symbol with type
data Var = Var {
    name :: Name,
    typ :: Expr
} deriving (Show, Eq)

type Tuple a = [a]

-- Finally, our core language expression type:
data Expr = 
    Id Name -- any kind of identifier
  | IdT Var
  | U Int -- Universe types of different levels, so U 0 is a type, U 1 is kind etc
  | App  Expr (Tuple Expr) -- applications ONLY to tuples, since it generalizes 1 expr anyway
  | Lam (Tuple Var) Expr -- abstracting a tuple of Vars from Expr
  | Pi  (Tuple Var) Expr -- functional type from tuple of Vars into Expr
  | Sigma (Tuple Var) -- it's automatically a record? but how do we distinguish between variable names and field names?
  deriving (Show, Eq)

-- now, question is, can we represent type operators here? We should be able to as its COC!!!:
{- 
data Simple a = SimpleCon a -> expands into:
SimpleCon = \a:Type. \x:a. x -> thus, it's a
MLLam ("a",MLU 0) (MLLam ("x", MLVar "a") (MLVar "x"))
-}



{- 
data Pair a b = Pair a b:
Sigma [Var "a" (U 0), Var "b" (U 0))

data Person = Person String Int Date:
Sigma [String, Int, Date]

Semigroup a where (+)::a->a->a:
Sigma [TVar "a" U 0, Pi [TVar "a" U 0, TVar "a" U 0] TVar "a" U 0 ]

data Maybe a = Just a | Nothing

-}