{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Simply Typed N-Tuple Calculus machinery!
{- 
`XT = (x1:t1, ..., xn:tn)` - an N-tuple of variables with types. Then,

- Generalized Pi-type: `Pi XT B(subset(XT))`
- Lambda abstraction: `Lam (x1:t1[=v1], ..., xn:tn[=vn]) Expr` - here, we are allowing optional default values for the named parameters (so we will do some sort of memoization instead of currying)
- Application: `App Expr (x1=v1, ..., xn=vn) || (v1,...,vn)` - so, application either to the named tuple or anonymous, and then we treat it in order
-}

module Core.STNTC.Core
where

type Name = String

data Field = Field {
    name :: Name,
    typ :: Type,
    val :: Expr
}

type Tuple a = [a]

data Type = Type Name | FunType (Tuple Field) Type

data Expr = 
    EMPTY
  | Id Name
  | AApp Expr (Tuple Expr)  -- anonymous application
  | NApp Expr (Tuple Field) -- named application, we need to distinguish between the 2, as simplification rules are different
  | Lam (Tuple Field) Expr  -- lambda abstraction, with optional default values

{- 
Some examples:
id x = x
Lam [Field "x" ...] = Id "x"

fact n = rec2(0, n*fact(n-1), n==0) -->
Lam [Field "n" Int EMPTY] (AApp (Id "rec2") [Id "0", AApp (Id "(*)" [n, fact(n-1)]), AApp (Id "(==)" [n,0])]

etc. Seems to work.

-}
