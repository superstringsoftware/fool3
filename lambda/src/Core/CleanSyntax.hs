{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Surface language syntax, Expr is generated directly from the Parser

module Core.CleanSyntax
where

import Util.PrettyPrinting
-- import Core.Syntax

type Name = String
type ConsTag = String

-- Any variable, can be a type variable or "real" variable, they all differ in their type only
data Var = Var Name Type deriving (Show, Eq)
-- binding of Expr to a Variable Var
type Binding = (Var, Expr) 

varName v = fst v
varType v = snd v

-- Predicates can and will be extended
data Pred
  = Exists Type -- TApp (TCon name) (TVar name) - e.g., Num a --> TApp (TCon "Num") (TVar "a")
  | Unconstrained -- variables can be anything
  deriving (Show, Eq)

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Var -- TVar -- a:U etc, type with constraints 
  | HighVar Name [Pred] -- for rank-n polymorphism, predicates for a specific type variable -- DO WE NEED IT?
  | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the type
  -- Constructor application - Maybe Int, List a etc; as it should be always saturated tuple is a nobrainer
  -- Ok, this now gets tricky as we can have types dependent on values, so we can apply it not just to the type but to the literals etc
  -- E.g., if we have Vector a:U n:Int and we have a call to some calculation within the type:
  -- Vector Int (4+x) -- means we need to be able to parse expressions inside type applications
  | TApp Type [Type] -- due to the above, we may want to store BOTH types and expressions in ONE type. The only exception would be Arrows and Universe hierarchy?
  | TExpr Expr -- or this would resolve the issue above??? think through.
  -- the above would probably work - we would record Vector Int (4+x) something like - TApp (TCon "Vector") [TCon "Int", TExpr (BinaryOp "+" 2 "x")]
  -- so, should be OK, yes!
  | TArr Type Type -- Function sig - Maybe a -> String etc
  | TFunSig Record Type -- alternative way to store function signatures - {x1:t1, ..., xn:tn} : Type. 
  -- ^^^ Might be BETTER for our purposes, as we want better documentation etc, and here we are storing variable names as additional information.
  -- converting between TArr and TFunSig should be a no-brainer.
  -- | TForall [Pred] [TVar] Type
  | ToDerive -- added it to handle initial parsing
  | Universe Int -- for types, following the Martin-Lof theory - "Universe 0" is "small types", so regular types etc.
  deriving (Show, Eq)

-- Record is the representation of any record or tuple (with names being absent) - a list of typed field names, nothing more
type Record = [(Name,Type)]

data Expr = 
    VarId Name
  | Lit Literal
  {-| 
    Predicates here apply to the whole function, for rank-n need to use HighVar in Type
    if a function 'Expr' is a 'Tuple' - it's a data constructor!!!
    "normal" functions expression will be either 'Let' (with in non-EMPTY!) or 'App'
  -}
  | Lam [Var] Expr Type [Pred] 
  | App Expr [Expr] -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
  -- polymorphic tuple representing values - DATA CONSTRUCTORS return it, also used in TYPECLASSES
  -- "Normal" functions have something else as their expression
  -- Typeclasses should have Class in their type, even if we use subtyping <: lingo eventually
  | Tuple ConsTag [Expr] Type 
  | Let [Binding] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  -- 1 occurence of pattern match
  -- since we are NOT repeating function name on the left side of the pattern match but put ONLY arguments there,
  -- first [Expr] here is simply a list of expressions to which lambda bound variables need to be evaluated!!!
  -- second expression is the normal expression as usual
  | PatternMatch [Expr] Expr 
  | Patterns [Expr] -- only PatternMatch should be used inside here, it's only used inside lambda with patterns!!!
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | EMPTY
  | ERROR String -- used only in Interpreter
  deriving (Show, Eq)


data Literal = LInt !Int | LFloat !Double | LChar !Char |
               LString !String | LList [Expr] | LVec [Expr]
               deriving (Eq, Show)