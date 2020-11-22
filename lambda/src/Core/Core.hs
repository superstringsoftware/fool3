{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Surface language syntax, Expr is generated directly from the Parser
-- Re-designing the core language from the ground up with fewer entities based on the records logic

module Core.Core
where

import Util.PrettyPrinting
-- import Core.Syntax

type Name = String
type ConsTag = (String, Int)

-- Any variable, can be a type variable or "real" variable, they all differ in their type only
data Var = Var {
    varName  :: Name
  , varType  :: Type
  , varValue :: Expr
 } deriving (Show, Eq)

type SimpleRecord = [Var] 
data Record = Record {
          rec     :: [Var]
        , recType :: Type
        , consTag :: ConsTag 
      } deriving (Show, Eq)

simpleRecord2Record :: SimpleRecord -> Record
simpleRecord2Record r = Record {
    rec = r, recType = ToDerive, consTag = ("",0)
}

record2SimpleRecord :: Record -> SimpleRecord
record2SimpleRecord r = rec r


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
  | TFunSig Record Type -- alternative way to store function signatures - {x1:t1, ..., xn:tn} -> Type. 
  -- ^^^ Might be BETTER for our purposes, as we want better documentation etc, and here we are storing variable names as additional information.
  -- converting between TArr and TFunSig should be a no-brainer.
  -- | TForall [Pred] [TVar] Type
  | THasFields [Var] -- special "constraint" type, for situations such as "print r.name + " " + show r.age" - then deduced type will be HasFields (name:String, age:exists Show a)
  | ToDerive -- added it to handle initial parsing
  | SmallType -- synonim for Universe 0, since that's the most common type we'll use
  | Universe Int -- for types, following the Martin-Lof theory - "Universe 0" is "small types", so regular types etc.
  | TClass -- typeclass or type family
  deriving (Show, Eq)


data Lambda = Lambda {
    params  :: SimpleRecord -- {x1:t1 = v1, ..., xn:tn = vn} - supporting default values
  , body    :: Expr -- whatever our lambda is bound to
  , lamType :: Type -- return type of the function
  , preds   :: [Pred] -- rank-1 predicates (related to the whole type signature)
} deriving (Show, Eq)

-- Application of an expression to the arguments - making it a separate type for better typechecking
data Application  = Application Expr SimpleRecord deriving (Show, Eq)
data PatternMatch = PatternMatch Application Expr deriving (Show, Eq)

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] deriving (Eq, Show)

data Expr = 
    VarId Name
  | Lit Literal
  {-
  Supporting the new logic described in the Parser in CoreLambda.

  Binding - binding of a typed identifier with arguments to an expression. Used to model ALL functions, types, typeclasses, constants etc at the top level.
  KEY type for us as it can represent pretty much anything.

  On the top level, only Binding, PatternMatch and VarDefinition (which is used for declaring type signatures mostly for the functions) should exist!
  -}
  | Binding Var Lambda
  | Lam Lambda
  | Rec SimpleRecord -- generic record used to pass things around
  | Value Record -- The ONLY object that is a value object in our land (well, except Lit for primitives)
  | VarDefinition Var -- for standalone type signatures for functions and other symbols, e.g. fact : Int -> Int etc
  | RecordAccess [Expr] -- Accessing fields of a record. r.address.city will be recorded as RecordAccess (VarId "r") ["address", "city"]
  | LetIns [Var] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  | App Application -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
  -- 1 occurence of pattern match
  -- since we are NOT repeating function name on the left side of the pattern match but put ONLY arguments there,
  -- first [Expr] here is simply a list of expressions to which lambda bound variables need to be evaluated!!!
  -- second expression is the normal expression as usual
  | Patterns [PatternMatch] -- only PatternMatch should be inside
  | Prim Name -- primitive operation or function
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | EMPTY
  | ERROR String -- used only in Interpreter
  deriving (Show, Eq)