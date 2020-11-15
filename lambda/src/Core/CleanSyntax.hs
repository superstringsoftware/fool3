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
-- type Binding = (Var, Expr) 

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
  | TFunSig Record Type -- alternative way to store function signatures - {x1:t1, ..., xn:tn} -> Type. 
  -- ^^^ Might be BETTER for our purposes, as we want better documentation etc, and here we are storing variable names as additional information.
  -- converting between TArr and TFunSig should be a no-brainer.
  -- | TForall [Pred] [TVar] Type
  | THasFields [Field] -- special "constraint" type, for situations such as "print r.name + " " + show r.age" - then deduced type will be HasFields (name:String, age:exists Show a)
  | ToDerive -- added it to handle initial parsing
  | Universe Int -- for types, following the Martin-Lof theory - "Universe 0" is "small types", so regular types etc.
  | TClass -- typeclass or type family
  deriving (Show, Eq)

-- Record is the representation of any record or tuple (with names being absent) - a list of typed field names, nothing more
data Field = Field {
    fieldName  :: Name, -- name of the field
    fieldType  :: Type, -- type of the field
    fieldValue :: Expr  -- value of the field (can be EMPTY)
} deriving (Show, Eq)
type Record      = [Field]
type TypedRecord = (Record, Type) -- record is the list of Fields and a Type of the record itself

data Expr = 
    VarId Name
  | Lit Literal
  {-| 
    Predicates here apply to the whole function, for rank-n need to use HighVar in Type
    "normal" functions expression will be either 'Let' (with in non-EMPTY!) or 'App'
  -}
  | Lam {
      boundVars  :: Record, -- list of the bound variables, possibly with default values
      lambdaBody :: Expr,   -- body of the function, whatever it is bound to - either App, or LetIns
      lambdaType :: Type,   -- type signature of the lambda
      predicates :: [Pred]
    } 
  | Cons {
      consBoundVars :: Record, -- list of the bound variables that is simultaneously a returning Record!
      consTag  :: ConsTag, -- constructor tag
      consType :: Type -- type this constructor belongs to
    }
  | Value {
      valueRecord  :: Record, -- record of the value
      valueConsTag :: ConsTag, -- constructor tag used to create the record
      valueType    :: Type -- type of the record
    }
  | RecordAccess Expr [Name] -- Accessing fields of a record. r.address.city will be recorded as RecordAccess (VarId "r") ["address", "city"]
  | Let Field -- simple binding x = g 4 -> (Name="x", Type = ToDerive, Value = App "g" 4)
  | LetIns [Field] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  | App Expr Record -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
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

{-|
    Ok, our Expr type needs to play nicely with all scenarious we can run into in different surface languages, which are reasonably typed and functional. 
    So this section is needed for the sanity check.
    
    These are (let's just use Haskell for now):
    
    - Data declaration
    data Maybe a = Nothing | Just a:
    "Nothing" -> Cons [] "Nothing" `Maybe a` -- so, a function with no vars that returns an empty Tuple of type Maybe a
    "Just"    -> Cons ["":a] "Just" `a -> Maybe a` [] -- type of the function can be easily deduced from its vars and a tuple type
    
    - Typeclasses are arguably the most interesting case at this point:
    class Semigroup a where (+) :: a -> a -> a
    instance Semigroup Int where (+) = +#
    instance Semigroup Float where (+) = +f#

    So we have a definition of typeclass and 2 instances. How do we treat it?
    Ok, Semigroup itself can be recorded as a Lambda that generates a Record of function headers and type definitions.
    But then we need to create a Function Factory in our environment:
    FFactory(+) = \a:U . {...} :: a -> a -> a -- which returns specific functions depending on the type given to it.
    See Environment for this!
    Then instance definition adds a corresponding record to a factory!
    When we are adding - CHECK THE LAWS! (randomization etc)
    (+) x:a y:a -> z:a
    4:int + 3:int -- instantiating a to int, how do we ask the factory for the correct function? -> it has to find the type from the type of the arguments.
    How? By the full signature maybe??
    3.4:float + 3.2:float

    length ls:List a -> Int 
-}