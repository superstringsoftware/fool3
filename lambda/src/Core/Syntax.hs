{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Surface language syntax, Expr is generated directly from the Parser

module Core.Syntax
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
  | SmallType -- synonim for Universe 0, since that's the most common type we'll use
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
-- Since everything is either a lambda or a Record in our land, we need a separate type for Lambda as well:
data Lambda = Lambda {
    params :: Record -- {x1:t1 = v1, ..., xn:tn = vn} - supporting default values
  , body   :: Expr -- whatever our lambda is bound to
  , sig    :: Type -- full type signature
  , pred   :: [Pred] -- rank-1 predicates (related to the whole type signature)
} deriving (Show, Eq)


var2field :: Var -> Field
var2field (Var n t) = Field n t EMPTY

vars2record :: [Var] -> Record
vars2record vs = map var2field vs

isTConOrTApp (TCon _)   = True
isTConOrTApp (TApp _ _) = True
isTConOrTApp _          = False

binding2field (Binding (Var nm tp) (Lambda [] EMPTY _ _)) = Field nm tp EMPTY
binding2field (Binding (Var nm tp) lam) = Field nm tp (Lam lam)

-- converts a list of expressions to a record - need it for parsing 
recordFromExprs :: [Expr] -> Record
recordFromExprs exs = map (\ex -> Field "" ToDerive ex) exs

data Expr = 
    VarId Name
  | Lit Literal
  {-
  Supporting the new logic described in the Parser in CoreLambda.

  Binding - binding of a typed identifier with arguments to an expression. Used to model ALL functions, types, typeclasses, constants etc at the top level.
  KEY type for us as it can represent pretty much anything.
  -}
  | Binding Var Lambda
  | Lam Lambda
  {-| 
    Predicates here apply to the whole function, for rank-n need to use HighVar in Type
    "normal" functions expression will be either 'Let' (with in non-EMPTY!) or 'App'
  -}
  | LamOld {
      boundVars  :: Record, -- list of the bound variables, possibly with default values
      lambdaBody :: Expr,   -- body of the function, whatever it is bound to - either App, or LetIns
      lambdaType :: Type,   -- type signature of the lambda
      lamPredicates :: [Pred]
    } 
  | Cons {
      consBoundVars :: Record, -- list of the bound variables that is simultaneously a returning Record!
      consTag  :: ConsTag, -- constructor tag
      consType :: Type -- type this constructor belongs to
    }
  | Rec Record
  | Value {
      valueRecord  :: Record, -- record of the value
      valueConsTag :: ConsTag, -- constructor tag used to create the record
      valueType    :: Type -- type of the record
    }
  | VarDefinition Var -- for standalone type signatures for functions and other symbols, e.g. fact : Int -> Int etc
  | RecordAccess Expr [Name] -- Accessing fields of a record. r.address.city will be recorded as RecordAccess (VarId "r") ["address", "city"]
  | Tuple ConsTag [Expr] Type 
  | Let Field -- simple binding x = g 4 -> (Name="x", Type = ToDerive, Value = App "g" 4)
  | LetIns [Field] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  | App Expr [Expr] -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
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

instance PrettyPrint Field where
  ppr (Field fn ft EMPTY) = fn ++ pprTypeOnly ft
  ppr (Field fn ft (Lam lam)) = fn ++ pprTypeOnly ft ++ " " ++ ppr lam
  ppr (Field fn ft fv) = fn ++ pprTypeOnly ft ++ " = " ++ (ppr fv)

instance PrettyPrint Pred where
  ppr Unconstrained = ""
  ppr (Exists typ) = (as [bold,green] "∃" ++ (ppr typ))  

instance PrettyPrint Lambda where
  ppr (Lambda params body sig pred) = ppr params ++ if (body == EMPTY) then "" else " = " ++ ppr body

instance PrettyPrint [Field] where
  ppr [] = ""
  ppr fs = (showListCuBrSpace ppr fs)

instance PrettyPrint [Pred] where
  ppr [] = ""
  ppr preds = (showListRoBr ppr preds) ++ " => "
  
instance PrettyPrint Literal where
  ppr (LInt i) = as [lmagenta] $ show i
  ppr (LFloat i) = as [lmagenta] $ show i
  ppr (LChar i) = as [lyellow] $ show i
  ppr (LString i) = as [lyellow] $ show i
  ppr (LList []) = "[]" 
  ppr (LList (x:xs)) = foldl fn ("[" ++ ppr x) xs ++ "]"
      where fn acc e = acc ++ ", " ++ ppr e
  ppr (LVec []) = "< >"
  ppr (LVec (x:xs)) = foldl fn ("<" ++ ppr x) xs ++ ">"
      where fn acc e = acc ++ ", " ++ ppr e

instance PrettyPrint Type where
  ppr ToDerive = "(?)"
  ppr (TCon n) = as [bold,lyellow] n
  ppr (TApp con args) = "(" ++ ppr con ++ " " ++ (showListPlain ppr args) ++ ")"
  ppr (TVar n) = ppr n
  ppr (Universe n) = as [bold,lyellow] $ "U" ++ (show n)
  ppr SmallType = as [bold,lyellow] "Type"
  ppr e = show e

pprTypeOnly ToDerive = ""
pprTypeOnly e = ":" ++ ppr e

instance PrettyPrint Var where
  ppr (Var n t) = as [bold] n ++ pprTypeOnly t      

instance PrettyPrint Expr where
  ppr (Lit l) = ppr l
  ppr (VarId v) = v
  ppr (Binding v lam) = ppr v ++ " " ++ ppr lam
  ppr (Lam lam) = ppr lam
  ppr (Rec r) = ppr r
  -- ppr (Let ((v,e):[]) _ ) = ppr v ++ " = " ++ ppr e
  -- ppr (Lam vars expr tp preds) = ppr preds ++ (as [bold,lgray] "λ ") ++ (showListPlain ppr vars) ++ " . " ++ ppr expr
  ppr (BinaryOp n e1 e2) = ppr e1 ++ " " ++ n ++ " " ++ ppr e2
  ppr (UnaryOp n e) = n ++ ppr e
  ppr (Tuple cons exprs typ) = cons ++ (showListCuBr ppr exprs) ++ " : " ++ ppr typ
  ppr (App f args) = (ppr f) ++ " " ++ (showListRoBrPlain ppr args)
  ppr (Patterns ps) = showListCuBr ppr1 ps
      where ppr1 (PatternMatch args e2) = (showListPlain ppr args) ++ " -> " ++ ppr e2
  ppr (PatternMatch args e2) = (showListPlain ppr args) ++ " = " ++ ppr e2
  ppr (ERROR err) = as [bold,yellow] err
  ppr e = show e
  -- λ  