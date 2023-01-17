{-# LANGUAGE OverloadedStrings, NamedFieldPuns, GADTs #-}

-- This is our PRIMARY CORE LANGUAGE, based on HoTT with some changes and extensions, notably - 
-- we are using N-tuples explicitly, but there's more.

module Surface
where

import Util.PrettyPrinting
import Logs

type Name = String

data Var = Var {
    name :: Name,
    typ :: Expr,
    val :: Expr
} deriving (Show, Eq)

-- type Tuple a = [a]
type Record = [Var]
-- using this cover type in the arguments for lambdas
-- to take care of the dependent functions with implicit arguments
-- they are usually created from "structures" (typeclasses),
-- but may be defined explicitly as well, e.g.:
-- structure Show(a:Type) { function show(x:a):String } =>
-- show [a:Type] (x:a) -- where [a:Type] is implicit first record
data LamArgs = LamArgs Record | LamArgsImpl Record Record

-- constructor tag placeholder type
data ConsTag = ConsTag Name !Int deriving (Show, Eq)

-- this is "naive" arity as it does not take into account functions
-- that return other functions as a result
arity :: Lambda -> Int
arity (Lambda _ args _ _) = length args

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] | LTuple [Expr] deriving (Eq, Show)

-- Lambda - represents EVERYTHING pretty much (see README in HoTT folder). Its type signature is
-- also obvious from the definition, so it encodes Pi types already!
data Lambda = Lambda {
    lamName    :: Name
  , params     :: Record -- (x1:t1 = v1, ..., xn:tn = vn) - supporting default values
  , body       :: Expr -- whatever our lambda is bound to
  , lamType    :: Expr -- return type of the function, used in type checking: 
   -- Type for type constructors, Sigma for typeclasses etc, specific type for type constructors and functions etc
} deriving (Show, Eq)

-- lambda is only a constructor if its body is a tuple and it has a clear
-- return type!
isLambdaConstructor :: Lambda -> Bool
isLambdaConstructor (Lambda _ _ (Tuple _) tp) = if (tp == UNDEFINED) then False else True
isLambdaConstructor _ = False

data Expr =
    UNDEFINED -- used basically instead of Nothing in default values etc
  | Id Name
  | Lit Literal
  | Typed Expr Expr -- expression with type
  | Binding Var -- Var contains both the name and the expression to be bound to, used inside Actions etc
  | Function Lambda -- defining a function by abstracting a bunch of variables in a tuple
  | Action Lambda -- Action is simply a list of expressions in order
  | Constructors [Lambda] -- only for constructor list inside sum types
  | Structure Lambda [Name] -- storing type classes / type families etc.
  -- the body of lambda is a list of functions / items that are part of the 
  -- structure
  -- second [Name] list is a list of functions mandatory for the implementation
  | App Expr [Expr] -- application
  | ExprConsTagCheck ConsTag Expr -- check if Expr was created with a given constructor
  | RecFieldAccess (Name,Int) Expr -- access a field of the Expr by name or index
  | CaseOf Record Expr SourceInfo -- in the course of optimizations we transfer pattern matches
  -- to the case x of val -> expr statements. However, since we prefer lists vs trees
  -- in our implementation, we are actually combining function variables with
  -- the expressions they have to match in the Record, e.g.:
  -- f(x:t1,y:t2) = { {a,b} -> expr } turns into:
  -- CaseOf [Var "x" t1 a, Var "y" t2 b] expr!
  -- this way we can try if else, case of etc approaches depending on the 
  -- compilation target
  
  | ExpandedCase [Expr] Expr SourceInfo -- this is what CaseOf gets converted
  -- into in the course of optimizations and expansions - first part 
  -- is simply a list of comparisons of Expr to specific ConsTag or a value eventually,
  -- ALL of them need to be True for the case to work.
  
  | PatternMatches [Expr] -- only CaseOf or ExpandedCase is allowed inside this!!!
  | Tuple [Expr] -- any tuple { ... , ... }
  | ConTuple ConsTag [Expr] -- only CONSTRUCTORS return it - tagged by the constructor tag!
  | Statements [Expr] -- for Action body, simply a list of statements to execute in order
  | SumType Lambda -- sum type definition, which is also basically a lambda with 
  -- body expression being a tuple of Lambdas which are constructors
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | Type -- U 0 synonim
  | Prim Lambda -- primitive function that is handled "magically"
  | PrimCall -- filler for the body of primitive functions
  | Implicit Expr -- current solution for implicit parameter functions
  -- that result e.g. from structure (typeclass) expansions -
  -- only used in the TYPE place!!!
  | ERROR String

  -- | U Int -- universe hierarchy
  -- | Universe -- biggest universe, when we want to refer to any type, kind, etc - see e.g. pifte function!
  
  
  -- ^^^ in case of anonymous application, `name` fields will be empty or index; 
  -- typ is calculated for type checking. Optional + Explicit params. 
    deriving (Show, Eq)

{-
We will also handle types via Expr quite easily:
- Id Name - concrete
- App Expr [Expr] - type constructor (Maybe a etc) or a function calculating type
- Function Lambda - arrow type
-}

-- function that generates built-in operation to access i-th field
-- in a given tuple
mkTupleFieldAccessExpr :: Int -> Expr -> Expr
mkTupleFieldAccessExpr i e = RecFieldAccess ("",i) e

-- non-monadic traverse
traverseExpr :: (Expr -> Expr) -> Expr -> Expr
traverseExpr f UNDEFINED = UNDEFINED
traverseExpr f e@(Id name) = f e
traverseExpr f e@(Typed e1 e2) = Typed (f $ traverseExpr f e1) (f $ traverseExpr f e2)
traverseExpr f (App ex exs) = App (f $ traverseExpr f ex) (map f (map (traverseExpr f) exs) )
traverseExpr f (CaseOf args ex si) = CaseOf args (f $ traverseExpr f ex) si
traverseExpr f (PatternMatches exs) = PatternMatches (map f (map (traverseExpr f) exs))
traverseExpr f (Lit (LList exs)) = Lit (LList (map f (map (traverseExpr f) exs)))
traverseExpr f (Lit (LVec exs)) = Lit (LVec (map f (map (traverseExpr f) exs)))
traverseExpr f (Lit (LTuple exs)) = Lit (LTuple (map f (map (traverseExpr f) exs)))
traverseExpr f (Lit e) = Lit e
traverseExpr f (RecFieldAccess a ex) = RecFieldAccess a (f $ traverseExpr f ex)
traverseExpr f (ExprConsTagCheck ct ex) = ExprConsTagCheck ct (f $ traverseExpr f ex)
traverseExpr f (Tuple exs) = Tuple (map f (map (traverseExpr f) exs))
traverseExpr f (ConTuple ct exs) = ConTuple ct (map f (map (traverseExpr f) exs))
traverseExpr f (ExpandedCase exs ex si) = ExpandedCase (map f (map (traverseExpr f) exs)) (f $ traverseExpr f ex) si
traverseExpr f (Statements exs) = Statements (map f (map (traverseExpr f) exs))
traverseExpr f (UnaryOp nm ex) = UnaryOp nm (f $ traverseExpr f ex)
traverseExpr f (BinaryOp nm e1 e2) = BinaryOp nm (f $ traverseExpr f e1) (f $ traverseExpr f e2)
-- for functions only traversing the body for now:
-- probably need to add type sig as well???
traverseExpr f (Function lam) = Function $ lam { body = f $ traverseExpr f (body lam) }
traverseExpr f (Action lam) = Action $ lam { body = f $ traverseExpr f (body lam) }
traverseExpr f (Structure lam nm) = Structure (lam { body = f $ traverseExpr f (body lam) }) nm
traverseExpr f (SumType lam) = SumType $ lam { body = f $ traverseExpr f (body lam) }
-- TODO: CHECK IF THIS IS THE CORRECT TRAVERSAL: !!!!
traverseExpr f (Constructors lams) = Constructors $ map (\l-> l {body = f $ traverseExpr f (body l) } ) lams
traverseExpr f (Binding (Var nm tp val)) = Binding (Var nm (f $ traverseExpr f tp) (f $ traverseExpr f val))
traverseExpr f e = ERROR $ "Traverse not implemented for: " ++ ppr e

-- App (Id "Succ") [App (Id "plus") [Id "n",Id "x"]]

-- f(x) = expr, x = val, substituting all x appearances in expr for val
betaReduce :: Var -> Expr -> Expr
-- substituting all nm occurences in expr for val
-- no typechecking whatsoever
betaReduce (Var nm tp val) expr = traverseExpr subs expr
  where subs :: Expr -> Expr
        subs e@(Id name) = if (nm == name) then val else e
        subs e = e

-- --------------------------------- IMPLICIT / STRUCTURE LAMBDAS --------------------------------------------
{-
Since we are representing functions inside structures as functions with implicit
type parameters, we need some helper methods to manipulate them
-}
-- first, identifying such a function:
hasImplicit :: Lambda -> Bool
hasImplicit (Lambda _ ((Var _ (Implicit _) _):xs) _ _) = True
hasImplicit _ = False

-- getting "default" case from inside any function that has cases:
getDefaultCase :: Lambda -> Expr
getDefaultCase (Lambda _ _ (PatternMatches ( (CaseOf [] ex _):xs ) ) _) = ex
getDefaultCase (Lambda _ _ (PatternMatches ( (ExpandedCase [] ex _):xs ) ) _) = ex
getDefaultCase (Lambda _ _ b _) = b

-- --------------------------------- PRETTY PRINTING --------------------------------------------

instance PrettyPrint Lambda where
  ppr (Lambda name params body sig) = name ++ " " 
    ++ showListRoBr ppr params 
    ++ pprTyp sig
    ++ if (body == UNDEFINED) then "" else " = " ++ ppr body

pprTyp ex = if (ex == UNDEFINED) then "" else ":" ++ ppr ex

instance PrettyPrint Var where
  ppr (Var n t _) = as [bold] n ++ if (t == UNDEFINED) then "" else ":" ++ ppr t

ppVarCaseOf :: Var -> String
ppVarCaseOf (Var n t val) = "case " ++ n ++ " of " ++ ppr val

pprRecordRo :: (Var -> String) -> Record -> String
pprRecordRo f r = showListRoBr f r

instance PrettyPrint Expr where
  ppr UNDEFINED = ""
  ppr (Id v) = as [bold] v
  ppr (CaseOf e1 e2 _) = showListCuBr ppVarCaseOf e1 ++ " -> " ++ ppr e2
  ppr (ExprConsTagCheck (ConsTag nm i) ex) = (as [bold,lblue] "checkConsTag")
    ++ "(" ++ nm ++ ", " ++ ppr ex ++ ")"
  ppr (ExpandedCase exs ex _) = showListSqBr ppr exs ++ " -> " ++ ppr ex
  ppr (PatternMatches ps) = showListCuBr ppr ps
  ppr (RecFieldAccess (nm,i) e) = ppr e ++ "." ++ nm ++"("++show i ++")"
  ppr (App e ex) = (ppr e) ++ showListRoBr ppr ex
  ppr (Tuple ex) = showListCuBr ppr ex
  ppr (Structure (Lambda name params body sig) nms) = (as [bold,yellow] "structure ") 
    ++ name ++ " "
    ++ showListRoBr ppr params 
    ++ pprTyp sig ++ " = "
    ++ ppr body
  ppr (Function lam) = (as [bold,red] "function ") ++ ppr lam
  ppr (Action lam) = (as [bold,blue] "action ") ++ ppr lam
  ppr (ConTuple (ConsTag nm i) ex) = (as [bold] nm) ++ " "  ++ show i ++ " " ++ showListCuBr ppr ex
  ppr (Statements ex) = showListCuBr ppr ex
  ppr (Binding (Var nm tp val)) = as [bold] nm ++ pprTyp tp ++ " = " ++ ppr val 
  ppr (Constructors cs) = showListCuBr ppr cs
  ppr (UnaryOp nm ex) = nm ++ ppr ex
  ppr (BinaryOp nm e1 e2) = ppr e1 ++ nm ++ (ppr e2)
  ppr (SumType (Lambda name params body sig)) = name ++ " "
    ++ showListRoBr ppr params 
    ++ pprTyp sig ++ " = "
    ++ ppr body
  ppr e = show e
  -- λ  