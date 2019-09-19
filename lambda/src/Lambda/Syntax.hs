{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Surface language syntax, Expr is generated directly from the Parser

module Lambda.Syntax
where

type Name = String
type ConsTag = String

data Var = Var Name Type deriving (Show, Eq)
type Binding = (Var, Expr) -- binding of Expr to a Variable Var

varName v = fst v
varType v = snd v

data Pred
  = Exists Type -- TApp (TCon name) (TVar name) - e.g., Num a --> TApp (TCon "Num") (TVar "a")
  | Unconstrained -- variables can be anything
  deriving (Show, Eq)

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Name -- TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
  | HighVar Name [Pred] -- for rank-n polymorphism, predicates for a specific type variable
  | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the constructor
  | TApp Type [Type] -- Constructor application - Maybe Int, List a etc; as it should be always saturated tuple is a nobrainer
  | TArr Type Type -- Function sig - Maybe a -> String etc
  | TExpr Expr -- since we will support dependent types, type constructors can be applied to quite complicated expressions
  -- | TForall [Pred] [TVar] Type
  | ToDerive -- added it to handle initial parsing
  -- | TClass Name -- for initial typeclass parsing, might change this
  -- | InsType Expr -- this is probably a workaround - when we are beta-reducing Type lambdas for variables like \a. x:a
  -- "a" needs to be able to become any kind of expression (since we are applying lambdas to expressions).
  -- Type checking etc will fix this.
  deriving (Show, Eq)

isTConOrTApp (TCon _)   = True
isTConOrTApp (TApp _ _) = True
isTConOrTApp _          = False

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
  deriving (Show, Eq)

-- checks if an expression was probably supposed to be a data constructor  
isTupleOrEmpty (Tuple _ _ _) = True
isTupleOrEmpty EMPTY         = True
isTupleOrEmpty _             = False

data Literal = LInt !Int | LFloat !Double | LChar !Char |
               LString !String | LList [Expr] | LVec [Expr]
               deriving (Eq, Show)


-- the very first pass that we run right after parsing the source file               
afterparse :: Expr -> Expr
afterparse (BinaryOp n e1 e2) = App (VarId n) ( (afterparse e1):(afterparse e2):[])
afterparse (UnaryOp n e) = App (VarId n) ( (afterparse e):[])
-- top level binding: this is a DATA CONSTRUCTOR (unnamed tuple, bound to typed var) - crazy pattern, need to simplify
-- we are naming the tuple and assigning it's type to var type, since that's how types are being created
afterparse (Let (( v@(Var n typ) , (Tuple "" args ToDerive) ):[]) EMPTY ) = Let [(v, Tuple n (map afterparse args) typ )] EMPTY 
afterparse (Let (( v@(Var n typ) , (Lam vars (Tuple "" args ToDerive) typ1 preds )):[]) EMPTY ) = 
  Let [(v, Lam vars (Tuple n (map afterparse args) typ) typ1 preds )] EMPTY 
afterparse (Lam vars ex typ preds) = Lam vars (afterparse ex) typ preds
afterparse (App ex exs) = App (afterparse ex) (map afterparse exs)
afterparse (Tuple cons exs typ) = Tuple cons (map afterparse exs) typ
afterparse (Let bnds ex) = Let (map ( \(v,e)-> (v,afterparse e) ) bnds ) (afterparse ex)
afterparse (PatternMatch args e2) = PatternMatch (map afterparse args) (afterparse e2)
afterparse (Patterns exs) = Patterns (map afterparse exs)
afterparse e = e

-- check if tuple is anonymous, need it for some passes
isAnonymousTuple (Tuple "" _ _) = True
isAnonymousTuple _ = False

class Printer a where
  ppr :: a -> String

{-
instance Printer a => Printer [a] where
  ppr = showListWFormat ppr "[" "]" ", " "[]"
-}

instance Printer Pred where
  ppr Unconstrained = ""
  ppr (Exists typ) = "∃" ++ ppr typ
  

instance Printer [Pred] where
  ppr [] = ""
  ppr preds = (showListRoBr ppr preds) ++ " => "
  
instance Printer Literal where
  ppr (LInt i) = show i
  ppr (LFloat i) = show i
  ppr (LChar i) = show i
  ppr (LString i) = show i
  ppr (LList []) = "[]" 
  ppr (LList (x:xs)) = foldl fn ("[" ++ ppr x) xs ++ "]"
      where fn acc e = acc ++ ", " ++ ppr e
  ppr (LVec []) = "< >"
  ppr (LVec (x:xs)) = foldl fn ("<" ++ ppr x) xs ++ ">"
      where fn acc e = acc ++ ", " ++ ppr e

instance Printer Type where
  ppr ToDerive = "(?)"
  ppr (TCon n) = n
  ppr (TApp con args) = "(" ++ ppr con ++ " " ++ (showListPlain ppr args) ++ ")"
  ppr (TVar n) = n
  ppr e = show e

instance Printer Var where
  ppr (Var n t) = n ++ ":" ++ ppr t      

instance Printer Expr where
  ppr (Lit l) = ppr l
  ppr (VarId v) = v
  ppr (Let ((v,e):[]) _ ) = ppr v ++ " = " ++ ppr e
  ppr (Lam vars expr tp preds) = ppr preds ++ "λ " ++ (showListPlain ppr vars) ++ " . " ++ ppr expr
  ppr (BinaryOp n e1 e2) = ppr e1 ++ " " ++ n ++ " " ++ ppr e2
  ppr (UnaryOp n e) = n ++ ppr e
  ppr (Tuple cons exprs typ) = cons ++ (showListCuBr ppr exprs) ++ " : " ++ ppr typ
  ppr (App f args) = ppr f ++ " " ++ (showListRoBrPlain ppr args)
  ppr (Patterns ps) = showListCuBr ppr1 ps
      where ppr1 (PatternMatch args e2) = (showListPlain ppr args) ++ " -> " ++ ppr e2
  ppr (PatternMatch args e2) = (showListPlain ppr args) ++ " = " ++ ppr e2
  ppr e = show e
  -- λ  

instance Printer Binding where
  ppr (v, ex) = ppr v ++ " = " ++ ppr ex

-- function that generically outputs a list of values   
showListWFormat :: (a -> String) -> String -> String -> String -> String -> [a] -> String
showListWFormat showF beginWith endWith sep empty [] = empty
showListWFormat showF beginWith endWith sep empty (x:xs) = foldl fn (beginWith ++ showF x) xs ++ endWith
    where fn acc e = acc ++ sep ++ showF e

showListSqBr fun = showListWFormat fun "[" "]" ", " "[]"
showListRoBr fun = showListWFormat fun "(" ")" ", " "()"     
showListCuBr fun = showListWFormat fun "{" "}" ", " "{}"     
showListPlain fun = showListWFormat fun "" "" " " ""
showListRoBrPlain fun = showListWFormat fun "(" ")" " " ""