{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module DependentTypes.Core where

import TermColors

type Name = String

{-
Moving to new approach: everything is a tuple
-}

-- Core AST type to handle both lazy and strict hopefully a bit more efficiently
-- So, no currying
data Expr
  = Lit Literal
  | VarId Name -- for bound variables and functions???
  | Lam Name (SizedTuple Var) Expr -- named function. To be non-partially applied, needs all arguments. size of a tuple is arity.
  | Tuple    (SizedTuple Expr) -- polymorphic tuple
  | App Expr Expr -- e1 can only be Lam, e2 can only be Tuple - so there should be a better way to handle this
  | If  Expr Expr Expr -- will get rid of this once case patterns are in, since we can model it with a function
  | Let Name Expr Expr -- ok, need to figure out how GHC does it - here we are binding first Expr to symbol Name in 2nd Expr
  | BinaryOp Name Expr Expr
  | UnaryOp  Name Expr
  deriving (Eq, Ord, Show)


data SizedTuple a = SzT { tuple :: [a], size :: Int}  deriving (Eq, Ord, Show)
data Literal = LInt !Int | LFloat !Double | LChar !Char | LString String | LBool Bool deriving (Eq, Ord, Show)

data Var = Id Name Type | TyVar Name Kind
  deriving (Show, Eq, Ord)

type TVar = Var -- type synonim to handle Forall predicates

varName (Id n _) = n
varName (TyVar n _) = n
varType (Id _ t) = t


-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Name -- TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
  | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the constructor
  | TApp Type Type -- Constructor application - Maybe Int, List a etc
  | TArr Type Type -- Function sig - Maybe a -> String etc
  | TForall [Pred] [TVar] Type
  | ToDerive -- added it to handle initial parsing
  | InsType Expr -- this is probably a workaround - when we are beta-reducing Type lambdas for variables like \a. x:a
  -- "a" needs to be able to become any kind of expression (since we are applying lambdas to expressions).
  -- Type checking etc will fix this.
  deriving (Show, Eq, Ord)

-- Since we are doing dependent types, we need to be able to do both the standard:
-- Maybe :: * -> * as well as something for Vector a:* n:Int which would look like:
-- Vector :: * -> Int -> *
-- We are using KTerm constructor to describe it - just need to be careful, since valid value there is only
-- *Concrete* type!
data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name
  | KTerm Type
  deriving (Show, Eq, Ord)

data TyCon
  = AlgTyCon { tyId :: Name }
  | PrimTyCon { tyId :: Name }
  deriving (Show, Eq, Ord)

data Pred
  = IsIn Name Type
  deriving (Show, Eq, Ord)

{-
-- GHC Core:
type CoreExpr = Expr Var

data Expr b	-- "b" for the type of binders,
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]
-}

-------------------------------------------------------------------------------
-- AST manipulation stuff
-------------------------------------------------------------------------------
-- calculate arity of an expression: trivial in our language approach
arity :: Expr -> Int
arity (Lam _ v _) = size v




-------------------------------------------------------------------------------
-- Pretty Print typeclass
-------------------------------------------------------------------------------
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Expr where
  prettyPrint (VarId n) = n
  prettyPrint (Lam n v e) = n ++ " " ++ prettyPrint v ++ prettyPrint e
  prettyPrint (Tuple tpl) = prettyPrint tpl
  prettyPrint (Lit l) = prettyPrint l
  prettyPrint (App e1 e2) = prettyPrint e1 ++ " " ++ prettyPrint e2
  prettyPrint (If e1 e2 e3) = as [bold, green] "if " ++ prettyPrint e1 ++
                              as [bold, green] " then " ++ prettyPrint e2 ++
                              as [bold, green] " else " ++ prettyPrint e3
  prettyPrint (Let nm e1 e2) = as [bold, green] "let " ++ nm ++ " = " ++ prettyPrint e1 ++
                            as [bold, green] " in " ++ prettyPrint e2

  prettyPrint (BinaryOp n e1 e2) = "("++n++") " ++ prettyPrint e1 ++ " " ++ prettyPrint e2 
  prettyPrint e = show e
{-
  | BinaryOp Name Expr Expr
  | UnaryOp  Name Expr

data SizedTuple a = SzT { tuple :: [a], size :: Int}  deriving (Eq, Ord, Show)
-}

embrace s = as [bold, blue] "(" ++ s ++ as [bold, blue] ")"
-- additional nicer formatting for output for top level expressions
prettyPrintTopLevel (App e1 e2) = embrace (prettyPrint e1) ++ embrace (prettyPrint e2)
prettyPrintTopLevel e = prettyPrint e

instance PrettyPrint (SizedTuple Var) where
  prettyPrint SzT {tuple} = foldr fn "" tuple
      where fn el acc = as [bold, dgray] "λ" ++ prettyPrint el ++ ". " ++ acc


instance PrettyPrint (SizedTuple Expr) where
  prettyPrint SzT {tuple} = pp tuple where
      pp []     = ""
      pp [x]    = "{" ++ prettyPrint x ++ "}"
      pp (x:xs) = "{" ++ prettyPrint x ++ foldr fn "" xs ++ "}"
          where fn el acc = ", " ++ prettyPrint el ++ acc

instance PrettyPrint Literal where
  prettyPrint (LInt x) = as [magenta] $ show x
  prettyPrint (LFloat x) = as [magenta] $ show x
  prettyPrint (LBool x) = as [magenta] $ show x
  prettyPrint (LString s) = as [green] $ show s
  prettyPrint e = show e

instance PrettyPrint Type where
  prettyPrint (TVar nm) = nm
  prettyPrint (TCon nm) = as [yellow, bold] nm
  prettyPrint ToDerive  = as [dgray, bold] "?"
  prettyPrint (TApp t1 t2) = "(" ++ prettyPrint t1 ++ " " ++ prettyPrint t2 ++")"
  prettyPrint (InsType ex) = prettyPrint ex
  prettyPrint e = show e

{-
  | TArr Type Type
  | TForall [Pred] [TVar] Type
-}

instance PrettyPrint Var where
  prettyPrint (Id nm tp)   = nm ++ ":" ++ prettyPrint tp
  prettyPrint (TyVar nm k) = nm ++ ":" ++ prettyPrint k

instance PrettyPrint Kind where
  prettyPrint KStar = "*"
  prettyPrint k = show k
