{-# LANGUAGE OverloadedStrings #-}

module DependentTypes.Core where

import TermColors

type Name = String

-- Core AST type
data Expr
  = Lit Literal
  | VarId Name -- for bound variables and functions???
  | Lam Var  Expr
  | App Expr Expr
  | If  Expr Expr Expr -- will get rid of this once case patterns are in, since we can model it with a function
  | Let Name Expr Expr -- ok, need to figure out how GHC does it - here we are binding first Expr to symbol Name in 2nd Expr
  | Tuple Name [Expr]
  deriving (Eq, Ord, Show)
{-
-- read a bunch of stuff in test.fool, it explains how we can do everything via tuples
-- and yes, we can do it via App, but lists are more convenient (maybe?) to manipulate than trees in this case
-- Name is a tag (constructor tag most often if it's applicable)
-- [Expr] is a tuple itself
-- Type is the type of the resulting tuple

-- Basically, Tuple without any lambdas inside is a term
-- And again, we can (probably) do it via App

Some examples:

Maybe a = Just a | Nothing translates to:
Lam (TyVar "a" KStar)
  Tuple "Maybe" [
    Lam (Id "x" (TVar "a")) (Tuple "Just" [VarId "x"] (TApp (TCon "Maybe") (TVar "a"))),
    Tuple "Nothing" [] (TApp (TCon "Maybe") (TVar "a"))
  ] (KArr KStar KStar)

Bool = True | False:
  Tuple "Bool" [
    Tuple "True" [] (TCon "Bool"),
    Tuple "False" [] (TCon "Bool")
  ] KStar

etc.
-}


data Literal = LInt !Int | LFloat !Double | LChar !Char | LString String | LBool Bool deriving (Eq, Ord, Show)

data Var = Id Name Type | TyVar Name Kind
  deriving (Show, Eq, Ord)

type TVar = Var -- type synonim to handle Forall predicates

varName (Id n _) = n
varName (TyVar n _) = n
varType (Id _ t) = t

-- needed for mega-abstract tuple in Expr
data TypeOrKind = Tp Type | Kn Kind deriving (Show, Eq, Ord)

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
-- calculate arity of an expression (basically, # of lambdas)
arity :: Expr -> Int
arity (Lam v e) = 1 + arity e
-- arity (App e1 e2) = arity e1 + arity e2
arity e = 0



-------------------------------------------------------------------------------
-- Pretty Print typeclass
-------------------------------------------------------------------------------
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Expr where
  prettyPrint (VarId n) = n
  prettyPrint (Lam v e) = as [bold, dgray] "λ" ++ prettyPrint v ++ ". " ++ prettyPrint e
  prettyPrint (Tuple nm exs) = as [magenta] nm ++ fn exs --  : " ++ prettyPrint tp - not showing types for now
      where fn [] = ""
            fn (e:es) = " {" ++ foldl (\acc x -> acc ++ ", " ++ prettyPrint x) (prettyPrint e) es ++ "} "
  prettyPrint (Lit l) = prettyPrint l
  prettyPrint (App e1 e2) = prettyPrint e1 ++ " " ++ prettyPrint e2
  prettyPrint (If e1 e2 e3) = as [bold, green] "if " ++ prettyPrint e1 ++
                              as [bold, green] " then " ++ prettyPrint e2 ++
                              as [bold, green] " else " ++ prettyPrint e3
  prettyPrint (Let nm e1 e2) = as [bold, green] "let " ++ nm ++ " = " ++ prettyPrint e1 ++
                            as [bold, green] " in " ++ prettyPrint e2
  -- prettyPrint e = show e
{-
    = Lit Literal
    | App Expr Expr
-}

embrace s = as [bold, blue] "(" ++ s ++ as [bold, blue] ")"
-- additional nicer formatting for output for top level expressions
prettyPrintTopLevel (App e1 e2) = embrace (prettyPrint e1) ++ embrace (prettyPrint e2)
prettyPrintTopLevel e = prettyPrint e

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

instance PrettyPrint TypeOrKind where
  prettyPrint (Tp t) = prettyPrint t
  prettyPrint (Kn k) = prettyPrint k
