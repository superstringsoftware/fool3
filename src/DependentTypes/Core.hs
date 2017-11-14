{-# LANGUAGE OverloadedStrings #-}

module DependentTypes.Core where

import TermColors

type Name = String

{-
-- Intermediate explicity typed IR
-- For now, we can't really represent records this way, or can we?
Person { name:String, age:Int } translates into something like:
\name:String. \age:Int. Person name string, i.e. named access field info is erased. Where do we store it?
E.g., a person writes:
p = Person "James" 23 -- and then
f p = p.name ++ show p.age -- which goes something like:
\p:Person. (++) (Person.name p) (show (Person.age p))
which means we need to create some sort of qualified function name that accesses record fields?
Or simply convert it to numbered accessor as in any anonymous pair?

What if we have something like:
data Widget = Box {x, y: Int, ...} | Point {x,y:Int, ...} - and we want a function
f :: Widget -> Int
f w = w.x * w.x
b = Box ...; p = Point ...
main = f b + f p -- what happens here??
\w:Widget. (*) (.x w) (.x w)
- we can't use numbered approach because what if x has completely different numeric representation in different Constructors?
so, .x here would be = Box.x for Box and Point.x for Point, so it's sort of an implicit pattern match

Ok, this is quite complicated, need to think it through.

Using fully qualified names is probably way to go, at least explore it, so when creating a record we automatically create
fully qualified accessor functions, e.g:
Widget.Box.x w:Box = n-th accessor
Widget.Point.x w:Point = k-th accessor
Widget.x w:Widget = case w of Box -> Widget.Box.x
                              Point -> Widget.Point.x

Something like that.
-}

{-
Refresher on lambdas.
Maybe a:
Just = \a:Type \x:a. x : Maybe a
Nothing = ()
Pair = \a:Type \b:Type \x:a \y:b . (x y) : Pair a b

It's all good, but how do we determine type of our lambdas???

In IR above:
Just = DLam (Id a TStar) (DLam (Id x (TVar a))  DApp (DVar x) ??? )
- we can substitute ??? to our type info, something like DType (TApp (TCon Maybe) (TVar a))
this way, we annotate types by the last Expression in application tree.
-}

-- Core AST type
data Expr
  = Lit Literal
  | VarId Name -- for bound variables and functions???
  | VarIn Int  -- variable index for building constructor function values mostly
  | Lam Var  Expr
  | App Expr Expr
  | If  Expr Expr Expr -- will get rid of this once case patterns are in, since we can model it with a function
  | Let Name Expr Expr -- ok, need to figure out how GHC does it - here we are binding first Expr to symbol Name in 2nd Expr
  | Tuple Name [Expr] TypeOrKind
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


data Literal = LInt !Int | LFloat !Double | LChar !Char deriving (Eq, Ord, Show)

data Var = Id Name Type | TyVar Name Kind
  deriving (Show, Eq, Ord)

type TVar = Var -- type synonim to handle Forall predicates

varName (Id n _) = n
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
  prettyPrint (VarIn n) = "v" ++ show n
  prettyPrint (Lam v e) = as [bold, dgray] "λ" ++ prettyPrint v ++ ". " ++ prettyPrint e
  prettyPrint (Tuple nm exs tp) = nm ++ fn exs --  : " ++ prettyPrint tp - not showing types for now
      where fn [] = " {}"
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

instance PrettyPrint Literal where
  prettyPrint (LInt x) = show x
  prettyPrint e = show e

instance PrettyPrint Type where
  prettyPrint (TVar nm) = nm
  prettyPrint (TCon nm) = as [yellow, bold] nm
  prettyPrint ToDerive  = as [dgray, bold] "?"
  prettyPrint (TApp t1 t2) = "(" ++ prettyPrint t1 ++ " " ++ prettyPrint t2 ++")"
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
