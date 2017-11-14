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
  | Var Name -- for bound variables and functions???
  | Lam Var  Expr
  | App Expr Expr
  deriving (Eq, Ord, Show)


data Literal = LInt !Int | LFloat !Double | LChar !Char deriving (Eq, Ord, Show)

data Var = Id Name Type | TyVar Name Kind
  deriving (Show, Eq, Ord)

type TVar = Var -- type synonim to handle Forall predicates

varName (Id n _) = n
varType (Id _ t) = t

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
  | TCon TyCon -- Maybe, String, Int etc - just a name of the constructor
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
-- Pretty Print typeclass
-------------------------------------------------------------------------------
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Type where
  --prettyPrint (TVar nm) = nm
  --prettyPrint (TCon nm) = as [yellow, bold] nm
  prettyPrint ToDerive  = as [dgray, bold] "?"
  prettyPrint (TApp t1 t2) = prettyPrint t1 ++ " " ++ prettyPrint t2
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
