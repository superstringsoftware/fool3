{-# LANGUAGE OverloadedStrings #-}

module Core where

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
data DExpr
  = DLit Literal
  | DVar Name -- for bound variables and functions???
  | DLam Var DExpr
  | DApp DExpr DExpr
    deriving (Eq, Ord, Show)

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

data Literal = LInt !Int | LFloat !Double | LChar !Char deriving (Eq, Ord, Show)

data Var
  = Id Name Type
  | TyVar Name Kind
  deriving (Show, Eq, Ord)

getVarName (Id n _) = n
getVarName (TyVar n _) = n

data Type
  = TVar Name -- TVar
  | TCon Name -- TyCon
  | TApp Type Type
  | TArr Type Type
  | TForall [Pred] [TVar] Type
  | ToDerive -- added it to handle initial parsing
  | TStar -- what if we are doing fully dependent types - then there should be no difference between Kinds and types,
    -- we will represent KStar as TStar, then * -> * would be TApp TStar TStar, but at the same time we can build different
    -- crazy functions that can take types, values etc arguments and construct any kind of stuff.
  deriving (Show, Eq, Ord)

data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name
  deriving (Show, Eq, Ord)

data TyCon
  = AlgTyCon { tyId :: Name }
  | PrimTyCon { tyId :: Name }
  deriving (Show, Eq, Ord)

data Pred
  = IsIn Name Type
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Type Variables
-------------------------------------------------------------------------------

data TVar = TV
  { tvName   :: Name
  } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- Alpha Equivalence
-------------------------------------------------------------------------------

class Alpha a where
  aeq :: a -> a -> Bool

instance Alpha TVar where
  aeq _ _ = True

instance Alpha Type where
  aeq (TVar _) (TVar _)     = True
  aeq (TApp a b) (TApp c d) = aeq a c && aeq b d
  aeq (TArr a b) (TArr c d) = aeq a c && aeq b d
  aeq (TCon a) (TCon b)     = a == b
  aeq _ _                   = False

instance Alpha Kind where
  aeq KStar KStar = True
  aeq KPrim KPrim = True
  aeq (KArr a b) (KArr c d) = aeq a c && aeq b d
  aeq _ _ = False

-------------------------------------------------------------------------------
-- Pretty Print typeclass
-------------------------------------------------------------------------------
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Type where
  prettyPrint (TVar nm) = nm
  prettyPrint (TCon nm) = as [yellow, bold] nm
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
