-- Base library: v0.0.1

-- Types
type NamedRecord = NamedRecord age:Int * name:String; 
type Bool = True + False;
type Maybe a = Nothing + Just :a;
type List a = Nil + (::) :a * :(List a);
-- type NList a = Nil + (::) head:a * tail:(NList a);
-- type Test = Test :(Maybe Int); - WORKS
-- type Test = Maybe Int; - FAILS!
type Record b = Record test:Int * typed:b;
type NList a = Nil + (::) head:a * tail:(NList a);


-- Functions
id x = x;

class Eq a =
    (==):Bool x:a y:a = not (x /= y), -- default implementation
    (/=):Bool x:a y:a = not (x == y);   
    
-- Basic algebra stuff
class Semigroup a =
    (+):a x:a y:a;
  -- forall x:a, y:a, z:a => x + (y + z) == (x + y) + z -- constraint (law), associativity
 
-- monoid is a semigroup plus zero
class Monoid a <: Semigroup a =
    z0:a; -- zero element for the (+) operation
  -- forall x:a => (Z0 + x == x, x + Z0 == x) -- constraint (law)

-- group is a monoid plus negation
class Group a <: Monoid a = 
    (-):a x:a y:a;
    -- forall x:a => (Z0 - x + x == Z0)

-- ring is a group plus another binary operation that is a monoid itself and a bunch of laws
class Ring a <: Group a =
    (*):a x:a y:a,
    z1:a;
{-
  forall x:a, y:a => (x + y == y + x) -- require Monoid in (+) to be commutative!
  forall x:a => (Z1 * x == x, x * Z1 == x) -- it's a monoid in (*)
  forall x:a, y:a, z:a => ( 
      x * (y + z) == x * y + x * z
      (x + y) * z == x * z + y * z
  ) -- distributivity
  forall x:a => (Z0 * x == Z0, x * Z0 == Z0)
-}

-- field is a ring plus reverse to multiplication
class Field a <: Ring a =
    (/):a x:a y:a;
    -- forall x:a => (Z1 / x * x == Z1)

-- multiparameter typeclass test
class Mult a b = 
    result,
    (*):result x:a y:b;

-- Functor
class Functor f = 
  fmap:(f b) g:(a->b) x:(f a);

instance Functor Maybe = 
  fmap f x = x ? Nothing -> Nothing | Just y -> Just (f y);

instance Functor List = 
  fmap f l = l ? Nil -> Nil | (x::xs) -> (f x) :: (map f xs);
    
    

instance Eq Int = (==) x y = primop_equals x y;
instance Semigroup Int = (+) x y = primop_plus x y;
instance Monoid Int = z0 = 0;
instance Group Int = (-) x y = primop_minus x y;

-- need to separate functions somehow explicitly, otherwise wrong parsing
instance Ring Int = 
    (*) x y = primop_mul x y,
    z1 = 1;

-- instance for the type constructor:
instance Semigroup (List a) = 
  (+) ls1 ls2 = concat ls1 ls2;

instance Semigroup (Vector a n) where exists Semigroup a = 
  (+) v1 v2 = primop_plus;

    -- TEST PROGRAM

f x y = x*x + y;

g = f 1;
h = (f 1) 3;
h' = f 1 3;

k = f x y z;

fact n = n ?
    z0 -> z1
  | otherwise -> n * fact(n-1);

main = print (fact (f (g 2) 1)); -- should be 120

test = 1::2::3::[];

l1 = [1,2,3,4];
v1 = <1,3,2,3.3>;


