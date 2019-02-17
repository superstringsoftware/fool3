-- Base library: v0.0.1

-- Types
type Bool = True + False;
type Maybe a = Nothing + Just :a;
type List a = Nil + (::) :a * :(List a);

-- Functions
id x = x;

class Eq a =
    (==) 4 x:a y:a = not (x /= y) -- default implementation
    (/=) 4 x:a y:a = not (x == y);   
    
-- Basic algebra stuff
class Semigroup a =
    (+) 4 x:a y:a;
  -- forall x:a, y:a, z:a => x + (y + z) == (x + y) + z -- constraint (law), associativity

-- monoid is a semigroup plus zero
class Monoid a <: Semigroup a =
    z0:a; -- zero element for the (+) operation
  -- forall x:a => (Z0 + x == x, x + Z0 == x) -- constraint (law)

-- group is a monoid plus negation
class Group a <: Monoid a = 
    (-) 4 x:a y:a;
    -- forall x:a => (Z0 - x + x == Z0)

-- ring is a group plus another binary operation that is a monoid itself and a bunch of laws
class Ring a <: Group a =
    (*) 3 x:a y:a
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
    (/) 3 x:a y:a;
    -- forall x:a => (Z1 / x * x == Z1)

instance Semigroup Int = (+) x y = primop_plus x y;
instance Monoid Int = z0 = 0;
instance Group Int = (-) x y = primop_minus x y;
instance Ring Int = (*) x y = primop_mul x y;


{-
# trying typeclasses
class Functor f = 
  fmap:(f b) g:(a->b) x:(f a);

# typeclass implementation
instance Functor List = 
  fmap f ls = ls ? Nil -> Nil | x :: xs -> (f x) :: (fmap f xs);

fact n = n ?
    0 -> 1
  | otherwise -> n * fact(n-1);
 -} 
