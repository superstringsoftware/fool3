-- Base library: v0.0.1

mul = primop;

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
