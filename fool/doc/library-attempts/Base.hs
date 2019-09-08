-- Base library for our language, another attempt
-- Build everything from scratch with the strong math foundation and typeclasses that are the same as type families 
-- (see vector)
-- Idea: vars and funcs can be capitalized, but types are always *bold*
-- After much looking at ugly braces, we are back to Haskell-like currying signatures,
-- but making them optionally named (for better documentation and named currying functionality)

-- basics
type Bool = True + False
not x = x ? False | True 

id : a -> a
id x = x

-- by default, if there's no :* - it's a type :*
class Eq a where
    infix 4 (==): x:a -> y:a -> a = not (x /= y) -- default implementation
    infix 4 (/=): x:a -> y:a -> a = not (x == y)    

-- Basic algebra stuff
class Semigroup a where
    infixl 4 (+): a -> a -> a
    forall x:a, y:a, z:a => x + (y + z) == (x + y) + z -- constraint (law), associativity

-- monoid is a semigroup plus zero
class Semigroup a => Monoid a where
    Z0:a -- zero element for the (+) operation
    forall x:a => (Z0 + x == x, x + Z0 == x) -- constraint (law)

-- group is a monoid plus negation
class Monoid a => Group a where
    infixl 4 (-): a -> a -> a
    forall x:a => (Z0 - x + x == Z0)

-- ring is a group plus another binary operation that is a monoid itself and a bunch of laws
class Group a => Ring a where
    infixl 3 (*): a -> a -> a
    Z1:a
    forall x:a, y:a => (x + y == y + x) -- require Monoid in (+) to be commutative!
    forall x:a => (Z1 * x == x, x * Z1 == x) -- it's a monoid in (*)
    forall x:a, y:a, z:a => ( 
        x * (y + z) == x * y + x * z
        (x + y) * z == x * z + y * z
    ) -- distributivity
    forall x:a => (Z0 * x == Z0, x * Z0 == Z0)

-- field is a ring plus reverse to multiplication
class Ring a => Field a where
    infixl 3 (/): a -> a -> a
    forall x:a => (Z1 / x * x == Z1)

-- then we can make numeric classes instances of whatever is relevant - integers Ring, Reals Field etc
-- E.g.:
instance Semigroup Int where (+) = primop+
instance Monoid Int where Z0 = 0
instance Group Int where (-) = primop-
instance Ring Int where 
    (*) = primop*
    Z1 = 1

class Vector v : n:Int -> a:* -> * where
    -- Vec : a -> n -> * -- data constructor
    generate : (Int -> a) -> n:Int -> v n a -- generate a container with a generator function f
    map : (a -> b) -> v n a -> v n a
    (.) : Vec a n -> Int -> a -- indexing, v.4 etc
    (++) : Vec a n -> Vec a m -> Vec a (n+m) -- concatenation of 2 vectors
    zipWith : (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n -- applies function f to all elements of both vectors - basically, zipWith

instance Semigroup a => Semigroup (Vector a n) where v1 + v2 = zipWith (+) v1 v2  
instance Monoid a => Monoid (Vector a n) where Z0 = generate (const Z0:a) n
instance Group a => Group (Vector a n) where v1 - v2 = zipWith (-) v1 v2  
instance Ring a => Ring (Vector a n) where 
    v1 * v2 = zipWith (*) v1 v2
    Z1 = generate (const Z1:a) n

-- as an example, implementation of Vector for built-in .net Arrays (primitive)
type NVector a n = NVector (primop.newarray# a n) n -- data representation, primitive array of size n and it's size 
instance NVector a n => Vector a n where
    Vec = NVector
    map f v = -- create new vector, fill it, freeze it, ST manipulation etc... - difficult