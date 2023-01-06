-- Base library for our language, another attempt - now with "Everything is a Record" concept and a bit more curly braces tolerance
-- Build everything from scratch with the strong math foundation and typeclasses that are the same as type families 
-- (see vector)
-- Idea: vars and funcs can be capitalized, but types are always *bold*
-- Quantifiers!!!

-- Keywords: 
--      - type: all types and type functions
--      - class: all typeclasses and type families
--      - cons: constructor functions inside types

-- basics
{-
Keep simpliefied syntaxis for later stages, start PoC with full syntaxis first!

type Bool = True + False
not x = x ? False | True

type Maybe a:* = Just :a + Nothing
-}

-- full syntaxis for the type defintion:
type Bool = {
    cons True;
    cons False;
}
type Maybe a:* = {
    cons Nothing = {};
    cons Just x:a = {x};
}

id:a x:a = x

-- example of a record
type Person = {
    cons Person {
        fname, lname : String,
        age : Int,
        dob : Date
    }
}

-- simplified record syntaxis for a sumtype that's just one constructor of the same name:
record Person = {
    fname, lname : String, age : Int, dob : Date
}

-- by default, if there's no :* - it's a type :*
class Eq a = {
    infix 4 (==):Bool { x, y : a } = not (x /= y); -- default implementation
    infix 4 (/=):Bool { x, y : a } = not (x == y);    
}

-- Basic algebra stuff
class Semigroup a = {
    -- infixl 4 (+): a -> a -> a -- can use standard type signature, but preferred way of writing functions is record based:
    infixl 4 (+):a { x, y : a }; -- function that returns value of type a with 2 parameters of type a
    -- constraint (law), associativity (can use "forall" instead of the unicode quantifier)
    ∀x:a, y:a, z:a => x + (y + z) == (x + y) + z; 
}
-- monoid is a semigroup plus zero
class Semigroup a => Monoid a = {
    Z0:a; -- zero element for the (+) operation
    ∀x:a => {Z0 + x == x, x + Z0 == x}; -- constraint (law)
}
-- group is a monoid plus negation
class Monoid a => Group a = {
    infixl 4 (-):a { x, y : a };
    ∀x:a => Z0 - x + x == Z0;
}
-- ring is a group plus another binary operation that is a monoid itself and a bunch of laws
class Group a => Ring a = {
    infixl 3 (*):a { x, y : a };
    Z1:a;
    ∀x:a, y:a => x + y == y + x; -- require Monoid in (+) to be commutative!
    ∀x:a => {Z1 * x == x, x * Z1 == x}; -- it's a monoid in (*)
    ∀x:a, y:a, z:a => { 
        x * (y + z) == x * y + x * z
        (x + y) * z == x * z + y * z
    }; -- distributivity
    ∀x:a => {Z0 * x == Z0, x * Z0 == Z0};
}
-- field is a ring plus reverse to multiplication
class Ring a => Field a = {
    infixl 3 (/):a { x, y : a };
    ∀x:a => Z1 / x * x == Z1;
}
-- then we can make numeric classes instances of whatever is relevant - integers Ring, Reals Field etc
-- defining a class instance is simply a type function call definition with a type of a pattern match
-- Also, can define all superclasses via subclass, since subclass includes ALL superclass members, e.g.:
Group Int = {
    (+) = primop+;
    Z0 = 0;
    (-) = primop-;
    (*) = primop*;
    Z1 = 1;
}

type Ratio = {
    cons Ratio p:Int q:Int; 
}

-- make vector primitive?
type VectorN a:* n:Int = {
    cons VectorN a n = {data = primarray# a n, size = n }
}