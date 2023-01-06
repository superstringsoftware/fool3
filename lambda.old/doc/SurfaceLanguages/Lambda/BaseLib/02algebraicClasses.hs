Semigroup : Class = \a:Type . {
    -- "normal" function has no body - it's ok because it's a typeclass definition!
    (+):a = \x:a y:a;
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
    -- associativity = x + (y + z) == (x + y) + z 
};

Monoid : Class = ∃ Semigroup a => \a. {
    E0:a
    -- forall x:a => (Z0 + x == x, x + Z0 == x) -- constraint (law)
};

-- group is a monoid plus negation
Group : Class = ∃ Monoid a => \a. {
    (-):a = \x:a y:a;
    -- forall x:a => (E0 - x + x == E0)
}

-- ring is a group plus another binary operation that is a monoid itself and a bunch of laws
Ring : Class = ∃ Group a => \a. {
    (*):a = \x:a y:a;
    Z1 :a;
    {-
    forall x:a, y:a => (x + y == y + x) -- require Monoid in (+) to be commutative!
    forall x:a => (Z1 * x == x, x * Z1 == x) -- it's a monoid in (*)
    forall x:a, y:a, z:a => ( 
        x * (y + z) == x * y + x * z
        (x + y) * z == x * z + y * z
    ) -- distributivity
    forall x:a => (Z0 * x == Z0, x * Z0 == Z0)
    -}
}

-- field is a ring plus reverse to multiplication
Field : Class = ∃ Ring a => \a. {
    (/):a = \x:a y:a;
    -- forall x:a => (Z1 / x * x == Z1)
}

-- Instances for the primitive types
{-
How do we represent this:
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
        (a,b) <> (a',b') = (a<>a',b<>b')
        stimes n (a,b) = (stimes n a, stimes n b)

and then haskell defines similar *by hand* for tuples of different sizes - this was one of the annoying 
things in haskell, so we need a way to manipulate Tuple indexes somehow!!! 
-}

Semigroup (∃Semigroup a, b => {a,b}) = {
    {x,y} + {x',y'} = {x+x',y+y'};
} 

Semigroup (∃Semigroup b => (a -> b)) = {
    f + g = \x . f x + g x;
} 

Semigroup Int = {
    (+) = primPlusInt;
}

-- an attempt to define semigroup for a Tuple of any size:
-- need to think this through. 
-- One way is to simply define Tuple / Record as a type of list and then we can use
-- regular mapping etc operations.
-- TBD but the direction is good!
Semigroup (∀a in {Type} ∃Semigroup a => {a_i}) = {
    {} + {} = {};
    r1:::rs1 + r2:::rs2 = (r1+r2):::(rs1 + rs2)
}