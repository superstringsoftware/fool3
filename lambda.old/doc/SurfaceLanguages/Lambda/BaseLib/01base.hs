{-
Foundational types and functions
-}

id : a = \x:a . x

Bool     : Type = { True; False }
Ordering : Type = { LT; EQ; GT  }

not True  = False
not False = True

Eq : Class = \a:Type . {
    (==):Bool = \x:a y:a. not (x /= y);
    (/=):Bool = \x:a y:a. not (x == y); 
    (≠) = (/=);
    required = (==) || (/=)
    {-
    Laws to define further:
    Reflexivity
    x == x = True
    Symmetry
    x == y = y == x
    Transitivity
    if x == y && y == z = True, then x == z = True
    Substitutivity
    if x == y = True and f is a "public" function whose return type is an instance of Eq, then f x == f y = True
    Negation
    x /= y = not (x == y)
    -}
}

-- Don't need type signature for a below since it is given in the superclass definition
Ord : Class = ∃ Eq a => \a . {
    compare : Ordering = \x,y:a . 
        if x == y then EQ else
            if x <= y then LT else GT; 
    (<)  : Bool = \x,y:a . (x <= y) && (x /= y);
    (<=) : Bool = \x,y:a . let r = compare x y in 
        if (r == LT) || (r == EQ) then True else False;
    (>)  : Bool = \x,y:a . y < x;
    (>=) : Bool = \x,y:a . y <= x;
    max : a = \x,y:a . if x >= y then x else y;
    min : a = \x,y:a . if x <= y then x else y;
    required = (<=) || compare
    {-
    Laws to define further:
    Transitivity
    if x <= y && y <= z = True, then x <= z = True
    Reflexivity
    x <= x = True
    Antisymmetry
    if x <= y && y <= x = True, then x == y = True
    -}
}

{-
-- Tuple / Record manipulations. Not sure if we'll want to expose them to the end users,
-- but they are very much needed for more efficient typeclass definitions.
-- Tuples are stored internally as {x1:t1, ..., xn:tn}
-- This is even more foundational than the List.
-- In pseudocode:

Record : Type = {
    {}  : Record {}; -- Empty record of type Record {}
    (:::) : Record = \field:{String, Type} rec:Record . {field, rec}
}

Basically, what we are saying is that there's an empty record {} and concatenation operator
::: that appends another record element to an already existing record. I.e.:
r1 = x1:t1 ::: {} is equal to r1 = {x1:t1}, while
r2 = x1:t1 ::: x2:t2 ::: {} is equal to {x1:t1, x2:t2}.

These operators are built-in, but thanks to them we can define record manipulation functions:
-}

recmap = \func:({String, Type}->{String, Type}) rec . {
    {}     -> {};
    r:::rs -> (func r):::(recmap func rs) 
} 
-- func has a weird type of converting one field to another field. Since we have dependent functions,
-- it should work for us quite well.