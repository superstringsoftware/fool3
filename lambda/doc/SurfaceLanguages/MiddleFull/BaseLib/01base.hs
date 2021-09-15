{-
Foundational types and functions
-}

id (x:a) : a = x 

Bool     : Type = ( True, False )
Ordering : Type = ( LT, EQ, GT  )

not (:Bool) : Bool
not True  = False
not False = True

(||) (:Bool, :Bool) : Bool
True  || _     = True
False || True  = True
False || False = False

(&&) (:Bool, :Bool) : Bool
True && True  = True
_ && _ = False


Eq (a:Type) : Sigma = (
    (==) (x,y:a) : Bool = not (x /= y),
    (/=) (x,y:a) : Bool = not (x == y),
    (≠) = (/=),
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
)

-- Don't need type signature for a below since it is given in the superclass definition
Ord (∃ Eq a => a : Type) : Sigma =  (
    compare (x,y:a) : Ordering = if x == y then EQ else if x <= y then LT else GT,
    (<)  (x,y:a) : Bool = (x <= y) && (x /= y),
    (<=) (x,y:a) : Bool = let r = compare (x, y) in 
                                  if (r == LT) || (r == EQ) then True else False,
    (>)  (x,y:a) : Bool = y < x,
    (>=) (x,y:a) : Bool = y <= x,
    max (x,y:a) : a = if x >= y then x else y,
    min (x,y:a) : a = if x <= y then x else y,
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
)

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