{-
Foundational types and functions - NO CUSTOM OPERATORS!!!
-}

id (x:a) : a = x 

Bool     : Type = ( True, False )
Ordering : Type = ( LT, EQ, GT  )

not (:Bool) : Bool
not (True)  = False
not (False) = True

or (:Bool, :Bool) : Bool
or (True, _)      = True
or (False, True)  = True
or (_, _)         = False

and (:Bool, :Bool) : Bool
and (True, True) = True
and (_,_) = False

-- desugaring if then else construct
-- This is a cool function, "a" can be anything, which will help us to define a dependent pifte below!
ifte [a:Universe] (cond:Bool, ex1:a, ex2:a) : a
ifte (True,  ex1, _) = ex1
ifte (False, _, ex2) = ex2

-- making a crazy dependently typed if then else with the help of ifte:
-- this works because ifte works on types exactly the same as it does on values!
pifte [a,b:Type] (cond:Bool, ex1:a, ex2:b) : ifte(cond,a,b)
pifte (True,  ex1, _) = ex1
pifte (False, _, ex2) = ex2

Eq (a:Universe) : Sigma = (
    eq  (x,y:a) : Bool = not (neq (x,y)),
    neq (x,y:a) : Bool = not (eq  (x,y)),
    required = or (eq,neq)
)

-- Don't need type signature for a below since it is given in the superclass definition
Ord (∃ Eq (a) => a : Type) : Sigma =  (
    compare (x,y:a) : Ordering = if eq(x, y) then EQ else if lte (x,y) then LT else GT,
    lt  (x,y:a) : Bool = and (lte (x,y), neq (x,y)),
    lte (x,y:a) : Bool = let r = compare (x, y) in 
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