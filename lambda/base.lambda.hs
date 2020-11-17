{-
Minimal baselib
-}


id : a -> a;
id x = x;

Bool     : Type = { True, False };
Ordering : Type = { LT, EQ, GT  };
Maybe    : Type { a:Type } = { Nothing, Just { :a } };

not True  = False;
not False = True;

Eq : Class { a } = {
    (==):Bool { x:a, y:a } = not (x /= y)
  , (/=):Bool { x:a, y:a } = not (x == y) 
  , (≠) = (/=)
  , required = (==) || (/=)
};

{-
-- Don't need type signature for a below since it is given in the superclass definition
Ord : Class = ∃ Eq a => \a . {
    compare : Ordering { x ,y : a } = 
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
}
-}


Semigroup:Class { a:Type } = {
    (+):a { x, y : a },
    associativity = x + (y + z) == (x + y) + z 
};

∃ Semigroup a => Monoid {a} = { E0:a };

Semigroup Int = {
    (+) = primplus
};

Monoid Int = {
    (+) = primplus,
    E0 = 0
};


