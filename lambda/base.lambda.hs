{-
Minimal compilation - factorial
-}

fact:Int = \n:Int . {
    0 -> 1;
    n -> n * fact(n-1)
};

plus2:Int = \x:Int . x + 2;

Semigroup : Class = \a:Type . {
    -- "normal" function has no body - it's ok because it's a typeclass definition!
    (+):a = \x:a y:a;
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
    associativity = x + (y + z) == (x + y) + z 
};

Semigroup Int = {
    -- (+) (I# x) (I# y) = I# (x +# y) - pattern matched option, works, but lambda should be preferred (why?):
    (+) = primplus_int
};

Nothing : (Maybe a) = {};
Just : (Maybe a) = \ x:a. {_};

MInt = Maybe Int;