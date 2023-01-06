-- File containing various language constructs that we will need to test parsing and representation of

-- we can't parse pattern matches with operators using operator syntax on the left side, need to use function syntax!
-- but lambda syntax is preferred anyway

--I# : Int    = \x:Int#.    {_};

False : Bool = {};
True : Bool = {};

Nothing : (Maybe a) = {};
Just : (Maybe a) = \ x:a. {_};

Nil : (List a) = {};
-- if a function OUTSIDE of typeclass has NO body (; right after last argument) -- it's a DATA CONSTRUCTOR
-- "Normal" functions MUST be defined right away.
Cons : (List a) = \ :a :(List a); -- . {head tail};
-- testing GADTs - typing env gets them in automatically and looks like correctly
UnitList : (List Unit) = \ n:Int;

Eq : Class = \a:Type . {
    (==):Bool = \x:a y:a. not (x /= y);
    (/=):Bool = \x:a y:a. not (x == y); 
    (≠) = (/=)
};

Semigroup : Class = \a:Type . {
    -- "normal" function has no body - it's ok because it's a typeclass definition!
    (+):a = \x:a y:a;
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
    associativity = x + (y + z) == (x + y) + z 
};

Monoid : Class = ∃ Semigroup a => \a. {
    E0:a
};

Semigroup Int = {
    -- (+) (I# x) (I# y) = I# (x +# y) - pattern matched option, works, but lambda should be preferred (why?):
    (+) = primplus_int
};
Monoid Int = {
    E0 = 0
};

-- pattern match syntax for functions - match ONLY arguments!!!
-- errors in wrong number of arguments are caught!
map = \f ls:(List a) . {
    _ Nil -> Nil;
    f (Cons x xs) -> Cons (f x) (map f xs)
};

length:Int = \ ls:(List a) . {
    Nil -> 0;
    (Cons _ xs) -> 1 + length xs
};

fact:Int = \n:Int . {
    0 -> 1;
    n -> n * fact(n-1)
};

s = "hello";

g = <1, 27.4, 14>;



(*) x y = x *# y;

Person : Person = \ 
    name:String 
    age:Int 
    salary:Int;

f = map g;

sq = \n. n * n;
h = \x. x + 2;