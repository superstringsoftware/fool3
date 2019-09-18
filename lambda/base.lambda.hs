-- we can't parse pattern matches with operators using operator syntax on the left side, need to use function syntax!

I# : Int    = \x:Int#.    {_};

False : Bool = {};
True : Bool = {};

Nothing : (Maybe a) = {};
Just : (Maybe a) = \x:a. {_};

Nil : (List a) = {};
Cons : (List a) = \x:a xs:(List a); -- . {head tail};

Eq : Class = \a:Type . {
    (==):Bool = \x:a y:a. not (x /= y);
    (/=):Bool = \x:a y:a. not (x == y); 
    (≠) = (/=)
};

Semigroup : Class = \a:Type . {
    (+):a = \x:a y:a;
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
    associativity = x + (y + z) == (x + y) + z 
};

Monoid : Class = ∃ Semigroup a => \a. {
    Z0:a
};

Semigroup Int = {
    (+) (I# x) (I# y) = I# (x +# y)
};
Monoid Int = {
    Z0 = 0
};

-- pattern match syntax for functions - match ONLY arguments!!!
map = \f ls:(List a) . {
    Nil -> Nil;
    f (Cons x xs) -> Cons (f x) (map f xs)
};

length:Int = \ ls:(List a) . {
    Nil -> 0;
    (Cons _ xs) -> 1 + length xs
};

s = "hello";

g = <1, 27.4, 14>;

square n = n * n;

(*) x y = x *# y;