{-
Minimal baselib
-}


id : a -> a;
id x = x;

Bool     : Type = { True, False };
Ordering : Type = { LT, EQ, GT  };
Maybe    : Type { a:Type } = { Nothing, Just { :a } };
List     : Type { a:Type } = { Nil, (::) { head:a, tail:(List a) } };
Pair     : Type { a:Type, b:Type} = { Pair { fst:a, snd:b } };

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

∃ Semigroup a => Monoid:Class {a} = { E0:a };

Semigroup Int = {
    (+) = primPlusInt
};

Monoid Int = {
    E0 = 0
};


------------------ some list functions -----------------

length : List a -> Int;
length [] = 0;
length (x::xs) = 1 + length xs;

map : (List b) { func:(a->b), ls:(List a) };
map _ [] = [];
map f (x::xs) = (f x)::(map f xs);

--------------------- test program --------------------

ls = [1,3,4,3];

fib {n:Int};
fib 0 = 1;
fib 1 = 1;
fib n = fib(n-1) + fib(n-2);

main = print# (fib 20);
-- main = print# (map (+2) ls);

-- Expr:Type {a} = {Val:(Expr Int) {:Int}, Bool:(Expr Bool)};




