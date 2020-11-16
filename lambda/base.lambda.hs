{-
File to test all possible Syntaxis in parsing!
-}

-- Testing currently:
Bool : Type = { True, False };

-- map : (a->b) -> List a -> List b
-- map func:(a->b) ls:(List a) -> List b
#doc map = "function that takes a function from type a to b - func - and applies it to all ls list members"
     func = ""
#end
map:(List b) { func:a->b, ls:List a } = {
    _ [] -> [],
    f (x::xs) -> (f x)::(map f xs)
}
plus2:Int { x:Int } = x + 2;
Maybe:Type { a:Type } = { Nothing, Just :a }
Semigroup:Class { a:Type } = {
    (+):a { x, y : a },
    associativity = x + (y + z) == (x + y) + z 
}
Person:Type = {
    Person {
        fname, lname : String,
        age : Int,
        dob : Date
    }
}
Nil :(List a)
Cons:(List a) { :a, :List a }
Person:Person {
    fname, lname : String,
    age : Int,
    dob : Date
}

{-

l = [1,3,4];
v = <1,3,4>;
f = {1,3,v, 2+2};

-- Function with explicit type signatures: OK
id : a = \x:a . x;
-- Function without type signatures: OK 
mul = \x . x * 4;
-- same as a pattern match:
mul x = x * 4;

-- Function defined as lambda: OK, but see note
fact = \n . {
    0 -> 1; -- semicolon here and a colon in type definition, need to be consistent, colons everywhere?
    n -> n * fact(n-1)
};
-- Function defined as pattern match: OK
fact 0 = 1;
fact n = n * fact(n-1);
-- 2-var function defined as pattern match: OK
map _ [] = [];
map f (x:-xs) = (f x):-(map f xs);
-- Complex constructor pattern match: OK
complexFunc (Cons (Var 4 name)) f = f name;

-------------------- TYPES ----------------------
-- Simplest sum type: OK? Returns Tuple, we may want to change that.
Bool : Type = { True, False };
-- Sum type with one field anonymous tuple: OK?
Maybe : Type = \a . { Nothing, Just :a };
-- with 2 - FAILS
-- List : Type = \a . {Nil, Cons :a :(List a)};

-- Every constructor separately - OK, so GADTs should work fine.
Nil : (List a) = {};
-- if a function OUTSIDE of typeclass has NO body (; right after last argument) -- it's a DATA CONSTRUCTOR
-- "Normal" functions MUST be defined right away.
Cons : (List a) = \ :a :(List a); -- . {head tail};

-- Typeclasses - OK
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
-}
