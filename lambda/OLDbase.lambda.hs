I# : Int    = \x:Int#.    {_};

False : Bool = {};
True : Bool = {};

Nothing : (Maybe a) = {};
Just : (Maybe a) = \x:a. {_};

Nil : (List a) = {};
(<>) : (List a) = \x:a xs:(List a); -- . {head tail};

plus : Int = \x:Int y:Int . x +# y;

Vector : (Vector a n) = \arr:(primarray# a n) n:Int . {_ _};


Nil  : (Vect 0 a) = {};
Cons : (Vect (k+1) a) = \head:a tail:(Vect k a). {head tail};

fact : Int = \n:Int;
fact 0 = 1;
fact n = n * fact (n - 1);

(+) : Int = \x y . x +# y;

g = map (+2) lst;

tr x y = x == y;

Person : (Person a) = \name:String 
    age:Int 
    tag:a; --. {name age tag};

-- Employee : (Employee <: Person) = \s:Int . {salary};
Employee = Person Int;

r*(x+y) == r*x+r*y;
(+) (I# x) (I# y) = I# (x +# y);

Test : Test = \f:(Int -> String);

t x y = x ≠ y;

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

tt : a;

ZZ;

Monoid : Class = ∃ Semigroup a => \a. {
    Z0:a
};

fact : a = ∃ (Num a, Ord a) => \n:a;

Semigroup Int = {
    (+) (I# x) (I# y) = I# (x +# y)
};

Monoid Int = {
    Z0 = 0
};

(!):Int = \n:Int;
(!) 1 = 1;
(!) n = n * (!(n-1));

(√) x = sqrt x;

t = √ 298.4;

concat : (Vector a (n+m)) = \v1:(Vector a n) v2:(Vector a m);

Functor : Class = \f:(Type -> Type) . {
    fmap:(f b) = \g:(a->b) x:(f a)
};

Functor List = {
    fmap f [] = [];
    fmap f (Cons x xs) = Cons (f x) (map f xs)
};

f (Cons x (Cons y _)) = x + y;

Cons : (List a) = \ :a :(List a);
Person = \ name:String age:Int salary:Int;