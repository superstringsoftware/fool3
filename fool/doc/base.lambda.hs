-- Base library based on the lambda syntaxis described in such named file

-- boxed primitive number types
I# : Int    = \x:Int#.    {_}
D# : Double = \x:Double#. {_}

True, False : Bool;
Nothing, Just = \x:a. {_} : Maybe a;
Nil, (::) = \x:a xs:(List a). {head tail} : List a;

-- same syntaxis as for typeclasses, cleaner?
Bool : Type = {
    True, False
}
Maybe : Type = \a:*. {
    Nothing 
  , Just = \x:a. {_}
}

Nat : Type = {
    Z
  , S = \k:Nat. {_}
}

id : a = \x:a. x;

Eq : Class = \a:*. {
    (==):Bool = \x:a y:a. not (x /= y)
  , (/=):Bool = \x:a y:a. not (x == y) 
}

-- for constraints, we should still parse them and keep em - eventually may come up with some
-- testing method that checks some random examples and tries to find bad ones
Semigroup : Class = \a:*. {
    (+):a = \x:a y:a.
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
  , constraint associativity = forall x:a, y:a, z:a => x + (y + z) == (x + y) + z 
}

Monoid : Class = \a : (exists Semigroup a). {
    Z0:a 
  , constraints = forall x:a => {
        Z0 + x == x, x + Z0 == x
    }
}

Group : Class = \a : (exists Monoid a). {
    (-):a = \x:a y:a.
  , constraints = forall x:a => {
        Z0 - x + x == Z0
    }
}

AbelianGroup : Class = \a:(exists Group a). {
    constraints = x + y === y + x -- adding commutativity
}

Ring : Class = \a : (exists Group a). {
    Z1 : a
  , (*):a = \x:a y:a.
  , constraints = {
        commutativity = forall x:a, y:a => x + y == y + x -- require Monoid to be commutative
      , isAMonoid = forall x:a => {Z1 * x == x, x * Z1 == x} -- Ring is itself a Monoid in (*)
      , distributivity = forall x:a, y:a, z:a => { 
            x * (y + z) == x * y + x * z
          , (x + y) * z == x * z + y * z
        }
      , forall x:a => (Z0 * x == Z0, x * Z0 == Z0)
    }
}

Field : Class = \a : (exists Ring a). {
    (/):a = \x:a y:a.
  , constraints = forall x:a => (Z1 / x * x == Z1)
}

-- dependent vector
Vect : Type = \n:Nat a:*. {
    Nil  : Vect     Z a
  , (::) : Vect (S k) a = \head:a tail:(Vect k a). {head tail}
}

-- same but with Ints
VectI : Type = \n:Int a:*. {
    Nil  : VectI     0 a
  , (::) : VectI (k+1) a = \head:a tail:(Vect k a). {head tail}
}


-- https://www.wikiwand.com/en/Module_(mathematics)
-- Module (Left-R)
-- m has to be Abelian Group! (so commutative in + for module elements)
Module : Class = \r:(∃ Ring r) m:(∃ AbelianGroup m). {
    (·):m = \s:r v:m. -- scalar multiplication
  , constraints = {
        r·(x+y)===r·x+r·y
      , (r+s)·x===r·x+s·x
      , (r*s)·x===r·(s·x)
      , z1 · x === x
    }
}

-- Vector space is simply a module over a field, so
VectorSpace : Class = ∃(Field r, Module r m) => \r:* m:*. {
-- what's inside? need to define scalar division for vectors at least with default implementation
    (./.):m = \v:m s:r. (z1 / s) • v
}

RealVectorSpace = VectorSpace Double -- defining another typeclass where Field r is set to Double! i.e.:
-- equivalent to:
RealVectorSpace : Class = ∃(Module Double m) => \m:*. {
    (./.):m = \v:m s:Double . (1/s) • v
}

InnerProductVectorSpace : Class = ∃ VectorSpace r m => \r:* m:*. {
  -- inner product, dot product in case of regular coordinate stuff
    (·):r = \v1:m v2:m. dot v1 v2
  , dot:r = \v1:m v2:m. v1 · v2
}

NormedVectorSpace : Class = ∃ InnerProductVectorSpace r m => \r:* m:*. {
    norm:r = \v:m. sqrt (v · v)
}


-- Algebra
-- https://www.wikiwand.com/en/Algebra_over_a_field
F_Algebra : Class = ∃(VectorSpace f a) => \f:* a:*. {
    (•):a = \v1:a v2:a -- "vector multiplication", but here it's a generalization anyway
  , constraints = {
        (x + y) • z === x • z + y • z -- left distributivity
      , x • (y + z) === x • y + x • z -- right distributivity
      , (s·x)•(r·y) === (s*r)·(x•y) -- compatibility with scalars
    }
}

-- Dependent types for efficient vectors
PVector : Type = \a:* n:Int. {
    PVector = \a:* n:Int . {primarray# a n} : PVector a n
    -- now, this is tricky. Constructor simply allocates memory for the given *primitive* type
}

generate : PVector a n = \n:Int g:(Int->a).

-- some instances
-- Can define all operations in the subclass! (how to implement?)
Field Double = {
    Z0 = 0
  , Z1 = 1
  , (+) = (+#)
  , (-) = (-#)
  , (*) = (*#)
  , (/#) = (/#)
}

RealVectorSpace (Vect n Double) = {
    
}

-- Now, what if we want a (*) operation that would work on Vectors and their components, if they are a Ring themselves?
-- Here, a is already a Ring (e.g., Doubles) and we are defining a Ring for b over this Ring, e.g. Vectors or Polynomials
-- In essence, it means we already have 2 closed rings for a and b, now we just want to define a*b = b*a
RingOverRing : Class = \a:(∃ Ring a) b:(∃ Ring b). {
    (*):b = \x:a y:b
  , (*):b = \x:b y:a
}
-- The above will not type check in Haskell without defining 2-.. separate multiparam typefamilies
-- there has to be a better syntaxis to reflect this:
instance RingOverRing Double (Vector Double) where type Res Double (Vector Double) = Vector Double ...
instance RingOverRing (Vector Double) Double where type Res (Vector Double) Double = Vector Double ...
-- and we want to make sure it can be written in ONE DECLARATION!!!
-- HOW??????
-- We require: 1 return type (WHY? With dependent functions do we really need it?) ==>
-- ==> main: arguments need to define type of return statement unambigously!
-- 2) ability to let the function know we don't care about the order of the arguments -- IMPORTANT!!!
-- this point ^^^ may lead to new approach, it's not currying, but rather "bag of arguments" where order is not important
-- ==> so we are back to Tuples?

-- SO LET'S START WITH DEFINING THIS CLASS FOUNDATION FOR MATH AND ALGEBRA STUFF



-- enough with abstract algebra
fact : a = ∀a ∃ Num a =>  \n:a. n ?
    0         -> 1
  | otherwise -> n * fact (n - 1);


VarId : Expr = \n:Name . {_}
Lam   : Expr = \v:Var e:Expr t:Type . {_ _ _}  

{-
data Expr = 
    VarId Name
  | Lam    Var Expr Type -- currying based machinery
  | TLam [Var] Expr Type -- tuple based machinery
  | App Expr Expr
  | Tuple ConsTag [Expr] Type -- polymorphic tuple. 
  | Let Var Expr -- binding a var / symbol to an expression

-- Functor
class Functor f = 
  fmap:(f b) g:(a->b) x:(f a);

instance Functor Maybe = 
  fmap f x = x ? Nothing -> Nothing | Just y -> Just (f y);

instance Functor List = 
  fmap f l = l ? Nil -> Nil | (x::xs) -> (f x) :: (map f xs);
    
    

instance Eq Int = (==) x y = primop_equals x y;
instance Semigroup Int = (+) x y = primop_plus x y;
instance Monoid Int = z0 = 0;
instance Group Int = (-) x y = primop_minus x y;

-- need to separate functions somehow explicitly, otherwise wrong parsing
instance Ring Int = 
    (*) x y = primop_mul x y,
    z1 = 1;

-- instance for the type constructor:
instance Semigroup (List a) = 
  (+) ls1 ls2 = concat ls1 ls2;

instance Semigroup (Vector a n) where exists Semigroup a = 
  (+) v1 v2 = primop_plus;



-- now this has appeared as I was thinking about generalizing fibonacchi sequence, canonically defined as
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n - 2)
-- naive attempt was to require a Ring, b/c there's 0 and 1, but there's no 2nd binary op in fibonacci, 
-- so it's only a Group, but it needs to be *ordered* as there's no other way to get 1 from z0 in a monoid / group.
-- once we do that, we can define generalized fibonacci (see below)
class Ordered a =
  succ:a element:a;

-- generalized fibonacci, the only requirement is that n is an Ordered Group and we define it for n >= z0
fib n = n ?
    z0 -> z0
  | succ z0 -> succ z0
  | otherwise -> fib (n - 1) + fib (n - 2);

-- as a note, of course it's possible to define (+) and (-) via succ and it's an interesting exercize, but not very practical

-- TEST PROGRAM

f x y = x*x + y;

g = f 1;
h = (f 1) 3;
h' = f 1 3;

k = f x y z;

fact n = n ?
    z0 -> z1
  | otherwise -> n * fact(n-1);


main = print (fact (f (g 2) 1)); -- should be 120

test = 1::2::3::[];

l1 = [1,2,3,4];
v1 = <1,3,2,3.3>;
-}