{-
Let's try this thing bottom up.
- Strong math foundation - base types, efficient vectors (sized?), basic typeclasses (for polymorphic operations).
- Basic input / output based on Effects
- Int, Float, Double, Byte, Ref and corresponding arrays - built in types
-}

-- Tuple { x1:t1, ..., xn:tn } : t - is the ONLY data format we work with, so full syntaxis is showing it explicitly
type Bool
cons True  = {} : Bool
cons False = {} : Bool
-- however, to avoid verbosity we can omit tuples in such easy cases

type Maybe a:U 
cons Nothing  -> Maybe a
cons Just x:a -> Maybe a

-- or in full form
cons Nothing  = { } : Maybe a
cons Just x:a = {x} : Maybe a -- this is much better to grasp, basically constructor Just is a function that receives x of type a and returns tuple {x} of type Maybe a
-- or, anonymous:
cons Just :a = {_} : Maybe a

-- even shorter format for anonymous cases
type Bool = True | False
type Maybe a:U = Nothing | Just :a
type List  a:U = ([]) | (::) :a :(List a)

class Semigroup a:U = {
    (+) { x, y : a } -> a;
    law "associativity" ∀x, y, z : a => x + (y + z) == (x + y) + z; 
}

class Semigroup a:U => Monoid a = {
    E0 : a;
    law "commutativity" ∀x:a => { Z0 + x == x; x + Z0 == x; }
}

-- no need to define each superclass separately, can define all operations in the subclass - e.g., here (+) is taken from Semigroup
Monoid Int = {
    (+) = Realms.prim.int_plus;
    E0 = 0;
}

func fact n:Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- dependent types and GADTs in one go:
type Vector a:U n:Int
cons IntVector n:Int = {intarray n, n} : Vector Int n
cons FloatVector n:Int = {floatarray n, n} : Vector Float n
cons Vector a n:Int = {boxedarray a n, n} : Vector a n


{- -------------------------------------------------------------------------------------- -}

type Bool
True  : Bool
False : Bool

type Maybe A:U
Nothing : Maybe A:U
Just    : x:A -> Maybe A:U

cons Nothing =  { } : Maybe A
cons Just x:A = {x} : Maybe A


type Expr A:U
I : i:Int  -> Expr Int
B : b:Bool -> Expr Bool

cons I i:Int = {i} : Expr Int
cons B b:Bool = {b} : Expr Bool

type List A:U
([]) : List A:U
(::) : A -> List A -> List A

cons ([]) = {} : List A
cons (::) head:A tail:List A = {head, tail} : List A

type ListN A:U n:Int
([]) : ListN A 0
(::) : A -> List A k -> List A (k+1)

cons ([]) = {} : List A 0
cons (::) head:A tail:List A k = {head, tail} : List A (k+1)

cons (::) head:A tail:List A k -> List A (k+1)


class Semigroup A:U = {
    (+) x:A y:A -> A;
    law "associativity" ∀x, y, z : A => x + (y + z) == (x + y) + z; 
}

Semigroup Int = {
    (+) = primint_plus;
}

class Semigroup A:U => Monoid A = {
    Z0 : A;
    law "commutativity" ∀x:A => { Z0 + x == x; x + Z0 == x; }
}

Monoid Int = {
    Z0 = 0;
}

type Person
cons Person 
    fname, lname : String
    age : Int
    dob : Date 
    -> Person

record Person = 
    fname, lname : String 
  * age : Int
  * dob : Date

record Employee = Person * salary : Int * doe : Date 

record Person = {
    fname, lname : String; 
    age : Int;
    dob : Date;
}

record Employee = Person * { salary : Int; doe : Date } 

class Relationship from:U to:U = {
    get obj:from -> to;
}


record Num A:U => Point A = { x, y : A }
record Box A = Point A * { w, h : A }

area bx = bx.w * bx.h

type Vector a:U n:Int
cons IntVector n:Int = {intarray n, n} : Vector Int n
cons FloatVector n:Int = {floatarray n, n} : Vector Float n
cons Vector a n:Int = {boxedarray a n, n} : Vector a n

