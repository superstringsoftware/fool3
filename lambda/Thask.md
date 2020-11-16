# Thask Surface Language summary

**lambda** comes with a reference surface language implemented. You can use it as is or use it as a basis for implementing your own surface languages that will utilize powerful lambda Core and Realms infrastructure. Below is a summary of all lambda features compared to Haskell and "C-land" to get you started real quick.

**lambda** syntax is based on two very simple, but very powerful concepts:
- Everything is a tuple - or a typed record - represented as `{x1:t1, ... , xn:tn} : t`. This allows us to model algebraic data types, type functions, dependent types, type families etc - all using very similar syntax
- Everything is a lambda abstraction - without any free variables in case of a constant

This approach gives rise to a very powerful language expressed via very simple concepts. Let's jump in!

### Basics
Tuples / records: `(x1:t1, ..., xn:tn)` - typed tuples are first class objects! (unlike Haskell)
Lists: `[x1,...,xn]`
Vectors: `<x1, ..., xn>`

### Algebraic datatypes
Type system supports the usual ADTs - both product and sum types. Here are the familiar examples:

```Haskell
type Bool = True | False
type Maybe a:U = Nothing | Just :a
type List a:U = ([]) | (::) :a :(List a)

-- Named Record
type Person = Person 
{
    fname, lname : String,
    age : Int,
    dob : Date
}

-- the below statements are equal
james = Person "James" "Smith" 34 (Date 22 09 1978)
alsoJames = Person {
    fname = "James",
    lname = "Smith",
    age = 34,
    dob = Date 22 09 1978
}

-- accessing record fields:
main = print $ "Name: " + p2.fname + " " + p2.lname

-- You can have sum types with records, as well as parametrized of course:
type Num a => Shapes a = 
    Box {
        x, y, w, h : a
    } |
    Circle {
        x,y,r : a
    }

-- You can also use fully qualified data constructor names:
s = Shapes.Circle 10 20 50;
-- This will be very handy in more advanced type definitions.
```

These are just the very basic ADT examples, easily implemented in Haskell. It gradually gets more and more interesting with extensible records, dependent types and type families, but let's cover the other basics first.

### Functions

Basic functions are the same as in Haskell.

```haskell
g = x + 2
-- pattern matching:
fact 0 = 1
fact n = n * fact (n-1)

-- type signatures are given with 1 semicolon:
length : [a] -> Int
length [] = 0
length (x::xs) = 1 + length xs
```

### Generalized ADTs 
For motivating example, see the (Haskell notes on GADTs)[https://wiki.haskell.org/Generalised_algebraic_datatype]. 

Here's a simple Haskell example and how it would be written in lambda:
```Haskell
data Empty
data NonEmpty
data List x y where
     Nil :: List a Empty
     Cons:: a -> List a b ->  List a NonEmpty

safeHead:: List x NonEmpty -> x
safeHead (Cons a b) = a

-- Lambda:
Empty    : Type;
NonEmpty : Type;

List : Type = \x,y:Type . {
    Nil  : List a Empty;
    Cons : List a NonEmpty = :a :(List a b)
}
```
Basically, we just give an explicit type signature for the constructor functions and that's it. 

### Extensible sum and product types (records)
With extensible sum types, it's also extremely easy. Extensible sum types are basically data families in Haskell, but we can do much more powerful things in lambda. Recall type `Shapes` from above and let's say we imported a module that defines `Shapes` but want to add another shape to it. All we need to do is explicitly write a data constructor with an extension operator on sum types:

```Haskell
Shapes += Point { x,y : a }
```

Now our `Shapes` data type contains 3 data constructors: `Box`, `Circle` and `Point`. However, this creates one complication - functions that work on our types. Let's say we had a function that calculated an area of a shape. Now we need to make sure it works for `Point` as well. Here's how:

```Haskell
-- original area function:
area s@(Box ...)    = s.w * s.h
area s@(Circle ...) = s.r * s.r * pi / 2


-- extending with another pattern elsewhere is trivial:
area (Point ...) = 0;
```

Very easy! 

We can do the same with records, which comes very handy when we want to interact with Realms of .Net, javascript and other object oriented languages, which use inheritance quite a bit. Recall the type `Person` from before. We can extend it to `Employee` like this:

```Haskell
type Employee = Person * {
    salary : Int, doe : Date
}

-- Once we have defined it, we can us Employee *everywhere* where we can use Person, e.g.:
fullname : Person -> String
fullname p = p.fname + p.lname

-- now recall person 'james' defined above, we can do:
jamesEmployee = Employee james 5000 (Date 20 12 2020) -- passing james for the first 3 fields of Employee

print (fullname james) -- ok, james is of type Person
print (fullname jamesEmployee) -- also ok, Employee is a subtype of Person
```

This is something we can't easily easily do in haskell except via making Person a part of the Employee record, which may become quite cumbersome to type for things such as GUI frameworks due to deep inheritances. With polymorphic `fullname` function - can't do it in haskell at all.

In fact, we can make it even more general, by omitting type signatures and simply stating:
```Haskell
fullname p = p.fname + p.lname
```
This will create an extremely general type contraint for our function - `exists Semigroup a => hasfields "fname":a "lname":a -> a` - which in effect says that it will work on any record that has fields named "fname" and "lname" so that the type of these fields is the same and supports the `Semigroup` class operation (+). How cool is that?

This brings us to the very powerful polymorphic mechanism of typeclasses.

### Typeclasses and type families
Type classes give us an ability to create polymorphic functions that work on different types using one name. Type families do the same, but also empower us to use different data constructor implementations for one and the same type signature (similar to GADTs). 

Strictly speaking, typeclasses create polymorphic functions dependent on type of the arguments, but since all mathematical structures as well as many of the extremely powerful FP / CT concepts (Functor, Monad etc) can be expressed as type classes or families, it's useful to treat them explicitly.

Some examples:
```Haskell
Semigroup : Class = \a:Type . {
    -- "normal" function has no body - it's ok because it's a typeclass definition!
    (+):a = \x:a y:a;
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
    associativity = forall x,y,z : a => x + (y + z) == (x + y) + z 
};

Monoid : Class = ∃ Semigroup a => \a . {
    E0:a
};

Semigroup Int = {
    (+) = primplus_int
};
Monoid Int = {
    E0 = 0
};

Functor : Class = \f:Type -> Type . {
    fmap : (f b) = \ func:(a->b) arg:(f a);
    identity = fmap id == id;
    composition = fmap (f · g) == fmap f · fmap g
}

Functor Maybe = {
    fmap = \ func:(a->b) arg:(Maybe a) . {
        _ Nothing   -> Nothing;
        f (Just x)  -> Just (f x)
    }
}
```


As mentioned above, `Semigroup` definition creates a type-dependent polymorphic function:
`(+) : a:Type -> (a->a->a)`
that given a concrete type returns a specific function that works on this type.
So, in theory we could have omitted the whole debacle with writing down the hierarchy of Semigroup - Monoid - Group and Functor - Applicative - Monad etc and simply define all the functions they contain separately as such polymorphic functions. However, this approach is more difficult to read and follow, human mind does like hierarchies.

### Dependent types
We'll omit the textbook example of a size-dependent List that you can find in Idris or Agda, this can be done trivially in lambda. We'll jump to the size-dependent vectors right away - as they are needed for practical applications much more.

```Haskell
VectorN : Type = \a:Type n:Int . {
    VectorN = data:(Array a n) n
}

-- then we can do things like:
vecplus : VectorN a n = \v1, v2 : VectorN a n . {
    -- implementation
}

Semigroup (VectorN a n) = {
    (+) = vecplus;
}

-- and then having vectors:
v1 = <1,3,2>;
v2 = <2,3>;
v3 = <5,8>;

-- we'll have:
v4 = v2 + v3; -- ok, both vectors are of type `VectorN Int 2`
v5 = v1 + v2; -- FAILS! won't typecheck, as the first is `VectorN Int 3` while the second is VectorN Int 2
```

### Objects
Let's say we want to model virtual inheritance as in C# / C++ or Java. Let's redefine our Shapes type for this purpose and turn it from the Sum Type to extensible product type.

```Haskell
Shape : Type = exists Num a => \a:Type . {
    x, y : a;
    area : a = 0;
} 
```

Here we have a new concept, we are making a member `area` of type `a` part of our record - it's like a type-level constant. However, we can overload `area` in subclasses like this:

```Haskell
Box : Type = Shape * { w,h : a};
Box.area = w*h;
-- alternative function overloading syntax:
Box : Type = Shape {
    area = w * h;
} * {w, h : a}

Circle : Type = Shape * {r : a};
Circle.area = r * r * pi / 2;
```
Here we have extended our Shape type with width and height, and then overloaded the `area` function with another expression. Not only that, but as you can see that when a function is a part of a record type, it can access other fields of this same record without the need to pass them in directly - so they behave very similar to "classic" methods in the c-land!

Now we can mimic the "virtual inheritance" functionality as follows:

```Haskell
s = Shape 0 0;
b = Box s 10 20;
c = Circle 10 10 10;

print s.area; -- 0, s is a Shape
print b.area; -- 10*20 = 200, b is a Box
print c.area; -- 10*10*pi/2, c is a Circle
```

Once we add Mutability to the mix, we can fully support OO paradigms.

As a side note, the above effect can be just as easily reached via typeclasses - by defining `Areable : Class = \a:Type { area:(exists Num b => b) = \s:a; }`. Whether we want to support dynamic typing and so downcasts, as well as accessing parent methods etc - the usual stuff in the C-land - remains to be seen.

### Mutability
Mutability is implemented via `Effects`.
### Realms
Extremely powerful mechanism to support "write once run anywhere" approach.
### Persistence
### Events and FRP
### Concurrent processes