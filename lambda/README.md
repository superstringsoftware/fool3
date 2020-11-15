# lambda

Compiler architecture for typed (supporting dependent types up to Calculus of Constructions) functional languages that supports:

- Any number of surface languages, parsed and translated to the Core lambda language
- Encourages Type-driven development
- Write once, run anywhere -> 'Realms' support with shared Type Universe: JS, .Net, barebone...
- Graph logic for "realworld" data - modeled via relations between records
- FRP for efficient and clean UI modeling
- Seemless interop with Realm languages (.Net, js)
- Object-oriented languages support, modelled in a 100% functional way

### some other feature highlights
- Strong math foundation
- Int, Float, Double, Byte, Ref and their Arrays primitive types - 'basis'
- String both constructed in our language as well as relies on JS / .Net etc libraries
- Dependent types
- Type families, generalizing typeclasses
- Good records
- Events as first class language constructs (FRP out of box?)
- Mutability etc via 'Effects' (??? - a lot of experimentation needed yet)

# Reference Surface Language summary

**lambda** comes with a reference surface language implemented. You can use it as is or use it as a basis for implementing your own surface languages that will utilize powerful lambda Core and Realms infrastructure. Below is a summary of all lambda features compared to Haskell and "C-land" to get you started real quick.

**lambda** syntax is based on two very simple, but very powerful concepts:
- Everything is a tuple - or a typed record - represented as `{x1:t1, ... , xn:tn} : t`. This allows us to model algebraic data types, type functions, dependent types, type families etc - all using very similar syntax
- Everything is a lambda abstraction - without any free variables in case of a constant

This approach gives rise to a very powerful language expressed via very simple concepts. Let's jump in!

### Algebraic datatypes
Type system supports the usual ADTs - both product and sum types. Here are the familiar examples:

```Haskell
Bool : Type = \ . {
    True  = {};
    False = {}
}

-- for constructors that return empty records (Unit type strictly speaking) we can omit the "{}"
-- Also, lambda abstraction can be omitted if there are no variables being bound, so:
Bool : Type = { True; False }

-- Constructor functions inside the type can be written down fully like this, and it makes sense
-- to do while learning the concepts:
Maybe : Type = \ a:Type . {
    Nothing;
    -- Just is a data constructor that takes a value of type `a` and returns a value of type `Maybe a`
    Just:(Maybe a) = \x:a {x}; 
}

-- However this is of course too verbose once you know what you are doing, so you can simply write:
Maybe : Type = \ a:Type . {
    Nothing;
    -- Just is a data constructor that takes a value of type `a` and returns a value of type `Maybe a`
    -- by storing this value in an anonymous field of a one-field record:
    Just = { :a }; 
}

List : Type = \ a:Type . {
    ([]) = {};
    (::) = { :a; :(List a) };
}

-- any record {x1:t1; ... xn:tn} can be rewritten as simply `x1:t1 ... xn:tn` - so no braces and no semicolons, so List becomes:

List : Type = \ a:Type . {
    ([]);
    (::) = :a :(List a);
}

-- Instantiating values:
x1 = True; -- type Bool inferred
x2 = Just 4; -- type Maybe Int inferred
ls = 4::5::7::[]; -- type List Int inferred

-- Named Record
{- 
    lambda abstraction can be omitted if there are no arguments to the function
    for single constructor types constructor name can be omitted and it defaults to the type name
-}
Person : Type =  
{
    fname, lname : String;
    age : Int;
    dob : Date;
}

-- the below statements are equal
james = Person "James" "Smith" 34 (Date 22 09 1978);
alsoJames = Person {
    fname = "James";
    lname = "Smith";
    age = 34;
    dob = Date 22 09 1978;
};

-- accessing record fields:
main = print ("Name: " + p2.fname + " " + p2.lname);

-- You can have sum types with records, as well as parametrized of course:
Shapes : Type = \ a:Type . {
    Box    = {x,y,w,h : a};
    Circle = {x,y,r : a};
}

-- You can also use fully qualified data constructor names:
s = Shapes.Circle 10 20 50;
-- This will be very handy in more advanced type definitions.
```

These are just the very basic ADT examples, easily implemented in Haskell. It gradually gets more and more interesting with extensible records, dependent types and type families, but let's cover the other basics first.

### Functions

Functions are obviously represented as lambda abstractions as well.

```Haskell
g:Int = \ x:Int . x + 2; -- simple function with explicit types
g = \x . x + 2; -- will deduce more general type (Semigroup, see below for type families)

-- pattern matching:
fact = \n:Int . {
    0 -> 1;
    n -> n * fact (n-1);
}

length:Int = \ ls:(List a) . {
    []    -> 0;
    x::xs -> 1 + length xs;
};
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
Shapes += {
    Point = { x,y : a }
}
```

Now our `Shapes` data type contains 3 data constructors: `Box`, `Circle` and `Point`. However, this creates one complication - functions that work on our types. Let's say we had a function that calculated an area of a shape. Now we need to make sure it works for `Point` as well. Here's how:

```Haskell
-- original area function:
area = \ s:Shapes . {
    (Box ...)    -> s.w * s.h;
    (Circle ...) -> s.r * s.r * pi / 2;
}

-- extending with another pattern elsewhere:
area (Point ...) = 0;
```

Very easy! 

We can do the same with records, which comes very handy when we want to interact with Realms of .Net, javascript and other object oriented languages, which use inheritance quite a bit. Recall the type `Person` from before. We can extend it to `Employee` like this:

```Haskell
Employee : Type = Person * {
    salary : Int;
    doe : Date
}

-- Once we have defined it, we can us Employee *everywhere* where we can use Person, e.g.:
fullname = \p:Person . p.fname + p.lname;

-- now recall person 'james' defined above, we can do:
jamesEmployee = Employee james 5000 Date (20 12 2020) -- passing james for the first 3 fields of Employee

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

### Mutability
Mutability is implemented via `Effects`.
### Realms
Extremely powerful mechanism to support "write once run anywhere" approach.
### Persistence
### Events and FRP
### Concurrent processes


# Under the Hood stuff

The below may be quite outdated in places, but keeping it here for historical reasons.

## High-level compilation pipeline logic

- Parse surface language file, setting up initial structure of functions and types
- Desugar into Core Representation
- Setup "function factories" for functions dependent on types (typeclasses at least, maybe something else)
- Typecheck & dialog with the user on building the types

## Core implementation thoughts

- "Program" is a list of top-level bindings, just like GHC.
- Everything is a lambda, data is tuples (also see below)
- Polymorphic functions are mostly handled at compile time via "function factories":
  - Typeclass definition creates a factory
  - Typeclass application to a certain type creates an entry in the factory table corresponding to the parameters given, e.g.:

```Haskell
class Semigroup a where 
    (+) :: a -> a -> a
-- creates something like:
Semigroup.plus = λ a:U . {...} : a -> a -> a
-- then instantiation creates an entry in the table:
instance Semigroup Int where 
    (+) = primop_plus
-- we are adding an entry to
Semigroup.plus.add (a = Int, primop_plus)
-- then if we have 
4 + 5
-- we lookup "Semigroup.plus" instead of '+' in the global function lookup table (hashtable in implementation?), and then find a specific function once we can be sure of the type of parameters.
```

The above hints that we don't even need actual `typeclasses` anywhere during compilation and especially runtime, it's more for human readability - nice to have the hierarchy etc; also gives better error messages.

**!NB** How to handle general functions dependent on type? Shall we allow such functions *outside* of typeclasses?

```haskell
fmap = λ f:U->U g:a->b x:a . {} : f b
fmap:(f b) f:U->U g:a->b x:a = ...
fmap : f:(U->U) -> g:(a->b) -> x:a -> b
fmap f:(U->U) g:(a->b) x:(f a) -> b 
-- then
fmap Maybe _ Nothing  = Nothing
fmap Maybe f (Just a) = Just (f a)
-- however, Maybe can be inferred from the arguments anyway?
-- Still, typeclasses give a much better hierarchy for the human mind
```

## Low level implementation thoughts

We want to store all data as Tuples:

```js

{n1:t1, ..., nk:tk} : tt

```

that translate nicely to .Net classes / structs or JS Objects.

### SumTypes problems
Problems arise with Sumtypes primarily: when several totally different records are supposed to be referenced somehow from one field in our representation - which is probably easier to resolve with JS than .Net.

### Single constructor types

Straightforward, 1-1 translation to a Class.

### 2-constructor types with 1 Unit constructor

Examples are Maybe, List etc. 
Here, we also translate to a single Class and use "null" in place of the Unit constructor. That should improve performance for the most common foundational functional data structures cases.

### SumTypes with more constructors, or with 2 constructors both of which are not Unit

This needs to be resolved generically. Let's start with a simple example

```Haskell
data Widgets = Box Int Int | Circle Int Int Int
```

These constructor functions would generate records similar to:

```c#
public class Box {
    int _f0, _f1;
    public static int _constag = 1; 
}

public class Circle {
    int _f0, _f1, _f2;
    public static int _constag = 2;
}
```

Then let's say we have a function
```Haskell
area (Box w h) = w * h
area (Circle x y r) = pi * r * r
-- then we can use it on any shape:
s1 = Box 10 20
s2 = Circle 10 20 30
a1 = area s1 -- box option is called
a2 = area s2 -- circle option is called
```

In principle, straightforward way to resolve it is to define functions for Box and Circle separately and treat them as functions that *depend not on types per se, but on the constructor function within the type!* Which in the strict type-theoretic view is still a function dependent on type, since SumTypes are simply, well, sums of other types.

Then we can have a generic approach to Typeclass functions - they take typeclass parameters as parameters! - as well as functions defined for sum types take constructor function as a parameter, taking semigroup and the `area` functions above as examples:

```Haskell
class Semigroup a where {
    (+) :: a->a->a
}
```

they would be represented something like:

```haskell
(+) = λ a:U . {some lookup table depending on the type} : a -> a -> a
-- similarly, for area. Note that it's a "function factory", that returns a function depending on the constructor!!! Same as (+) above is a function factory!!!
area = λ c:(constructor of w)  w:Widget . {
    case c of 
        Box    -> area_box    w = w._f0 * w._f1
        Circle -> area_circle w = pi * w._f3 * w._f3

} : Widget -> Int
```

So, we are creating **Function Factories**, which are basically lookup tables that our compiler / typechecker stores at the code generation stage. Whether we want to propagate them to dynamics - is an open question, but definitely interesting to keep it as an option in case we want to implement some logic where type is unknown at the compilation stage. However, with dependent types it shouldn't be the case?

#### More difficult question: type containing sum type as one of the fields

Continuing the `Widget` example:
```Haskell
data Strange = Strange Int Widget
```

So we have a record that contains an int and a Widget:
```c#
public class Strange {
    int _f0;
    Widget _f1; // <-- wrong! no such type!
}
```

**NB!** Sort of a critical point - since .Net is inherently typed in the dynamic stage as well, we can simply stop worrying and use dynamic typecasts relying on .Net CLR implementers to handle performance issues. Most of them will be addressed via "nullable types" approach anyway.

Then, we can model SumTypes as descendants from one empty parent type and then use `x as Type` casts at runtime. After all, GHC does check constructor tag as well at runtime.

**NB!** This approach is difficult with LAZY evaluation though!!! (since it drives the need for thunks etc)

Using this approach above, function area simplifies further:
```c#
public int area(Widget w) {
    switch (w._constag) {
        case 1: return (w._f0 * w._f1);
        case 2: return (w._f3 * w._f3); 
    }
    // need to add a cast in the code above, as we are doing elsewhere already.
}
```