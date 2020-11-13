# lambda

## Implementation thoughts

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