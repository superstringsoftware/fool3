# fool

This is a from scratch implementation of the type-theory based functional language with flexible compilation targets (initially - javascript, .Net, potentially x86 native) based on the learnings of the last 6 years.


## Key grammar concepts

### Sum Types
**type** is a keyword for sum type, most ubiquitous in FP:

```typescript
type Bool = {True, False}

type Nat = {Z, Succ {:Nat} }
// or, full correct version of the same:
type Nat() = {Z():Nat = {}, Succ(n:Nat):Nat = {n} }

// same for Maybe as an example:
type Maybe(a) = {Nothing, Just {:a} }
type Maybe(a) = {Nothing():Maybe(a) = {}, Just(x:a):Maybe(a) = {x} }
```

So, it is defined as **type** then name of the type with potential arguments, then list of constructor functions in curly braces.

Type name must start with a capital letter, as well as constructor names.

### Product Types
Above sum types are using product types defined with each constructor. Fool supports proper records as well, here are some examples and explanation.

```typescript
// definition that follows the sum type format above, perfectly fine
type Person = { Person {name:String, age:Int} }

// sum type of 2 record types - again, very similar to what we've seen above
type Shapes = { Circle {x,y,r: Float}, Square {x,y,a,b:Float} }

// however, for 1-constructor types this is an overkill, and since we will be using record 
// types quite a bit in real life, there's syntactic sugar for it:
record Person = {name:String, age:Int}
// the above expands into synonimous sum type with the only constructor Person - 
// much easier to manipulate this in the future
```

### Open SumTypes

Sometimes we may want to leave the sum type open - so that it may be extended by the other constructors in the future - this way we can mimic haskell data families etc.

```typescript
open type Shapes = { Circle {x,y,r: Float}, Square {x,y,a,b:Float} }

// adding another constructor, "Dot":
Shapes = Shapes + Dot {x,y:Float}
// alternatively:
constructor Dot(x,y:Float):Shapes = {x,y}
// need the constructor keyword here as we are introducing new constructor outside of type
// definition
```

Obvious downside of this is that we'll need to add additional case for all the functions that worked with this type will need to be refefined, that's why this works much better in conjunction with typeclasses and typefamilies, more on that below.

### GADTs 

GADTs are obvious and easy in fool, e.g. typical haskell example:
```haskell
data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool
```

translates into:

```typescript
type Expr(a) = {
    I (:Int)  : Expr(Int),
    B (:Bool) : Expr(Bool),
    Add (:Expr(Int), :(Expr Int)) : Expr(Int)
}
```

Also note how in the above we are skipping actual tuple definition. However this looks confusing, since in Maybe and record definitions we are using curly braces???
TODO: Fix this inconcistency - () look to be preferrable for consistency, as they denote function call / application, while {} denote a tuple.

### Functions
Functions are defined via pattern matching as usual, preferred way being with all patterns grouped at one place:
```typescript
function plus(x:Nat, y:Nat) : Nat = {
  (x, Z) -> x,
  (Succ(n), x) -> Succ (plus(n,x))
}
```

Different types of dependent functions: 

- Values from values - "normal" functions as above.

- Types from types - "type constructor" functions, such as List a, Maybe a etc, but also various type manipulation functions such as extending a sum type or adding a new field to the record type (see the example above in open sum types section).

- Values from types - basically, typeclasses:

```typescript
structure Semigroup(a:Type) = {
  function (+) (x:a,y:a):a,
  assoc : forall (x:a,y:a,z:a) => x+(y+z) === (x+y)+z
}
// the above generates a type-dependent function with implicit params:
function (+) [a:Type] (x:a,y:a):a
```

Apart from the structures that generate implicit parameter functions, we can have functions that depend on types explicitly:

```typescript
function ConsNum(a:Type) : Int 
// how many constructors does a given type have?

function StructuralEquality(a:Type,b:Type):Bool
// are two types structurally equal?
```

The challenge is to have such functions execute ONLY at compile time and in the interpreter, but not runtime, as we want to erase all (or at least most of) type info at runtime. This means we need to somehow have the ability to understand that a given function is dependent on type and identify it as such.

- Last one are classic "PI" types - types dependent on values, which encompasses both value to value PI functions as well as type constructors. Classic example - n-vectors:

```typescript
type List (a:Type,n:Nat) = {
  (:::) (x:a, xs:List(a,m)) : List(a, Succ(m)), // explicit type sig!
  Nil : List(a,Z)
}

function concat(x:List(a,n), y:List(a,m)) : List(a,m+n)
```

Both the constructors and the concat function return type depends on the values of the function. This opens up a very dangerous door to writing stuff like:

```typescript
f(4) = "Hello"
f("world") = True
f(False) = 29
// the type of f is something crazy like:
f(x:forall t in {int,string,bool} => t) : ft(x)
ft(int) = string
ft(string) = bool
ft(bool) = int
// or another example:
g(4) = "Hello"
g(5) = False
// where the type would be:
g(x:int) : gt(x)
gt(x) = {
  4 -> String,
  5 -> Bool
}
// and type of gt is:
gt(x : Set[4,5] ) : Type 
```

Basically, we probably *can* write stuff like that, but since the whole point of programming language is to make functions compose - good luck composing such functions with anything.

### Sets

The last example above raises another interesting question - sets and subsets. E.g., we have Nat, but what if we want to have a type of only even natural numbers?

```typescript
type EvenNat = { n:Nat | n mod 2 == 0 }
// or only squares?
type NatSquares = { n*n | forall n:Nat }
```

... aaaaand we are back to basics, need to start from types again.


## Things to take care of:

* Type levels - e.g., Nat on values level vs type level???

## Compiler internals

Current version deals with the simplest possible type set and a program:
```typescript
type Bool = { True, False };
type Nat = {
  Z,
  Succ (n:Nat)
};

function id (x:a):a = x;

function not (b:Bool) : Bool = {
    True -> False,
    False -> True
};

function plus (x:Nat,y:Nat):Nat = {
  plus (Z, x) -> x,
  plus (Succ(n), x) -> Succ (plus(n,x))  
};

action main = {
  one = Succ(Z),
  three = Succ(Succ(one)),
  res = plus(three,one),
  print# (res)
};
```

We are defining types Bool and Nat, functions not and plus, and then run a simple program that (magically) prints the result of 1+3.

However, such a simple case is enough to test the whole pipeline, from parsing to typechecking to optimizations to code gen.

Let's start with this in order - after the program is parsed into Expr via our parser, we need to build an environment for the current "module" so that it's convenient to operate with. If we forget typechecking for a second, the main thing we need to do is extract ALL lambdas to the top level, including constructor functions from the SumTypes. That is done in the **Environment** state.

### Structures

Structures are handling Haskell type classes and type families and their implementation raises a bunch of interesting questions to ponder. E.g., let's look at the following code:

```typescript
one:Nat = Succ(Z);
three:Nat = Succ(Succ(one));
function plus(x:Nat,y:Nat):Nat;

structure Semigroup(a:Type) = {
  function (+) (x:a,y:a):a;
}

instance Semigroup(String) = { (+) = concat# }

// then we encounter a couple of cases:
plus(one,three)
// and
"hello" + "world"
```

What's our compiler strategy?

**Semigroup** definition creates a function `(+) [a:Type] (x:a,y:a):a` with implicit type parameter. Then instance statement creates a function `"(+)$String (x:String,y:String):String` that should be returned by the `(+) [String]` call. Then:

For *plus*, we look it up in the environment, find a lambda, check arity, check types, everything is ok - so we give a compiled *plus* function call in the compiled code.

For *(+)* we look it up in the environment and we need to somehow see that it's dependent on the type and that we can't give a specific function right away but need to do some lookup, namely:

```typescript
(+) [a:Type] (x:a,y:a):a // found this
(+) [a:Type] ("hello":String, "world":String) // is the typed call we have encountered
// need to exctract the fact that a = String from the above and put it in the 
// implicit params:
(+) [String] ("hello":String, "world":String)
// need to understand that the above MUST be executed during compile stage,
// not at runtime!!! HOW??? For all type-dependent functions???
// so we now apply (+) to String and have to get a proper function 
// that is fully typed and which can be used in the compilation.
```

So it looks like the tentative rule is: **only value dependent simply typed functions are allowed at runtime, anything other has to be executed during compile stage!** Which means that:

- Surface language needs to be more complex to handle all the different dependent cases
- CLM should be simply typed value-level functions only

Actually, we can represent such functions as another case statement:

```typescript
function show [a:Type] (x:a) : String = {
  Bool -> showBool,
  String -> showString
  ... etc
}
```

Then the algo becomes: 

- we setup a function show(a:Type) in the environment with CASE statements as body, we add default implementation as the LAST case
- once we encounter instance declaration for some type, we EXPAND this function definition with a case for a specific type

One consequence of the above is that our case statements inside the function definition should be generalized from constructor checks to any boolean checks.

### Treating cases

This is an interesting and arguably the most complicated issue apart from type-checking. Let's say we have a crazy function like the below:

```typescript
function tst(x:t1, y:t2) : T = {
  {Cons (a1,a2, Cell(b1,b2) ), Z} -> g( f(a1,a2), b1, b2 ),
  {Z, n} -> y
};
```
Let's forget about the trivial second case for now and focus on the first. It should convert into something like

```haskell
case x:t1 of Cons (a1,a2, Cell(b1,b2) ) -> case y:t2 of Z -> g (...) 
```

However this is incorrect, since we need to convert all cases left to right, thus the correct conversion would be:

{case x of Cons (a1,a2, Cell(b1,b2) ), case y of Z} ==>
{case x of Cons (a1,a2, _temp_var_1 ), case _temp_var_1 of Cell(b1,b2), case y of Z} 

So we need to make a temp var substitution to all occurences of the constructor application and make a case analysis of them in turn. Since in the next step we'll also need to make substitutions to get fields access properly:

```typescript
tupleField : i:Nat -> {t0,...,tn} -> ti
consTag(x) is Cons ? ->
let a1 = tupleField(0,x)
let a2 = tupleField(1,x)
let _temp_var_1 = tupleField(2,x)
consTag(_temp_var_1) is Cell ? ->
let b1 = tupleField(0,_temp_var_1)
let b2 = tupleField(1,_temp_var_1)
```
And then we substitute occurences of a1,a2,b1,b2 in the right side for the field accesses above. Also of note that since we can do dependent types, we can actually define tuple manipulation in our language. So, in essence case analysis is reduced to consTag checks together with beta-reduction for substituted variables for tupleField!

So, function f(x,y,z) = {
  { n, Z, Cons (a1, Cell (b1, b2) ) } -> g (n,a1,b1,b2)
}

expands in the first pass into the following case:

{ case x of n, case y of Z, case z of Cons (a1, Cell (b1, b2) ) } 
  -> g (n,a1,b1,b2)

Algorithm: go left to right inside the Case tuple:

1) n: it's an Id, look it up in the environment. There's nothing, so treat it as a variable and do an n = x beta reduce while killing the case altogether, as it's only purpose is to do such a substitution:

{ case y of Z, case z of Cons (a1, Cell (b1, b2) ) } 
  -> g (x,a1,b1,b2)

2) Z: it's an Id, look up, find that it's one of Nat constructors. Convert it to either direct comparison or consTag comparison, no beta-reduction:

{ consTag(y) == Z, case z of Cons (a1, Cell (b1, b2) ) } 
  -> g (x,a1,b1,b2)

3) z is matched to constructor application, the most complex case. First step, add constructor comparison in any case:

{ consTag(y) == Z, consTag(z) == Cons  } 
  -> g (x,a1,b1,b2)
  | expand case z of Cons (a1, Cell (b1, b2) ):

App (Id Cons) [Id a1, App (Id Cell) [Id b1, Id b2] ]

so, in the step above we take Id Cons and convert it to consTag(z) == ...

Then we start working with the arguments tuple left to right:
0: Id a1: beta-reduce a1 = tupleField(0,z) ==>

{ consTag(y) == Z, consTag(z) == Cons  } 
  -> g (x,tupleField(0,z),b1,b2)
  | expand Cell (b1, b2):

1: App (Id Cell) [Id b1, Id b2]: add consTag check for the 2nd tuple field:

{ consTag(y) == Z, consTag(z) == Cons, consTag(tupleField(1,z)) == Cell  } 
  -> g (x,tupleField(0,z),b1,b2)
  | expand (b1, b2):

now we come to a recursive call inside arguments of a constructor application, falling inside the [Id b1, Id b2] tuple:

2: Id b1: beta-reduce b1 = tupleField(0, tupleField(1,z) ):
{ consTag(y) == Z, consTag(z) == Cons, consTag(tupleField(1,z)) == Cell  } 
  -> g (x,tupleField(0,z),tupleField(0, tupleField(1,z) ),b2)
  | expand (b2):

same as above for b2 and final result:
{ consTag(y) == Z, consTag(z) == Cons, consTag(tupleField(1,z)) == Cell  } 
  -> g (x,tupleField(0,z),tupleField(0, tupleField(1,z) ), tupleField(1, tupleField(1,z) ))

Then the thing in {} is joined with "and" and the right part is executed only if it's True. Otherwise we check the next case.

Ok there's lots of complications with parsing and simplifying this thing, so we MUST require that all Constructor calls and Type names are Capitalized.