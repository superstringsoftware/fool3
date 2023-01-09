# fool

This is a from scratch implementation of the type-theory based functional language with flexible compilation targets (initially - javascript, .Net, potentially x86 native) based on the learnings of the last 6 years.

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

## Things to take care of:

* Type levels - e.g., Nat on values level vs type level???