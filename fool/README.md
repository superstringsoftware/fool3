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
TODO: Fix this inconcistency

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