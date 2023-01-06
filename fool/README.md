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

## Things to take care of:

* Type levels - e.g., Nat on values level vs type level???