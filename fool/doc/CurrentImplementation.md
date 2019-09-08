### Dependent Types: Types are first class values

##### constructor function that creates a pair where 2nd el is always bigger

```haskell
depPair x:Int y:Float = if x < y then DepPair x y else DepPair y x;

vecRand n a = Vec (Array a) n;
```
let's say we want a safe polymorphic Vector addition function in Haskell, we can only do something like: `vadd :: Vector -> Vector -> Vector` which WON'T work for vectors of different length and should fail somehow or we have to define `vadd2f :: Vector2f -> Vector2f -> Vector2f` etc for each vector size

now, with extensions, we can define type-level Peano numbers, something like:
```haskell
data Z
data S Z
...
SVector a pn
vadd :: SVector Int pn -> SVector Int pn -> SVector Int pn
```
but we still cannot write a function `newSV :: Peano -> SVector Int Peano`

##### Let's experiment

```haskell
data Vector a:* n:Nat where
  VectorInternal :: Ptr a:* -> n:Nat -> Vector a n -- data representation - should be hidden
  NewVector a:* n:Nat = VectorInternal (newVectorA# n) n --should be exposed as a constructor function
```

### Extending types defined in other modules via Sum and Product.
E.g.: `data Maybe a = Just a | Nothing` and we want to work with the type `Maybe a b = Just a b | Nothing | Almost b` for some reason. We do: `Maybe a b = Maybe {Just = Just :*: b} :+: Almost b` and then we can use Maybe a everywhere where Maybe a b could have been used (?) -- pretty crazy.

> This tells us we can interpret each *constructor* as a type, and then "real" data types defined via | in haskell as simple sums that they are.

> does it mean that if we have an arbitrary function that returns some tuple, named or unnamed, it can be treated as a constructor???

```haskell
Person name:String age:Int = {name, age} -- this is the record constructor, type {String, Int}
Pair :a :b = {.0, .1} -- this is an unnamed tuple constructor, type {*, *}
Just :a = {.0} -- type {*}
Nothing  = {}  -- type {} (empty tuple)
Left :a = {.0:a}
Right :b = {.0:b}
```

Ha, sum types are EXACTLY the same, they just return constructor functions as tuple elements!!!!! :->
```haskell
Maybe a:* = {Just :a, Nothing} => {Just {:a}, Nothing {}}
Either a:* b:* = {Left :a, Right :b} => {Left {:a}, Right {:b}}
Pair a:* b:* = {Pair {:a, :b}}
-- Pair = \a:* b:*. { \x:a y:b. Pair {a, b} : (Pair a b)  }
-- Maybe = \a:*. { \x:a. Just {a} : Maybe a, Nothing {} : Maybe a }
```
then, GADTs:
```haskell
data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool

-- Expr = \a:*. { \x:Int. I {x} : Expr Int, \x:Bool. B {x} : Expr Bool  }
```

> So, we can express BOTH sum and product types as Lambdas, same as regular functions!!! This shows us that all type de-re-etc-construction is done simply via tuple accessors: constructors are indexed inside sumtype tuple, values are indexed inside producttype tuple.

the only question remains - how to handle records.
```haskell
Widget = { Point {x:Int, y:Int}, Box {x:Int, y:Int, w:Int, h:Int} }
-- then somebody has
f : Widget -> Int
f w = w.x * w.y
```
typechecking algo if we don't have a signature (if we do it's trivial):

* type of w must have x and y named fields
* x and y must be of types supported by '\*' operation
* f translates to (\*) (<Constructor of w>.x w) (<Constructor of w>.y w)
* then we instantiate <Constructor of w> at the call sites and make sure:
  * they have x and y
  * their types are ok for (\*)

So, record declaration will generate accessor functions in the form of:
`<Module Name>.<Type Name>.<Constructor>.<field Name> : <Type>`
and in our internal representation will simply point to index of the field in our tuple
When compiling, it will have to be converted to either record field access or byte offset etc

> these guys above do not create a type, they are simply a function with implicitly defined type. However, if we *name* those types, we get proper haskell data types!

```haskell
Person name:String age:Int = {name, age}:Person # or
type Person = Person {name:String, age:Int} # defined same as above in our existing syntax
Pair :a :b = {.0:a, .1:b}:Pair a:* b:*
type Maybe a:* = Just :+: Nothing
-- or
ExtPerson = Person :*: dob:Date :*: father:Person :*: mother:Person
-- which leads to ExtPerson being {name, age, dob, father, mother}
```
