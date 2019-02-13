# Surface language syntax description

Sort-of formal(-ish)

- Variables and functions can be lower and upper case, as well as types
- Types are always *bold* (how do we implement it?)

## Types

```haskell
-- sum types
type Bool = True + False
-- product types
type SimplePair = SimplePair :Int * :String
-- type functions
type Maybe a:* = Nothing + Just :a
-- type definition with operator like constructor
type List a:* = [] + (::) :a * :List a
-- type function with parametric sum
type Either a:* b:* = Left :a + Right :b

-- Records: simply named product types
type Person = Person name, lastName: String * age: Int
-- parametric record
type Rec a:* = Rec name:String * data:a
```

Syntax is as follows:

type <Name> [var1[:Type | Kind | Constraint]] ... = <ConsName1> [var]:<Type | var> [* [var]:<Type | var>] + <ConsName2> ...;

Types are following typical sum / product types. Anonymous fields are ommitted and type names are always preceeded by ":" to make distinction between types and variables / values explicit.

### Dependent Types

We are introducing types dependent on value as well as constraints mechanism (functions into Bool).

```haskell

-- sized vector type. NativeVector Int 4 and NativeVector Int 3 are *different* types
type NativeVector a:* n:Int = NativeVector (primtype.array a n)

-- ordered pair: more complicated. Type var a has to belong to Num typeclass (-- how to distinguish with a Type???)
-- using keyword "in"?
type OrderedPair a: in (Num, Ord) = OrderedPair :a :a where .0 < .1 -- anonymous fields access?
-- alternative, smart constructor, returning a tuple
type OrderedPair a: in (Num, Ord) = OrderedPair x y. if (x < y) then {x y} else {y x}

-- Don't like either...
```