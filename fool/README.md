# FOOL3: Functional Object-oriented low-level language

## Internal Language Construction 

Based on several iterations we can have a strict construction of our language from the ground up using only 2 key "primitive" objects: everything is either a **tuple** or a **lambda**.

Tuple is, uhm, a tuple of any typed objects -- `{x1:t1, ... , xn:tn}` -- of (structural) type `{t1:T1, ..., tn:Tn}`. It can represent not just "values", which are ubiquitous in every language, but also: product types, sum types, *structures* (typeclasses, type families, but not only), etc - basically, anything we need.

Lambdas are any functions - anything that takes a parameter and returns something else based on this parameter. They will represent constructor functions, type constructors, "normal" functions, dependent functions etc.

Having these two concepts is enough to build everything in Haskell, Idris, and provide all kinds of meta-programming we are looking to create with our language.

Due to the above, since we are building things from the ground up, we need basic tuple manipulation functions: construction, projection, mapping, etc. 

### Tuple construction

```typescript
type Void // "0" in type theory
type Unit // "1" in type theory

type Tuple = {
    ({}), // empty tuple constructor, analogous to empty list
    (:::) (x:t, tpl:Tuple) : Tuple // attach new tuple element
}
```

The above is what we want to have to construct a tuple, but we need a correct type signature to `(:::)` operator. Also, since we don't have any way to manipulate tuples yet, we can't really write "type Tuple = " etc, since we will construct the sum types using our tuple formation function! So in a sense it's the most important function we have:

```typescript
{} : Void

function (:::) (forall (n:Nat, t, t1, ..., tn : Type) => x:t, xs:{t1,...,tn}) : {t, t1, ..., tn}
```

So, basically it's a typical PI-function, where the type of the return argument depends on the type of its' arguments. Since structural type - `{t1,...,tn}` has all elements of the same type - `Type` - we can represent it as a simple list.

### Constructor functions

Constructor functions are the next most important type of functions - they construct values of specific types. In essence, they simply return a tagged tuple of values, some examples from haskell:

```typescript
function Just(x:a) : Maybe(a) = {x} // or x ::: {}
function Nothing() : Maybe(a) = {}

function True() : Bool = {}
function False(): Bool = {}

function Cons(x:a, xs:List(a)) : List(a) = {x,xs} // or x ::: xs ::: {}
```

So, constructor functions are basically functions that return a *tuple* while giving it a specific named type, there's nothing more to them. We combine them into "type" (in haskell - data) sum types for convenience.

Then, what is a **product type**? Nothing more than a tuple of values of some other types, it's basically a name for a structural type `{t1,...,tn}` for some tuple. We can easily create it from other types by using the `(:::)` operator defined above.

Same with **sum types** - it's merely a tuple of constructor functions that return certain value tuples and represent product types! E.g., taking the above definitions into account we can write:

```typescript
type Maybe(a:Type) = Nothing ::: Just ::: {}
type Bool = True:::False:::{}
```

etc! It's merely a tuple of constructors!

### ConsTag issue

Different product types inside a top-level Sum Type are distinguished by the constructor tag. Haskell (GHC) elegantly addresses the issue in their *tagless* g-machine by simply calling corresponding closures, but since we plan to compile to .Net and javascript, we will most likely have no way around having constructor tags - they map quite nicely into underlying machinery of .Net (e.g., as child classes of a parent Sum Type class). This means we do need a function that checks what the constructor tag of a given tuple is, but what is the elegant way to define it?

We either have to make it "magical" and be able to give any tuple to it, OR - we are completely transparent but then we are storing the constructor tag somewhere along with the tuple. 

For consistency, the second way is probably preferable, but then our definition of a tuple will have to change into something like:

```typescript
// tuple internal representation:
{ x1:t1, ..., xn:tn } ConsTag

// then, constag check function can be defined explicitly:
function getConsTag ( { x1:t1, ..., xn:tn } ct ) : ConsTag = ct
```

The above is a working solution, but I don't like it.

### Tuple manipulation

Then we can adopt the usual list manipulation functions to our tuples, while being careful to write the types correctly of course. First attempt, capturing the idea, but not necesserily strict:

```typescript
// projection / field access:
function project(forall (i,n:Nat, i<=n, t1,..,tn : Type) => i:Nat, x:{t1,...,tn}):ti = {
    {i, {x1:t1, ..., xn:tn} } -> xi:ti
}

// map:
function tmap( f: {t1->t1', ..., tn->tn'} , {x1:t1, ..., xn:tn} ) : {t1', ..., tn'} = {
    {f, {} } -> {},
    {f, x:::xs} -> f(x) ::: tmap(f, xs)
}
```

Map is pretty straightforward and practically the same as map for list, however the type of the mapping function is crazy, as we need to somehow capture that it changes specific types into other specific types - how do we even write it down? Possible with the help of additional typing function in any case.

Why is the above important? Because we want to provide meta-programming capabilities to the language users with as much flexibility as possible. E.g., this way we can define "deriving" functions in structures (typeclasses), the simplest example being equality. For one value of the type to be equal to another - we need to check that their constructors are the same and then map over elements of their tuples and compare them one by one. If at least one is not equal - they are not equal. Attempt to write it down:

```typescript
// generic equality:
function genEq (x:a, y:a) : Bool = 
    if getConsTag(x) /= getConsTag(y) then False
    else _genEq (x,y)

function _genEq(x:a, y:a) : Bool = {
    { {}, {} } -> True,
    { x:::xs, y:::ys } -> if not(genEq (x,y)) then False else _genEq(xs,ys) 
}
```

The above definition returns `False` as soon as any two members are not equal, and `True` only if all of them are equal. Something similar to the above will be used in the deriving of the (==) in the Eq structure.