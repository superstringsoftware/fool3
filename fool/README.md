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