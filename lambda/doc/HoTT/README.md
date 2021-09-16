# Based on the HoTT

```
ε = V -- variables and symbols
  | U Nat -- universe hierarchy
  | (εε) -- application
  | (ε1, ...) - N-tuple
  | 𝜆V:ε.ε -- lambda abstraction
  | ∏V:ε.ε -- pi types (generalization of -> )
  | ∑V:ε.ε -- sigma types (generalization of product; ehm, so can THIS be a tuple??? No, because we can't put values - which are applications of constructor functions - in place of v:E)
  | Sum -- how to represent???
  | Void -- 0 type
  | Unit -- 1 type
  | WType -- how???
```

## ∏ examples 

`∏(x:A) B(x)`: why can't we write it simply as `x:A -> B(x)`???

- `∏(n:Nat) Fin(n)` - constructor function for the set of all numbers smaller than n
- `∏(n:Nat) ∏(a:U0) Vec(a,n)` - constructor function for vectors of length n

More complex, with the 2nd argument depending on the first:

`∏(x:A) ∏(y:B(x)) C(x,y)`: `∏(n:Nat) ∏(v:Vec(n Nat)) C(n,v)`, again, why can't we write it as:
`n:Nat -> v:Vec(n, Nat) -> C(n,v)`?

Or, moving to our function definition format:

- `f (x:A) : B(x)` - general one,
- `f (n:Nat) : Fin(n)`
- `f (n:Nat, a:U0) : Vec(a, n)`
- `f (n:Nat, v:Vec(n, Real)) : C(n,v)`

Apparently, we can put all of them into this format. Which hints that we can write Pi-types as simply:

`(x1:t1,..., xn:tn) : T(xk,...,xm)` where each subsequent t may depend on a previous x, `k>=1` and `m<=n`. 

Can we have "circular" dependencies like: `(x:A, y:B(x,z), z:C(x,y))`? Doesn't really make sense, but how do we catch it in the compiler?!

Since Pi types are about functions, they have to be very closely correlated with Application and Lambda abstraction. To be more rigorous, let's define:

`XT = (x1:t1, ..., xn:tn)` - an N-tuple of variables with types. Then,

- Generalized Pi-type: `Pi XT B(subset(XT))`
- Lambda abstraction: `Lam (x1:t1[=v1], ..., xn:tn[=vn]) Expr` - here, we are allowing optional default values for the named parameters (so we will do some sort of memoization instead of currying)
- Application: `App Expr (x1=v1, ..., xn=vn) || (v1,...,vn)` - so, application either to the named tuple or anonymous, and then we treat it in order

Before we move further, it makes sense to start with a simply-typed n-tuple calculus to work through the function machinery! See Core.STNTC.

Can we express constructor functions now?

`Bool = True | False` -> `True = Lam [] [], False = Lam [] []` - so, both are constructors that don't take any arguments and return an empty tuple, but we need to distinguish between them by ConsTag - which we cannot do before sum types (or inductive types?) are defined.

`Just = \a:Type x:a . (x)` -> `Just = Lam [a:U0, x:a] [x]` -- how do we capture the fact `a` is implicit???

### Some thoughts on compilation to .Net / JS

Let's use a typical Pi-type, sized vector:

```haskell
data Vect : Nat -> Type -> Type where
    Nil  : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

app : Vect n a -> Vect m a -> Vect (n + m) a
app Nil       ys = ys
app (x :: xs) ys = x :: app xs ys
```

TBD

## ∑ examples

Since we want to generalize ∑ to n-tuples as well, we want to represent it simply as `Sigma Record`. Now, for non-dependent cases it works fine, since the Record is just that - a product type with named fields. However, how about a basic typeclass hierarchy?

```haskell
Semigroup (a:Type) : Sigma = (
    -- "normal" function has no body - it's ok because it's a typeclass definition!
    (+) (x,y:a) :a
    -- constraint (law), associativity (basically a function with a predicate, constraint simply says it's a constraint)
    -- associativity = x + (y + z) == (x + y) + z 
),

Monoid : Class = ∃ Semigroup a => \a. {
    E0:a
    -- forall x:a => (Z0 + x == x, x + Z0 == x) -- constraint (law)
};
```

Translates to:

```
Semigroup = Sigma [
    Field "a" U0 UNDEFINED,
    Field "(+)" (Pi [Field "x" (Id "a") UNDEFINED, Field "y" (Id "a") UNDEFINED] (Id "a")) UNDEFINED,
    <how to record associativity???>
]

Monoid = Sigma [
    Field "a" (App (Id "Semigroup") (Id "a")) UNDEFINED,
    Field "E0" (Id "a") UNDEFINED
]
```

Ok something close to it should work. Multiparam will be tricky, for now, more basic - Equality:

```
Eq : Class = \a:Type . {
    (==):Bool = \x:a y:a. not (x /= y);
    (/=):Bool = \x:a y:a. not (x == y); 
    (≠) = (/=);
    required = (==) || (/=)
}
```

translates to:

```
Eq = Sigma [
    Field "a" U0 UNDEFINED,
    Field {
        var = "(==)",
        typ = Pi [Field "x" (Id "a") UNDEFINED, Field "y" (Id "a") UNDEFINED] (Id "Bool"), 
        val = App (Id "not") [Field "" UNDEFINED 
                                (App (Id "/=" ) [Field "x", Field "y"] )] }
]
```

Not really, should be lambda, so redo, but at least it works. How about multiparameter typeclasses with type synonims?

```haskell
class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

-- ours:
Add = Sigma [
    Field "a" U0 UNDEFINED,
    Field "b" U0 UNDEFINED,
    Field {
        var = "SumTy",
        typ = Pi [Field "" (Id "a") UNDEFINED, Field "" (Id "b") UNDEFINED] (U 0),
        val = UNDEFINED 
    },
    Field {
        var = "add",
        typ = Pi [Field "" (Id "a") UNDEFINED, Field "" (Id "b") UNDEFINED] (App (Id "SumTy") [Field "a",Field "b"]),
        val = UNDEFINED 
    }
]
```

Something like the above. So, what I don't really get is why wouldn't we simply record both Pi and Sigma as lambdas?!

### Pi and Sigma as Lambdas

\n:Nat. \v:Vec(n,Real). Expr : C(n,v) - this is a Pi type, where we can deduce the signature by simply ignoring the Expr. Other examples:

Constructors:

Maybe = \a:U0. (
    Just = \a:U0. \x:a. (x) : Maybe (a),
    Nothing \a:U0.      ()  : Maybe (a)
) : U0

^^^ this way, we can *both* apply Maybe to some type and generate specific constructors, as well as just apply constructors directly and deduce the types from the arguments, but all of them are a lambda!!!

Nil = \a:U0. () : List(a)
Cons = \a:U0. \x:a xs:List(a). (x,xs):List(a)

Dependent constructors:

Vec = \a:U0 r:Nat. (
    Nil  = \a:U0. () : Vec (a,0),
    (::) = \a:U0. \x:a xs:Vec(a,n). (x,xs) : Vec(a, n+1)
)

Dependent functions:

(++) = \v1:Vec (n,a) \v2:Vec (m,a) . case v1 of
    Nil     -> v2
    (x::xs) -> x :: xs ++ ys
: Vec (n+m, a)

Sigmas:

Semigroup = \a:U0. (
    (*) = \a:U0. \x:a y:a. Expr : a
) : Sigma

Monoid = Semigroup a => \a:U0. (
    Z0 = \a:U0. Expr : a
) : Sigma

The whole "exists" thing becomes tricky, and we actually need a Semigroup instance inside our Monoid instance, don't we? So, maybe like:

Monoid = \a:U0. (
    exists Semigroup(a),
    Z0 = \a:U0. Expr : a
) : Sigma


Add = \a:U0 b:U0. (
    SumTy = \a:U0 b:U0. Expr : U0,
    add = \a:U0 b:U0. \x:a y:b . Expr : SumTy (a,b)
) : Sigma

Etc.

**So, we are back to the whole "everything is either a Lambda or a Tuple" concept!!!**

So, Core language should be explicitly typed Lambdas + Tuples?!

## Another take at the surface language, based on "Lambdas + Tuples" thing

#### Function definition: 
`<Name> [opt params] (params) : <Type> = <body>`

#### Regular function:

fact (n:Int) : Int = if n==0 then 0 else n*fact(n)
fact (n: exists Num(a) => a:Type) : Type = ...

#### Simple types:

Bool : Type = (True, False) -- simply a tuple of constructors. So, can omit functions from value constructors.

Maybe (a:Type) : Type = (
    Just (x:a) : Maybe (a) = (x),
    Nothing () : Maybe (a) = ()
)

List (a:Type) : Type = (
    (::) (x:a, xs: List(a)) : List (a) = (x,xs)
    ([]) () : List(a) = ()
)

#### GADTs and records

Expr (a:Type) : Type = (
    I (:Int) : Expr(Int) = (_),
    B (:Bool): Expr(Bool) = (_),
    Add (:Expr(Int), :Expr(Int)):Expr Int = (_,_)
    -- etc
)

Record:

Person : Type = (
    MkPerson (fname, lname: String, dob: Date) : Person
)

In fact, can introduce the type directly, if it's a simple record (without the sum type):

MkPerson (fname, lname: String, dob: Date, ) : Person

#### Typeclasses and explicit type parameters

Show function via function (no typeclass):

show [a:Type] (x:a) : String
show [String] = id
show [Int] = ...
etc

This is in fact exactly what a typeclass would generate:

Show (a:Type) : Sigma = (
    show (x:a) : String
)

Eq (a:Type) : Sigma = (
    (==) (x,y:a) : Bool = not (x /= y),
    (/=) (x,y:a) : Bool = not (x == y),
    (≠) = (/=),
    required = (==) || (/=)
    {-
    Laws to define further:
    Reflexivity
    x == x = True
    Symmetry
    x == y = y == x
    Transitivity
    if x == y && y == z = True, then x == z = True
    Substitutivity
    if x == y = True and f is a "public" function whose return type is an instance of Eq, then f x == f y = True
    Negation
    x /= y = not (x == y)
    -}
)

#### Aha, so the type signature problem

Arises when we want to write a function type in *arguments*, e.g. for the Functor:

Functor (f:Type -> Type) : Sigma = (
    fmap (g:a->b, x:f(a)) : f(b)
)

For regular functions we can use arrows, but what if we need a proper Pi?

g: (x:a)->B(x)
f: (x,y:a) -> B(x,y)  

Write it as a full function sig???

fmap ( g(x:a):b, x:f(a) ) -- looks ugly. So, back to arrows I guess?

#### Some runtime considerations

show [a:Type] (x:a) : String
show [Int] = toString
show [String] = id

show (4) --> show (4:Int) --> show [a = Int] (x = 4)

Should we make optional params only type params? Then we can track tables of polymorphic functions depending on the type params - and make these tables extendable?

**Ok, so we can start with functions-only (no operators) language! --> this will allow us to test all end-to-end concepts, since the core language remains the same, so we will compile to .Net etc!!! Then we will add operators, since they are nothing more than syntactic sugar that makes parsing much more difficult, so bells and whistles!**



