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

Not really, should be lambda, so redo.