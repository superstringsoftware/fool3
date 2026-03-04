# Categorical Type System Design for tulam

## Motivation

tulam is built on two primitives: **tuples** and **lambdas**. Category theory provides the natural mathematical framework for understanding how types and functions compose. Rather than bolting on categorical concepts after the fact (as Haskell does with its typeclass hierarchy), tulam has the opportunity to make categorical structure explicit in the surface language from the beginning.

This document describes a layered vocabulary of categorical constructs, how they map to existing tulam infrastructure, and a practical implementation roadmap.

---

## 1. The Categorical Hierarchy

### 1.1 Core Insight

tulam's `structure` keyword currently serves as a catch-all for typeclasses. But structures that depend on different numbers and kinds of type parameters are categorically *different things*:

| Parameters | Categorical concept | What it does | Example |
|---|---|---|---|
| 1 type | **Algebra** | Equips one type with operations | `Monoid(a)`, `Group(a)` |
| 2+ types | **Morphism** | Relates types directionally | `Convertible(a,b)`, `Iso(a,b)` |
| 1 type constructor | **Functor** | Structure-preserving map | `Functor(f)`, `Monad(m)` |
| 2 type constructors | **Natural transformation** | Morphism between functors | `safeHead : List ~> Maybe` |
| Morphisms + composition | **Category** | Universe of composable arrows | `Category(arr)` |

All of these compile down to the same implicit-parameter functions that tulam already supports. The categorical vocabulary is a *surface* distinction that enables better error messages, automatic derivation, and principled composition.

### 1.2 Design Principle

**`structure` remains the general-purpose keyword.** The categorical keywords (`algebra`, `morphism`, `functor`, `natural`, `category`) are *refinements* that carry additional semantic meaning. Code using plain `structure` still works -- it's just less precise, like using `Object` in Java when you could use a specific interface.

---

## 2. Relationships Between Structures: `extends` vs `requires`

### 2.1 The Problem

In Haskell, multi-parameter typeclasses use a single mechanism (`=>` constraints) for two fundamentally different relationships:

```haskell
class Eq a => Ord a where ...          -- Ord REFINES Eq (same param)
class (Monad m, MonadIO m) => App m    -- App NEEDS these (same param, but constraint)
class Container f a where ...          -- f and a are different shapes entirely
```

tulam distinguishes these with two keywords: **`extends`** and **`requires`**.

### 2.2 `extends` — Same-shape refinement

`extends` means "this structure is a refinement of that one." Both structures share the same parameter(s) in the same positions. The child adds new operations to the parent's operations.

```tulam
algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) = combine(x, combine(y,z)) === combine(combine(x,y), z)
};

algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a,
    law leftIdentity(x:a)  = combine(empty(), x) === x,
    law rightIdentity(x:a) = combine(x, empty()) === x
};

algebra Group(a:Type) extends Monoid(a) = {
    function inverse(x:a) : a,
    law leftInverse(x:a)  = combine(inverse(x), x) === empty(),
    law rightInverse(x:a) = combine(x, inverse(x)) === empty()
};
```

**Semantics of `extends`:**
- Parameters must match exactly (same names, same types, same positions)
- All operations from the parent are inherited
- All laws from the parent are inherited
- An instance of the child automatically satisfies the parent
- Compiler can verify parameter shape match at declaration time

**Compilation:** `extends` generates the same implicit-parameter functions as `structure`, but the compiler registers the inheritance relationship. When you provide an `instance Group(Int)`, the compiler automatically has `Monoid(Int)` and `Semigroup(Int)` available for dispatch.

### 2.3 `requires` — External constraints

`requires` means "this structure needs evidence that some other structure is satisfied, but the other structure may have different parameters or a different shape."

```tulam
morphism Convertible(a:Type, b:Type) = {
    function convert(x:a) : b
};

// Requires a constraint with a DIFFERENT shape (Eq has 1 param, this has 2)
morphism OrdConvertible(a:Type, b:Type) requires Ord(a), Ord(b) = {
    function convert(x:a) : b,
    law preserveOrder(x:a, y:a) = (x < y) ==> (convert(x) < convert(y))
};

// Requires a constraint on a RELATED but different parameter
algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b)
};

algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f:a -> m(b)) : m(b)
};

// Kleisli category: the constraint (Monad) has a different shape than Category
algebra Category(arr: Type -> Type -> Type) = {
    function id(a:Type) : arr(a, a),
    function compose(f:arr(b,c), g:arr(a,b)) : arr(a,c),
    law leftId(f:arr(a,b))  = compose(id(b), f) === f,
    law rightId(f:arr(a,b)) = compose(f, id(a)) === f,
    law assoc(f:arr(c,d), g:arr(b,c), h:arr(a,b)) =
        compose(f, compose(g, h)) === compose(compose(f, g), h)
};

instance Category(Kleisli(m)) requires Monad(m) = {
    function id(a:Type) : Kleisli(m, a, a) = { {x} -> return(x) },
    function compose(f:Kleisli(m,b,c), g:Kleisli(m,a,b)) : Kleisli(m,a,c) = {
        {x} -> bind(g(x), f)
    }
};
```

**Semantics of `requires`:**
- Constraints can have completely different parameter shapes
- Constraints must be satisfied for the structure/instance to be used
- The required structures' operations are available inside the body
- Multiple `requires` clauses are comma-separated

**Compilation:** `requires` compiles to additional implicit parameters. `instance Category(Kleisli(m)) requires Monad(m)` means the dispatch for `compose` on Kleisli arrows needs a `Monad(m)` dictionary available at the call site.

### 2.4 Summary

| Keyword | Relationship | Parameters | Example |
|---------|-------------|------------|---------|
| `extends` | Refinement (adds operations) | Same shape | `Monoid extends Semigroup` |
| `requires` | Constraint (needs evidence) | Any shape | `instance X requires Monad(m)` |

---

## 3. Laws

### 3.1 Syntax

Laws are declared inside structures using the `law` keyword. They express equalities that instances should satisfy.

```tulam
law <name>(<params>) = <LHS> === <RHS>
```

- **`===`** is propositional equality — "these two expressions should always produce the same result." It is distinct from `==` which is the computable equality function from `Eq`.
- **`==>`** is implication — "if the left side holds, the right side must also hold." Used for conditional laws.

### 3.2 Examples

```tulam
algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) =
        combine(x, combine(y, z)) === combine(combine(x, y), z)
};

algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a,
    law leftIdentity(x:a)  = combine(empty(), x) === x,
    law rightIdentity(x:a) = combine(x, empty()) === x
};

algebra Eq(a:Type) = {
    function (==)(x:a, y:a) : Bool = not(x != y),
    function (!=)(x:a, y:a) : Bool = not(x == y),
    law reflexivity(x:a)         = (x == x) === True,
    law symmetry(x:a, y:a)       = (x == y) === (y == x),
    law transitivity(x:a, y:a, z:a) =
        ((x == y) == True) ==> ((y == z) == True) ==> ((x == z) === True)
};

algebra Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function (<)(x:a, y:a)  : Bool = compare(x,y) == LT,
    function (>)(x:a, y:a)  : Bool = compare(x,y) == GT,
    function (<=)(x:a, y:a) : Bool = not(x > y),
    function (>=)(x:a, y:a) : Bool = not(x < y),
    law totalOrder(x:a, y:a) = (x <= y) === True ==> True
                             |  (y <= x) === True ==> True,
    law antisymmetry(x:a, y:a) =
        ((x <= y) == True) ==> ((y <= x) == True) ==> ((x == y) === True)
};

morphism Iso(a:Type, b:Type) extends Convertible(a, b) = {
    function unconvert(x:b) : a,
    law roundtrip1(x:a) = unconvert(convert(x)) === x,
    law roundtrip2(y:b) = convert(unconvert(y)) === y
};
```

### 3.3 Semantics

Laws are **carried but not enforced** until the type checker is ready. The plan is:

1. **Phase 1 (now):** Parse and store laws in the AST. They're visible in `:list structures` output.
2. **Phase 2 (with type checker):** Laws are available for property-based testing. The compiler generates QuickCheck-style properties.
3. **Phase 3 (advanced):** Laws inform optimizations. If `fmap(id) === id` is a law, the compiler can eliminate identity fmaps. If associativity holds, the compiler can rebalance expression trees.

### 3.4 `===` vs `==`

| Operator | Meaning | Lives in | Computable? |
|----------|---------|----------|-------------|
| `==` | Decidable equality | `Eq` structure | Yes — returns `Bool` at runtime |
| `===` | Propositional equality | Laws | Not directly — it's a *specification*, not a computation |

`===` says "for all valid inputs satisfying the parameter types, these two sides reduce to the same value." It's the moral equivalent of Agda's `≡` or Lean's `=` in propositions.

`==>` says "if the antecedent is true, the consequent must hold." Used for conditional laws where the property only needs to hold under certain conditions.

---

## 4. Level 1: Algebras — Structure on One Type

An algebra equips a single carrier type with operations and laws.

### Syntax

```tulam
algebra Monoid(a:Type) = {
    function empty() : a,
    function combine(x:a, y:a) : a,
    law leftIdentity(x:a)    = combine(empty(), x) === x,
    law rightIdentity(x:a)   = combine(x, empty()) === x,
    law associativity(x:a, y:a, z:a) =
        combine(x, combine(y, z)) === combine(combine(x, y), z)
};

algebra Group(a:Type) extends Monoid(a) = {
    function inverse(x:a) : a,
    law leftInverse(x:a)  = combine(inverse(x), x) === empty(),
    law rightInverse(x:a) = combine(x, inverse(x)) === empty()
};
```

### What the compiler knows

When something is declared as `algebra` rather than `structure`:
- There is exactly **one carrier type** parameter
- **Product derivation** is sound: if `Monoid(A)` and `Monoid(B)`, then `Monoid({a:A, b:B})` can be derived automatically (pointwise operations)
- **Free construction** may be available: the free monoid over `a` is `List(a)`
- Laws can be checked by property testing

### Compilation

Identical to current `structure` compilation. `algebra` is sugar that tells the compiler "this is an algebra" for future derivation and checking.

### Examples

```tulam
algebra Eq(a:Type) = {
    function (==)(x:a, y:a) : Bool = not(x != y),
    function (!=)(x:a, y:a) : Bool = not(x == y),
    law reflexivity(x:a)    = (x == x) === True,
    law symmetry(x:a, y:a)  = (x == y) === (y == x)
};

algebra Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function (<)(x:a, y:a)  : Bool = compare(x,y) == LT,
    function (>)(x:a, y:a)  : Bool = compare(x,y) == GT,
    function (<=)(x:a, y:a) : Bool = not(x > y),
    function (>=)(x:a, y:a) : Bool = not(x < y)
};

algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) =
        combine(x, combine(y, z)) === combine(combine(x, y), z)
};

algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a,
    law leftIdentity(x:a)  = combine(empty(), x) === x,
    law rightIdentity(x:a) = combine(x, empty()) === x
};
```

---

## 5. Level 2: Morphisms — Relations Between Types

A morphism establishes a directed relationship between two (or more) types. This is structure *between* objects in our category, not *within* a single object.

### Syntax

```tulam
morphism Convertible(a:Type, b:Type) = {
    function convert(x:a) : b
};

morphism Iso(a:Type, b:Type) extends Convertible(a, b) = {
    function unconvert(x:b) : a,
    law roundtrip1(x:a) = unconvert(convert(x)) === x,
    law roundtrip2(y:b) = convert(unconvert(y)) === y
};
```

### What the compiler knows

When something is declared as `morphism`:
- There are **two or more type** parameters with a directional relationship
- **Composition** is automatic: if `Convertible(A,B)` and `Convertible(B,C)`, the compiler can derive `Convertible(A,C)` via `convert(x) = convert_BC(convert_AB(x))`
- **Identity** exists: `Convertible(A,A)` is trivially `convert = id`
- This means morphisms form a **category** automatically (see Section 8)

### Compilation

Same as multi-parameter `structure`. The compiler additionally registers composability.

### Practical value

On .NET, `Convertible(A,B)` maps to implicit conversion operators. Composition means the compiler can chain conversions automatically:

```tulam
instance Convertible(Int, Float) = { function convert(x:Int):Float = intToFloat#(x) };
instance Convertible(Float, String) = { function convert(x:Float):String = showFloat#(x) };

// compiler can derive: Convertible(Int, String) via Float
```

### Value-dependent morphisms

Structures can also depend on values, creating *indexed* or *parameterized* morphisms:

```tulam
morphism LinearMap(k:Type, v:Type, w:Type) requires Field(k) = {
    function apply(f:v -> w, x:v) : w,
    function scale(s:k, x:v) : v,
    law linearity(f:v -> w, s:k, x:v) =
        apply(f, scale(s, x)) === scale(s, apply(f, x))
};
```

Here `requires Field(k)` provides evidence that `k` is a field. The structure is constrained by a separate structure on one of its parameters.

---

## 6. Level 3: Functors — Structure-Preserving Maps

A functor is a type constructor `F : Type -> Type` that also maps functions: if you have `f : a -> b`, you get `fmap(f) : F(a) -> F(b)`.

### Syntax (two options)

**Option A: Functor as a declaration that combines type + mapping**

```tulam
functor Maybe(a:Type) : Type = { Just(x:a), Nothing };
// This BOTH defines the sum type AND declares fmap exists.
// fmap must be provided or derived.

functor List(a:Type) : Type = { Nil, Cons(head:a, tail:List(a)) };
```

**Option B: Functor as an algebra on type constructors**

```tulam
type Maybe(a:Type) = { Just(x:a), Nothing };

algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b),
    law identity(x:f(a))        = fmap(id, x) === x,
    law composition(f:b -> c, g:a -> b, x:f(a)) =
        fmap(compose(f, g), x) === fmap(f, fmap(g, x))
};

instance Functor(Maybe) = {
    function fmap(g, x) = {
        {g, Nothing} -> Nothing,
        {g, Just(v)} -> Just(g(v))
    }
};
```

### Recommendation

**Option B is better.** Here's why:

1. Not every type constructor should be a functor (e.g., `Set` requires `Ord` on elements)
2. Separating the type definition from the functor instance is cleaner -- you can define types without committing to functoriality
3. It's consistent: `Functor` is just another algebra, but on `Type1` (kinds) instead of `Type`
4. Option A conflates two things (data definition + structure instance) which violates tulam's design of keeping things orthogonal

However, we could add `functor` as **sugar** that expands to Option B:

```tulam
// this:
functor Maybe(a:Type) : Type = { Just(x:a), Nothing };
// expands to:
type Maybe(a:Type) = { Just(x:a), Nothing };
// plus auto-derived: instance Functor(Maybe) = { ... }
```

### What the compiler knows

- `Functor(F)` means `F` preserves composition: `fmap(f . g) === fmap(f) . fmap(g)`
- `Functor(F)` and `Functor(G)` implies `Functor(F . G)` -- functor composition is automatic
- This is the foundation for Applicative, Monad, Traversable, etc.

### Higher-kinded types requirement

Functors require the type system to handle `f:Type1` -- type constructors as first-class parameters. This is already representable with our universe hierarchy (`Type1 = U 1`), but the parser and pipeline need to handle type-constructor application (`f(a)` where `f` is a type variable of kind `Type -> Type`).

---

## 7. Level 4: Natural Transformations — Morphisms Between Functors

A natural transformation is a family of functions `F(a) -> G(a)` that is *uniform* in `a` -- it doesn't inspect or depend on what `a` is.

### Syntax

```tulam
natural safeHead : List ~> Maybe = {
    function transform(xs:List(a)) : Maybe(a) = {
        {Nil} -> Nothing,
        {Cons(x, rest)} -> Just(x)
    }
};

natural flatten : List . List ~> List = {
    function transform(xss:List(List(a))) : List(a) = concat(xss)
};
```

### What the compiler knows

- The function inside `natural` must be parametrically polymorphic in the element type
- Natural transformations compose: if `alpha : F ~> G` and `beta : G ~> H`, then `beta . alpha : F ~> H`
- Vertical composition (above) and horizontal composition (with functors) both work
- The naturality condition `fmap_G(f) . alpha === alpha . fmap_F(f)` holds by parametricity (free theorem)

### Why this matters

Natural transformations are the **right abstraction for polymorphic container operations**:
- `safeHead : List ~> Maybe` -- extract first element
- `reverse : List ~> List` -- natural endomorphism
- `toList : Set ~> List` -- forget ordering
- `flatten : List . List ~> List` -- this is `join` for the List monad!

By marking these as `natural`, the compiler knows they compose and can optimize chains of container transformations.

### Implementation

A `natural` declaration compiles to a rank-2 polymorphic function internally:
```tulam
// natural safeHead : List ~> Maybe compiles to:
function safeHead [a:Type] (xs:List(a)) : Maybe(a) = ...
```

The `natural` keyword is a *contract* that this function doesn't inspect `a`, which the type checker can verify later.

---

## 8. Categories and Arrows

### 8.1 The Default Category

tulam programs live in a default category implicitly:
- **Objects** = types (inhabitants of `Type`)
- **Morphisms** = functions (`a -> b`)
- **Composition** = function composition (`.` or `compose`)
- **Identity** = `id : a -> a`

This is **Type**, the category of types and functions. We don't need to declare it -- it's the ambient universe.

### 8.2 Category as a Structure

Other categories can be defined as structures. A category needs:

```tulam
// A category is parameterized by its morphism type
// Objects are implicit (they're the types that arr connects)
algebra Category(arr: Type -> Type -> Type) = {
    function id(a:Type) : arr(a, a),
    function compose(f:arr(b,c), g:arr(a,b)) : arr(a,c),
    law leftId(f:arr(a,b))  = compose(id(b), f) === f,
    law rightId(f:arr(a,b)) = compose(f, id(a)) === f,
    law assoc(f:arr(c,d), g:arr(b,c), h:arr(a,b)) =
        compose(f, compose(g, h)) === compose(compose(f, g), h)
};

// The default instance: plain functions form a category
instance Category(Function) = {
    function id(a:Type) : a -> a = { {x} -> x },
    function compose(f, g) = { {x} -> f(g(x)) }
};
```

Note: `Category` takes `arr : Type -> Type -> Type` -- a two-parameter type constructor. This requires our type system to handle `Type -> Type -> Type` as a kind, which is `Type2` in our universe hierarchy.

### 8.3 Kleisli Categories

Every monad gives rise to a category -- the Kleisli category. This is where monads connect to the categorical framework:

```tulam
// Kleisli arrow: a function a -> m(b) for some monad m
type Kleisli(m: Type -> Type, a:Type, b:Type) = a -> m(b);

// Given a Monad(m), Kleisli(m) forms a Category
instance Category(Kleisli(m)) requires Monad(m) = {
    function id(a:Type) : Kleisli(m, a, a) = { {x} -> return(x) },
    function compose(f:Kleisli(m,b,c), g:Kleisli(m,a,b)) : Kleisli(m,a,c) = {
        {x} -> bind(g(x), f)
    }
};
```

This is extremely powerful: it means **every monad automatically gives you a category** of effectful computations, with composition working correctly.

### 8.4 Arrows

Arrows generalize both functions and monadic computations. An Arrow is a Category with additional structure:

```tulam
algebra Arrow(arr: Type -> Type -> Type) extends Category(arr) = {
    function arr(f: a -> b) : arr(a, b),           // lift a function
    function first(f:arr(a,b)) : arr({a,c}, {b,c}), // process first component
    // derived:
    function second(f:arr(a,b)) : arr({c,a}, {c,b}) = ...,
    function split(f:arr(a,b), g:arr(c,d)) : arr({a,c}, {b,d}) = ...,
    function fanout(f:arr(a,b), g:arr(a,c)) : arr(a, {b,c}) = ...,
    law arrId() = arr(id) === id,
    law arrCompose(f:a -> b, g:b -> c) = arr(compose(g, f)) === compose(arr(g), arr(f))
};
```

### 8.5 Should Category and Arrow be first-class keywords?

**No. They should be structures (algebras).**

Reasoning:
- Categories and Arrows are *algebraic structures* on morphism types -- they fit the `algebra` pattern perfectly
- Making them keywords would add complexity without enabling anything that structures can't express
- The Haskell approach (Category and Arrow as typeclasses) is correct here
- The interesting thing about categories is not their definition but their *use* -- composition operators, do-notation, arrow notation
- What we DO want is **syntactic sugar** that works with any Category instance (see Section 10)

However, `Category` and `Arrow` should be **built-in structures** in the standard library (like `base.tl`), not user-defined, because the compiler needs to know about them for:
- Optimizing composition chains
- Providing do-notation and arrow-notation
- Deriving Kleisli categories from monads automatically

---

## 9. Monads — Structure on Functors

### 9.1 Where Monads Fit

A monad is a functor with extra algebraic structure. In categorical terms:

> A monad on a category C is an endofunctor `M : C -> C` together with two natural transformations: `return : Id ~> M` and `join : M . M ~> M`, satisfying associativity and unit laws.

In tulam terms, this translates to: **a Monad is an algebra on a Functor**.

### 9.2 Should Monad be a keyword or a structure?

**Monad should be a structure (algebra), not a first-class keyword.**

Reasoning:

**Arguments for first-class keyword:**
- Monads are pervasive (IO, State, Maybe, List, Parser, ...)
- `do`-notation is syntactic sugar tied specifically to monads
- The compiler could optimize monadic code better if it knows the monad laws hold

**Arguments for structure (stronger):**
- A monad is literally "a functor with extra structure" -- that's exactly what `algebra extends` expresses
- `do`-notation doesn't need a keyword -- it needs the compiler to recognize the `Monad` structure and desugar accordingly (just like Haskell)
- Monad transformers are structures too -- keeping everything uniform is cleaner
- Not every functor is a monad, not every monad is used with do-notation -- a keyword would be over-specific
- `Applicative` sits between `Functor` and `Monad` -- making `Monad` special but not `Applicative` would be arbitrary

### 9.3 The Monad Hierarchy

```tulam
// Functor: can map over contents
algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b),
    law identity(x:f(a)) = fmap(id, x) === x,
    law composition(f:b -> c, g:a -> b, x:f(a)) =
        fmap(compose(f, g), x) === fmap(f, fmap(g, x))
};

// Applicative: can lift multi-argument functions
algebra Applicative(f:Type1) extends Functor(f) = {
    function pure(x:a) : f(a),
    function ap(ff:f(a -> b), fa:f(a)) : f(b),
    law apIdentity(v:f(a)) = ap(pure(id), v) === v,
    law apHomomorphism(f:a -> b, x:a) = ap(pure(f), pure(x)) === pure(f(x))
};

// Monad: can sequence dependent computations
algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f:a -> m(b)) : m(b),
    law leftUnit(x:a, f:a -> m(b))  = bind(pure(x), f) === f(x),
    law rightUnit(x:m(a))           = bind(x, pure) === x,
    law associativity(x:m(a), f:a -> m(b), g:b -> m(c)) =
        bind(bind(x, f), g) === bind(x, { {a} -> bind(f(a), g) })
};

// Example instance:
instance Monad(Maybe) = {
    function bind(x, f) = {
        {Nothing, f} -> Nothing,
        {Just(v), f} -> f(v)
    }
};
```

### 9.4 Do-notation as syntactic sugar

The `do`-notation desugars into `bind` calls, working with anything that has a `Monad` instance:

```tulam
// this:
action main : IO(Unit) = {
    name <- readLine(),
    greeting = "Hello, " + name,
    putStrLn(greeting)
};

// desugars to:
function main() : IO(Unit) =
    bind(readLine(), { {name} ->
        bind(putStrLn("Hello, " + name), { {_} -> pure({}) })
    });
```

Since tulam already has `action` as a keyword for sequential computation, this is a natural fit: **`action` IS do-notation**. The action body is a sequence of statements that desugar into monadic bind chains.

### 9.5 Monad gives you a Category for free

As shown in Section 8.3, every `Monad(m)` automatically gives `Category(Kleisli(m))`. The compiler should derive this automatically:

```tulam
// The compiler generates this whenever it sees Monad(m):
instance Category(Kleisli(m)) = { ... }  // derived from Monad(m)
```

This means monadic composition (`>=>` in Haskell) comes for free as `compose` in the Kleisli category.

---

## 10. Syntactic Sugar Tied to Structures

The categorical structures enable specific syntactic sugar. The key insight is that **sugar is tied to structure instances, not keywords**:

| Sugar | Requires | Desugars to |
|-------|----------|-------------|
| `do` / `action` body | `Monad(m)` | `bind` chains |
| `<- ` in actions | `Monad(m)` | `bind(expr, { {var} -> ... })` |
| `f . g` composition | `Category(arr)` | `compose(f, g)` |
| `for x in xs` | `Traversable(t)` | `traverse` / `mapM` |
| `f <$> x` | `Functor(f)` | `fmap(f, x)` |
| `f <*> x` | `Applicative(f)` | `ap(f, x)` |
| Automatic conversion | `Convertible(a,b)` | `convert(x)` inserted by compiler |

---

## 11. The Full Picture

```
                    Category(arr)
                   /      |       \
                  /       |        \
         Arrow(arr)   Kleisli(m)   (other categories)
              |           |
              |       Monad(m)
              |       /       \
              |   Applicative(m)  MonadTransformer(t)
              |       |
              |   Functor(f)
              |       |
         _____|_______|________
        |                      |
   Morphism(a,b)          Algebra(a)
        |                      |
   Convertible            Monoid, Group
   Iso                    Eq, Ord, Show
        |                      |
         \____________________/
                  |
            structure (general)
```

Everything above the dashed line compiles to the same implicit-parameter functions.
The hierarchy tells the compiler what *extra things* it can derive and optimize.

---

## 12. Summary of Keywords

| Keyword | Status | Meaning |
|---------|--------|---------|
| `structure` | **Exists now** | General-purpose, catch-all |
| `algebra` | **New sugar** | Single-type structure (enables product derivation) |
| `morphism` | **New sugar** | Multi-type structure (enables composition) |
| `extends` | **New** | Same-shape refinement (inherits operations + laws) |
| `requires` | **New** | External constraints (different shape allowed) |
| `law` | **New** | Declares equational property using `===` |
| `===` | **New operator** | Propositional equality in laws |
| `==>` | **New operator** | Implication in conditional laws |
| `functor` | **Future** | Sugar for type + Functor instance (requires HKT) |
| `natural` | **Future** | Parametrically polymorphic functor morphism (requires HKT) |
| `category` | **Not a keyword** | Defined as algebra in standard library |
| `arrow` | **Not a keyword** | Defined as algebra in standard library |
| `monad` | **Not a keyword** | Defined as algebra in standard library |

### What needs first-class keywords vs what doesn't

**First-class keywords** are warranted when the concept:
1. Enables syntactic sugar that can't work otherwise (`action`/do-notation)
2. Changes how the compiler processes definitions fundamentally (`type`, `function`)
3. Is so pervasive that verbosity hurts readability (`algebra`, `morphism`)

**Standard library structures** suffice when the concept:
1. Is just algebraic structure with laws (`Category`, `Monad`, `Arrow`)
2. Doesn't require special syntax (though it may *enable* sugar via the structure)
3. Is one of many possible structures at the same level (`Monad` vs `Comonad`, `Arrow` vs `Profunctor`)

---

## 13. Implementation Roadmap

### Phase 1: Foundation (COMPLETE)
- [x] Universe hierarchy (`U Int`, `Type`, `Type1`, ...)
- [x] Instance declarations: parse, process, case-optimize, CLM-convert, interpret
- [x] Instance dispatch via constructor tag type inference
- [x] `base.tl` with `Eq(Nat)`, `Eq(Bool)` instances

### Phase 2: Basic language completeness
- [ ] `if/then/else` expression
- [ ] `let/in` bindings
- [ ] Records (named product types)
- [ ] List literals and basic list operations
- [ ] Primitive operations (`print#`, `concat#`, arithmetic)

### Phase 3: Algebra and Morphism keywords
- [ ] `algebra` keyword — parser, AST node, pipeline (validates single-param)
- [ ] `morphism` keyword — parser, AST node, pipeline (validates 2+ params)
- [ ] Both compile identically to `structure` in the pipeline
- [ ] `law` declarations — parse and store in AST (not enforced yet)
- [ ] `===` operator in law context (parsed as `PropEq` AST node, not a runtime op)
- [ ] `==>` implication in law context

### Phase 4: `extends` for same-shape refinement
- [ ] `extends` in parser for structure/algebra/morphism declarations
- [ ] Validate parameter shape match at parse time
- [ ] Inherit operations from parent structure
- [ ] Inherit laws from parent structure
- [ ] Auto-derive parent instances from child instances

### Phase 5: `requires` for constraints
- [ ] `requires` clause in parser for structures and instances
- [ ] Compile `requires` to additional implicit parameters
- [ ] Constraint resolution during instance dispatch
- [ ] Support on both structure declarations and instance declarations

### Phase 6: Higher-kinded types
- [ ] `f:Type1` parameters in structures (type constructor parameters)
- [ ] Type-constructor application (`f(a)` where `f` is a type variable)
- [ ] `Functor`, `Applicative`, `Monad` in standard library
- [ ] `action` body desugaring to `bind` chains
- [ ] Automatic morphism composition for `Convertible`

### Phase 7: Full categorical infrastructure
- [ ] `natural` keyword for natural transformations
- [ ] `functor` keyword as sugar for type + Functor instance
- [ ] `Category`, `Arrow` in standard library
- [ ] Automatic Kleisli category derivation from Monad
- [ ] Composition operator (`.`) dispatching through Category instances

### Phase 8: Type checker
- [ ] Universe level checking (`Type : Type1`, etc.)
- [ ] Basic type inference / unification
- [ ] Structure constraint checking
- [ ] Law verification via property-based testing

### Phase 9: Advanced
- [ ] Automatic algebra derivation for product types
- [ ] Law-based optimizations (rewrite rules)
- [ ] Profunctors, Comonads, Adjunctions as standard library structures
- [ ] Universe polymorphism (if needed)

---

## 14. Relation to Existing tulam Concepts

### How this fits with "everything is tuples + lambdas"

The categorical vocabulary doesn't change the foundation. It's a *classification system* for the structures we build from tuples and lambdas:

- An **algebra** is a tuple of lambdas that operate on one type
- A **morphism** is a tuple of lambdas that connect two types
- A **functor** is an algebra whose carrier is itself a lambda (type constructor)
- A **natural transformation** is a lambda between functors that's uniform in its parameter
- A **category** is an algebra on a two-parameter type constructor, with composition

All of these are still tuples and lambdas internally. The categorical keywords are a *type discipline* on top, telling us what kind of tuple-of-lambdas we're looking at and what laws it should satisfy.

### How this fits with compilation targets

- **.NET**: algebras map to interfaces with one type parameter, morphisms map to interfaces with multiple type parameters or implicit conversion operators, functors map to generic interfaces with HKT encoding
- **JavaScript**: all structures map to dictionaries (vtables) passed as arguments, composition is function composition
- **Native**: structures are erased where possible (monomorphization), kept as vtables where needed

### How this fits with the pipeline

```
Source (.tl)
  -> Parser (recognizes algebra/morphism/structure/law keywords)
  -> Surface AST (Expr nodes carry the categorical classification + laws)
  -> Pass 1: Environment building (registers algebras, morphisms, extends/requires)
  -> Pass 2: Case optimization (unchanged)
  -> Pass 3: CLM conversion (categorical info erased -- CLM is simply-typed)
  -> Pass 4: Type checking (validates laws, derives compositions, checks extends/requires)
  -> Pass 5: Code generation (uses categorical info for target-specific optimization)
```

The categorical classification is a *surface* and *middle-end* concept. By the time we reach CLM, everything is simply-typed functions operating on n-tuples -- exactly as before.
