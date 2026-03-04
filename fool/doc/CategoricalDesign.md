# Categorical Type System Design for FOOL3

## Motivation

FOOL3 is built on two primitives: **tuples** and **lambdas**. Category theory provides the natural mathematical framework for understanding how types and functions compose. Rather than bolting on categorical concepts after the fact (as Haskell does with its typeclass hierarchy), FOOL3 has the opportunity to make categorical structure explicit in the surface language from the beginning.

This document describes a layered vocabulary of categorical constructs, how they map to existing FOOL3 infrastructure, and a practical implementation roadmap.

---

## 1. The Categorical Hierarchy

### 1.1 Core Insight

FOOL3's `structure` keyword currently serves as a catch-all for typeclasses. But structures that depend on different numbers and kinds of type parameters are categorically *different things*:

| Parameters | Categorical concept | What it does | Example |
|---|---|---|---|
| 1 type | **Algebra** | Equips one type with operations | `Monoid(a)`, `Group(a)` |
| 2+ types | **Morphism** | Relates types directionally | `Convertible(a,b)`, `Iso(a,b)` |
| 1 type constructor | **Functor** | Structure-preserving map | `Functor(f)`, `Monad(m)` |
| 2 type constructors | **Natural transformation** | Morphism between functors | `safeHead : List ~> Maybe` |
| Morphisms + composition | **Category** | Universe of composable arrows | `Category(arr)` |

All of these compile down to the same implicit-parameter functions that FOOL3 already supports. The categorical vocabulary is a *surface* distinction that enables better error messages, automatic derivation, and principled composition.

### 1.2 Design Principle

**`structure` remains the general-purpose keyword.** The categorical keywords (`algebra`, `morphism`, `functor`, `natural`, `category`) are *refinements* that carry additional semantic meaning. Code using plain `structure` still works -- it's just less precise, like using `Object` in Java when you could use a specific interface.

---

## 2. Level 1: Algebras -- Structure on One Type

An algebra equips a single carrier type with operations and laws.

### Syntax

```fool
algebra Monoid(a:Type) = {
    function empty() : a,
    function combine(x:a, y:a) : a
};

algebra Group(a:Type) extends Monoid(a) = {
    function inverse(x:a) : a
};
```

### What the compiler knows

When something is declared as `algebra` rather than `structure`:
- There is exactly **one carrier type** parameter
- **Product derivation** is sound: if `Monoid(A)` and `Monoid(B)`, then `Monoid({a:A, b:B})` can be derived automatically (pointwise operations)
- **Free construction** may be available: the free monoid over `a` is `List(a)`
- Laws (when we add them) can be checked by property testing

### Compilation

Identical to current `structure` compilation. `algebra` is sugar that tells the compiler "this is an algebra" for future derivation and checking.

### Examples

```fool
algebra Eq(a:Type) = {
    function ==(x:a, y:a) : Bool = not(!=(x,y)),
    function !=(x:a, y:a) : Bool = not(==(x,y))
};

algebra Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function <(x:a, y:a) : Bool  = compare(x,y) == LT,
    function >(x:a, y:a) : Bool  = compare(x,y) == GT,
    function <=(x:a, y:a) : Bool = not(>(x,y)),
    function >=(x:a, y:a) : Bool = not(<(x,y))
};

algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a
    -- law: associativity: combine(x, combine(y,z)) == combine(combine(x,y), z)
};

algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a
    -- law: left identity:  combine(empty(), x) == x
    -- law: right identity: combine(x, empty()) == x
};
```

---

## 3. Level 2: Morphisms -- Relations Between Types

A morphism establishes a directed relationship between two (or more) types. This is structure *between* objects in our category, not *within* a single object.

### Syntax

```fool
morphism Convertible(a:Type, b:Type) = {
    function convert(x:a) : b
};

morphism Iso(a:Type, b:Type)
    extends Convertible(a, b) = {
    function unconvert(x:b) : a
    -- law: unconvert(convert(x)) == x
    -- law: convert(unconvert(y)) == y
};
```

### What the compiler knows

When something is declared as `morphism`:
- There are **two or more type** parameters with a directional relationship
- **Composition** is automatic: if `Convertible(A,B)` and `Convertible(B,C)`, the compiler can derive `Convertible(A,C)` via `convert(x) = convert_BC(convert_AB(x))`
- **Identity** exists: `Convertible(A,A)` is trivially `convert = id`
- This means morphisms form a **category** automatically (see Section 6)

### Compilation

Same as multi-parameter `structure`. The compiler additionally registers composability.

### Practical value

On .NET, `Convertible(A,B)` maps to implicit conversion operators. Composition means the compiler can chain conversions automatically:

```fool
instance Convertible(Int, Float) = { function convert(x:Int):Float = intToFloat#(x) };
instance Convertible(Float, String) = { function convert(x:Float):String = showFloat#(x) };

-- compiler can derive: Convertible(Int, String) via Float
```

### Value-dependent morphisms

Structures can also depend on values, creating *indexed* or *parameterized* morphisms:

```fool
morphism LinearMap(k:Type, v:Type, w:Type, field:Field(k)) = {
    function apply(f:v -> w, x:v) : w,
    function scale(s:k, x:v) : v
    -- law: apply(f, scale(s,x)) == scale(s, apply(f,x))
};
```

Here `field:Field(k)` is a *value* (a proof that `k` is a field) living in `Type1` thanks to our universe hierarchy. The structure is parameterized by both types and evidence.

---

## 4. Level 3: Functors -- Structure-Preserving Maps

A functor is a type constructor `F : Type -> Type` that also maps functions: if you have `f : a -> b`, you get `fmap(f) : F(a) -> F(b)`.

### Syntax (two options)

**Option A: Functor as a declaration that combines type + mapping**

```fool
functor Maybe(a:Type) : Type = { Just(x:a), Nothing };
-- This BOTH defines the sum type AND declares fmap exists.
-- fmap must be provided or derived.

functor List(a:Type) : Type = { Nil, Cons(head:a, tail:List(a)) };
```

**Option B: Functor as an algebra on type constructors**

```fool
type Maybe(a:Type) = { Just(x:a), Nothing };

algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b)
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
4. Option A conflates two things (data definition + structure instance) which violates FOOL3's design of keeping things orthogonal

However, we could add `functor` as **sugar** that expands to Option B:

```fool
-- this:
functor Maybe(a:Type) : Type = { Just(x:a), Nothing };
-- expands to:
type Maybe(a:Type) = { Just(x:a), Nothing };
-- plus auto-derived: instance Functor(Maybe) = { ... }
```

### What the compiler knows

- `Functor(F)` means `F` preserves composition: `fmap(f . g) == fmap(f) . fmap(g)`
- `Functor(F)` and `Functor(G)` implies `Functor(F . G)` -- functor composition is automatic
- This is the foundation for Applicative, Monad, Traversable, etc.

### Higher-kinded types requirement

Functors require the type system to handle `f:Type1` -- type constructors as first-class parameters. This is already representable with our universe hierarchy (`Type1 = U 1`), but the parser and pipeline need to handle type-constructor application (`f(a)` where `f` is a type variable of kind `Type -> Type`).

---

## 5. Level 4: Natural Transformations -- Morphisms Between Functors

A natural transformation is a family of functions `F(a) -> G(a)` that is *uniform* in `a` -- it doesn't inspect or depend on what `a` is.

### Syntax

```fool
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
- The naturality condition `fmap_G(f) . alpha == alpha . fmap_F(f)` holds by parametricity (free theorem)

### Why this matters

Natural transformations are the **right abstraction for polymorphic container operations**:
- `safeHead : List ~> Maybe` -- extract first element
- `reverse : List ~> List` -- natural endomorphism
- `toList : Set ~> List` -- forget ordering
- `flatten : List . List ~> List` -- this is `join` for the List monad!

By marking these as `natural`, the compiler knows they compose and can optimize chains of container transformations.

### Implementation

A `natural` declaration compiles to a rank-2 polymorphic function internally:
```fool
-- natural safeHead : List ~> Maybe compiles to:
function safeHead [a:Type] (xs:List(a)) : Maybe(a) = ...
```

The `natural` keyword is a *contract* that this function doesn't inspect `a`, which the type checker can verify later.

---

## 6. Categories and Arrows

### 6.1 The Default Category

FOOL3 programs live in a default category implicitly:
- **Objects** = types (inhabitants of `Type`)
- **Morphisms** = functions (`a -> b`)
- **Composition** = function composition (`.` or `compose`)
- **Identity** = `id : a -> a`

This is **Type**, the category of types and functions. We don't need to declare it -- it's the ambient universe.

### 6.2 Category as a Structure

Other categories can be defined as structures. A category needs:

```fool
-- A category is parameterized by its morphism type
-- Objects are implicit (they're the types that arr connects)
algebra Category(arr: Type -> Type -> Type) = {
    function id(a:Type) : arr(a, a),
    function compose(f:arr(b,c), g:arr(a,b)) : arr(a,c)
    -- law: compose(id, f) == f            (left identity)
    -- law: compose(f, id) == f            (right identity)
    -- law: compose(f, compose(g,h)) == compose(compose(f,g), h)  (associativity)
};

-- The default instance: plain functions form a category
instance Category(Function) = {
    function id(a:Type) : a -> a = { {x} -> x },
    function compose(f, g) = { {x} -> f(g(x)) }
};
```

Note: `Category` takes `arr : Type -> Type -> Type` -- a two-parameter type constructor. This requires our type system to handle `Type -> Type -> Type` as a kind, which is `Type2` in our universe hierarchy.

### 6.3 Kleisli Categories

Every monad gives rise to a category -- the Kleisli category. This is where monads connect to the categorical framework:

```fool
-- Kleisli arrow: a function a -> m(b) for some monad m
type Kleisli(m: Type -> Type, a:Type, b:Type) = a -> m(b);

-- Given a Monad(m), Kleisli(m) forms a Category
instance Category(Kleisli(m)) requires Monad(m) = {
    function id(a:Type) : Kleisli(m, a, a) = { {x} -> return(x) },
    function compose(f:Kleisli(m,b,c), g:Kleisli(m,a,b)) : Kleisli(m,a,c) = {
        {x} -> bind(g(x), f)
    }
};
```

This is extremely powerful: it means **every monad automatically gives you a category** of effectful computations, with composition working correctly.

### 6.4 Arrows

Arrows generalize both functions and monadic computations. An Arrow is a Category with additional structure:

```fool
algebra Arrow(arr: Type -> Type -> Type) extends Category(arr) = {
    function arr(f: a -> b) : arr(a, b),           -- lift a function
    function first(f:arr(a,b)) : arr({a,c}, {b,c}) -- process first component
    -- derived:
    function second(f:arr(a,b)) : arr({c,a}, {c,b}) = ...,
    function split(f:arr(a,b), g:arr(c,d)) : arr({a,c}, {b,d}) = ...,
    function fanout(f:arr(a,b), g:arr(a,c)) : arr(a, {b,c}) = ...
};
```

### 6.5 Should Category and Arrow be first-class keywords?

**No. They should be structures (algebras).**

Reasoning:
- Categories and Arrows are *algebraic structures* on morphism types -- they fit the `algebra` pattern perfectly
- Making them keywords would add complexity without enabling anything that structures can't express
- The Haskell approach (Category and Arrow as typeclasses) is correct here
- The interesting thing about categories is not their definition but their *use* -- composition operators, do-notation, arrow notation
- What we DO want is **syntactic sugar** that works with any Category instance (see Section 8)

However, `Category` and `Arrow` should be **built-in structures** in the standard library (like `base.fool`), not user-defined, because the compiler needs to know about them for:
- Optimizing composition chains
- Providing do-notation and arrow-notation
- Deriving Kleisli categories from monads automatically

---

## 7. Monads -- Structure on Functors

### 7.1 Where Monads Fit

A monad is a functor with extra algebraic structure. In categorical terms:

> A monad on a category C is an endofunctor `M : C -> C` together with two natural transformations: `return : Id ~> M` and `join : M . M ~> M`, satisfying associativity and unit laws.

In FOOL3 terms, this translates to: **a Monad is an algebra on a Functor**.

### 7.2 Should Monad be a keyword or a structure?

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

### 7.3 The Monad Hierarchy

```fool
-- Functor: can map over contents
algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b)
};

-- Applicative: can lift multi-argument functions
algebra Applicative(f:Type1) extends Functor(f) = {
    function pure(x:a) : f(a),
    function ap(ff:f(a -> b), fa:f(a)) : f(b)
};

-- Monad: can sequence dependent computations
algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f:a -> m(b)) : m(b)
    -- derived from Applicative:
    -- pure comes from Applicative
    -- fmap(f,x) = bind(x, compose(pure, f))
};

-- Example instance:
instance Monad(Maybe) = {
    function bind(x, f) = {
        {Nothing, f} -> Nothing,
        {Just(v), f} -> f(v)
    }
};
```

### 7.4 Do-notation as syntactic sugar

The `do`-notation desugars into `bind` calls, working with anything that has a `Monad` instance:

```fool
-- this:
action main : IO(Unit) = {
    name <- readLine(),
    greeting = "Hello, " + name,
    putStrLn(greeting)
};

-- desugars to:
function main() : IO(Unit) =
    bind(readLine(), { {name} ->
        bind(putStrLn("Hello, " + name), { {_} -> pure({}) })
    });
```

Since FOOL3 already has `action` as a keyword for sequential computation, this is a natural fit: **`action` IS do-notation**. The action body is a sequence of statements that desugar into monadic bind chains.

### 7.5 Monad gives you a Category for free

As shown in Section 6.3, every `Monad(m)` automatically gives `Category(Kleisli(m))`. The compiler should derive this automatically:

```fool
-- The compiler generates this whenever it sees Monad(m):
instance Category(Kleisli(m)) = { ... }  -- derived from Monad(m)
```

This means monadic composition (`>=>` in Haskell) comes for free as `compose` in the Kleisli category.

---

## 8. Syntactic Sugar Tied to Structures

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

## 9. The Full Picture

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

## 10. Summary of Keywords

| Keyword | Status | Meaning |
|---------|--------|---------|
| `structure` | **Exists now** | General-purpose, catch-all |
| `algebra` | **New sugar** | Single-type structure (enables product derivation) |
| `morphism` | **New sugar** | Multi-type structure (enables composition) |
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

## 11. Implementation Roadmap

### Phase 1: Now (no type checker needed)
- [x] Universe hierarchy (`U Int`, `Type`, `Type1`, ...)
- [ ] `algebra` keyword as alias for single-param `structure`
- [ ] `morphism` keyword as alias for multi-param `structure`
- [ ] `extends` for structure inheritance
- [ ] Carry the algebra/morphism distinction through the pipeline

### Phase 2: With basic type checking
- [ ] Higher-kinded type parameters (`f:Type1` in structures)
- [ ] `Functor` as a standard library algebra
- [ ] `Applicative`, `Monad` as standard library algebras
- [ ] `action` body desugaring to `bind` chains
- [ ] Automatic morphism composition for `Convertible`

### Phase 3: Full categorical infrastructure
- [ ] `natural` keyword for natural transformations
- [ ] `functor` keyword as sugar for type + Functor instance
- [ ] `Category`, `Arrow` in standard library
- [ ] Automatic Kleisli category derivation from Monad
- [ ] Composition operator (`.`) dispatching through Category instances

### Phase 4: Advanced
- [ ] Law specifications and property-based testing
- [ ] Automatic algebra derivation for product types
- [ ] Profunctors, Comonads, Adjunctions as standard library structures
- [ ] Universe polymorphism (if needed)

---

## 12. Relation to Existing FOOL3 Concepts

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
Source (.fool)
  -> Parser (recognizes algebra/morphism/structure keywords)
  -> Surface AST (Expr nodes carry the categorical classification)
  -> Pass 1: Environment building (registers algebras, morphisms with their properties)
  -> Pass 2: Case optimization (unchanged)
  -> Pass 3: CLM conversion (categorical info erased -- CLM is simply-typed)
  -> Pass 4: Type checking (validates laws, derives compositions)
  -> Pass 5: Code generation (uses categorical info for target-specific optimization)
```

The categorical classification is a *surface* and *middle-end* concept. By the time we reach CLM, everything is simply-typed functions operating on n-tuples -- exactly as before.
