# tulam Language Reference

## Introduction

tulam (Functional Object-Oriented Low-level Language) is a type-theory based functional language compiler/interpreter written in Haskell. It targets JavaScript, .NET, and potentially x86 native code.

tulam is built on two primitives: **tuples** and **lambdas**. Everything else — sum types, product types, structures (typeclasses), records — is derived from these two building blocks.

### Building and Running

```bash
stack build              # Build the project
stack exec tulam          # Start the REPL (loads base.tl automatically)
stack test               # Run tests
stack clean              # Clean build artifacts
```

The project uses Stack with Hpack. Edit `package.yaml` for dependency changes (not `tulam.cabal`).

---

## Getting Started: The REPL

When you run `stack exec tulam`, you enter the interactive REPL. It automatically loads `base.tl`, which provides basic types like `Nat` and `Bool`.

### REPL Commands

| Command           | Description                              |
|-------------------|------------------------------------------|
| `:load <file>`    | Load and compile a `.tl` source file   |
| `:list types`     | Show all defined types                   |
| `:list functions` | Show all defined functions               |
| `:env`            | Show the current environment             |
| `:all`            | Show everything (types, functions, etc.) |
| `:clm`            | Show CLM (Core List Machine) IR output   |
| `:quit`           | Exit the REPL                            |

You can also type expressions directly to evaluate them, or enter top-level declarations (terminated with `;`).

---

## Comments

tulam supports two styles of comments:

```
// This is a single-line comment

/* This is a
   multi-line comment */
```

Multi-line comments do **not** nest.

---

## Literals

### Integers

Non-negative integer literals (natural numbers):

```
0
42
1000
```

### Floating-Point Numbers

Decimal numbers with a fractional part:

```
3.14
0.5
100.0
```

### Strings

Double-quoted string literals:

```
"hello"
"world"
""
```

Standard escape sequences are supported (e.g., `\"`, `\\`, `\n`).

### Lists

Square brackets with comma-separated elements:

```
[1, 2, 3]
[True, False]
[]
```

### Tuples

Curly braces with comma-separated elements:

```
{1, 2, 3}
{True, Z}
```

Tuples are one of the two fundamental primitives in tulam.

### Vectors

Angle brackets with comma-separated elements:

```
<1, 2, 3>
<0.5, 1.0, 1.5>
```

---

## Identifiers and Naming Conventions

Identifiers start with a letter or underscore, followed by letters, digits, underscores, apostrophes, or `#`.

### Naming Rules

- **Types and constructors** must start with an **uppercase** letter: `Nat`, `Bool`, `Succ`, `True`
- **Functions and variables** must start with a **lowercase** letter: `plus`, `eq`, `not`, `x`
- The **`#` suffix** denotes built-in/primitive operations: `print#`, `concat#`, `+#`, `-#`

### Operators

Operators are sequences of symbolic characters. They can include standard symbols (`+`, `-`, `*`, `/`, `=`, `<`, `>`, etc.) as well as Unicode symbols. Operators can be defined as functions by wrapping them in parentheses:

```
function (+)(x:Nat, y:Nat) : Nat = plus(x, y);
```

---

## Type Annotations

Type annotations are optional and use the `name:Type` syntax:

```
x:Nat           // x has type Nat
b:Bool          // b has type Bool
f:Type          // f is a type
```

Type annotations appear in function parameters, variable bindings, and return types.

### Rich Type Expressions

Type annotations support the full type expression syntax:

- **Simple names**: `Nat`, `Bool`, `a`
- **Type application**: `Vec(a, n)`, `Maybe(a)`, `Pair(Nat, Bool)`
- **Arrow types** (right-associative): `Nat -> Bool`, `a -> b -> c`, `(a -> b) -> c`
- **Universes**: `Type`, `Type1`, `Type2`, `Type3`
- **Parenthesized**: `(a -> b) -> c`

Arrow types represent non-dependent function types. They are right-associative, so `a -> b -> c` means `a -> (b -> c)`.

```
function apply(f: Nat -> Bool, x:Nat) : Bool = f(x);
function compose(f: b -> c, g: a -> b, x:a) : c = f(g(x));
```

### Universe Hierarchy

tulam has a universe hierarchy for classifying types:

| Name     | Universe Level | Description                     |
|----------|----------------|---------------------------------|
| `Type`   | `U 0`         | The type of ordinary values     |
| `Type0`  | `U 0`         | Alias for `Type`                |
| `Type1`  | `U 1`         | The type of types (kinds)       |
| `Type2`  | `U 2`         | The type of kinds               |
| `Type3`  | `U 3`         | Higher universe                 |

Most user code only needs `Type`. Higher universes are used for type-level programming (e.g., parameterized records and structures that take types as arguments).

---

## Sum Types (Algebraic Data Types)

Sum types define a type as a choice between multiple constructors. This is tulam's equivalent of Haskell's `data` declarations or Rust's `enum`.

### Syntax

```
type Name = { Constructor1, Constructor2(args) };
```

Each constructor can optionally take arguments in parentheses. Arguments are comma-separated `name:Type` pairs.

### Examples

**Simple enumeration:**

```
type Bool = { True, False };
```

**Recursive type:**

```
type Nat = {
  Z,
  Succ(n:Nat)
};
```

`Nat` represents natural numbers. `Z` is zero, and `Succ(n)` is the successor of `n`. So `Succ(Succ(Z))` represents 2.

**Parameterized type:**

```
type ConstructorTag = { ConstructorTag(n:Nat) };
```

### How It Works

Under the hood, constructors create tagged tuples. Each constructor in a sum type gets an integer tag, which is used for pattern matching. This is essential for compilation to targets like .NET and JavaScript.

### GADTs (Generalized Algebraic Data Types)

Constructors can specify their own return type, which may be more specific than the parent type. This is done with a `: ReturnType` annotation after the constructor:

```
type Vec(a:Type, n:Nat) = {
    VNil : Vec(a, Z),
    VCons(head:a, tail:Vec(a, n)) : Vec(a, Succ(n))
};
```

Here `VNil` returns `Vec(a, Z)` (a vector of length zero) while `VCons` returns `Vec(a, Succ(n))` (a vector one longer). Without the `: ReturnType` annotation, the constructor's return type defaults to the parent type with its parameters (e.g., `Vec(a, n)`).

GADTs enable encoding type-level invariants directly in the type system, such as length-indexed vectors, balanced trees, and propositional equality.

### Propositional Equality (`PropEqT`)

The standard library defines propositional equality as a GADT:

```
type PropEqT(a:Type, x:a, y:a) = {
    Refl : PropEqT(a, x, x)
};
```

The `Refl` constructor can only produce `PropEqT(a, x, x)` — both indices must be the same value. This is the type-theoretic equality witness: a value of type `PropEqT(a, x, y)` is proof that `x` and `y` are equal.

Note: This is distinct from `===` in law declarations, which is a syntactic marker for documentation. `PropEqT` is a first-class value type that can be constructed, pattern-matched, and passed as an argument.

---

## Records (Product Types)

Records are syntactic sugar for single-constructor sum types with named fields. They provide a convenient way to define product types.

### Basic Record

```
record Point = { x:Nat, y:Nat };
```

This desugars to:

```
type Point = { Point(x:Nat, y:Nat) };
```

### Parameterized Record

Records can take type parameters:

```
record Pair(a:Type, b:Type) = { fst:a, snd:b };
```

### Record Spread

You can include all fields from another record using the `..` spread syntax:

```
record Point3D = { ..Point, z:Nat };
```

This expands to include all fields from `Point` (`x:Nat`, `y:Nat`) plus the new field `z:Nat`. Spread fields are resolved during compilation when the environment is available.

---

## Functions

Functions are the primary way to define computations in tulam.

### Basic Function

```
function name(args) : ReturnType = body;
```

The return type annotation is optional.

**Simple function:**

```
function f(x) = [x, 18, 29];
```

**With type annotations:**

```
function typeOf(ex:tp) : Type = tp;
```

### Pattern Matching

Functions can use pattern matching with curly-brace syntax. Each case is `{patterns} -> expression`, separated by commas:

```
function eq(x:Nat, y:Nat) : Bool = {
  {Z, Z}         -> True,
  {Z, n}         -> False,
  {n, Z}         -> False,
  {Succ(m), Succ(n)} -> eq(m, n)
};
```

The number of patterns in each case must match the number of function parameters.

**More examples:**

```
function not(b:Bool) : Bool = {
  {True}  -> False,
  {False} -> True
};

function plus(x:Nat, y:Nat) : Nat = {
  {Z, n}       -> n,
  {Succ(n), m} -> Succ(plus(n, m))
};
```

Patterns can be:
- **Variables** (`n`, `m`, `x`) — match anything and bind the value
- **Constructors** (`Z`, `True`) — match a specific constructor
- **Nested constructor applications** (`Succ(n)`, `Succ(Succ(Z))`) — match and destructure

### Operators as Functions

Operators can be defined as functions by wrapping the operator in parentheses:

```
function (==)(x, y:a) : Bool = not(x != y);
function (!=)(x, y:a) : Bool = not(x == y);
```

---

## Expressions

### Function Application

Call a function by name with arguments in parentheses:

```
plus(x, y)
Succ(Z)
eq(m, n)
not(b)
```

The function position can also be a parenthesized expression:

```
(f)(x, y)
```

### Binary Operators

Binary operators are written infix:

```
x + y
a * b
x == y
n +# m      // primitive addition
```

### Unary Operators

Prefix unary operators:

```
-x
```

### Operator Precedence

From highest to lowest precedence (all left-associative):

| Precedence | Operators              | Description          |
|------------|------------------------|----------------------|
| Highest    | `*`, `/`, `*#`, `/#`   | Multiplication, division |
| Medium     | `+`, `-`, `+#`, `-#`   | Addition, subtraction    |
| Lower      | Custom operators       | User-defined operators   |
| Lowest     | `==`                   | Equality comparison      |

The `#`-suffixed operators are primitive (built-in) variants.

### If/Then/Else

Conditional expressions:

```
if condition then expr1 else expr2
```

Both branches are required. The condition should evaluate to a `Bool`.

**Example:**

```
function testIf(b:Bool) : Nat = if b then Succ(Z) else Z;
```

### Let/In

Local bindings with `let ... in`:

```
let name = expr in body
```

Multiple bindings are separated by commas:

```
let x = expr1, y = expr2 in body
```

Bindings can have optional type annotations:

```
let x:Nat = Succ(Z) in plus(x, x)
```

**Example:**

```
function testLet(n:Nat) : Nat = let one = Succ(Z) in plus(n, one);
```

### Parenthesized Expressions

Parentheses can be used for grouping:

```
(x + y) * z
```

---

## Structures (Typeclasses)

Structures are tulam's equivalent of Haskell's typeclasses. They define a set of functions that can be implemented for different types.

### Syntax

```
structure Name(args) = {
  function fn1(params) : ReturnType = defaultImpl,
  function fn2(params) : ReturnType = defaultImpl
};
```

### Categorical Keywords

tulam provides specialized keywords for classifying structures by their categorical role. Each has both a mathematical and a programmer-friendly alias:

| Math keyword | Friendly keyword | Meaning | Use when |
|---|---|---|---|
| `algebra` | `trait` | Single-type operations | Eq, Monoid, Ord, etc. |
| `morphism` | `bridge` | Multi-type relations | Convertible, Iso, etc. |
| `structure` | — | General-purpose (no alias) | Everything else |

Both forms are fully interchangeable:

```
// Math style:
algebra Semigroup(a:Type) = { function combine(x:a, y:a) : a };

// Programmer-friendly style (identical semantics):
trait Semigroup(a:Type) = { function combine(x:a, y:a) : a };

// Multi-type:
morphism Convertible(a:Type, b:Type) = { function convert(x:a) : b };
bridge Convertible(a:Type, b:Type) = { function convert(x:a) : b };
```

The compiler issues a warning (not an error) if:
- An `algebra`/`trait` has more or fewer than 1 type parameter
- A `morphism`/`bridge` has fewer than 2 type parameters

### Example

```
algebra Eq(a:Type) = {
  function (==)(x, y:a) : Bool = not(x != y),
  function (!=)(x, y:a) : Bool = not(x == y),
  law reflexivity(x:a) = (x == x) === True,
  law symmetry(x:a, y:a) = (x == y) === (y == x)
};
```

This defines an `Eq` algebra with two functions and two law declarations. Laws are desugared into proof-returning functions (see Law Declarations below).

### How Structures Work

When a structure is declared, its functions are transformed into functions with **implicit parameters**. For example, the `(==)` function from `Eq` becomes:

```
(==) [a:Type] (x:a, y:a) : Bool
```

The `[a:Type]` is an implicit parameter that gets resolved at compile time based on the types of the arguments. This means type-dependent dispatch happens during compilation, not at runtime.

---

## Law Declarations

Laws declare expected properties of structure functions. They use the `law` keyword as syntactic sugar, but are **desugared into real functions** returning `PropEqT` proof terms via the Curry-Howard correspondence: propositions are types, proofs are programs.

### Syntax

```
law name(params) = lawBody
```

Law bodies use two special operators:

| Operator | Meaning | Desugars to |
|----------|---------|-------------|
| `===` | Propositional equality | `PropEqT` return type |
| `==>` | Implication (right-associative) | Extra proof parameter |

### Desugaring Rules

A law is desugared into a function returning a `PropEqT` proof:

```
// Source:
law reflexivity(x:a) = (x == x) === True

// Desugars to:
function reflexivity(x:a) : PropEqT(_, x == x, True) = Refl
```

Implications (`==>`) become additional proof parameters:

```
// Source:
law transitivity(x:a, y:a, z:a) =
    ((x == y) == True) ==> ((y == z) == True) ==> ((x == z) === True)

// Desugars to:
function transitivity(x:a, y:a, z:a,
    __proof0: PropEqT(_, (x == y) == True, True),
    __proof1: PropEqT(_, (y == z) == True, True))
    : PropEqT(_, x == z, True) = Refl
```

A bare expression without `===` is treated as `expr === True`. A premise with `===` (e.g., `a === b ==> ...`) becomes `PropEqT(_, a, b)`.

### Examples

```
law reflexivity(x:a) = (x == x) === True;

law symmetry(x:a, y:a) = (x == y) === (y == x);

law transitivity(x:a, y:a, z:a) =
    ((x == y) == True) ==> ((y == z) == True) ==> ((x == z) === True);
```

Laws can only appear inside structure/algebra/morphism declarations. After desugaring, they become regular functions with implicit structure parameters, just like other structure functions.

---

## Structure Inheritance (`extends`)

A structure can inherit functions and laws from parent structures using `extends`.

### Syntax

```
algebra Child(a:Type) extends Parent1(a), Parent2(a) = {
  function childFunc(x:a) : a
};
```

### Behavior

- All functions and laws from parent structures are automatically included in the child
- The child can override inherited functions by declaring a function with the same name
- When an `instance` is declared for the child, its function implementations are **propagated to parent structures** as well, so dispatch works through both child and parent

### Example

```
algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) =
        combine(x, combine(y, z)) === combine(combine(x, y), z)
};

algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a
};

instance Monoid(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y),
    function empty() : Nat = Z
};
// combine is now available via both Semigroup(Nat) and Monoid(Nat)
```

---

## External Constraints (`requires`)

Structures and instances can declare external constraints using `requires`. These are validated at declaration time (the required structure must exist) but runtime resolution is deferred to the type checker.

### Syntax

```
morphism MonoidHom(a:Type, b:Type) requires Monoid(a), Monoid(b) = {
    function hom(x:a) : b
};

instance SomeStruct(Nat) requires Eq(Nat) = {
    function foo(x:Nat) : Bool = (x == Z)
};
```

---

## Instance Declarations

Instances provide implementations of structure functions for specific types.

### Syntax

```
instance StructureName(TypeArgs) = {
  function fn1(params) : ReturnType = implementation
};
```

Instances can also declare constraints:

```
instance StructureName(TypeArgs) requires Constraint1(Type), Constraint2(Type) = {
  function fn1(params) : ReturnType = implementation
};
```

### Examples

**Eq instance for Nat using a helper function:**

```
instance Eq(Nat) = {
  function (==)(x:Nat, y:Nat) : Bool = eq(x, y)
};
```

**Eq instance for Bool using pattern matching:**

```
instance Eq(Bool) = {
  function (==)(x:Bool, y:Bool) : Bool = {
    {True, True}   -> True,
    {False, False}  -> True,
    {True, False}   -> False,
    {False, True}   -> False
  }
};
```

When an instance is declared, its function implementations are added as new pattern match cases to the structure's functions, specialized for the given type arguments.

---

## Actions

Actions represent imperative-style sequences of operations, similar to Haskell's `do` notation.

### Syntax

```
action name(args) : Type = {
  binding1 = expr1,
  binding2 = expr2,
  finalExpr
};
```

### Example

```
action main = {
  one = Succ(Z),
  three = Succ(Succ(one)),
  res = plus(three, one),
  print#(res)
};
```

Bindings inside an action are evaluated in order. Each binding makes a name available for subsequent expressions. The last expression is the return value.

---

## Built-in Operations

tulam provides several primitive operations, identified by the `#` suffix:

| Operation  | Description                     |
|------------|---------------------------------|
| `print#`   | Print a value                   |
| `concat#`  | Concatenate strings             |
| `+#`       | Primitive integer addition      |
| `-#`       | Primitive integer subtraction   |
| `*#`       | Primitive integer multiplication|
| `/#`       | Primitive integer division      |
| `primop#`  | Generic primitive operation     |

These operations bypass the normal type system and are handled specially by the compiler.

---

## Standard Library (base.tl)

The standard library is loaded automatically when you start the REPL. It provides the foundational types and functions.

### Nat (Natural Numbers)

```
type Nat = {
  Z,
  Succ(n:Nat)
};
```

- `Z` — zero
- `Succ(n)` — successor of `n`

**Arithmetic:**

```
function plus(x:Nat, y:Nat) : Nat = {
  {Z, n}       -> n,
  {Succ(n), m} -> Succ(plus(n, m))
};
```

**Equality:**

```
function eq(x:Nat, y:Nat) : Bool = {
  {Z, Z}             -> True,
  {Z, n}             -> False,
  {n, Z}             -> False,
  {Succ(m), Succ(n)} -> eq(m, n)
};
```

### Bool

```
type Bool = { True, False };
```

**Negation:**

```
function not(b:Bool) : Bool = {
  {True}  -> False,
  {False} -> True
};
```

### Eq Algebra

The `Eq` algebra provides overloaded equality with law declarations:

```
algebra Eq(a:Type) = {
  function (==)(x, y:a) : Bool = not(x != y),
  function (!=)(x, y:a) : Bool = not(x == y),
  law reflexivity(x:a) = (x == x) === True,
  law symmetry(x:a, y:a) = (x == y) === (y == x)
};

instance Eq(Nat) = {
  function (==)(x:Nat, y:Nat) : Bool = eq(x, y)
};

instance Eq(Bool) = {
  function (==)(x:Bool, y:Bool) : Bool = {
    {True, True}   -> True,
    {False, False}  -> True,
    {True, False}   -> False,
    {False, True}   -> False
  }
};
```

### Utility Functions

```
function typeOf(ex:tp) : Type = tp;     // Returns the type of an expression
function consOf(ex:tp) : ConstructorTag = primop#;  // Returns the constructor tag
```

---

## Reserved Words

The following words are reserved and cannot be used as identifiers:

`type`, `function`, `if`, `then`, `else`, `in`, `action`, `structure`, `instance`, `let`, `case`, `of`, `where`, `exists`, `forall`, `record`, `algebra`, `trait`, `morphism`, `bridge`, `law`, `extends`, `requires`

The Unicode symbols `∃` and `∀` are also reserved (for future quantifier support).

### Reserved Operators

`;` `=` `,` `.` `..` `:` `->` `=>` `|` `?` `<:` `\` `===` `==>`

---

## Top-Level Declarations

All top-level declarations must be terminated with a semicolon (`;`):

```
type Bool = { True, False };
function not(b:Bool) : Bool = { {True} -> False, {False} -> True };
record Point = { x:Nat, y:Nat };
structure Eq(a:Type) = { ... };
instance Eq(Nat) = { ... };
action main = { ... };
```

Within pattern match cases, cases are separated by commas (`,`), not semicolons.

---

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Sum type | `type Name = { Con1, Con2(args) };` |
| GADT constructor | `Con(args) : ReturnType` (inside sum type) |
| Arrow type | `a -> b`, `(a -> b) -> c` (in type position) |
| Type application | `Vec(a, n)`, `Maybe(a)` (in type position) |
| Record | `record Name = { field:Type };` |
| Record spread | `record Name = { ..Other, field:Type };` |
| Parameterized record | `record Name(a:Type) = { field:a };` |
| Function | `function name(args) : Type = body;` |
| Pattern match | `function f(x) = { {pat} -> expr, ... };` |
| Operator function | `function (+)(x, y) = ...;` |
| If/then/else | `if cond then e1 else e2` |
| Let/in | `let x = e1, y = e2 in body` |
| Structure | `structure Name(a:Type) = { functions };` |
| Algebra/Trait | `algebra Name(a:Type) = { ... };` or `trait Name(a:Type) = { ... };` |
| Morphism/Bridge | `morphism Name(a:Type, b:Type) = { ... };` or `bridge ...` |
| Extends | `algebra Child(a:Type) extends Parent(a) = { ... };` |
| Requires | `morphism M(a,b) requires X(a), Y(b) = { ... };` |
| Law | `law name(params) = lhs === rhs` |
| Instance | `instance Name(Type) = { functions };` |
| Action | `action name = { stmts };` |
| Application | `f(x, y)` |
| List | `[1, 2, 3]` |
| Tuple | `{1, 2, 3}` |
| Vector | `<1, 2, 3>` |
| Comment | `// line` or `/* block */` |
