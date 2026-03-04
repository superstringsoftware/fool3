# FOOL3 Language Reference

## Introduction

FOOL3 (Functional Object-Oriented Low-level Language) is a type-theory based functional language compiler/interpreter written in Haskell. It targets JavaScript, .NET, and potentially x86 native code.

FOOL3 is built on two primitives: **tuples** and **lambdas**. Everything else — sum types, product types, structures (typeclasses), records — is derived from these two building blocks.

### Building and Running

```bash
stack build              # Build the project
stack exec fool          # Start the REPL (loads base.fool automatically)
stack test               # Run tests
stack clean              # Clean build artifacts
```

The project uses Stack with Hpack. Edit `package.yaml` for dependency changes (not `fool.cabal`).

---

## Getting Started: The REPL

When you run `stack exec fool`, you enter the interactive REPL. It automatically loads `base.fool`, which provides basic types like `Nat` and `Bool`.

### REPL Commands

| Command           | Description                              |
|-------------------|------------------------------------------|
| `:load <file>`    | Load and compile a `.fool` source file   |
| `:list types`     | Show all defined types                   |
| `:list functions` | Show all defined functions               |
| `:env`            | Show the current environment             |
| `:all`            | Show everything (types, functions, etc.) |
| `:clm`            | Show CLM (Core List Machine) IR output   |
| `:quit`           | Exit the REPL                            |

You can also type expressions directly to evaluate them, or enter top-level declarations (terminated with `;`).

---

## Comments

FOOL3 supports two styles of comments:

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

Tuples are one of the two fundamental primitives in FOOL3.

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

### Universe Hierarchy

FOOL3 has a universe hierarchy for classifying types:

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

Sum types define a type as a choice between multiple constructors. This is FOOL3's equivalent of Haskell's `data` declarations or Rust's `enum`.

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

Functions are the primary way to define computations in FOOL3.

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

Structures are FOOL3's equivalent of Haskell's typeclasses. They define a set of functions that can be implemented for different types.

### Syntax

```
structure Name(args) = {
  function fn1(params) : ReturnType = defaultImpl,
  function fn2(params) : ReturnType = defaultImpl
};
```

### Example

```
structure Eq(a:Type) = {
  function (==)(x, y:a) : Bool = not(x != y),
  function (!=)(x, y:a) : Bool = not(x == y)
};
```

This defines an `Eq` structure with two functions: `==` and `!=`. Each provides a default implementation in terms of the other, so instances only need to implement one of them.

### How Structures Work

When a structure is declared, its functions are transformed into functions with **implicit parameters**. For example, the `(==)` function from `Eq` becomes:

```
(==) [a:Type] (x:a, y:a) : Bool
```

The `[a:Type]` is an implicit parameter that gets resolved at compile time based on the types of the arguments. This means type-dependent dispatch happens during compilation, not at runtime.

---

## Instance Declarations

Instances provide implementations of structure functions for specific types.

### Syntax

```
instance StructureName(TypeArgs) = {
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

FOOL3 provides several primitive operations, identified by the `#` suffix:

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

## Standard Library (base.fool)

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

### Eq Structure

The `Eq` structure provides overloaded equality:

```
structure Eq(a:Type) = {
  function (==)(x, y:a) : Bool = not(x != y),
  function (!=)(x, y:a) : Bool = not(x == y)
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

`type`, `function`, `if`, `then`, `else`, `in`, `action`, `structure`, `instance`, `let`, `case`, `of`, `where`, `exists`, `forall`, `record`

The Unicode symbols `∃` and `∀` are also reserved (for future quantifier support).

### Reserved Operators

`;` `=` `,` `.` `..` `:` `->` `=>` `|` `?` `<:` `\`

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
| Record | `record Name = { field:Type };` |
| Record spread | `record Name = { ..Other, field:Type };` |
| Parameterized record | `record Name(a:Type) = { field:a };` |
| Function | `function name(args) : Type = body;` |
| Pattern match | `function f(x) = { {pat} -> expr, ... };` |
| Operator function | `function (+)(x, y) = ...;` |
| If/then/else | `if cond then e1 else e2` |
| Let/in | `let x = e1, y = e2 in body` |
| Structure | `structure Name(a:Type) = { functions };` |
| Instance | `instance Name(Type) = { functions };` |
| Action | `action name = { stmts };` |
| Application | `f(x, y)` |
| List | `[1, 2, 3]` |
| Tuple | `{1, 2, 3}` |
| Vector | `<1, 2, 3>` |
| Comment | `// line` or `/* block */` |
