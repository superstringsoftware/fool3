# tulam Implementation Plan

Incremental roadmap from current state to categorical type system. Each step is testable independently and unlocks the next.

---

## Current State Summary (Updated after Phase 1 completion)

**What works:** Parsing sum types, functions with pattern matching, structures (typeclasses), instance declarations, actions. Environment building extracts types/constructors/lambdas and stores instance-specialized functions. Case optimization expands patterns into constructor tag checks (for both top-level and instance functions). CLM conversion produces simply-typed IR including instance CLM functions. Interpreter evaluates: function application, constructor matching, field access, literals, action sequences, pattern match cases, and **implicit parameter dispatch** (CLMIAP) -- resolving type from constructor tags and dispatching to instance-specific implementations.

**Phase 1 COMPLETE.** Structures and instances work end-to-end. `Z == Z` dispatches through `instance Eq(Nat)` correctly.

**Remaining gaps:**
1. No `extends` for structure inheritance
2. No `if/then/else`, `let/in`, `where` -- basic language constructs reserved but not parsed
3. No `algebra`/`morphism` categorical keywords
4. No higher-kinded types (needed for Functor, Monad)
5. No type checker

---

## Phase 1: Make Structures Actually Work

**Goal:** `structure Eq(a:Type)` + `instance Eq(Nat)` produces a working `==` function that dispatches on type.

This is the single most important missing piece. Everything in the categorical design depends on structures working end-to-end.

### Step 1.1: Parse `instance` declarations

**Files:** Lexer.hs, Parser.hs

`instance` is not currently a reserved word. Add it to the lexer, then add a parser rule:

```tulam
instance Eq(Nat) = {
    function ==(x:Nat, y:Nat) : Bool = eq(x,y),
    function !=(x:Nat, y:Nat) : Bool = not(eq(x,y))
};
```

Parser produces a new Expr variant or reuses Structure with a flag. Simplest: add `Instance Lambda [Expr]` to Expr -- the Lambda carries the structure name + type args, the [Expr] carries the function implementations.

**Surface.hs changes:** Add `| Instance Lambda [Expr]` to Expr. Add to traverseExpr, ppr.

**Test:** Parse `instance Eq(Nat) = { ... };` and see it in `:all` output.

### Step 1.2: Process `instance` in environment building (Pass 1)

**Files:** Pipeline.hs, State.hs

When we encounter `instance Eq(Nat)`:
1. Look up the structure `Eq` in the environment
2. Look up the implicit-parameter function `==` that was created from the structure
3. Add a new case to its PatternMatches body: `CaseOf [Var "a" Type (Id "Nat")] (Function implFunction)`
4. Update the lambda in the environment

This is the mechanism already sketched in the structure processing code -- `fixStr` in Pipeline.hs creates the initial `CaseOf [] (Function l)` with empty cases. Instance processing *adds* concrete cases.

**Test:** Load base.tl with uncommented `instance Eq(Nat)`. Check that `==` function in `:list functions` now has a `{Nat} -> ...` case alongside the default.

### Step 1.3: Complete interpreter evaluation

**Files:** Interpreter.hs

Fill in the missing cases in `evalCLM`:
- `CLMLIT` -- return the literal as-is
- `CLMPROG` -- evaluate expressions in sequence, return last
- `CLMCASE` -- evaluate constructor tag checks, return body if all true
- `CLMIAP` -- resolve implicit type parameter, look up the specialized function, apply

Start with CLMLIT and CLMPROG (trivial), then CLMCASE (needed for pattern matching to work in the evaluator), then CLMIAP (needed for structure dispatch).

**Test:** In REPL, evaluate `not(True)` and get `False`. Evaluate `plus(Succ(Z), Succ(Z))` and get `Succ(Succ(Z))`. Then with instances working, evaluate `==(Z, Z)` and get `True`.

### Step 1.4: Uncomment and fix base.tl instances

**Files:** base.tl

Uncomment the `instance Eq(Nat)` and `instance Eq(Bool)` declarations. Verify they load and work.

Add a simple test:
```tulam
instance Eq(Nat) = {
    function ==(x:Nat, y:Nat) : Bool = eq(x,y)
};
```

**Test:** `:load base.tl` succeeds, `==(Z, Z)` evaluates to `True`, `==(Z, Succ(Z))` evaluates to `False`.

**Milestone: Structures work end-to-end.** This is the foundation for everything that follows.

### Phase 1 Learnings & Implementation Notes

**Completed.** Key decisions and discoveries:

1. **Instance storage**: Instance functions are stored separately from top-level lambdas in `instanceLambdas` (Surface) and `clmInstances` (CLM) maps, keyed by `"funcName\0typeName"`. This avoids fighting with the CaseOf/PatternMatches mechanism which is designed for value-level constructor matching, not type-level dispatch.

2. **Type inference from values**: CLMIAP dispatch infers the type from constructor tags on the evaluated arguments. `CLMCON (ConsTag "Z" 0) _` -> look up "Z" in constructors -> get `lamType` -> extract type name "Nat". Also handles literal types (Int, Float, String, Char).

3. **Case optimization scope**: Instance lambda functions with pattern matching (like `instance Eq(Bool)`) MUST go through `caseOptimizationPass` (Pass 2). Initially missed this -- instance functions were stored but their patterns weren't expanded, causing runtime errors.

4. **Interactive desugaring**: The `afterparse` pass (BinaryOp->App desugaring) only runs on file loads. Interactive REPL expressions need manual desugaring in `processInteractive`. Note: `traverseExpr f` transforms children but NOT the root node -- must apply `f` to root separately.

5. **What the plan got right**: The step ordering was correct. Instance parsing -> processing -> interpreter -> base.tl was the right sequence. The "instance processing adds cases to PatternMatches" approach from the original plan was wrong though -- direct instance maps turned out to be cleaner.

6. **What to watch for next**: The implicit-param function `== [a:Type]` still exists with its default body. Currently the default path (no instance found) falls through to the general function's `PatternMatches` body, which has only a default case that returns another function. This works but is fragile. Future: type checker should validate instance coverage.

---

## Phase 2: Basic Language Completeness

**Goal:** Fill in the missing basic constructs so the language is usable for non-trivial programs.

See also: `doc/RecordDesign.md` for full record system design rationale.

### Step 2.1: Parse and desugar `if/then/else`

**Files:** Surface.hs, Parser.hs, Pipeline.hs

Add `IfThenElse Expr Expr Expr` to the `Expr` type. This keeps parsing clean and puts desugar logic in the pipeline where it belongs.

**Surface.hs:**
- Add `| IfThenElse Expr Expr Expr` to `Expr`
- Add `traverseExpr f (IfThenElse c t e) = IfThenElse (f c) (f t) (f e)`
- Add `ppr (IfThenElse c t e) = "if " ++ ppr c ++ " then " ++ ppr t ++ " else " ++ ppr e`

**Parser.hs:**
- Add `pIfThenElse` parser using existing reserved words `if`, `then`, `else`
- Add `try pIfThenElse` to `pFactor` before `pApp`

```haskell
pIfThenElse :: Parser Expr
pIfThenElse = do
    reserved "if"
    cond <- pExpr
    reserved "then"
    e1 <- pExpr
    reserved "else"
    e2 <- pExpr
    return $ IfThenElse cond e1 e2
```

**Pipeline.hs — afterparse desugaring:**
```haskell
afterparse (IfThenElse cond thenE elseE) =
    App (Function (Lambda "" []
        (PatternMatches
            [ CaseOf [Var "" (Id "Bool") (Id "True")]  thenE defaultSI
            , CaseOf [Var "" (Id "Bool") (Id "False")] elseE defaultSI
            ]) EMPTY)) [cond]
```

The desugared form feeds into existing Pass 2 case optimization, which expands it into constructor tag checks. No CLM or interpreter changes needed.

**Test:**
```
> if True then Succ(Z) else Z
Succ(Z)
> if False then Succ(Z) else Z
Z
```

### Step 2.2: Parse `let/in` expressions

**Files:** Surface.hs, Parser.hs, Pipeline.hs

Add `LetIn [(Var, Expr)] Expr` to `Expr` — list of `(variable, value)` bindings and a body expression.

**Surface.hs:**
- Add `| LetIn [(Var, Expr)] Expr`
- Add traverseExpr: apply f to binding values and body
- Add ppr

**Parser.hs:**
```haskell
pLetIn :: Parser Expr
pLetIn = do
    reserved "let"
    bindings <- sepBy1 pLetBinding (reservedOp ",")
    reserved "in"
    body <- pExpr
    return $ LetIn bindings body

pLetBinding :: Parser (Var, Expr)
pLetBinding = do
    nm <- identifier
    tp <- optionMaybe (reservedOp ":" >> concreteType)
    reservedOp "="
    val <- pExpr
    return (Var nm (maybe UNDEFINED id tp) UNDEFINED, val)
```

Add `try pLetIn` to `pFactor` before `pApp`.

**Pipeline.hs — afterparse desugaring:**
```haskell
-- let x = e1, y = e2 in body  =>  (\x -> (\y -> body)(e2))(e1)
afterparse (LetIn [(v, val)] body) =
    App (Function (Lambda "" [v] body EMPTY)) [val]
afterparse (LetIn ((v,val):rest) body) =
    App (Function (Lambda "" [v] (LetIn rest body) EMPTY)) [val]
```

Nested lets desugar to nested lambda applications. The recursive case gets caught by the next afterparse traversal.

**Test:**
```
> let one = Succ(Z) in plus(one, one)
Succ(Succ(Z))
> let a = Succ(Z), b = Succ(Succ(Z)) in plus(a, b)
Succ(Succ(Succ(Z)))
```

### Step 2.3: Named records with functions as fields

**Files:** Lexer.hs, Parser.hs

Records are products with named fields. They can contain any values, including functions. They desugar to single-constructor sum types, so the existing pipeline handles them with no changes.

**Lexer.hs:** Add `"record"` to reserved words.

**Parser.hs:**
```haskell
pRecord :: Parser Expr
pRecord = do
    reserved "record"
    name <- identifier
    -- optional type parameters
    tparams <- optionMaybe (parens (sepBy1 pTypeParam (reservedOp ",")))
    reservedOp "="
    fields <- braces (sepBy1 pRecordField (reservedOp ","))
    -- Desugar: record Foo = { x:A, y:B }  =>  type Foo = { Foo(x:A, y:B) }
    let vars = map fieldToVar fields
    let consLam = Lambda name vars EMPTY (Id name)
    let tpVars = maybe [] id tparams
    return $ SumType (Lambda name tpVars (Tuple [Function consLam]) (U 0))

pRecordField :: Parser (Name, Expr)
pRecordField = do
    nm <- identifier
    reservedOp ":"
    tp <- concreteType
    return (nm, tp)

fieldToVar :: (Name, Expr) -> Var
fieldToVar (nm, tp) = Var nm tp UNDEFINED
```

Add `try pRecord` to `pDef` before `pSumType`.

**Test:**
```tulam
record Point = { x:Nat, y:Nat };
record Pair(a:Type, b:Type) = { fst:a, snd:b };
```
```
> :load test.tl
> Point(Z, Succ(Z))
Point(Z, Succ(Z))
> let p = Point(Succ(Z), Z) in p.x
Succ(Z)
```

### Step 2.4: Record spread in declarations

**Files:** Parser.hs, Pipeline.hs

Allow `..Name` in record field lists to include all fields from another record.

**Parser.hs:** In `pRecordField`, handle the `..` prefix:
```haskell
pRecordField :: Parser (Either Name (Name, Expr))
pRecordField =
    try (reservedOp ".." >> identifier >>= return . Left)   -- spread
    <|> (do nm <- identifier; reservedOp ":"; tp <- concreteType; return $ Right (nm, tp))
```

**Pipeline.hs — resolve spread in Pass 0 or afterparse:**
When a spread `..Point` is encountered, look up `Point` in the parsed types, extract its constructor fields, and splice them into the field list. Duplicate field names from later in the list override spread fields.

**Test:**
```tulam
record Point = { x:Nat, y:Nat };
record Point3D = { ..Point, z:Nat };
// expands to: type Point3D = { Point3D(x:Nat, y:Nat, z:Nat) }
```

### Step 2.5: Named record construction syntax

**Files:** Parser.hs

Allow `Name { field = value, ... }` as an alternative to positional `Name(value, ...)`.

**Parser.hs:**
```haskell
pNamedConstruction :: Parser Expr
pNamedConstruction = do
    nm <- identifier
    fields <- braces (sepBy1 pFieldAssign (reservedOp ","))
    -- reorder fields to match declaration order, produce positional App
    return $ NamedRecord nm fields

pFieldAssign :: Parser (Name, Expr)
pFieldAssign = do
    nm <- identifier
    reservedOp "="
    val <- pExpr
    return (nm, val)
```

This needs a new `NamedRecord Name [(Name, Expr)]` constructor in Surface.hs that desugars in Pass 0/afterparse to a positional constructor call after looking up the field order from the environment.

Alternative: defer named construction to Phase 3 and only support positional `Point(Z, Z)` in Phase 2. Named construction requires environment access during desugaring which adds complexity.

**Test:**
```
> Point { x = Z, y = Succ(Z) }
Point(Z, Succ(Z))
> Point { y = Succ(Z), x = Z }    // order doesn't matter
Point(Z, Succ(Z))
```

### Step 2.6: Record update syntax

**Files:** Parser.hs, Pipeline.hs

Allow `expr { field = newValue }` to create a copy with fields changed.

**Parser.hs:** Parse `pExpr { field = val, ... }` as `RecordUpdate Expr [(Name, Expr)]`.

**Pipeline.hs — desugar:** Given record type info, expand to positional constructor call:
```tulam
p { x = Succ(Z) }
// desugars to:
Point(Succ(Z), p.y)   // changed fields from update, unchanged from field access
```

**Note:** This requires knowing the record's type to enumerate fields. Options:
- Require type annotation: `(p:Point) { x = Succ(Z) }`
- Infer from context (needs type checker)
- Defer to Phase 4+ when we have basic type inference

**Recommendation:** Defer record update to a later phase. It needs type info to know which fields to copy. Steps 2.1-2.4 give us records that work; update is convenience sugar.

### Phase 2 ordering and dependencies

```
Step 2.1 (if/then/else)        <- independent, simple desugar
Step 2.2 (let/in)              <- independent, simple desugar
Step 2.3 (named records)       <- independent, desugars to sum types
Step 2.4 (record spread)       <- depends on 2.3
Step 2.5 (named construction)  <- depends on 2.3, needs environment access
Step 2.6 (record update)       <- DEFER to later phase (needs type info)
```

Recommended order: **2.1 → 2.2 → 2.3 → 2.4 → 2.5(optional)**

Steps 2.1 and 2.2 are quick wins. Step 2.3 is the core record feature. Step 2.4 (spread) is valuable for record extension. Step 2.5 (named construction) can be deferred if environment access during desugaring proves complex. Step 2.6 (update) is deferred.

**Milestone: Language has conditionals, local bindings, records with functions-as-fields, and record extension. Usable for non-trivial programs.**

---

## Phase 3: Structure Inheritance (`extends`)

**Goal:** `algebra Ord(a) extends Eq(a)` inherits all Eq methods and can add new ones.

### Step 3.1: Add `extends` to parser

**Files:** Lexer.hs, Parser.hs

Add `extends` as reserved word. Modify structure/algebra parsing:

```tulam
structure Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function <(x:a, y:a) : Bool = ...
};
```

Parser stores the parent reference in the Structure/Lambda representation. Options:
- Add a field to `Lambda`: `lamExtends :: [Expr]`
- Or store it in a wrapper: `Structure Lambda [Name] [Expr]` -- third field is parent structures

### Step 3.2: Process inheritance in Pass 1

**Files:** Pipeline.hs

When building environment for a structure with `extends`:
1. Look up parent structure
2. Copy all parent functions into child (with the child's type parameters)
3. Allow child to override parent functions
4. When instantiating child, automatically satisfy parent constraint

### Step 3.3: Instance inheritance

When `instance Ord(Nat)` is declared, the compiler should also require/verify `instance Eq(Nat)` exists (or generate an error).

**Test:**
```tulam
structure Eq(a:Type) = { function ==(x:a,y:a):Bool };
structure Ord(a:Type) extends Eq(a) = { function <(x:a,y:a):Bool };
instance Eq(Nat) = { ... };
instance Ord(Nat) = { ... };
-- Ord(Nat) instance inherits == from Eq(Nat)
```

**Milestone: Structure inheritance works. Foundation for the algebra hierarchy (Semigroup -> Monoid -> Group).**

---

## Phase 4: Categorical Keywords (`algebra`, `morphism`)

**Goal:** `algebra` and `morphism` work as refined synonyms for `structure` with additional compiler knowledge.

### Step 4.1: Add keywords

**Files:** Lexer.hs, Parser.hs, Surface.hs

Add `algebra` and `morphism` to reserved words. Parse them identically to `structure` but tag the resulting Expr differently. Options:
- Add `| Algebra Lambda [Name]` and `| Morphism Lambda [Name]` to Expr
- Or add a classification field to Structure: `Structure Lambda [Name] StructKind` where `data StructKind = SGeneral | SAlgebra | SMorphism`

The second approach is simpler and avoids duplicating code.

### Step 4.2: Validate parameter counts

In Pass 1, when processing an algebra, verify exactly one type parameter. When processing a morphism, verify two or more type parameters. Emit warnings (not errors) if violated -- the categorical classification is advisory.

### Step 4.3: Automatic morphism composition

When two morphism instances exist -- `Convertible(A,B)` and `Convertible(B,C)` -- the compiler can automatically derive `Convertible(A,C)`. This is an environment-building step in Pass 1.

**Test:**
```tulam
morphism Convertible(a:Type, b:Type) = { function convert(x:a):b };
instance Convertible(Nat, Bool) = { function convert(x) = not(eq(x,Z)) };
instance Convertible(Bool, Nat) = { function convert(x) = { {True} -> Succ(Z), {False} -> Z } };
-- compiler could derive Convertible(Nat, Nat) via Bool (optional, can defer)
```

**Milestone: Categorical vocabulary exists in the surface language. Structures, algebras, and morphisms are distinguished.**

---

## Phase 5: Higher-Kinded Types

**Goal:** Type constructors (`Maybe`, `List`) can be passed as parameters to structures. Required for Functor, Monad, etc.

### Step 5.1: Parse type-constructor parameters

**Files:** Parser.hs

Currently `a:Type` parses the type as `U 0`. We need `f:Type1` to mean "f is a type constructor" (a function from Type to Type).

```tulam
algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b)
};
```

The parser already handles `Type1` via `parseUniverse`. The new part is parsing `f(a)` in type positions -- type-level application. This requires `concreteType` to handle `TypeApp`:

```haskell
-- in type positions, allow: Name(Type, Type, ...)
concreteType = try parseUniverse
           <|> try typeApp     -- NEW: Maybe(a), f(a), etc.
           <|> (Id <$> identifier)

typeApp = do
    nm <- identifier
    args <- parens (sepBy concreteType (reservedOp ","))
    return $ App (Id nm) args
```

### Step 5.2: Represent HKT in the environment

**Files:** State.hs, Pipeline.hs

When `Functor(f:Type1)` is declared, `f` is a type variable of kind `Type -> Type`. The implicit parameter function generated is:

```
fmap [f:Type1] (g: a -> b, x: f(a)) : f(b)
```

When `instance Functor(Maybe)` is declared, `f` is instantiated to `Maybe`, and the case is added.

### Step 5.3: Type application in CLM

**Files:** CLM.hs, Pipeline.hs

Type-level application (`f(a)` where `f` is a type variable) needs representation in CLM for the transition period before type erasure. This may just be `CLMAPP` applied to type arguments, which get erased later.

**Test:**
```tulam
algebra Functor(f:Type1) = { function fmap(g: a -> b, x:f(a)) : f(b) };
instance Functor(Maybe) = {
    function fmap(g, x) = { {g, Nothing} -> Nothing, {g, Just(v)} -> Just(g(v)) }
};
```

**Milestone: Higher-kinded types work. Functor can be defined and instantiated.**

---

## Phase 6: Arrow Types and Function Type Syntax

**Goal:** Parse `a -> b` as a type, required for Functor's `fmap` signature and for expressing function types in general.

### Step 6.1: Parse arrow types

**Files:** Parser.hs

In type positions, `a -> b` should parse to a function type. This is the standard Pi type (non-dependent case):

```tulam
function apply(f: Nat -> Bool, x:Nat) : Bool = f(x);
```

Parser needs `concreteType` to handle `->` as an infix type operator.

### Step 6.2: Represent function types

**Files:** Surface.hs

Function types are already representable as `Function (Lambda "" [Var "x" A UNDEFINED] UNDEFINED B)` but that's verbose. We may want a dedicated `ArrowType Expr Expr` constructor or just use `App (Id "->") [A, B]` as a convention.

**Test:** `function compose(f: b -> c, g: a -> b) : a -> c = ...;`

**Milestone: Function types expressible in type positions. Functor's fmap signature is parseable.**

---

## Phase 7: Do-Notation / Action Desugaring

**Goal:** `action` bodies desugar into monadic `bind` chains when a `Monad` instance is in scope.

### Step 7.1: Parse `<-` in action bodies

**Files:** Parser.hs

```tulam
action main : IO(Unit) = {
    name <- readLine(),        -- bind
    greeting = concat#("Hello, ", name),  -- let
    putStrLn(greeting)         -- then (bind with ignored result)
};
```

`<-` binds the result of a monadic computation. `=` is a local let. Plain expressions are sequenced.

### Step 7.2: Desugar to bind chains

**Files:** Pipeline.hs (Pass 0 or new pass)

```tulam
-- name <- readLine()   =>  bind(readLine(), \name -> ...)
-- greeting = expr      =>  let greeting = expr in ...
-- putStrLn(greeting)   =>  bind(putStrLn(greeting), \_ -> pure(Unit))
```

This is a straightforward syntactic transformation. The key decision is whether to desugar in the parser (immediate) or in a pipeline pass (deferred). Deferred is better because it can use environment information to verify Monad instances exist.

### Step 7.3: Define Monad in base.tl

```tulam
algebra Functor(f:Type1) = { function fmap(g: a -> b, x:f(a)) : f(b) };
algebra Applicative(f:Type1) extends Functor(f) = {
    function pure(x:a) : f(a),
    function ap(ff:f(a -> b), fa:f(a)) : f(b)
};
algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f:a -> m(b)) : m(b)
};
```

**Test:** Define `Maybe` monad, write an action that chains Maybe computations, verify it works.

**Milestone: Monadic programming works. Actions desugar into bind chains.**

---

## Phase 8: Standard Library and Category/Arrow

**Goal:** Rich standard library with categorical structures.

### Step 8.1: Core algebras

```tulam
algebra Semigroup(a:Type) = { function combine(x:a, y:a) : a };
algebra Monoid(a:Type) extends Semigroup(a) = { function empty() : a };
algebra Group(a:Type) extends Monoid(a) = { function inverse(x:a) : a };
```

### Step 8.2: Category and Arrow as library structures

```tulam
algebra Category(arr:Type2) = {
    function id() : arr(a, a),
    function compose(f:arr(b,c), g:arr(a,b)) : arr(a,c)
};

algebra Arrow(arr:Type2) extends Category(arr) = {
    function arr(f: a -> b) : arr(a, b),
    function first(f:arr(a,b)) : arr({a,c}, {b,c})
};
```

### Step 8.3: Kleisli derivation

When `Monad(m)` is instantiated, automatically derive `Category(Kleisli(m))` where `Kleisli(m,a,b) = a -> m(b)`.

### Step 8.4: Natural transformations

Add `natural` keyword for parametrically polymorphic functor morphisms:

```tulam
natural safeHead : List ~> Maybe = {
    function transform(xs:List(a)) : Maybe(a) = { ... }
};
```

**Milestone: Full categorical vocabulary available. Standard library mirrors the design document.**

---

## Phase 9: Type Checker (Pass 4)

**Goal:** Actually enforce types, universe levels, and structure laws.

### Step 9.1: Basic bidirectional type checking

Implement type synthesis (infer) and type checking (check) for:
- Literals (Int, Float, String, etc.)
- Variables (look up in environment)
- Function application (check argument types match parameter types)
- Constructor application (check arity and field types)

### Step 9.2: Universe level checking

Verify that `U n` values only appear in positions where `U (n+1)` is expected. Enforce cumulativity: `U n` is a subtype of `U (n+1)`.

### Step 9.3: Structure constraint checking

When a function requires `Eq(a)`, verify that an instance exists for the concrete type being used.

### Step 9.4: Implicit parameter resolution

Given `==(x:Nat, y:Nat)`, automatically resolve the implicit parameter `[a:Type]` to `Nat` by unifying parameter types.

**Milestone: Type errors are caught at compile time. Universe levels enforced.**

---

## Dependency Graph

```
Phase 1: Structures Work          (CRITICAL PATH - everything depends on this)
    |
    v
Phase 2: Language Completeness    (if/else, let/in, records - quality of life)
    |
    v
Phase 3: extends                  (structure inheritance)
    |
    v
Phase 4: algebra/morphism         (categorical keywords - depends on extends)
    |
    v
Phase 5: Higher-Kinded Types      (f:Type1 - depends on universe hierarchy)
    |
    +---> Phase 6: Arrow Types    (a -> b in type positions - depends on HKT)
    |         |
    |         v
    +---> Phase 7: Do-Notation    (depends on HKT + arrow types for Monad def)
              |
              v
          Phase 8: Std Library    (Category, Arrow, Functor, Monad instances)
              |
              v
          Phase 9: Type Checker   (can be started in parallel from Phase 3 onward)
```

Phase 2 can run in parallel with Phase 1 (different files, no dependencies). Phase 9 can start as early as Phase 3 -- basic type checking doesn't need HKT.

---

## Effort Estimates

| Phase | Complexity | Key Challenge |
|-------|-----------|--------------|
| 1 | Medium-High | Instance processing and CLMIAP evaluation are the hardest parts |
| 2 | Low-Medium | Straightforward parser additions + desugaring |
| 3 | Medium | Inheritance resolution in environment building |
| 4 | Low | Mostly lexer/parser sugar + validation |
| 5 | High | HKT requires rethinking type representation and application |
| 6 | Medium | Arrow type parsing interacts with expression parsing |
| 7 | Medium | Desugaring is mechanical but needs correct scoping |
| 8 | Low-Medium | Library code, but tests completeness of all prior phases |
| 9 | Very High | Type checking is the largest single feature |
