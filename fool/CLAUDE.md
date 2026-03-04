# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tulam is a type-theory based functional language compiler/interpreter written in Haskell. It targets JavaScript, .NET, and potentially x86 native. The language is built on two primitives: **tuples** and **lambdas** — everything else (sum types, product types, structures/typeclasses) is derived from these.

## Build & Run

```bash
stack build              # Build
stack exec tulam         # Run the REPL (loads base.tl automatically)
stack test               # Run tests (test/Spec.hs)
stack clean              # Clean build artifacts
```

The project uses Stack with Hpack (`package.yaml` generates `tulam.cabal`). Edit `package.yaml` for dependency changes, not `tulam.cabal` directly.

## REPL Commands

Once in the REPL: `:load <file>`, `:list types`, `:list functions`, `:env`, `:all`, `:clm`, `:quit`.

## Architecture

### Compilation Pipeline (6 passes)

```
Source (.tl) → Lexer/Parser → Surface AST (Expr/Lambda)
  → Pass 1: Environment Building (extract types, constructors, top-level lambdas)
  → Pass 2: Case Optimization (beta reduction, pattern expansion)
  → Pass 3: CLM Conversion (Surface AST → Core List Machine IR)
  → Pass 4: [Reserved] Type Checking
  → Pass 5: Code Generation (target-specific output)
```

### Key Modules

| Module | Role |
|--------|------|
| `src/Surface.hs` | Surface AST: `Expr`, `Lambda`, `Var`, `ConsTag` types. Core data structures for the entire compiler. |
| `src/Parser.hs` | Parsec-based parser. Grammar rules for sum types, records, functions, structures. |
| `src/Lexer.hs` | Tokenizer. Reserved keywords, operators, identifier rules. |
| `src/Pipeline.hs` | All compilation passes. Environment building, case optimization, CLM conversion. |
| `src/CLM.hs` | Core List Machine IR. Simply-typed intermediate form using explicit n-lists. Beta reduction, lambda application. |
| `src/State.hs` | Interpreter state and monad stack. Environment maps (types, constructors, topLambdas, clmLambdas). |
| `src/Interpreter.hs` | CLM evaluator. Pattern matching resolution, expression evaluation. |
| `src/Logs.hs` | Error handling with source location tracking. |
| `app/Main.hs` | REPL loop (Haskeline), file loading, interactive command dispatch. |

### Monad Stack

```haskell
type InputTState = InputT IntState           -- Haskeline layer
type IntState = StateT InterpreterState LambdaLoggerMonad  -- State layer
type LambdaLoggerMonad = LoggerMonadIO LogPayload          -- Logging layer
```

### Key Design Decisions

- **Constructor tags**: Integer tags on tuples for discriminated union pattern matching (needed for .NET/JS targets, unlike GHC's tagless approach).
- **Implicit parameters**: Structure (typeclass) functions generate implicit-parameter functions, e.g. `(+) [a:Type] (x:a,y:a):a`. Instance declarations expand the case body.
- **Type-dependent functions execute at compile time only**: Only value-dependent simply-typed functions survive to runtime; type-dependent dispatch is resolved during compilation.
- **Expression traversal**: `traverseExpr` (pure) and `traverseExprM` (monadic/stateful) for AST transformations throughout passes.
- **Universe hierarchy**: `U 0` = `Type` (types of values), `U 1` = `Type1` (kinds / type constructors), `U 2` = `Type2`, etc. `pattern Type = U 0` provides backward compatibility. Levels are carried but not yet enforced.
- **Categorical structure vocabulary** (planned): `algebra` (single-type structure, e.g. Monoid), `morphism` (multi-type structure, e.g. Convertible), with `functor` and `natural` planned for when HKT support lands. Monad, Category, Arrow are standard library structures, not keywords. See `doc/CategoricalDesign.md` for full design.

### Design Documents

- `doc/CategoricalDesign.md` — Categorical type system design (algebras, morphisms, functors, monads, categories, arrows)
- `doc/ImplementationPlan.md` — 9-phase incremental implementation roadmap from current state to full categorical type system

### Data Files

- `base.tl` — Standard library (Nat, Bool, Eq structure). Loaded automatically by the REPL.
- `parsertests.tl` — Parser test cases.

## Language Conventions

- Type and constructor names must start with a capital letter.
- `#` suffix denotes built-in/primitive operations (e.g., `print#`, `concat#`).
- Semicolons terminate top-level declarations. Pattern match cases use commas.
- `structure` = typeclasses, `instance` = typeclass instances, `record` = single-constructor product type sugar.
