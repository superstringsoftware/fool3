# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**hsdotnet** is a Haskell-to-.NET compiler that uses GHC as a frontend to compile Haskell source code down to STG (Spineless Tagless G-machine) intermediate representation, then converts STG into .NET code (C# and eventually IL). It leverages GHC's own pipeline (parsing, typechecking, desugaring, Core, STG) and then takes over at the STG level.

## Build and Run

This is a Stack-based Haskell project (resolver: lts-22.44, GHC 9.6.7).

```bash
stack build                    # Build the project
stack exec hsdotnet-exe        # Run (also: ./run)
stack test                     # Run tests (currently a placeholder)
stack ghc -- -fplugin Plugin Example.hs  # Run via GHC plugin mode (also: ./runPlugin)
```

## Architecture

### Compilation Pipeline

1. **GHC Frontend** (`app/Main.hs`): Uses GHC API to parse, typecheck, desugar, and compile Haskell source (`Example.hs`) through Core and STG passes. Applies optimization flags (StgCSE, EtaReduction, CallArity). Uses `GHC.Driver.Config.*` modules to construct pipeline configs (`CorePrepConfig`, `CoreToStgOpts`, `StgPipelineOpts`).

2. **STG → Intermediate Representation** (`src/Compiler.hs`): Converts GHC's STG types into simplified `DotNetObj` / `DotNetExpr` types. Key types:
   - `DotNetObj`: Heap objects — `FUN` (reentrant closures), `THUNK` (updatable), `ONCE` (single-entry), `CON` (saturated constructor application)
   - `DotNetExpr`: Expressions — `FUNCALL`, `CONCALL`, `PRIMOP`, `LET`, `LETREC`, `CASE`/`CASEDEFAULT`/`CASESIMPLE`
   - `CompilerState` / `SM`: State monad tracking indentation, free variable context, and scope
   - `BareStgProgram`: Simplified STG stripped of top-level literals and helper type constructors (KindRep, TrName, TyCon)
   - Uses `CgStgTopBinding` (CodeGen pass) types from `stg2stg` output. Free vars extracted from `DIdSet` extension field via `dVarSetElems`.

3. **C# Code Generation** (`src/CSharpGen.hs`): Converts `DotNetObj`/`DotNetExpr` into C# text via `CSharpable` typeclass. Handles free variable lookups (converting names to `__freeVars[N]` indices), illegal character escaping for C# identifiers, and literal conversion.

4. **GHC Plugin Mode** (`src/Plugin.hs`): Alternative entry point that hooks into GHC's compilation as a Core-to-Core plugin pass, accessing the same STG pipeline.

### .NET Runtime System (`rts/`)

- `rts/rts/rts/STG.cs`: Core runtime — `CLOSURE` base class with `ENTER`/`EVAL` semantics, `FUN` (function closures with arity), `PAP` (partial application), `CON` (constructors with tag), `CONPRIM<A>` (primitive type constructors), `THUNK` (suspended computations with memoization). Follows Eval/Apply paper semantics.
- `rts/rts/rts/FUN.cs`: Earlier generic-typed FUN/PAP experiment (commented out).
- `rts/test-apps/`: C# test harness for the runtime.

### Base Library (`baselib/`)

Minimal GHC base library stubs (`GHC.Base`, `GHC.Types`, `GHC.Prim`, `GHC.Magic`) for bootstrapping compilation. Types, CString, Magic, and Prim are NOT compiled — their functions are defined in the RTS instead.

## Key Patterns

- The compiler uses GHC 9.6.x API imports with the `GHC.*` namespace (e.g., `GHC.Stg.Syntax`, `GHC.Core`, `GHC.Core.TyCon`, `GHC.Core.DataCon`, `GHC.Types.Var`). These are internal GHC modules, not the stable GHC API.
- `showGhc` (using `showPprUnsafe` from `GHC.Driver.Ppr`) is the primary way to render GHC types as strings throughout the codebase.
- STG types use the Trees That Grow pattern with pass-indexed types (`CgStgTopBinding` = `GenStgTopBinding 'CodeGen`). Case alts are records (`GenStgAlt{alt_con, alt_bndrs, alt_rhs}`) not tuples.
- For STG pretty-printing, use `pprStgTopBindings` from `GHC.Stg.Syntax` with `StgPprOpts` — direct `Outputable` instances are not available for STG pass-indexed types.
- Free variable tracking in code generation uses index-based lookup (`ifind` from `ilist` package) to map variable names to `__freeVars[i]` positions in generated C# closures.
- `Example.hs` is the test input file — it contains many commented-out test programs for different compilation scenarios (primitives, typeclasses, ADTs, lists, etc.).
