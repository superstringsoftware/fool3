# Architecture and inner workings of the compiler

Overall design follows this logic:

[src/SurfaceLanguage](../../src/SurfaceLanguage) - different syntaxis of the surface languages we may want to support: parser, lexer etc.
[src/Core](../../src/Core) - Core syntaxis, with `Expr` being straightforward language ALL parsers should parse into (or at least get there reasonably fast in a conversion), optimized `ExprCore` data, into which all programs eventually convert after several optimization passes.
[src/Realms](../../src/Realms) - different compilation targets, with code generators for different languages from `ExprCore` and other data.

We want to be human-friendly, which means tips and error messages. For this purpose, we need to pass SourceInfo from the parser at every conversion stage so that it's attached to the expressions we are analyzing, as well as add verbose Annotation information where relevant to help fixing bugs and do **type-driven development**.

So, the logic is:

- Parser -> [optional]Surface Lang Expr -> Expr -- we probably need to rewrite Expr to include SourceInfo properly and add SourceInfo while parsing
- Optimization passes on Expr and building up typing environment
- Typechecking
- Conversion to ExprCore
- Optimization passes on ExprCore
- Codegen 