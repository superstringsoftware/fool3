# Architecture of the compiler

We need a better (less coupled) way of handling things. This means that we need to decouple the Parser from the overall compiler state, as now they are closely intertwined. We want to have roughly the following:

[Source File] -> [Parser] -> [List of Surface Expressions]

Then we'll take the list of Surface Expressions and process them further, converting into Core, add functions to the internal environment etc. This means Parser should not depend on IO at all!

This will also allow us to plug in different surface languages that compile to the same core much more easily! So, the idea is:

1) Surface Language File -> Parser (Surface Language) -> [Surface Expr]
2) [Surface Expr] -> [Core]

Steps 1&2 are plugins for surface languages!

3) Core optimization passes:
    - Renaming, z-lifting, bind analysis
    - Build the environment
    - Typecheck in the current context
    - Optimize (different passes and technics)

4) Convert to explicitly typed record-based language (similar to STG for Haskell, yet to be defined)

5) Code generation from this into different Realms - also via plugins!