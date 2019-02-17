# some ongoing thoughts on implementation

So, our parser needs to be rewritten in the compiler state / io monad - because we'll need line numbers etc info when doing different transformations. This also makes the whole thing more efficient since we will be applying certain optimizations while parsing, not in different passes.