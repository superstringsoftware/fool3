# some ongoing thoughts on implementation

So, our parser needs to be rewritten in the compiler state / io monad - because we'll need line numbers etc info when doing different transformations. This also makes the whole thing more efficient since we will be applying certain optimizations while parsing, not in different passes. -- done

Ok, typeclasses are sort of the trickiest thing to implement. We need to be able to pick the right function based on it's generic name and argument types - so, basically, the signature. How do we do that? 
One option: start a dictionary based on the generic name, where we select specific implementation based on the signature.

E.g., for all (+) implementations - we do lookup "(+)", which returns a list of pairs (signature, Lam)

Need to reconcile this with typechecking somehow.