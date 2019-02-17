module DotNet.Typechecker where

{-
Typechcking first attempt based on the fact function to test case based and silly f function for regular:
fact: = λn: . n ? 
	| z0 -> z1
    | otherwise -> (*) n fact ((-) n 1)
    
Lam "fact" [Id "n" ToDerive] 
    (Case (VarId "n") 
        [(VarId "z0",VarId "z1"),
          (VarId "otherwise",BinaryOp "*" (VarId "n") (App (VarId "fact") (BinaryOp "-" (VarId "n") (Lit (LInt 1)))))]) ToDerive

f: = λx: y: . x * x + y

Lam "f" [Id "x" ToDerive,Id "y" ToDerive] (BinaryOp "+" (BinaryOp "*" (VarId "x") (VarId "x")) (VarId "y")) ToDerive

In our model type signature should be:
forall a. Ring a => a -> a -- for fact and
forall a. Ring a => a -> a -> a -- for f

since Ring is the first typeclass that has both (*) and (-) operators, as well as z0 and z1.
How do we get there?

- Substitute types into bound variables in the function
-}