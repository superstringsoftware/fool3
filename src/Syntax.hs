-- Playing with Backwards Polish notation approach and some straigtforward naive
-- type system

module Syntax where

import Typesystem

data Expr
  = Float Double
  | Var Name
  | Call Name
    deriving (Eq, Ord, Show)

fact n = n * fact (n-1)

functionDefinition "fact" (Float 0.0) = [Float 1.0]
functionDefinition "fact" (Var "n") = Var "n" : Float 1.0 : Call "-" : Call "fact" : Var "n" : Call "*" : []

-- one step over [Expr]
-- arithmetic
callStep (Float x : Float y : Call "+" : xs) = Float (x + y) : xs
callStep (Float x : Float y : Call "-" : xs) = Float (x - y) : xs
callStep (Float x : Float y : Call "*" : xs) = Float (x * y) : xs
callStep (Float x : Float y : Call "/" : xs) = Float (x / y) : xs

-- factorial
callStep (Float 0.0 : Call "fact" : xs) = Float 1.0 : xs
callStep (Float x   : Call "fact" : xs) = Float x : Float 1.0 : Call "-" : Call "fact" : Float x : Call "*" : xs

-- full evaluation of a function call
fullEval [Float x] = [Float x]
fullEval xs = fullEval $ callStep xs

-- evaluating and adding all steps to a list for debugging purposes
fullEvalTrace xs = fullEvalTrace' [xs] xs
                    where fullEvalTrace' steps [Float x] = [Float x] : steps
                          fullEvalTrace' steps xs = fullEvalTrace' (callStep xs : steps) (callStep xs)
