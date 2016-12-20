-- Playing with Backwards Polish notation approach and some straigtforward naive
-- type system

{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

type Name = String

-- all available primitive types in the language that we will use as the basis vector to build the type System
data PrimitiveType = PrimInt | PrimFloat | PrimByte | PrimVector PrimitiveType deriving (Show)

-- the most generic representation of (name, Type) pair, where Type can be Type, function or a 'type'
data TypeTag = TYPE | FUNC | PrimType PrimitiveType | TypeTag Name deriving (Show)

-- typed variable of sorts. Int indexes variables for binding in terms
data GenericVar = GenVar Int TypeTag deriving (Show)
-- simple type term (product)
data ProductType = Cons Name [GenericVar] deriving (Show)

-- dependent type - type that can be parametrized over the list of variables 'vars',
-- e.g. Maybe a
data DependentType = DependentType {
    typeName :: Name,
    vars :: [GenericVar],
    constructors :: [ProductType]
} deriving (Show)

-- concrete type - type that has been instantiated, e.g. Maybe Int
data ConcreteType = ConcreteType {
    typeName :: Name,
    constructors :: [ProductType]
} deriving (Show)

-- some examples
typeBool = ConcreteType { typeName = "Bool", constructors = [Cons "False" [], Cons "True" []] }
-- dependent Maybe a
typeMaybea = DependentType { typeName = "Maybe", vars = [GenVar 0 TYPE], constructors = [Cons "Nothing" [], Cons "Just" [GenVar 0 TYPE] ] }
-- concrete Maybe Int
typeMaybeInt = ConcreteType {typeName = "MaybeInt", constructors = [Cons "Nothing" [], Cons "Just" [GenVar 0 (PrimType PrimInt)] ] }
-- MyType a b = MyType a b
typeMyType = DependentType { typeName = "MyType", vars = [GenVar 0 TYPE, GenVar 1 TYPE], constructors = [Cons "MyType" [GenVar 0 TYPE, GenVar 1 TYPE] ] }

-- now, question is, how do we get Concrete from Dependent when instantiating type variable?
-- we have to bind specific types to our type variables and get something out as a result
-- let's start with Maybe a --> Maybe PrimInt

-- instantiateType depType varValues =


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
