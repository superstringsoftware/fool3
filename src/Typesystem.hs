-- some straigtforward naive type system
-- next steps:
-- better pretty printing
-- instantiating values

{-# LANGUAGE DuplicateRecordFields #-}

module Typesystem where

import Data.Word

type Name = String

-- all available primitive types in the language that we will use as the basis vector to build the type System
data PrimitiveType = PrimInt | PrimFloat | PrimByte | PrimVector PrimitiveType

instance Show PrimitiveType where
    show PrimInt = "int "
    show PrimFloat = "float "
    show PrimByte = "byte "
    show (PrimVector t) = "vector " ++ (show t)

-- primitive values
data PrimitiveValue = PInt Int | PFloat Float | PByte Word8 deriving (Show)

checkPrimitiveType :: PrimitiveValue -> PrimitiveType
checkPrimitiveType (PInt _) = PrimInt
checkPrimitiveType (PFloat _) = PrimFloat
checkPrimitiveType (PByte _) = PrimByte

-- the most generic representation of (name, Type) pair, where Type can be Type, function or a 'type'
-- SELF__ is a dummy value used to define recursive types
data TypeTag = SELF | TYPE | FUNC | PrimType PrimitiveType | TypeTag Name

instance Show TypeTag where
    show (PrimType pt) = show pt
    show (TypeTag nm)  = nm
    show SELF = "SELF"
    show TYPE = "TYPE"
    show FUNC = "FUNCTION"

-- simple type term (product): Name is the name of the constructor and
-- List of Ints is the list of variable indices bound in the type record
data ProductType = Cons Name [Int] deriving (Show)

-- give a list of typeVars and it shows ProductType nicely
-- used in Show overload for DependentType
prettyProductTypeShow tts (Cons nm is) = nm ++ " " ++ (s1 is tts) where
    s1 [] tts = ""
    s1 (i:is) tts = show (tts !! i) ++ " " ++ s1 is tts

-- dependent type - type that can be parametrized over the list of variables 'vars',
-- e.g. Maybe a
data DependentType = DependentType {
    typeName :: Name,
    typeVars :: [TypeTag],
    constructors :: [ProductType]
}

instance Show DependentType where
    show (DependentType {typeName = tn, typeVars = tv, constructors = cs}) = tn ++ " = " ++ showNice cs where
        showNice x = show $ map (prettyProductTypeShow tv) x


-- some examples

-- "built-in" types that we are going to need
typeInt = DependentType { typeName = "Int", typeVars = [PrimType PrimInt], constructors = [Cons "Int" [0]] }
typeFloat = DependentType { typeName = "Float", typeVars = [PrimType PrimFloat], constructors = [Cons "Float" [0]] }
typeBool = DependentType { typeName = "Bool", typeVars = [], constructors = [Cons "False" [], Cons "True" []] }

-- dependent Maybe a
typeMaybea = DependentType { typeName = "Maybe", typeVars = [TYPE], constructors = [Cons "Nothing" [], Cons "Just" [0] ] }
-- concrete Maybe Int
typeMaybeInt = DependentType {typeName = "Maybe Int", typeVars = [PrimType PrimInt], constructors = [Cons "Nothing" [], Cons "Just" [0] ] }
-- MyType a b = MyType a b
typeMyType = DependentType { typeName = "MyType", typeVars = [TYPE, TYPE], constructors = [Cons "MyType" [0,1] ] }
-- List a = Nil | Cell a (List a)
typeList = DependentType {typeName = "List", typeVars = [TYPE, SELF], constructors = [Cons "Nil" [], Cons "Cell" [0,1]] }
-- now something that doesn't exist in Haskell, dependent on type var and regular var
-- Vector a:TYPE, n:Int
-- typeVector = DependentType {typeName = "Vector a n", vars = [GenVar 0 TYPE, GenVar 1 (TypeTag "Int")], constructors = [Cons "Vector" [0, 1] ] }

-- now, question is, how do we get Concrete from Dependent when instantiating type variable?
-- we have to bind specific types to our type variables and get something out as a result
-- let's start with Maybe a --> Maybe PrimInt

-- instantiating type variable - only changing TYPE to something else, the rest should THROW AN ERROR (which it doesnt now)
instantiateTypeVariable :: TypeTag -> TypeTag -> TypeTag
instantiateTypeVariable TYPE nt = nt
instantiateTypeVariable x _ = x

-- this one simply changes all TYPE occurences in the dependent type type variables into what is given in varVals
-- e.g., instantiateType [TypeTag "NewType"] typeMaybea -- creates Maybe NewType from Maybe a
instantiateType :: [TypeTag] -> DependentType -> DependentType
instantiateType varVals (DependentType {typeName = tn, typeVars = tv, constructors = cs}) = DependentType {typeName = tn, typeVars = newTVars, constructors = cs} where
    newTVars = zipWith instantiateTypeVariable tv varVals
