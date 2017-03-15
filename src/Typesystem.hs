{-# LANGUAGE DuplicateRecordFields #-}

module Typesystem where

-- When we Add several SumTypes - we simply combine Product Constructors!!!!!!!
-- TBD: define this op
-- need to store all SumTypes in Hashtable indexed by Name,
-- same with product constructors

-- Logic of building types goes: RecordField --> ProductConstructor --> SumType
-- now need to get to values

import Data.Word
import qualified Data.Vector.Unboxed as U

type Name = String

-- all available primitive types in the language that we will use as the basis vector to build the type System
data BuiltinType = PrimInt | PrimFloat | PrimByte | PrimReference | PrimArray BuiltinType deriving Eq

-- primitive values
data BuiltinTypeValue = PInt Int | PFloat Float | PByte Word8
                        | VInt (U.Vector Int)
                        | VFloat (U.Vector Double)
                        | VByte (U.Vector Word8)
    deriving (Show, Eq)

data TypeOrKind = Builtin BuiltinType | SELF | TYPE | FUNC | TypeRep SumType deriving Eq

data ValueRep = PrimValue BuiltinTypeValue deriving Eq

-- This is where instantiation happens
-- if varType is TYPE - then it's a type variable (a, b in Haskell)
-- if it's TypeRep or Builtin - it's a regular variable
-- if it's FUNC it's a function (Kinds??)
data RecordField = RecordVariableField {
    varLabel :: Maybe Name, -- Just name if a record is named, Nothing if anonymous
    varType :: TypeOrKind
    -- varValue :: Maybe ValueRep -- if Nothing, variable is unbound, otherwise - concrete value
} deriving Eq

-- Product type (including single-terms such as Nothing or True):
data ProductConstructor = ProductConstructor {
    consName :: Name,
    fields :: [RecordField]
} deriving Eq

-- Every concrete Type is a SumType of Products!
data SumType = SumType {
    typeName :: Name,
    productTypes :: [ProductConstructor]
} deriving (Eq)


{- ********************* Show and other service instances ********************** -}
instance Show SumType where
    show st = (typeName st) ++ " = " ++ show (productTypes st)

instance Show BuiltinType where
    show PrimInt = "int "
    show PrimFloat = "float "
    show PrimByte = "byte "
    show PrimReference = "reference "
    show (PrimArray t) = "array " ++ (show t)

instance Show TypeOrKind where
    show (TypeRep st)  = typeName st
    show SELF = "SELF"
    show TYPE = "TYPE"
    show FUNC = "FUNCTION"
    show (Builtin b) = show b

instance Show RecordField where
    show rf
        | varLabel rf == Nothing = "_anonymous_ :: " ++ show (varType rf)
        | otherwise              = n ++ " :: " ++ show (varType rf) where (Just n) = varLabel rf

instance Show ProductConstructor where
    show pc
        | (fields pc) == [] = (consName pc)
        | otherwise         = (consName pc) ++ " " ++ show (fmap show (fields pc))


{- ********************* EXAMPLE TYPES ********************** -}
-- Bool = True | False
tpBool = SumType {
    typeName = "Bool",
    productTypes = [
        ProductConstructor {consName = "True", fields = []},
        ProductConstructor {consName = "False", fields = []}
    ]
}

-- Point {x,y:int}, Circle, Square polymorphism
tpPoint = SumType {
    typeName = "Point",
    productTypes = [
        ProductConstructor {consName = "Point",
            fields = [RecordVariableField {varLabel = Just "x", varType = Builtin PrimInt},
                      RecordVariableField {varLabel = Just "y", varType = Builtin PrimInt}]}
    ]
}

-- Circle = Point * {r:Int} - extending Point, as Point is anonymous, can do:
-- Circle c; c.x = 5 -- accessing Point fields directly!
tpCircle = SumType {
    typeName = "Circle",
    productTypes = [
        ProductConstructor {consName = "Circle",
            fields = [RecordVariableField {varLabel = Nothing, varType = TypeRep tpPoint}, -- anonymous field, means can access Point fields directly!!!
                      RecordVariableField {varLabel = Just "r", varType = Builtin PrimInt}]}
    ]
}

-- Circle = {p::Point  * r:Int} - extending Point, as Point is named, can do:
-- Circle c; c.r = 5, c.p.x = 5 -- accessing Point fields via p:Points!
tpCircle1 = SumType {
    typeName = "Circle1",
    productTypes = [
        ProductConstructor {consName = "Circle1",
            fields = [RecordVariableField {varLabel = Just "p", varType = TypeRep tpPoint},
                      RecordVariableField {varLabel = Just "r", varType = Builtin PrimInt}]}
    ]
}

-- Maybe a = Nothing | Just a
tpMaybe = SumType {
    typeName = "Maybe",
    productTypes = [
        ProductConstructor {consName = "Nothing", fields = []},
        ProductConstructor {consName = "Just",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE}]}
    ]
}

-- Maybe Int - exploring difference between concrete and parametric
tpMaybeInt = SumType {
    typeName = "Maybe Int",
    productTypes = [
        ProductConstructor {consName = "Nothing", fields = []},
        ProductConstructor {consName = "Just",
            fields = [RecordVariableField {varLabel = Nothing, varType = Builtin PrimInt}]}
    ]
}

-- Vector a n - "parametric type" goodness, as n is Int variable
-- then, can do things such as Vector Float 2 or Vector Float 4 !
tpVector = SumType {
    typeName = "Vector",
    productTypes = [
        ProductConstructor {consName = "Vector",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE},
                      RecordVariableField {varLabel = Just "n", varType = Builtin PrimInt}]
                  }
            ]
        }

-- how about Vector Float 4? -- can't do it yet!
tpVectorF4 = SumType {
    typeName = "Vector Float 4",
    productTypes = [
        ProductConstructor {consName = "Vector",
            fields = [RecordVariableField {varLabel = Nothing, varType = Builtin PrimFloat},
                      RecordVariableField {varLabel = Nothing, varType = Builtin PrimInt}]
                  }
            ]
        }

-- List a = Nil | Cons a (List a) - recursive datatype, tricky!
tpList = SumType {
    typeName = "List",
    productTypes = [
        ProductConstructor {consName = "Nil", fields = []},
        ProductConstructor {consName = "Cons",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE},
                      RecordVariableField {varLabel = Nothing, varType = SELF}]}
    ]
}
