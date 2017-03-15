{-# LANGUAGE DuplicateRecordFields #-}

module Typesystem where

import Data.Word
import qualified Data.Vector.Unboxed as U

type Name = String

-- all available primitive types in the language that we will use as the basis vector to build the type System
data BuiltinType = PrimInt | PrimFloat | PrimByte | PrimArray BuiltinType deriving Eq

-- primitive values
data BuiltinTypeValue = PInt Int | PFloat Float | PByte Word8
                        | VInt (U.Vector Int)
                        | VFloat (U.Vector Double)
                        | VByte (U.Vector Word8)
    deriving (Show)

instance Show BuiltinType where
    show PrimInt = "int "
    show PrimFloat = "float "
    show PrimByte = "byte "
    show (PrimArray t) = "array " ++ (show t)

data TypeOrKind = Builtin BuiltinType | SELF | TYPE | FUNC | TypeRep SumType deriving Eq

instance Show TypeOrKind where
    show (TypeRep st)  = typeName st
    show SELF = "SELF"
    show TYPE = "TYPE"
    show FUNC = "FUNCTION"
    show (Builtin b) = show b

data RecordField = RecordVariableField {
    varLabel :: Maybe Name, -- Just name if a record is named, Nothing if anonymous
    varType :: TypeOrKind
} deriving Eq

instance Show RecordField where
    show rf
        | varLabel rf == Nothing = "_anonymous_ :: " ++ show (varType rf)
        | otherwise              = n ++ " :: " ++ show (varType rf) where (Just n) = varLabel rf

-- Product type (including single-terms such as Nothing or True):
-- Nothing --> name="Nothing", fields = []
-- Point {x,y::Int} --> name="Point", fields = [{varLabel="x", varType=Builtin PrimInt}, {varLabel="y", varType=Builtin PrimInt}]
-- Vector {x::Int, a::TYPE} --> name="Vector", fields = [{varLabel="x", varType=Builtin PrimInt}, {varLabel="a", varType=TYPE}]
data ProductConstructor = ProductConstructor {
    consName :: Name,
    fields :: [RecordField]
} deriving Eq

instance Show ProductConstructor where
    show pc
        | (fields pc) == [] = (consName pc)
        | otherwise         = (consName pc) ++ " " ++ show (fmap show (fields pc))

-- Every concrete Type is a SumType of Products!
data SumType = SumType {
    typeName :: Name,
    productTypes :: [ProductConstructor]
} deriving (Show, Eq)

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



-- When we Add several SumTypes - we simply combine Product Constructors!!!!!!!
-- TBD: define this op
-- need to store all SumTypes in Hashtable indexed by Name,
-- same with product constructors

{-
checkPrimitiveType :: PrimitiveValue -> PrimitiveType
checkPrimitiveType (PInt _) = PrimInt
checkPrimitiveType (PFloat _) = PrimFloat
checkPrimitiveType (PByte _) = PrimByte
-}
