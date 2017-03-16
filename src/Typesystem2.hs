{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Typesystem2 where

-- Trying bottom up approach - translating to C from the start
import Data.Word
import qualified Data.Vector.Unboxed as U

import Data.List

type Name = String

-- all available primitive types in the language that we will use as the basis vector to build the type System
data BuiltinType = PrimInt | PrimFloat | PrimByte | PrimReference
                 | PrimArray BuiltinType | SELF | ADType SumType deriving (Show, Eq)

-- primitive values
data BuiltinTypeValue = PInt Int | PFloat Float | PByte Word8
                        | VInt (U.Vector Int)
                        | VFloat (U.Vector Double)
                        | VByte (U.Vector Word8)
                        | RecordValue ConcreteRecord
                        | UNDEFINED
    deriving (Show, Eq)

data ConcreteVar = ConcreteVar {
    varName :: Name,
    varType :: BuiltinType,
    varValue :: BuiltinTypeValue
} deriving (Show, Eq)

data ConcreteRecord = ConcreteRecord {
    consName :: Name,
    recFields :: [ConcreteVar]
} deriving (Show, Eq)

type SumType = [ConcreteRecord]

data Function = Function {
    funName :: Name,
    vars :: [ConcreteVar],
    funDefinition :: [Name]
} deriving (Show, Eq)

primopMul = Function {
    funName = "primitive multiply",
    vars = [ConcreteVar "" PrimInt UNDEFINED, ConcreteVar "" PrimInt UNDEFINED],
    funDefinition = []
}

-- main execution function, builds up upon a number of primops
callFunction :: Function -> BuiltinTypeValue
callFunction primopMul = let v1 = (vars primopMul) !! 0
                             v2 = (vars primopMul) !! 1
                             in (varValue v2) -- * (varValue v1)



tpNothing = ConcreteRecord {consName="Nothing", recFields = []}
tpJustInt = ConcreteRecord {consName= "Just", recFields=[ConcreteVar "" PrimInt UNDEFINED]}

tpMaybeInt = [tpNothing, tpJustInt]

tpComplexRecord = ConcreteRecord {
    consName = "ComplexRecord",
    recFields = [
        ConcreteVar "x" PrimInt UNDEFINED,
        ConcreteVar "y" PrimInt UNDEFINED,
        ConcreteVar "maybeBig" (ADType tpMaybeInt) UNDEFINED
    ]
}

-- return type of the primitive value (needed for type comparisons etc)
checkPrimitiveType :: BuiltinTypeValue -> BuiltinType
checkPrimitiveType (PInt _) = PrimInt
checkPrimitiveType (PFloat _) = PrimFloat
checkPrimitiveType (PByte _) = PrimByte
checkPrimitiveType (VInt _) = PrimArray PrimInt
checkPrimitiveType (VFloat _) = PrimArray PrimFloat
checkPrimitiveType (VByte _) = PrimArray PrimByte

class Compilable a where
    toCString :: a -> String

instance Compilable BuiltinType where
    toCString PrimInt = "int "
    toCString PrimFloat = "double "
    toCString PrimByte = "char "

instance Compilable BuiltinTypeValue where
    toCString (PInt x) = show x
    toCString (PFloat x) = show x
    toCString (PByte x) = show x

instance Compilable ConcreteRecord where
    toCString pt = ""
