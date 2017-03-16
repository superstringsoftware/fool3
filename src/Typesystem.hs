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

import Data.List

type Name = String

-- all available primitive types in the language that we will use as the basis vector to build the type System
-- data BuiltinType = PrimInt | PrimFloat | PrimByte | PrimReference | PrimArray BuiltinType deriving Eq

-- primitive values
data BuiltinTypeValue = PInt Int | PFloat Float | PByte Word8
                        | VInt (U.Vector Int)
                        | VFloat (U.Vector Double)
                        | VByte (U.Vector Word8)
    deriving (Show, Eq)

-- data TypeOrKind = Builtin BuiltinType | SELF | TYPE | FUNC | TypeRep SumType deriving Eq

-- what can be assigned to a type
data RTypeValue = TYPE | Primitive | RTypeValSum SumType
                | RTypeValBound Int deriving (Show, Eq)-- int is an index to which Type Level var we are bound
-- what can be assigned to value of a var
data RValue = RValPrim BuiltinTypeValue | RValSum Constructor | RValBound Int | UNDEFINED deriving (Show, Eq)

{-
-- return type of the primitive value (needed for type comparisons etc)
checkPrimitiveType :: BuiltinTypeValue -> BuiltinType
checkPrimitiveType (PInt _) = PrimInt
checkPrimitiveType (PFloat _) = PrimFloat
checkPrimitiveType (PByte _) = PrimByte
checkPrimitiveType (VInt _) = PrimArray PrimInt
checkPrimitiveType (VFloat _) = PrimArray PrimFloat
checkPrimitiveType (VByte _) = PrimArray PrimByte
-}

-- checkRValType :: RValue

-- checking if RValue is of type RTypeValue
isValueCorrectType :: RValue -> RTypeValue -> Bool
-- checking primitives
isValueCorrectType (RValSum _) TYPE = True
isValueCorrectType _ _ = False



-- generic variable that can hold whatever
data Variable = Variable {
    varLabel :: Name,
    varType :: RTypeValue,
    varValue :: RValue
    -- varDefaultValue :: RValue
} deriving (Show, Eq)

-- shortcut for Variable initialization
undefinedVar = Variable {
    varLabel = "",
    varType = TYPE,
    varValue = UNDEFINED
}

primitiveVar = Variable {
    varLabel="",
    varType = Primitive,
    varValue = UNDEFINED
}

-- Product type constructor
data Constructor = Constructor {
    consName :: Name,
    consVars :: [Variable], -- Constructor-level variables: Just a --> a etc
    vars :: [Variable]
} deriving (Eq)

tcInt = Constructor {
    consName = "I#",
    consVars = [],
    vars = [primitiveVar]
}

tpInt = SumType {
    typeName = "Int",
    typeVars = [],
    constructors = [tcInt]
}

-- Every concrete Type is a SumType of Products!
data SumType = SumType {
    typeName :: Name,
    typeVars :: [Variable], -- list of bound vars in the type declaration
    constructors :: [Constructor]
} deriving (Eq)

-- binding type variable in a dependent type - e.g., Maybe Int
-- returning new type
-- bindTypeVar :: SumType -> Name -> TypeOrKind -> SumType
-- bindTypeVar st var tok = let pt = productTypes st


{- ********************* Show and other service instances ********************** -}
instance Show SumType where
    show st = (typeName st) ++ show (typeVars st) ++ " = " ++ show (constructors st)

{-
instance Show BuiltinType where
    show PrimInt = "int "
    show PrimFloat = "float "
    show PrimByte = "byte "
    show PrimReference = "reference "
    show (PrimArray t) = "array " ++ (show t)
-}

instance Show Constructor where
    show pc
        | (vars pc) == [] = (consName pc)
        | otherwise         = (consName pc) ++ " " ++ show (fmap show (vars pc))


{- ********************* EXAMPLE TYPES ********************** -}

-- Bool = True | False
tpBool = SumType {
    typeName = "Bool",
    typeVars = [],
    constructors = [
        Constructor {consName = "True", consVars = [], vars = []},
        Constructor {consName = "False", consVars = [], vars = []}
    ]
}

-- Maybe a = Nothing | Just a
tpMaybe = SumType {
    typeName = "Maybe",
    typeVars = [undefinedVar {varLabel="a"}],
    constructors = [
        Constructor {consName = "Nothing", consVars = [], vars = []},
        Constructor {consName = "Just",
            consVars = [undefinedVar {varLabel="a", varValue=RValBound 0}],
            vars     = [undefinedVar {varType = RTypeValBound 0}]
        }
    ]
}

tpDepType = SumType {
    typeName = "DepTypeExample",
    typeVars = [undefinedVar {varLabel="a", varType = TYPE},
                undefinedVar {varLabel="n", varType = RTypeValSum tpInt}
               ],
    constructors = [
        Constructor {consName = "DepType",
            consVars = [undefinedVar {varLabel="a", varType = TYPE, varValue=RValBound 0},
                        undefinedVar {varLabel="n", varType = RTypeValSum tpInt, varValue=RValBound 0}
                       ],
            vars     = [undefinedVar {varLabel="name", varType = RTypeValSum tpMaybe}]
        }
    ]
}

-- binds Variable at place Int and returns adjusted Constructor
-- bindVar :: Variable -> RValue -> Either String Variable
bindVar var val =
    if (isValueCorrectType val (varType var))
        then (Right var{varValue = val})
        else (Left "ERROR: Type mismatch")

{-
-- Int - packed int
tpInt = SumType {
    typeName = "Int",
    typeVars = [],
    productTypes = [
        ProductConstructor {consName = "I#",
            fields = [RecordVariableField {varLabel = Nothing, varType = Builtin PrimInt}]}
    ]
}

-- Float - packed float
tpFloat = SumType {
    typeName = "Float",
    typeVars = [],
    productTypes = [
        ProductConstructor {consName = "F#",
            fields = [RecordVariableField {varLabel = Nothing, varType = Builtin PrimFloat}]}
    ]
}


-- Bool = True | False
tpBool = SumType {
    typeName = "Bool",
    typeVars = [],
    productTypes = [
        ProductConstructor {consName = "True", fields = []},
        ProductConstructor {consName = "False", fields = []}
    ]
}

-- Point {x,y:int}, Circle, Square polymorphism
tpPoint = SumType {
    typeName = "Point",
    typeVars = [],
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
    typeVars = [],
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
    typeVars = [],
    productTypes = [
        ProductConstructor {consName = "Circle1",
            fields = [RecordVariableField {varLabel = Just "p", varType = TypeRep tpPoint},
                      RecordVariableField {varLabel = Just "r", varType = Builtin PrimInt}]}
    ]
}

-- Maybe a = Nothing | Just a
tpMaybe = SumType {
    typeName = "Maybe",
    typeVars = ["a"],
    productTypes = [
        ProductConstructor {consName = "Nothing", fields = []},
        ProductConstructor {consName = "Just",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE}]}
    ]
}

-- Either a b = Left a | Right b
tpEither = SumType {
    typeName = "Either",
    typeVars = ["a", "b"],
    productTypes = [
        ProductConstructor {consName = "Left",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE}]},
        ProductConstructor {consName = "Right",
            fields = [RecordVariableField {varLabel = Just "b", varType = TYPE}]}
    ]
}



-- Maybe Int - exploring difference between concrete and parametric
tpMaybeInt = SumType {
    typeName = "Maybe Int",
    typeVars = [],
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
    typeVars = ["a", "n"],
    productTypes = [
        ProductConstructor {consName = "Vector",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE},
                      RecordVariableField {varLabel = Just "n", varType = Builtin PrimInt}]
                  }
            ]
        }


-- List a = Nil | Cons a (List a) - recursive datatype, tricky!
tpList = SumType {
    typeName = "List",
    typeVars = ["a"],
    productTypes = [
        ProductConstructor {consName = "Nil", fields = []},
        ProductConstructor {consName = "Cons",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE},
                      RecordVariableField {varLabel = Nothing, varType = SELF}]}
    ]
}

tpS = SumType {
    typeName = "S",
    typeVars = ["a"],
    productTypes = [
        ProductConstructor {consName = "S",
            fields = [RecordVariableField {varLabel = Just "a", varType = TYPE},
                      RecordVariableField {varLabel = Nothing, varType = SELF}]}
    ]
}
-}
