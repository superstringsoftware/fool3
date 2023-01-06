{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
-- typing etc environment when compiling - can be built from several modules

module Core.Environment
where

import Data.HashMap.Strict as Map

import Core.Syntax
import Data.Text
import Data.Text.Lazy as TL

import Util.PrettyPrinting
import Logs

import Text.Pretty.Simple (pShow)

type NameMap = HashMap Name

type LTProgram = [(Expr, SourceInfo)]

data TypeRep = LiftedTypeRep {
    name        :: Name, -- name of the type
    args        :: [Type], -- TVars or concrete types
    dataCons    :: [Expr] -- data constructors of the type, in order; can only be 'Lam'!!!
} | PrimTypeRep {
    name        :: Name,
    args        :: [Type] -- e.g., for primarrays creation!
} deriving Show

-- structure keeping our current environment
data Environment = Environment {
    -- Map that keeps all our TypeReps in the current environment
    types       :: NameMap TypeRep,
    topLambdas  :: NameMap Lambda,
    classFuncs  :: NameMap [(Type, Name)], -- lookup table for the class functions. Maps names defined inside a class ("+") to a lookup table of type signatures to fully qualified names, which then reference concrete functions. There's probably a better way to do it.
    jsProgram   :: NameMap String
} deriving Show

initialEnvironment = Environment {
    types       = Map.empty,
    topLambdas  = Map.empty,
    classFuncs  = Map.empty,
    jsProgram   = Map.empty 
}

-- We need to provide a clean and pure interface for different functions, types etc lookup and insertion,
-- also, to handle Classes nicely --> instead of forcing monad functions to use low-level Map manipulations etc.
-- see State for the monadic interface!!!

---------------------------- TYPE CLASSES & FAMILIES ---------------------
-- This is slightly tricky: we will store a typclass function (let's take + from Semigroup) and attach a lookup table to it that is based on the full type signature
-- + : [Type Signature --> specific function NAME] - indirection table
-- So, when we encounter <Class> <Type> application that defines a function for a given class instance, we simply add it to the table with the full type signature
-- The only question for now is - should we store it as a special Lambda or in a separate typeclass structure?
-- E.g., if we have Semigroup Int = {...}, we do the following:
    -- Add another mapping entry to the indirection table +: - mapping type signature Int->Int->Int to the fully qualified function NAME
    -- Add another TOP LEVEL lambda named Semigroup.Int.+ that maps to the actual body
    -- Then, when we do lookup during code generation - we ask the indirection table with the full type signature etc.

-- NEW IDEA:
-- Storing typeclass functions as simply a function with pattern matches for specific type signatures!!!
-- They still will redirect to the global level functions, e.g., for +:
-- + = {"Int->Int" -> VarId "Semigroup.Int.+", "Float->Float" -> VarId "Semigroup.Float.+" }
-- then on the top level we'll have:
-- Semigroup.Int.+ = primplusint
-- Semigroup.Float.+ = primplusfloat
-- ETC.
-- Maybe it's too many indirections, and we'll want to optimize for inline cases etc.

-- DEFAULT PATTERN is treated as the one with EMPTY instead of a pattern match!

-- adds one newly encountered class function to the top level environment (basically, sets up a default function if any at the top level and initializes a Sig -> Name) lookup table
-- TODO: ADD ERROR HANDLING IN CASE THERE ARE SAME NAMES!!!!!!
addOneClassFunc :: Field -> Environment -> Environment
addOneClassFunc (Field nm tp (Lam lam@(Lambda args body sig _) )) env = 
    let env' = addLambda nm (lam { sig = tp, body = Patterns [PatternMatch EMPTY body] }) env -- adding DEFAULT pattern match to the patterns in the class function. Can use this instead: (App (VarId nm) [Rec args])
    in  env' {
            classFuncs = Map.insert nm [] (classFuncs env')
        }
-- case such as E0 in monoid - just a constant basically
addOneClassFunc (Field nm tp EMPTY) env = 
    let env' = addLambda nm (emptyLambda { sig = tp, body = Patterns[PatternMatch EMPTY EMPTY] }) env
    in  env' {
            classFuncs = Map.insert nm [] (classFuncs env')
        }
addOneClassFunc ex@(Field nm _ _) env = addLambda nm (emptyLambda {body = ERROR ("This should NOT happen: tried adding this as a class func: " ++ (ppr ex))}) env

addClassFuncs :: Record -> Environment -> Environment
addClassFuncs r env = Prelude.foldl (flip addOneClassFunc) env r

---------------------------- BASIC FUNCTIONS -----------------------------
lookupLambda :: Name -> Environment -> Maybe Lambda
lookupLambda n env = Map.lookup n (topLambdas env)

addLambda :: Name -> Lambda -> Environment -> Environment
addLambda n l env = env { topLambdas = Map.insert n l (topLambdas env) }

addManyLambdas :: [(Name, Lambda)] -> Environment -> Environment
addManyLambdas ls env = env { topLambdas = Prelude.foldl (\acc (n1,l1) -> Map.insert n1 l1 acc) (topLambdas env) ls }

maybeTypeName :: Type -> Maybe TypeRep
maybeTypeName (TCon n)              = Just $ LiftedTypeRep n []   []
maybeTypeName (TApp (TCon n) args)  = Just $ LiftedTypeRep n args []
maybeTypeName _                     = Nothing


instance PrettyPrint (Type, Name) where
    ppr (t, n) = ppr t ++ " -> " ++ n

instance PrettyPrint [(Type, Name)] where
    ppr ls = showListSqBr ppr ls

instance PrettyPrint TypeRep where 
    ppr (LiftedTypeRep n args cons) = "Type " ++ n ++ " " ++ (showListPlain ppr args) ++ (showListCuBr ppr cons)