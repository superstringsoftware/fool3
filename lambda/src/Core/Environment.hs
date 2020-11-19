{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
    jsProgram   :: NameMap String
} deriving Show

initialEnvironment = Environment {
    types       = Map.empty,
    topLambdas  = Map.empty,
    jsProgram   = Map.empty 
}

-- We need to provide a clean and pure interface for different functions, types etc lookup and insertion,
-- also, to handle Classes nicely --> instead of forcing monad functions to use low-level Map manipulations etc.
-- see State for the monadic interface!!!

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




instance PrettyPrint TypeRep where 
    ppr (LiftedTypeRep n args cons) = "Type " ++ n ++ " " ++ (showListPlain ppr args) ++ (showListCuBr ppr cons)