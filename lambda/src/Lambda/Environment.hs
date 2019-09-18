{-# LANGUAGE OverloadedStrings #-}
-- typing etc environment when compiling - can be built from several modules

module Lambda.Environment
where

import Data.HashMap.Strict as Map

import Lambda.Syntax

type NameMap = HashMap Name
type Error = String

data TypeRep = LiftedTypeRep {
    name        :: Name, -- name of the type
    args        :: [Var], -- variables of the type, if any
    dataCons    :: [Expr] -- data constructors of the type, in order; can only be 'Lam'!!!
} | PrimTypeRep {
    name        :: Name,
    args        :: [Var] -- e.g., for primarrays creation!
} deriving Show

-- pure function that takes an expression and Environment and 
-- then either constructs a new type from the data constructor or adds constructor to the existing type
addDataConstructor :: Expr -> Environment -> Either Error Environment
-- addDataConstructor
addDataConstructor e _ = Left $ "Tried to add data constructor to the typing environment, but the expression is a " ++ ppr e

-- structure keeping our current environment
data Environment = Environment {
    types       :: NameMap TypeRep
} deriving Show

initialEnvironment = Environment {
    types = Map.empty
}