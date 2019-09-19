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
    args        :: [Type], -- TVars or concrete types
    dataCons    :: [Expr] -- data constructors of the type, in order; can only be 'Lam'!!!
} | PrimTypeRep {
    name        :: Name,
    args        :: [Type] -- e.g., for primarrays creation!
} deriving Show

-- structure keeping our current environment
data Environment = Environment {
    types       :: NameMap TypeRep
} deriving Show

initialEnvironment = Environment {
    types = Map.empty
}


-- pure function that takes an expression and Environment and 
-- then either constructs a new type from the data constructor or adds constructor to the existing type
-- at this point, data constructor is either Lam ... Tuple or just Tuple, in the latter case it must be empty.
addDataConstructor :: Expr -> Environment -> Either Error Environment
addDataConstructor e@(Lam vars (Tuple constag exs typ) typlam preds) env = _addDataConstructor e typ env
addDataConstructor e@(Tuple constag exs typ) env = _addDataConstructor e typ env
addDataConstructor e _ = Left $ "Tried to add data constructor to the typing environment, but the expression is a " ++ ppr e
_addDataConstructor e typ env@Environment{types=types} = 
    case (maybeTypeName typ) of
        Nothing -> Left $ "Tried to add data constructor to the typing environment, but it's type is " ++ ppr typ
        Just tr -> Right $ env{ types = Map.alter f (name tr) types }
            where f Nothing        = Just tr{dataCons=[e]}
                  f (Just typerep) = Just typerep{dataCons = (e: (dataCons typerep))}


maybeTypeName :: Type -> Maybe TypeRep
maybeTypeName (TCon n)              = Just $ LiftedTypeRep n []   []
maybeTypeName (TApp (TCon n) args)  = Just $ LiftedTypeRep n args []
maybeTypeName _                     = Nothing

instance Printer TypeRep where 
    ppr (LiftedTypeRep n args cons) = "Type " ++ n ++ " " ++ (showListPlain ppr args) ++ (showListCuBr ppr cons)