{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- typing etc environment when compiling - can be built from several modules

module Environment
where

import Data.HashMap.Strict as Map

import Core.Syntax
import Data.Text

import Util.PrettyPrinting
import Logs

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
    lambdas     :: NameMap (Var, Expr) -- not only lambdas, but all top-level bindings, including expressions or thunks
} deriving Show

initialEnvironment = Environment {
    types       = Map.empty,
    lambdas     = Map.empty 
}

-- only processing Let bindings on the top level
processOneBinding :: (Expr, SourceInfo) -> Environment -> Either LogPayload Environment
processOneBinding ( Let ((v, ex):[]) EMPTY , si) env = 
    case ex of
        e@(Lam vars (Tuple constag exs typ) typlam preds) 
            -> either (Left) (\env1 -> addLambda v e env1) (addDataConstructor e env)
        e@(Tuple constag exs typ)
            -> either (Left) (\env1 -> addLambda v e env1) (addDataConstructor e env)
        e   -> addLambda v e env
processOneBinding (ex, si) _ = Left $ LogPayload 
        (lineNum si) (colNum si) ""
        "Unsupported environment processing call"

-- pure function that takes an expression and Environment and 
-- then either constructs a new type from the data constructor or adds constructor to the existing type
-- at this point, data constructor is either Lam ... Tuple or just Tuple, in the latter case it must be empty.
addDataConstructor :: Expr -> Environment -> Either LogPayload Environment
addDataConstructor e@(Lam vars (Tuple constag exs typ) typlam preds) env = _addDataConstructor e typ env
addDataConstructor e@(Tuple constag exs typ) env = _addDataConstructor e typ env
addDataConstructor e _ = Left $ LogPayload 
    0 0 ""
    ("Tried to add data constructor to the typing environment, but the expression is a " ++ ppr e)
_addDataConstructor e typ env@Environment{types=types} = 
    case (maybeTypeName typ) of
        Nothing -> Left $ LogPayload 
            0 0 ""
            ("Tried to add data constructor to the typing environment, but it's type is " ++ ppr typ)
        Just tr -> Right $ env{ types = Map.alter f (name tr) types }
            where f Nothing        = Just tr{dataCons=[e]}
                  f (Just typerep) = Just typerep{dataCons = (e: (dataCons typerep))}


maybeTypeName :: Type -> Maybe TypeRep
maybeTypeName (TCon n)              = Just $ LiftedTypeRep n []   []
maybeTypeName (TApp (TCon n) args)  = Just $ LiftedTypeRep n args []
maybeTypeName _                     = Nothing

-- adds a lambda or expression to the typing environment
addLambda :: Var -> Expr -> Environment -> Either LogPayload Environment
addLambda v@(Var n _) e env = 
    let func = Map.lookup n (lambdas env)
    in  maybe (Right $ env { lambdas = Map.insert n (v,e) (lambdas env) }) 
              -- name conflict - need BETTER ERROR MESSAGING! (line numbers etc)
              (const $ Left $ LogPayload 
                0 0 ""
                ("Tried to add lambda or expression named " ++ n ++ " but it has already been defined before!")) 
              func
            



instance PrettyPrint TypeRep where 
    ppr (LiftedTypeRep n args cons) = "Type " ++ n ++ " " ++ (showListPlain ppr args) ++ (showListCuBr ppr cons)