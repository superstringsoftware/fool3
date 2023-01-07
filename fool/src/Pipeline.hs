{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Pipeline
where

import State
import Core
import Logs

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List.Index

import Util.PrettyPrinting as TC
import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

--------------------------------------------------------------------------------
-- PASS 0: initial desugaring etc after parsing
--------------------------------------------------------------------------------
-- this needs to be run right after parsing:
-- desugaring (changing op calls to App calls etc)
-- fixing remaining tags for Tuples in data constructors (what wasn't fixed during parsing)
-- reversing the order
afterparserPass :: IntState ()
afterparserPass = do
    s <- get
    mod <- (runExprPassAndReverse afterparse (parsedModule s))
    put (s { parsedModule = mod } )
    -- now for more interesting stuff, initial optimizations with error checks
    return ()

-- reversal plus a bunch of initial error checks
runExprPassAndReverse :: (Expr -> IntState Expr) -> LTProgram -> IntState LTProgram
runExprPassAndReverse f l = rev f l []
    where rev f [] a = pure a
          rev f ((ex, srci):xs) a = f ex >>= \ttt -> rev f xs ( (ttt, srci):a )

-- initial quick checks and optimizations
afterparse :: Expr -> IntState Expr
-- sum type - need to map over all constructors and expand them to the full signature form
afterparse (SumType lam@(Lambda typName typArgs (Constructors cons) typTyp)) = 
    pure $ SumType lam { body = Constructors $ Prelude.map fixCons cons }
    where fixCons lam@(Lambda nm args ex typ) = if (ex /= UNDEFINED) then lam else lam { body = Tuple $ Prelude.map (\v -> Id $ name v) args}
afterparse e = pure e

-- initial checks with errors - so monadic
initialCheckM :: Expr -> IntState Expr
-- function with pattern match - check arity etc
initialCheckM (Function lam@(Lambda nm args (PatternMatches pms) tp)) = do
    pms' <- mapM initialCheckM pms
    return $ Function (lam { body = PatternMatches pms'} )

--------------------------------------------------------------------------------
-- PASS 1: building initial typing and top-level lambdas environment
-- This is a pretty important pass - it establishes top-level lambdas, 
-- moves constructor functions from inside Type declarations to the top level,
-- and processes instances of typeclasses (so, applications of typeclass functions to specific types) - 
-- which involves some basic typechecking etc already.
--------------------------------------------------------------------------------

-- Only VarDefinition, Binding and PatternMatch should be seen at the top level
buildEnvironmentM :: (Expr, SourceInfo) -> IntState ()
buildEnvironmentM x@(e,si) = do
    s <- get
    let env = currentEnvironment s
    env' <- processBinding x env
    put s{currentEnvironment = env'}
    -- either (\err -> logWarning err { linePos = (lineNum si), colPos = (colNum si) } )
           
processBinding :: (Expr, SourceInfo) -> Environment -> IntState Environment
-- function definition
processBinding (Function lam, si) env = pure $ addLambda (lamName lam) lam env
-- treating actions the same way - they will have some proper type eventually anyway
processBinding (Action lam, si) env = pure $ addLambda (lamName lam) lam env

-- now extracting constructors from SumTypes, body is guaranteed to be
-- a list of Lambdas under Constructors constructor
processBinding ( tp@(SumType (Lambda typName typArgs (Constructors cons) typTyp)), si) env = do
    pure $ addManyNamedLambdas cons (addNamedSumType tp env)

processBinding (ex, si) env = do 
    let lpl = LogPayload 
                (lineNum si) (colNum si) ""
                ("Cannot add the following expression to the Environment during initial Environment Building pass:\n" 
                    ++ (ppr ex) ++ "\n" 
                    -- ++ (TL.unpack (pShow ex))
                    ++ "\nThe expression is parsed and stored in LTProgram, but is not in the Environment.")
    logWarning lpl { linePos = (lineNum si), colPos = (colNum si) } 
    return env



-- The pass itself   
buildEnvPass :: IntState ()
buildEnvPass = buildPrimitivePass >> get >>= pure . parsedModule >>= mapM_ buildEnvironmentM

-- killing primitive bindings for now to test iterative compilation
primBindings = []    
{-
primBindings = [
        Let [(Var "+" ToDerive, Prim PPlus)] EMPTY,
        Let [(Var "-" ToDerive, Prim PMinus)] EMPTY,
        Let [(Var "*" ToDerive, Prim PMul)] EMPTY,
        Let [(Var "/" ToDerive, Prim PDiv)] EMPTY
    ]
-}

buildPrimitivePass :: IntState ()
buildPrimitivePass = mapM_ (\b -> buildEnvironmentM (b, SourceInfo 0 0 "")) primBindings

--------------------------------------------------------------------------------
-- PASS 2: Preliminary Optimizations and basic sanity checks
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- PASS 3: Type checking
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 4: Further optimizations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 5: Compilation - JS
--------------------------------------------------------------------------------
mkConsName :: String -> String -> String
mkConsName typName consName = "cons_" ++ typName ++ "_" ++ consName

compileExprToJS :: Expr -> [String]
compileExprToJS (SumType lam@(Lambda typName typArgs (Constructors cons) typTyp)) = 
    imap (compileConstructorToJS ("cons_" ++ typName ++ "_") ) cons
compileExprToJS (Function lam) = [compileFunctionToJS "" lam]
compileExprToJS e = ["/* NOT SUPPORTED:\n" ++ ppr e ++ "\n*/"]

argsToString :: Record -> String
argsToString args = showListRoBr f args
    where f (Var nm tp vl) = nm

argsToTupleFields :: Record -> String
argsToTupleFields args = showListPlainSep f ", " args
    where f (Var nm tp vl) = nm ++ ": " ++ nm

compileConstructorToJS :: String -> Int -> Lambda -> String
compileConstructorToJS pref i (Lambda nm args ex tp) = "function " ++ pref++nm ++
                         argsToString args 
                         ++ " { return { __consTag: " ++ (show i) ++ ", "
                         ++ argsToTupleFields args ++ " } } "                         
    
compileFunctionToJS :: String -> Lambda -> String
compileFunctionToJS pref (Lambda nm args ex tp) = "function " ++ pref++nm ++
                         argsToString args 
                         ++ funBodyToString ex

-- compiling pattern matches is the most complicated thing as we need to 
-- consult the environment about the order of constructors etc
funBodyToString :: Expr -> String
funBodyToString (Id x) = "{ return " ++ x ++ "; }"
funBodyToString e = " { /* NOT IMPLEMENTED:\n" ++ ppr e ++ "\n*/ }" 
