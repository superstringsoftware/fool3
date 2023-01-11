{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Interpreter
where

import Core
import CLM
import State
import Pipeline

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.HashMap.Strict as Map

addBinding :: Expr -> IntState ()
addBinding ex@(Binding _) = do
    s <- get
    let env = currentEnvironment s
    let clmex@(CLMBIND nm expr) = exprToCLM env ex
    let bnd = Map.insert nm (nm, expr) (clmBindings env)
    let s' = s { currentEnvironment = env { clmBindings = bnd } }
    liftIO $ putStrLn $ "Added binding " ++ show (nm, expr)
    put s'
addBinding e = liftIO $ putStrLn $ "Cant add binding for expr " ++ show e

processInteractive :: Expr -> IntState ()
processInteractive ex = do
    s <- get
    let env = currentEnvironment s
    let clmex = exprToCLM env ex
    liftIO $ putStrLn $ "CLM:\n" ++ show clmex
    case clmex of
        CLMID nm -> do 
            liftIO $ putStrLn $ "Looking up id " ++ show nm
            let mid = Map.lookup nm (clmBindings env)
            case mid of 
                Nothing -> liftIO $ putStrLn "Nothing found"
                Just v  -> liftIO $ putStrLn $ show v
        ex1@(CLMAPP _ _) -> do
            ex1' <- evalCLM ex1
            liftIO $ putStrLn "Final result:"
            liftIO $ putStrLn $ show ex1'
        _ -> liftIO $ putStrLn "Not implemented yet"

    

traceExpr :: Expr -> IntState()
traceExpr ex = do
    s <- get
    let env = currentEnvironment s
    let clmex = exprToCLM env ex
    liftIO $ putStrLn $ "CLM:\n" ++ show clmex

{-
CLMEMPTY
  | CLMERR String
  | CLMID Name
  | CLMBIND Name CLMExpr
  | CLMAPP CLMExpr [CLMExpr] -- saturated application first expr to the tuple of exprs
  | CLMPAP CLMExpr [CLMExpr] -- partial application (When we know the types!)
  | CLMCON ConsTag [CLMExpr] -- saturated constructor application, value in a sense
  | CLMFieldAccess (Name, Int) CLMExpr -- accessing a field of an expr by name or number
  | CLMCASE [CLMConsTagCheck] CLMExpr -- list of constructor checks that must all fold to True bound to an expr
  | CLMPROG [CLMExpr] -- list of expressions, for now used for Action but needs to change
  | CLMTYPED CLMExpr CLMExpr -- in case we want to give a type to an expression
  | CLMPRIMCALL -- body of the function that is a primitive call
-}

evalCLM :: CLMExpr -> IntState CLMExpr
evalCLM (CLMID nm) = do
    s <- get
    let env = currentEnvironment s
    case (lookupCLMBindingOrLambda env nm) of 
        Nothing -> pure $ CLMID nm
        Just ex -> pure ex
evalCLM (CLMAPP ex exs) = do
    exs' <- mapM evalCLM exs
    (CLMAPP <$> (evalCLM ex) <*> pure exs')
evalCLM e = do
    trace $ "Unimplemented eval for expr " ++ show e
    pure e

lookupCLMBindingOrLambda :: Environment -> Name -> Maybe CLMExpr
lookupCLMBindingOrLambda env nm = 
    let bnd = Map.lookup nm (clmBindings env)
    in  case bnd of
            Just (nm1,expr) -> Just expr
            Nothing -> let mlam = Map.lookup nm (clmLambdas env) in
                case mlam of 
                    Nothing -> Nothing
                    Just lm -> Just $ CLMLAM lm
