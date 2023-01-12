{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Interpreter
where

import Core
import CLM
import State
import Pipeline
import Util.PrettyPrinting

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.HashMap.Strict as Map

import Data.Traversable.WithIndex

addBinding :: Expr -> IntState ()
addBinding ex@(Binding _) = do
    s <- get
    let env = currentEnvironment s
    -- ex' <- fixEmptyConstructors ex
    let clmex@(CLMBIND nm expr) = exprToCLM env ex
    let bnd = Map.insert nm (nm, expr) (clmBindings env)
    let s' = s { currentEnvironment = env { clmBindings = bnd } }
    liftIO $ putStrLn $ "Added binding " ++ ppr ex
    put s'
addBinding e = liftIO $ putStrLn $ "Cant add binding for expr " ++ ppr e

processInteractive :: Expr -> IntState ()
processInteractive ex = do
    s <- get
    let env = currentEnvironment s
    let clmex = exprToCLM env ex
    liftIO $ putStrLn $ "CLM:\n" ++ ppr clmex
    case clmex of
        CLMID nm -> do 
            liftIO $ putStrLn $ "Looking up id " ++ show nm
            let mid = Map.lookup nm (clmBindings env)
            case mid of 
                Nothing -> liftIO $ putStrLn "Nothing found"
                Just v@(nm,ex)  -> liftIO $ putStrLn $ ppr (CLMBIND nm ex)
        ex1@(CLMAPP _ _) -> do
            ex1' <- evalCLM 0 ex1
            exf <- _contEval 1 ex1 ex1'
            liftIO $ putStrLn "Final result:"
            liftIO $ putStrLn $ ppr exf
        
        _ -> liftIO $ putStrLn "Not implemented yet"

_contEval :: Int -> CLMExpr -> CLMExpr -> IntState CLMExpr
_contEval i e1 e2 = if (e1 == e2) then pure e1 
    else do (evalCLM i e2) >>= \e' -> _contEval (i+1) e2 e'

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

evalCLM :: Int -> CLMExpr -> IntState CLMExpr
evalCLM i (CLMID nm) = do
    s <- get
    let env = currentEnvironment s
    trace $ "Step " ++ show i ++ ": encountered id " ++ nm 
    case (lookupCLMBindingOrLambda env nm) of 
        Nothing -> do 
            trace $ "nothing found in the environment, returning id"
            pure $ CLMID nm
        Just ex -> do 
            trace $ "substituting " ++ nm ++ " for " ++ ppr ex
            pure ex
evalCLM i e@(CLMCON (ConsTag nm k) exs) = do            
    trace $ "Step " ++ show i ++ ": encountered CLMCON " ++ ppr e
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    pure $ CLMCON (ConsTag nm k) exs'
evalCLM i e@(CLMAPP (CLMLAM lam) exs) = do
    trace $ "Step " ++ show i ++ ": encountered CLMAPP of lambda" ++ ppr e
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    pure $ applyCLMLam lam exs'
evalCLM i e@(CLMAPP ex exs) = do
    trace $ "Step " ++ show i ++ ": encountered CLMAPP " ++ ppr e
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    (CLMAPP <$> (evalCLM (i+(length exs)+1) ex) <*> pure exs')
evalCLM i e = do
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
