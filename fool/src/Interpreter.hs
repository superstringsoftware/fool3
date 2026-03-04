{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Interpreter
where

import Surface
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
processInteractive ex0 = do
    -- apply afterparse desugaring (binary ops -> App calls etc)
    let ex = afterparse $ traverseExpr afterparse ex0
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
        ex1@(CLMIAP _ _) -> do
            ex1' <- evalCLM 0 ex1
            exf <- _contEval 1 ex1 ex1'
            liftIO $ putStrLn "Final result:"
            liftIO $ putStrLn $ ppr exf
        ex1@(CLMCON _ _) -> do
            liftIO $ putStrLn $ ppr ex1
        ex1@(CLMLIT _) -> do
            liftIO $ putStrLn $ ppr ex1
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
evalCLM i e@(CLMFieldAccess ("", fnum) (CLMCON ct tuple)) = do
    -- should we eval inside tuple first here or it's already done??
    trace $ "Step " ++ show i ++ ": encountered CLMFieldAccess " ++ ppr e
    if (fnum > Prelude.length tuple) then pure $ CLMERR $ "ERROR: tried to access field beyond tuple index in " ++ ppr e
    else pure (tuple Prelude.!! fnum)
-- evaluate inner expression first, then retry field access
evalCLM i e@(CLMFieldAccess acc inner) = do
    trace $ "Step " ++ show i ++ ": encountered CLMFieldAccess (eval inner) " ++ ppr e
    inner' <- evalCLM (i+1) inner
    if inner' == inner then pure e
    else evalCLM (i+1) (CLMFieldAccess acc inner')
-- literals are values
evalCLM _ e@(CLMLIT _) = pure e
-- CLMPROG: evaluate expressions in sequence, return last
evalCLM i (CLMPROG []) = pure CLMEMPTY
evalCLM i (CLMPROG [ex]) = evalCLM i ex
evalCLM i (CLMPROG (ex:exs)) = do
    _ <- evalCLM i ex
    evalCLM (i+1) (CLMPROG exs)
-- CLMCASE: evaluate constructor tag checks, return body if all match
-- Note: CLMCASE is inside CLMLamCases, so it's handled by applyCLMLam.
-- But we may encounter it during evaluation if it hasn't been resolved yet.
evalCLM i e@(CLMCASE ctchecks body) = do
    trace $ "Step " ++ show i ++ ": encountered CLMCASE " ++ ppr e
    if evalConsTagChecks ctchecks
    then evalCLM (i+1) body
    else pure e -- case didn't match, return as-is
-- CLMIAP: implicit param application (structure function dispatch)
-- Strategy: evaluate args, determine type from constructor tags,
-- look up instance-specialized function, apply it
evalCLM i e@(CLMIAP (CLMID funcNm) exs) = do
    trace $ "Step " ++ show i ++ ": encountered CLMIAP " ++ ppr e
    s <- get
    let env = currentEnvironment s
    -- evaluate arguments first
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    -- try to determine the type from the first argument
    let mTypeName = inferTypeFromExpr env (Prelude.head exs')
    trace $ "Inferred type: " ++ show mTypeName
    case mTypeName of
        Just typeNm -> do
            -- look up instance-specialized function
            case lookupCLMInstance funcNm typeNm env of
                Just clmLam -> do
                    trace $ "Found instance " ++ funcNm ++ " for " ++ typeNm
                    pure $ applyCLMLam clmLam exs'
                Nothing -> do
                    trace $ "No instance found for " ++ funcNm ++ "$" ++ typeNm ++ ", using default"
                    -- fall back to the general function, skipping the implicit param
                    case Map.lookup funcNm (clmLambdas env) of
                        Just clmLam -> pure $ applyCLMLam clmLam [CLMID typeNm]
                        Nothing -> pure $ CLMERR $ "No function or instance found for " ++ funcNm
        Nothing -> do
            -- can't determine type, return with evaluated args
            trace $ "Could not infer type for CLMIAP " ++ funcNm
            pure $ CLMIAP (CLMID funcNm) exs'
-- CLMIAP with non-id function: evaluate the function first
evalCLM i e@(CLMIAP ex exs) = do
    trace $ "Step " ++ show i ++ ": encountered CLMIAP (eval func) " ++ ppr e
    ex' <- evalCLM (i+1) ex
    exs' <- imapM (\j e1 -> evalCLM (i+j+2) e1) exs
    if ex' == ex && exs' == exs then pure e
    else pure $ CLMIAP ex' exs'
evalCLM i e = do
    trace $ "Unimplemented eval for expr " ++ show e
    pure e

-- Infer the type name from a CLM expression by looking at constructor tags
inferTypeFromExpr :: Environment -> CLMExpr -> Maybe Name
inferTypeFromExpr env (CLMCON (ConsTag consNm _) _) = lookupTypeOfConstructor consNm env
inferTypeFromExpr env (CLMLIT (LInt _)) = Just "Int"
inferTypeFromExpr env (CLMLIT (LFloat _)) = Just "Float"
inferTypeFromExpr env (CLMLIT (LString _)) = Just "String"
inferTypeFromExpr env (CLMLIT (LChar _)) = Just "Char"
inferTypeFromExpr _ _ = Nothing

lookupCLMBindingOrLambda :: Environment -> Name -> Maybe CLMExpr
lookupCLMBindingOrLambda env nm = 
    let bnd = Map.lookup nm (clmBindings env)
    in  case bnd of
            Just (nm1,expr) -> Just expr
            Nothing -> let mlam = Map.lookup nm (clmLambdas env) in
                case mlam of 
                    Nothing -> Nothing
                    Just lm -> Just $ CLMLAM lm
