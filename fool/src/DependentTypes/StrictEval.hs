{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module DependentTypes.StrictEval where

import DependentTypes.Core
import DependentTypes.Eval
import State

import qualified Data.HashTable.IO as H
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)
import Control.Monad


{-
Some theory.
If we have f x y = x y, it translates to:
\x. \y. x y
then let's say we call f (1+1) (2 + 2).
Lazy evaluation goes like, left to right:
f (1+1) (2+2)
(\x. \y. x y) (1+1) (2+2)
(\y. (1+1) y) (2+2)
(1+1) (2+2)
2 (2+2)
2 4
When we are strict, we need to evaluate arguments first
(and also make sure it's not a partial application? or not really?)
f (1+1) (2+2)
f (1+1) 4
f 2 4
(\x. \y. x y) 2 4
(\y. 2 y) 4
2 4
What if it's partial?
Lazy:
f (1+1)
(\x. \y. x y) (1+1)
\y. (1+1) y
Strict:
f (1+1)
f 2
(\x. \y. x y) 2
\y. 2 y

So, in our AST (or close enough) it would look like:

-}


-------------------------------------------------------------------------------
-- Strict Eval (kindof?)
-------------------------------------------------------------------------------
{-

-- looking up global symbol by name: for now, only Functions
-- need to make it work for types and process local contexts (letins)
evalStep b e@(VarId nm) = do
  excan <- lookupGlobalSymbol nm
  case excan of
    Nothing -> return e
    Just ex ->
      if b then liftIO (putStrLn $ "Substituting " ++ nm ++ " to " ++ prettyPrint ex) >> return ex
      else return ex

evalStep b (Tuple nm exs) = Tuple nm <$> mapM (evalStep b) exs

-}

evalStepStrict b (Tuple nm exs) = Tuple nm <$> mapM (evalStepStrict b) exs

-- If now calculates ok for arithmetic
evalStepStrict b (If e1 e2 e3) = do
  e1' <- evalStepStrict b e1
  case e1' of (Lit (LBool bool)) -> if bool then evalStepStrict b e2 else evalStepStrict b e3
              _ -> If <$> pure e1' <*> evalStepStrict b e2 <*> evalStepStrict b e3


-- built-in operators. There must be a better way of doing this.
-- now implements basic comparisons and arithmetics that converts ints to floats
evalStepStrict b e@(App v@(VarId nm) arg@(Tuple _ [Lit e1, Lit e2]) ) = do
  let pmop = findPrimOp nm
  case pmop of Just op -> return $ Lit $ LFloat $ op (litToPrim e1) (litToPrim e2)
               Nothing -> do
                  let bmop = findBoolOp nm
                  case bmop of Nothing -> evalStepStrict b v >>= \newV -> return (App newV arg)
                               Just bop -> return $ Lit $ LBool $ bop (litToPrim e1) (litToPrim e2)


-- built-in operators. There must be a better way of doing this.
-- now implements basic comparisons and arithmetics that converts ints to floats
evalStepStrict b (App v@(VarId nm) t@(Tuple _ _) ) = do
    -- the only differece with lazy is that we evaluate tuple first
    t' <- evalStepStrict b t
    excan <- lookupGlobalSymbol nm
    case excan of
      Nothing -> return (App v t')
      Just ex ->
        if b then liftIO (putStrLn $ "Substituting " ++ nm ++ " to " ++ prettyPrint ex) >> return (App ex t')
        else return $ App ex t'





-- applying a Lambda - beta reduction
evalStepStrict b e@(App (Lam nm vars expr) t ) = do
    -- the only differece with lazy is that we evaluate tuple first
    (Tuple _ vals) <- evalStepStrict b t
    (e', vars', vals') <- doubleMap betaRun expr vars vals
    -- liftIO $ print t
    let retE
          | not (null vals') = FAIL ("Too many arguements applied in expr " ++ show e)
          | null vars' = e'
          | otherwise = Lam nm vars' e'
    -- liftIO $ print retE
    return retE

-- small step semantics
evalStepStrict b (App e1 e2) = do
  when b $ liftIO (putStrLn "evalStepStrict b (App e1 e2)")
  e2' <- evalStepStrict b e2
  e1' <- evalStepStrict b e1
  return $ App e1' e2'


evalStepStrict b e = evalStep b e
