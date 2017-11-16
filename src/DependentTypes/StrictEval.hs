{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module DependentTypes.StrictEval where

import DependentTypes.Core
import DependentTypes.Eval
import State

import qualified Data.HashTable.IO as H
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)

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
evalStepStrict b e@(App (App v@(VarId nm) (Lit e1)) (Lit e2)) = do
  let pmop = findPrimOp nm
  case pmop of Just op -> return $ Lit $ LFloat $ op (litToPrim e1) (litToPrim e2)
               Nothing -> do
                  let bmop = findBoolOp nm
                  case bmop of Nothing -> evalStepStrict b v >>= \newV -> return (App (App newV (Lit e1)) (Lit e2))
                               Just bop -> return $ Lit $ LBool $ bop (litToPrim e1) (litToPrim e2)


evalStepStrict b e@(App (Lam var expr) val) = do
  val' <- evalStepStrict b val -- evaluating argument first!!! (the only difference with lazy)
  let ex =  beta (varName var) expr val'
  if b then liftIO (putStrLn $ "Instantiating " ++ prettyPrint var ++ " to " ++ prettyPrint val ++ " in " ++ prettyPrint expr)
    >> return ex
  else return ex

-- small step semantics
evalStepStrict b (App e1 e2) = do
  e2' <- evalStepStrict b e2
  e1' <- evalStepStrict b e1
  return $ App e1' e2'

evalStepStrict b e = evalStep b e
