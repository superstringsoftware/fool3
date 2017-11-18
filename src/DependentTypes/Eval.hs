{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module DependentTypes.Eval where

import DependentTypes.Core
import State

import qualified Data.HashTable.IO as H
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)
import Control.Monad


-------------------------------------------------------------------------------
-- Eval: we are only doing it on core!
-------------------------------------------------------------------------------
-- litToPrim :: Expr -> forall a. Num a => a
litToPrim (LInt x) = fromIntegral x
litToPrim (LFloat x) = x

-- helper in arithmetic ops for eval
findPrimOp "(+)" = Just (+)
findPrimOp "(-)" = Just (-)
findPrimOp "(*)" = Just (*)
findPrimOp "(/)" = Just (/)
findPrimOp _ = Nothing

findBoolOp "(==)" = Just (==)
findBoolOp "(<=)" = Just (<=)
findBoolOp "(>=)" = Just (>=)
findBoolOp "(>)" = Just (>)
findBoolOp "(<)" = Just (<)
findBoolOp _ = Nothing


-- function we are feeding to doubleMap when processing App (Lam...) in eval
-- to do beta reduction over lists of vars and vals
betaRun expr var val = do
  -- liftIO $ print $ show var ++ " --> " ++ show val ++ " in: " ++ show expr
  return $ betaReduce (varName var) val expr
  -- return $ beta (varName var) expr val

-- handy function that maps over 2 lists and accumulates accumulator
-- with an action and makes sure we don't go beyond size of any of them
-- doubleMap :: Monad m => (c -> b -> a -> m c) -> c -> [b] -> [a] -> m (c, [b], [a])
doubleMap action acc l1 l2 =
  if not (null l1 || null l2) then
    action acc (head l1) (head l2) >>= \acc' ->
      doubleMap action acc' (tail l1) (tail l2)
  else return (acc, l1, l2) -- returning accumulator AND remaining parts of lists over which we have mapped

-- small evaluation step
-- if True, printing stacktrace, if False, quiet
evalStep :: Bool -> Expr -> IntState Expr

-- built-in operators. There must be a better way of doing this.
-- now implements basic comparisons and arithmetics that converts ints to floats
evalStep b e@(App v@(VarId nm) arg@(Tuple _ [Lit e1, Lit e2]) ) = do
  let pmop = findPrimOp nm
  case pmop of Just op -> return $ Lit $ LFloat $ op (litToPrim e1) (litToPrim e2)
               Nothing -> do
                  let bmop = findBoolOp nm
                  case bmop of Nothing -> evalStep b v >>= \newV -> return (App newV arg)
                               Just bop -> return $ Lit $ LBool $ bop (litToPrim e1) (litToPrim e2)

-- applying a Lambda - beta reduction
evalStep b e@(App (Lam nm vars expr) (Tuple _ vals) ) = do
    -- liftIO $ putStrLn $ "Processing App for function " ++ nm ++ " of arity " ++ show (length vars) ++ " and " ++ show (length vals) ++ " arguments"
    -- liftIO $ print e
    -- now for the important part: beta reduction inside our App
    -- we are mapping over lists of vars and vals, returning new expr (with substitutions)
    -- and remaining vars and vals - to return either partially applied function (if vars' is not empty)
    -- or a Value (if vars' and vals' are both empty)
    -- remaining case - when vars' is empty and vals' is not - we are returning App (Tuple...) (Tuple...) -
    -- which is nonsense so should be an error - need to think how to handle it
    t@(e', vars', vals') <- doubleMap betaRun expr vars vals
    -- liftIO $ print t
    let retE
          | not (null vals') = FAIL ("Too many arguements applied in expr " ++ show e)
          | null vars' = e'
          | otherwise = Lam nm vars' e'
    -- liftIO $ print retE
    return retE

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

-- If now calculates ok for arithmetic
evalStep b (If e1 e2 e3) = do
  e1' <- evalStep b e1
  case e1' of (Lit (LBool bool)) -> if bool then evalStep b e2 else evalStep b e3
              _ -> return $ If e1' e2 e3


-- small step semantics
evalStep b (App e1 e2) = do
  when b $ liftIO (putStrLn "evalStep b (App e1 e2)")
  e1' <- evalStep b e1
  e2' <- evalStep b e2
  return $ App e1' e2'

evalStep b e = if b then liftIO (print e) >> return e else return e



lookupGlobalSymbol :: Name -> IntState (Maybe Expr)
lookupGlobalSymbol nm = do
  ls <- gets lambdas
  mayEx <- liftIO $ H.lookup ls nm
  case mayEx of Nothing -> return Nothing
                ex -> return ex
