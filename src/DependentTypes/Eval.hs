{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module DependentTypes.Eval where

import DependentTypes.Core
import State

import qualified Data.HashTable.IO as H
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)


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

-- small evaluation step
-- if True, printing stacktrace, if False, quiet
evalStep :: Bool -> Expr -> IntState Expr

-- built-in operators. There must be a better way of doing this.
-- now implements basic comparisons and arithmetics that converts ints to floats
evalStep b e@(App (App v@(VarId nm) (Lit e1)) (Lit e2)) = do
  let pmop = findPrimOp nm
  case pmop of Just op -> return $ Lit $ LFloat $ op (litToPrim e1) (litToPrim e2)
               Nothing -> do
                  let bmop = findBoolOp nm
                  case bmop of Nothing -> evalStep b v >>= \newV -> return (App (App newV (Lit e1)) (Lit e2))
                               Just bop -> return $ Lit $ LBool $ bop (litToPrim e1) (litToPrim e2)



-- applying a Lambda - beta reduction
evalStep b e@(App (Lam var expr) val) = do
  let ex =  beta (varName var) expr val
  if b then liftIO (putStrLn $ "Instantiating " ++ prettyPrint var ++ " to " ++ prettyPrint val ++ " in " ++ prettyPrint expr)
    >> return ex
  else return ex

-- looking up global symbol by name: for now, only Functions
-- need to make it work for types and process local contexts (letins)
evalStep b e@(VarId nm) = do
  excan <- lookupGlobalSymbol nm
  case excan of
    Nothing -> return e
    Just ex ->
      if b then liftIO (putStrLn $ "Substituting " ++ nm ++ " to " ++ prettyPrint ex) >> return ex
      else return ex

-- small step semantics
evalStep b (App e1 e2) = do
  e1' <- evalStep b e1
  e2' <- evalStep b e2
  return $ App e1' e2'

-- If now calculates ok for arithmetic
evalStep b (If e1 e2 e3) = do
  e1' <- evalStep b e1
  case e1' of (Lit (LBool bool)) -> if bool then evalStep b e2 else evalStep b e3
              _ -> return $ If e1' e2 e3



evalStep b e = if b then liftIO (print e) >> return e else return e


-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

-- beta reduction. substituting a var with an expr: walking the tree and applying changes
-- this has to be abstracted and generalized
beta :: Name -> Expr -> Expr -> Expr

-- simlpy "instantiating" a variable if names are the same (hence, need unique names!)
beta nm e@(VarId vname) val = if nm == vname then val else e

-- if we have a lambda inside, going deeper
beta nm (Lam v e) val = Lam (upd v val) ( beta nm e val)
  -- in case we are instantiating a type variable
  where upd vr@(Id vrName (TVar tvarName)) val = if nm == tvarName then Id vrName (InsType val) else vr
        upd vr val = vr

-- If it's an application, simply going deeper
beta nm (App e1 e2) val = App ( beta nm e1 val) ( beta nm e2 val)

-- if it's an "if", going deeper
beta nm (If e1 e2 e3) val = If ( beta nm e1 val) ( beta nm e2 val) ( beta nm e3 val)

-- if it's a "letins", going deeper
beta nm (Let sym e1 e2) val = Let sym ( beta nm e1 val) ( beta nm e2 val)

-- processing tuple
beta nm (Tuple tnm exs) val = Tuple tnm (map fn exs) where fn ex = beta nm ex val
beta nm e val = e

lookupGlobalSymbol :: Name -> IntState (Maybe Expr)
lookupGlobalSymbol nm = do
  ls <- gets lambdas
  mayEx <- liftIO $ H.lookup ls nm
  case mayEx of Nothing -> return Nothing
                ex -> return ex
