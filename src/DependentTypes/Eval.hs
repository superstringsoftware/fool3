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

{-
data Expr
  = Lit Literal
  | VarId Name -- for bound variables and functions???
  | VarIn Int  -- variable index for building constructor function values mostly
  | Lam Var  Expr
  | App Expr Expr
  | If  Expr Expr Expr -- will get rid of this once case patterns are in, since we can model it with a function
  | Let Name Expr Expr -- ok, need to figure out how GHC does it - here we are binding first Expr to symbol Name in 2nd Expr
  | Tuple Name [Expr] TypeOrKind
-}

-- evaluate expression until it stops simplifying
evalExpr :: Bool -> Expr -> IntState Expr
evalExpr b ex = fn 1 b ex
  where fn i b ex = do
                      ex' <- evalStep b ex
                      if ex == ex' then return ex
                      else do
                        liftIO $ putStrLn $ "[" ++ show i ++ "]\t" ++ prettyPrintTopLevel ex'
                        fn (i+1) b ex'

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
evalStep b e@(App (App v@(VarId nm) (Lit e1)) (Lit e2)) = do
  let pmop = findPrimOp nm
  case pmop of Just op -> return $ Lit $ LFloat $ op (litToPrim e1) (litToPrim e2)
               Nothing -> do
                  let bmop = findBoolOp nm
                  case bmop of Nothing -> evalStep b v >>= \newV -> return (App (App newV (Lit e1)) (Lit e2))
                               Just bop -> return $ Lit $ LBool $ bop (litToPrim e1) (litToPrim e2)



-- applying a Lambda - substituting
evalStep b e@(App (Lam var expr) val) = do
  let ex = replaceSymbolWithExpr (varName var) expr val
  if b then (liftIO $ putStrLn $ "Instantiating " ++ prettyPrint var ++ " to " ++ prettyPrint val ++ " in " ++ prettyPrint expr) >> return ex
  else return ex

evalStep b e@(VarId nm) = do
  excan <- lookupGlobalSymbol nm
  case excan of
    Nothing -> return e
    Just ex ->
      if b then (liftIO $ putStrLn $ "Substituting " ++ nm ++ " to " ++ prettyPrint ex) >> return ex
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



evalStep b e = if b then return e else return e -- liftIO (print e) >>

-- substituting a var with an expr: walking the tree and applying changes
-- this has to be abstracted and generalized
replaceSymbolWithExpr :: Name -> Expr -> Expr -> Expr
replaceSymbolWithExpr nm e@(VarId vname) val = if nm == vname then val else e
replaceSymbolWithExpr nm (Lam v e) val = Lam v (replaceSymbolWithExpr nm e val)
replaceSymbolWithExpr nm (App e1 e2) val = App (replaceSymbolWithExpr nm e1 val) (replaceSymbolWithExpr nm e2 val)
replaceSymbolWithExpr nm (If e1 e2 e3) val = If (replaceSymbolWithExpr nm e1 val) (replaceSymbolWithExpr nm e2 val) (replaceSymbolWithExpr nm e3 val)
replaceSymbolWithExpr nm e val = e

lookupGlobalSymbol :: Name -> IntState (Maybe Expr)
lookupGlobalSymbol nm = do
  ls <- gets lambdas
  mayEx <- liftIO $ H.lookup ls nm
  case mayEx of Nothing -> return Nothing
                ex -> return ex
