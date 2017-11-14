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
litToPrim (Lit (LInt x)) = fromIntegral x
litToPrim (Lit (LFloat x)) = x

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

-- evaluate expression until it stops simplifying (how do we know when to stop?)
evalExpr :: Bool -> Expr -> IntState Expr
evalExpr b ex = do
  ex' <- evalStep b ex
  if ex == ex' then return ex else (liftIO $ putStrLn $ prettyPrint ex') >> evalExpr b ex'

-- small evaluation step
-- if True, printing stacktrace, if False, quiet
evalStep :: Bool -> Expr -> IntState Expr
-- built-in operators. There must be a better way of doing this.
{-
evalStep b (App (App (VarId "(+)") e1) e2) = do
  e1' <- evalStep b e1
  e2' <- evalStep b e2
  return $ Lit $ LFloat $ litToPrim e1' + litToPrim e2'
-}

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
