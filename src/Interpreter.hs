{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.List (sortBy)
import Data.Function (on)

import qualified Data.HashTable.IO as H

import DependentTypes.Core
import Parser

import TermColors
import Data.Char (isUpper)

import Control.Monad (zipWithM_, void, when)

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)


import State
import DependentTypes.Eval
import DependentTypes.StrictEval


-- initializing starting state with tables etc
initializeInterpreter :: IO InterpreterState
initializeInterpreter = do
    ft <- H.new
    st <- H.new
    lt <- H.new
    tt <- H.new
    lam <- H.new
    return InterpreterState {
                funTable = ft,
                typeTable = tt,
                symTable = st,
                localSymTable = lt,
                logs = [],
                lambdas = lam,
                currentFlags = CurrentFlags {
                  strict = False
                , pretty = True
                , tracing = False
                }
             }

-- process a single expression and alter the interpreter state correspondigly
processExpr :: Expr -> IntState ()

-- binding functions and ops
processExpr e@(Lam name _ _) = do
  ls <- gets lambdas
  liftIO $ H.insert ls name (desugar e)

-- executing binary op
-- processExpr e@(BinaryOp name _ _) = processExprGeneric False e
-- processExpr e@(FlApp _ _) = processExprGeneric False e
-- processExpr e@(SymId _) = processExprGeneric False e

processExpr e = void (evalExpr False (desugar e))


-- evaluate expression until it stops simplifying and show it nicely
evalExpr :: Bool -> Expr -> IntState Expr
evalExpr b ex = do
  fl <- gets currentFlags
  let evalFunc = if strict fl then evalStepStrict else evalStep
  let printFunc = if pretty fl then prettyPrintTopLevel else show
  fn 1 b ex evalFunc printFunc (tracing fl) where
        fn i b ex ef pf tb = do
                      -- liftIO $ putStrLn $ "Iteration " ++ show i
                      ex' <- ef b ex
                      if ex == ex' then liftIO (putStrLn $ pf ex') >> return ex
                      else if tb then liftIO (putStrLn $ "[" ++ show i ++ "]\t" ++ pf ex') >> fn (i+1) b ex' ef pf tb
                           else fn (i+1) b ex' ef pf tb


-- getting main function from the function table
-- findMain :: IntState (Maybe Expr)
{-
findMain = do
  ls <- gets lambdas
  return $ liftIO $ H.lookup ls "main"
-}

-- print types
prettyPrintTT :: ExpressionTable -> IO ()
prettyPrintTT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ prettyPrint v

-- print functions
prettyPrintFT :: ExpressionTable -> IO ()
prettyPrintFT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ prettyPrint v

-- print symbols
prettyPrintST :: ExpressionTable -> IO ()
prettyPrintST ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ (k ++ " ≡ " ++ (show v))


prettyPrintLS :: CoreExpressionTable -> IO ()
prettyPrintLS ls = do
    list <- H.toList ls
    let res = sortBy (compare `on` fst) list
    mapM_ f res where
    f (k,v) = putStrLn $ clrLam k ++ " = " ++ prettyPrintTopLevel v


showLS :: CoreExpressionTable -> IO ()
showLS ls = H.mapM_ f ls
          where f (k,v) = putStrLn $ k ++ " = " ++ show v
