{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.List (sortBy)
import Data.Function (on)

import qualified Data.HashTable.IO as H

import TermColors
import Data.Char (isUpper)

import Control.Monad (zipWithM_, void, when)

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)

import State
import DotNet.Syntax


-- initializing starting state with tables etc
initializeInterpreter :: IO InterpreterState
initializeInterpreter = do
    ft <- H.new
    st <- H.new
    lt <- H.new
    tt <- H.new
    return InterpreterState {
                funTable = ft,
                typeTable = tt,
                symTable = st,
                localSymTable = lt,
                logs = [],
                currentFlags = CurrentFlags {
                  strict = False
                , pretty = True
                , tracing = False
                }
             }

-- processing new surface expression language             
processSurfaceExpr :: Expr -> IntState ()

-- inserting types into table
processSurfaceExpr e@(Type name _ _) = do
  ts <- gets typeTable
  liftIO $ H.insert ts name e

processSurfaceExpr e@(Lam name _ _ _) = do
  fs <- gets funTable
  liftIO $ H.insert fs name e
  

-- print types
prettyPrintTT :: ExpressionTable -> IO ()
prettyPrintTT = H.mapM_ f where
    f (k,v) = putStrLn $ show v

-- print functions
prettyPrintFT :: ExpressionTable -> IO ()
prettyPrintFT = H.mapM_ f where
    f (k,v) = putStrLn $ show v

{-
-- print symbols
prettyPrintST :: ExpressionTable -> IO ()
prettyPrintST ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ (k ++ " ≡ " ++ (show v))
-}
