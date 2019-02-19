{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

-- State / IO monad where we are doing all our transformations etc
-- So, our parser needs to work in this monad as well

module State where

import qualified Data.HashTable.IO as H

import Control.Monad.IO.Class (liftIO)

import DotNet.Syntax

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state

type IntState = StateT InterpreterState IO

type HashTable k v = H.BasicHashTable k v
type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    funTable :: ExpressionTable, -- global function and operator table
    symTable :: ExpressionTable, -- global symbol table for variable bidnings
    localSymTable :: ExpressionTable, -- local table in the current scope (e.g., when processing a function call)
    typeTable :: ExpressionTable,
    typeClassTable :: HashTable Name Typeclass,
    logs     :: [String],
    currentFlags :: CurrentFlags
} deriving Show

data CurrentFlags = CurrentFlags {
    strict    :: Bool -- true if strict, false if lazy
  , pretty    :: Bool -- pretty print or raw output
  , tracing   :: Bool -- whether to trace execution steps
} deriving Show

putTypeclass :: Typeclass -> IntState ()
putTypeclass tc = do
  ts <- gets typeClassTable
  liftIO $ H.insert ts (className tc) tc
