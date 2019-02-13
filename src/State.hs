{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module State where

import qualified Data.HashTable.IO as H

import DotNet.Syntax

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state

type IntState a = StateT InterpreterState IO a

type HashTable k v = H.BasicHashTable k v
type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    funTable :: ExpressionTable, -- global function and operator table
    symTable :: ExpressionTable, -- global symbol table for variable bidnings
    localSymTable :: ExpressionTable, -- local table in the current scope (e.g., when processing a function call)
    typeTable :: ExpressionTable,
    logs     :: [String],
    currentFlags :: CurrentFlags
} deriving Show

data CurrentFlags = CurrentFlags {
    strict    :: Bool -- true if strict, false if lazy
  , pretty    :: Bool -- pretty print or raw output
  , tracing   :: Bool -- whether to trace execution steps
} deriving Show
