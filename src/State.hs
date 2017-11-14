{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module State where

import qualified Data.HashTable.IO as H

import DependentTypes.Core
import Syntax

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state

type IntState a = StateT InterpreterState IO a

type HashTable k v = H.BasicHashTable k v
type ExpressionTable = HashTable Name FlExpr
type CoreExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    funTable :: ExpressionTable, -- global function and operator table
    symTable :: ExpressionTable, -- global symbol table for variable bidnings
    localSymTable :: ExpressionTable, -- local table in the current scope (e.g., when processing a function call)
    typeTable :: ExpressionTable,
    logs     :: [String],
    lambdas  :: CoreExpressionTable -- here we will store named lambda expressions for *both Constructors AND normal functions!!!*
} deriving Show
