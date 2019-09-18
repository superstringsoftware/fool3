{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

-- State / IO monad where we are doing all our transformations etc
-- So, our parser needs to work in this monad as well

module State where

import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict 

import Data.Text

import Lambda.Syntax

type IntState = StateT InterpreterState IO

-- some info for debugging to attach to initially parsed module expressions
data SourceInfo = SourceInfo {
    lineNum :: !Int, colNum :: !Int, notes :: Text
} deriving Show

type LTProgram = [(Expr, SourceInfo)]

--type HashTable k v = H.BasicHashTable k v
--type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    logs     :: [String],
    currentFlags :: CurrentFlags,
    -- this is being filled by the parser as we go, so last line in the file will be first here!
    parsedModule :: LTProgram 
} deriving Show

data CurrentFlags = CurrentFlags {
    strict    :: Bool -- true if strict, false if lazy
  , pretty    :: Bool -- pretty print or raw output
  , tracing   :: Bool -- whether to trace execution steps
} deriving Show

initializeInterpreter :: IO InterpreterState
initializeInterpreter = return $ InterpreterState {
    logs = [],
    currentFlags = CurrentFlags False True False,
    parsedModule = []
}