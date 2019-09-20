{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

-- State / IO monad where we are doing all our transformations etc
-- So, our parser needs to work in this monad as well

module State where

import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict 
import Control.Monad.Trans.Class (lift)

import Data.Text as L

import Data.Sequence as S

import Lambda.Syntax
import Lambda.Environment

import Util.IOLogger as Log
import Util.PrettyPrinting
import Lambda.Logs as Logs

-- scary, building a stack - stacking IO inside logger monad
-- type IntState = StateT InterpreterState IO
type IntState = StateT InterpreterState LambdaLoggerMonad

-- function to run IntState monad on top of Logger state monad
runIntState act s = evalStateT (evalStateT act s) initLogState
--type HashTable k v = H.BasicHashTable k v
--type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    currentFlags :: CurrentFlags,
    -- this is being filled by the parser as we go, so last line in the file will be first here!
    parsedModule :: LTProgram,
    currentSource :: Text,
    currentEnvironment :: Environment
} deriving Show

data CurrentFlags = CurrentFlags {
    strict    :: Bool -- true if strict, false if lazy
  , pretty    :: Bool -- pretty print or raw output
  , tracing   :: Bool -- whether to trace execution steps
} deriving Show

initializeInterpreter :: IO InterpreterState
initializeInterpreter = return $ InterpreterState {
    currentFlags = CurrentFlags False True False,
    parsedModule = [],
    currentSource = "",
    currentEnvironment = initialEnvironment
}

-- lifted versions of the IOLogger monad functions
logError :: LogPayload -> IntState ()
logError    = lift . Log.logError
logWarning :: LogPayload -> IntState ()
logWarning  = lift . Log.logWarning
logInfo :: LogPayload -> IntState ()
logInfo     = lift . Log.logInfo
logVerbose :: LogPayload -> IntState ()
logVerbose  = lift . Log.logVerbose
logDebug :: LogPayload -> IntState ()
logDebug    = lift . Log.logDebug
logTrace :: LogPayload -> IntState ()
logTrace    = lift . Log.logTrace

showAllLogsWSource :: IntState ()
showAllLogsWSource = do
    src <- currentSource <$> get
    lift (Logs.showAllLogsWSource src)

clearAllLogs :: IntState ()
clearAllLogs = lift Log.clearAllLogs    

getAllLogs :: IntState (Seq (LogMessage LogPayload))
getAllLogs = lift Log.getAllLogs

showAllLogs :: IntState ()
showAllLogs = State.getAllLogs >>= \logs -> liftIO (mapM_ (putStrLn . ppr) logs)
