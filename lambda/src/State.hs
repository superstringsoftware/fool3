{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

-- State / IO monad where we are doing all our transformations etc
-- So, our parser needs to work in this monad as well

module State where

import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict 

import Data.Text as L

import Data.Sequence as S

import Lambda.Syntax
import Lambda.Environment

import Util.Logger

type LogPayload = Text

-- lambda logger monad is a state monad that lies on top of IO
type LambdaLoggerMonad = LoggerMonadT LogPayload IO
-- scary, building a stack - stacking IO inside logger monad
-- type IntState = StateT InterpreterState IO
type IntState = StateT InterpreterState LambdaLoggerMonad

initLogState = LogState {
    logLevel = 6,
    logs = S.empty
}

-- function to run IntState monad on top of Logger state monad
runIntState act s = evalStateT (evalStateT act s) initLogState
--type HashTable k v = H.BasicHashTable k v
--type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    errors   :: [SourceInfo],
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
    errors = [],
    currentFlags = CurrentFlags False True False,
    parsedModule = [],
    currentSource = "",
    currentEnvironment = initialEnvironment
}

clearErrors :: IntState ()
clearErrors = modify' (\s-> s { errors = []})

showAllErrors :: IntState ()
showAllErrors = do
    s <- get
    let src  = currentSource s
    let errs = errors s
    liftIO $ mapM_ (\err -> putStrLn $ showErrorWithSource src err ) errs

showErrorWithSource :: L.Text -> SourceInfo -> String
showErrorWithSource s err = L.unpack $ L.unlines [
      "  ",
      "  " <> lineContents,
      "  " <> ((L.replicate col " ") <> "^^^"),
      (L.pack $ show err)
    ]
  where
    lineContents = (L.lines s) !! line
    line = lineNum err - 1
    col  = fromIntegral $ colNum err - 1

logError :: SourceInfo -> IntState ()
logError er = do
    s <- get
    let l = errors s
    put $ s { errors = er:l }

instance Show SourceInfo where
    show (SourceInfo l c note) = "At line " ++ show l ++ ", column " ++ show c ++ ": " ++ L.unpack note