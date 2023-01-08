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

import Core
import Data.HashMap.Strict as Map
-- import Core.Environment

import Util.IOLogger as Log
import Util.PrettyPrinting
import Logs as Logs

type NameMap = HashMap Name
type LTProgram = [(Expr, SourceInfo)]

-- structure keeping our current environment
data Environment = Environment {
    -- Map that keeps all our TypeReps in the current environment
    types       :: NameMap Expr,
    topLambdas  :: NameMap Lambda,
    outProgram  :: NameMap String
} deriving Show

initialEnvironment = Environment {
    types       = Map.empty,
    topLambdas  = Map.empty,
    outProgram   = Map.empty 
}


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

emptyIntState = InterpreterState {
    currentFlags = CurrentFlags False True False,
    parsedModule = [],
    currentSource = "",
    currentEnvironment = initialEnvironment
}

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

------------------ Monadic traversal of the Expr tree ---------------------
-- needed for optimizations, error checking etc

{-
data Expr =
  | Binding Var -- Var contains both the name and the expression to be bound to, used inside Actions etc
  | Function Lambda -- defining a function by abstracting a bunch of variables in a tuple
  | Action Lambda -- Action is simply a list of expressions in order
  | Constructors [Lambda] -- only for constructor list inside sum types
  | CaseOf Record Expr SourceInfo
  | SumType Lambda -- sum type definition, which is also basically a lambda with 
  -- body expression being a tuple of Lambdas which are constructors
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | Type -- U 0 synonim
-}

traverseExprM :: (Expr -> IntState Expr) -> Expr -> IntState Expr
traverseExprM f UNDEFINED = pure UNDEFINED
traverseExprM f (Typed e1 e2) = Typed <$> (f e1) <*> (f e2)
traverseExprM f (App e exs) = App <$> (f e) <*> (mapM f exs)
traverseExprM f (PatternMatches exs) = PatternMatches <$> (mapM f exs)
traverseExprM f (Tuple exs) = Tuple <$> (mapM f exs)
traverseExprM f (Statements exs) = Statements <$> (mapM f exs)
traverseExprM f (UnaryOp nm e) = UnaryOp <$> (pure nm) <*> (f e)
traverseExprM f e = f e

---------------------------- BASIC FUNCTIONS -----------------------------
lookupLambda :: Name -> Environment -> Maybe Lambda
lookupLambda n env = Map.lookup n (topLambdas env)

addLambda :: Name -> Lambda -> Environment -> Environment
addLambda n l env = env { topLambdas = Map.insert n l (topLambdas env) }

addNamedSumType :: Expr -> Environment -> Environment
addNamedSumType tp@(SumType lam) env = env { types = Map.insert (lamName lam) tp (types env) } 
addNamedSumType e env = env

addNamedLambda :: Lambda -> Environment -> Environment
addNamedLambda l env = env { topLambdas = Map.insert (lamName l) l (topLambdas env) }

addManyNamedLambdas :: [Lambda] -> Environment -> Environment
addManyNamedLambdas ls env = env { topLambdas = Prelude.foldl (\acc l1 -> Map.insert (lamName l1) l1 acc) (topLambdas env) ls }

addManyLambdas :: [(Name, Lambda)] -> Environment -> Environment
addManyLambdas ls env = env { topLambdas = Prelude.foldl (\acc (n1,l1) -> Map.insert n1 l1 acc) (topLambdas env) ls }


------------------ Monadic interface to the Environment ---------------------
-- lookupLambdaM :: Name -> IntState (Maybe Lambda)
-- lookupLambdaM n = get >>= pure . currentEnvironment >>= pure . (lookupLambda n)

-- outputs a message only if tracing is on
trace :: String -> IntState ()
trace msg = do
    tr <- currentFlags <$> get >>= pure . tracing
    if tr then liftIO (putStrLn msg) else pure ()

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
