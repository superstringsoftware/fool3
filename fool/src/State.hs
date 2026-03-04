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

import Surface
import CLM
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
    types        :: NameMap Expr,
    constructors :: NameMap (Lambda, Int),
    -- constructors are stored with their number inside the sum type
    topLambdas   :: NameMap Lambda,
    topBindings  :: NameMap Var,
    clmLambdas   :: NameMap CLMLam,
    clmBindings  :: NameMap CLMVar,
    -- instance-specialized functions: key is "funcName\0typeName"
    instanceLambdas :: NameMap Lambda,
    clmInstances    :: NameMap CLMLam,
    -- structure inheritance: maps child structure name to list of parent names
    structInheritance :: NameMap [Name],
    outProgram   :: NameMap String
} deriving Show

initialEnvironment = Environment {
    types       = Map.empty,
    constructors = Map.empty,
    topLambdas  = Map.empty,
    clmLambdas = Map.empty,
    topBindings = Map.empty,
    clmBindings = Map.empty,
    instanceLambdas = Map.empty,
    clmInstances = Map.empty,
    structInheritance = Map.empty,
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
  | U Int -- Universe hierarchy
  | Instance Name [Expr] [Expr] -- instance declarations
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

lookupConstructor :: Name -> Environment -> Maybe (Lambda, Int)
lookupConstructor n env = Map.lookup n (constructors env)

lookupType :: Name -> Environment -> Maybe Expr
lookupType n env = Map.lookup n (types env)

addLambda :: Name -> Lambda -> Environment -> Environment
addLambda n l env = env { topLambdas = Map.insert n l (topLambdas env) }

addCLMLambda :: Name -> CLMLam -> Environment -> Environment
addCLMLambda n l env = env { clmLambdas = Map.insert n l (clmLambdas env) }

addNamedSumType :: Expr -> Environment -> Environment
addNamedSumType tp@(SumType lam) env = env { types = Map.insert (lamName lam) tp (types env) } 
addNamedSumType e env = env

addNamedStructure :: Expr -> Environment -> Environment
addNamedStructure st@(Structure lam _si) env = env { types = Map.insert (lamName lam) st (types env) }
addNamedStructure e env = env

-- Register structure inheritance: child extends parents
registerInheritance :: Name -> [Name] -> Environment -> Environment
registerInheritance child parents env =
    env { structInheritance = Map.insert child parents (structInheritance env) }

-- Get all transitive parents of a structure
getAllParents :: Name -> Environment -> [Name]
getAllParents name env = go [name] []
  where
    go [] visited = visited
    go (n:ns) visited
      | n `Prelude.elem` visited = go ns visited
      | otherwise = case Map.lookup n (structInheritance env) of
          Nothing      -> go ns (visited ++ [n])
          Just parents -> go (parents ++ ns) (visited ++ [n])

addNamedLambda :: Lambda -> Environment -> Environment
addNamedLambda l env = env { topLambdas = Map.insert (lamName l) l (topLambdas env) }

addManyNamedLambdas :: [Lambda] -> Environment -> Environment
addManyNamedLambdas ls env = env { topLambdas = Prelude.foldl (\acc l1 -> Map.insert (lamName l1) l1 acc) (topLambdas env) ls }

addNamedConstructor :: Int -> Lambda -> Environment -> Environment
addNamedConstructor i l env = env { constructors = Map.insert (lamName l) (l,i) (constructors env) }

addManyNamedConstructors :: Int -> [Lambda] -> Environment -> Environment
addManyNamedConstructors i []     env = env
addManyNamedConstructors i (c:cs) env = addManyNamedConstructors (i+1) cs (addNamedConstructor i c env)

-- addManyNamedConstructors :: [Lambda] -> Environment -> Environment
-- addManyNamedConstructors ls env = env { constructors = Prelude.foldl (\acc l1 -> Map.insert (lamName l1) l1 acc) (topLambdas env) ls }

-- Instance functions: keyed by "funcName\0typeName"
mkInstanceKey :: Name -> Name -> Name
mkInstanceKey funcName typeName = funcName ++ "\0" ++ typeName

addInstanceLambda :: Name -> Name -> Lambda -> Environment -> Environment
addInstanceLambda funcNm typeNm lam env =
    env { instanceLambdas = Map.insert (mkInstanceKey funcNm typeNm) lam (instanceLambdas env) }

lookupInstanceLambda :: Name -> Name -> Environment -> Maybe Lambda
lookupInstanceLambda funcNm typeNm env = Map.lookup (mkInstanceKey funcNm typeNm) (instanceLambdas env)

addCLMInstance :: Name -> Name -> CLMLam -> Environment -> Environment
addCLMInstance funcNm typeNm clm env =
    env { clmInstances = Map.insert (mkInstanceKey funcNm typeNm) clm (clmInstances env) }

lookupCLMInstance :: Name -> Name -> Environment -> Maybe CLMLam
lookupCLMInstance funcNm typeNm env = Map.lookup (mkInstanceKey funcNm typeNm) (clmInstances env)

-- Reverse lookup: given a constructor name, find which type it belongs to
lookupTypeOfConstructor :: Name -> Environment -> Maybe Name
lookupTypeOfConstructor consName env =
    case Map.lookup consName (constructors env) of
        Just (lam, _) -> case lamType lam of
            Id nm -> Just nm
            _     -> Nothing
        Nothing -> Nothing

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
