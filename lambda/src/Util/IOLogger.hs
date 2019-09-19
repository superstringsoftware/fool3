-- Like Logger, but stacked with the IO monad to handle automatic timestamps, file outputs etc
{-# LANGUAGE OverloadedStrings, RecordWildCards, MultiParamTypeClasses #-}


module Util.IOLogger 
(
    LogMessage(..),
    LogLevel(..),
    LoggerMonadIO,

    initLogState,
    
    setTrace,
    setDebug,
    setVerbose,
    setInfo,
    setWarning,
    setError,
    setNone,

    logError,
    logWarning,
    logInfo,
    logVerbose,
    logDebug,
    logTrace,
    
    getCurrentLevel,
    getAllLogs
)
where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Sequence as S

import Data.Time.Clock.System
import Data.Time.Clock

import Util.PrettyPrinting

-- import Control.Monad.Trans.Class (lift)

data LogMessage e = LogMessage {
    payload :: e,
    timestamp :: SystemTime,
    level :: LogLevel
}

data LogLevel = LogNone
    | LogError 
    | LogWarning 
    | LogInfo 
    | LogVerbose 
    | LogDebug 
    | LogTrace 
    | LogLevelMax
    deriving (Show, Eq, Ord)

data LogState e = LogState {
    logLevel :: !LogLevel,
    logs :: Seq (LogMessage e)
}

initLogState = LogState {
    logLevel = LogLevelMax,
    logs = S.empty
}

-- monad transformer for our logger monad, with IO inside
type LoggerMonadIO e = StateT (LogState e) IO

setCurrentLevel :: LogLevel -> LoggerMonadIO e ()
setCurrentLevel i = modify' $ \s -> s{logLevel = i}

setTrace   = setCurrentLevel LogTrace
setDebug   = setCurrentLevel LogDebug
setVerbose = setCurrentLevel LogVerbose
setInfo    = setCurrentLevel LogInfo
setWarning = setCurrentLevel LogWarning
setError   = setCurrentLevel LogError
setNone    = setCurrentLevel LogNone

getCurrentLevel :: LoggerMonadIO e LogLevel
getCurrentLevel = logLevel <$> get

logError    = logMessage LogError
logWarning  = logMessage LogWarning
logInfo     = logMessage LogInfo
logVerbose  = logMessage LogVerbose
logDebug    = logMessage LogDebug
logTrace    = logMessage LogTrace

-- | logs the message if it's level is lower or equal to currently set
logMessage :: LogLevel -> e -> LoggerMonadIO e ()
logMessage lev msg' = do
    ts <- liftIO getSystemTime
    cl <- getCurrentLevel
    let msg = LogMessage msg' ts lev
    if clm msg cl 
    then logs <$> get >>= \lg -> modify' $ \s -> s { logs = lg |> msg }
    else pure ()
    where clm m cl = if (level m) <= cl then True else False

getAllLogs :: LoggerMonadIO e (Seq (LogMessage e))
getAllLogs = logs <$> get

instance PrettyPrint LogLevel where
    ppr LogNone = "[NONE] THIS REALLY SHOULDNT BE HAPPENING"
    ppr LogError = as [red] "[ERROR]"
    ppr LogWarning = as [yellow] "[WARNING]"
    ppr LogInfo = as [bold,blue] "[INFO]"
    ppr LogVerbose = as [bold,green] "[VERBOSE]"
    ppr LogDebug = as [bold,white] "[DEBUG]"
    ppr LogTrace = as [bold,lgray] "[TRACE]"
    ppr LogLevelMax = "[MAX] THIS REALLY SHOULDNT BE HAPPENING"

instance PrettyPrint e => PrettyPrint (LogMessage e) where
    ppr (LogMessage pl ts lvl) = ppr lvl ++ as [dgray] ("[" ++ show tm ++ "]\n") ++ ppr pl
        where tm = systemToUTCTime ts