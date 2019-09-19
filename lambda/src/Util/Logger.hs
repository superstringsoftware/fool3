{-# LANGUAGE OverloadedStrings, RecordWildCards, MultiParamTypeClasses #-}


module Util.Logger where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Sequence

data LogMessage e = LogError {
    message :: String,
    payload :: e
} | LogWarning {
    message :: String,
    payload :: e
} | LogInfo {
    message :: String,
    payload :: e
} | LogVerbose {
    message :: String,
    payload :: e
} | LogDebug {
    message :: String,
    payload :: e
} | LogTrace {
    message :: String,
    payload :: e
} deriving (Show, Eq, Ord)

data LogState e = LogState {
    logLevel :: !Int,
    logs :: Seq (LogMessage e)
}

type LogLevel = Int

-- monad transformer for our logger monad, allows embedding IO inside etc
type LoggerMonadT e m = StateT (LogState e) m

class Monad m => LogMonad m where
    getLogState :: m (LogState e)
    putLogState :: (LogState e) -> m ()
    modifyLogState :: (LogState e -> LogState e) -> m ()
    modifyLogState mf = getLogState >>= \s -> putLogState (mf s)

checkMessageLevel :: LogMessage e -> LogLevel
checkMessageLevel LogError{..}   = 0
checkMessageLevel LogWarning{..} = 1
checkMessageLevel LogInfo{..}    = 2
checkMessageLevel LogVerbose{..} = 3
checkMessageLevel LogDebug{..}   = 4
checkMessageLevel LogTrace{..}   = 5

setCurrentLevel :: LogLevel -> LoggerMonad e ()
setCurrentLevel i = modify' $ \s -> s{logLevel = i}

setTrace   = setCurrentLevel 6
setDebug   = setCurrentLevel 5
setVerbose = setCurrentLevel 4
setInfo    = setCurrentLevel 3
setWarning = setCurrentLevel 2
setError   = setCurrentLevel 1
setNone    = setCurrentLevel 0

getCurrentLevel :: LoggerMonad e LogLevel
getCurrentLevel = logLevel <$> get

-- | logs the message if it's level is lower than currently set
-- logMessage :: LogMessage e -> LoggerMonad e ()



-- this was a very useful excersize in defining Functor -> Applicative -> Monad for a custom data type
-- but now we are simply using monad transformers to be able to stack with IO etc
-- e - type of additional logging data structure
-- a - normal value under monad
newtype LoggerMonad e a = LoggerMonad { runLogger :: (LogState e) -> (a, LogState e) }

-- ok this took some time to figure out but was a very useful exercize - writing a monad definition from scratch
instance Functor (LoggerMonad e) where 
    -- fmap :: (a -> b) -> LoggerMonad e a -> LoggerMonad e b
    fmap f comp = LoggerMonad (\s -> let (v,s') = runLogger comp s in (f v, s'))

instance Applicative (LoggerMonad e) where 
    -- pure :: a -> LoggerMonad e a
    pure v = LoggerMonad $ \s -> (v,s)
    -- (<*>) :: LoggerMonad e (a -> b) -> LoggerMonad e a -> LoggerMonad e b
    comp <*> va = LoggerMonad $ 
                    \s -> let (v,s'') = runLogger va s' -- 2. then applying new state to state action in the monad to get the value
                              (f,s')  = runLogger comp s -- 1. getting function f from under monad by running the action w/ initial state
                          in (f v, s'') -- 3. finally returning f applied to value and chaining state again

instance Monad (LoggerMonad e) where
    -- fmap f xs  =  xs >>= pure . f
    -- (>>=) :: forall a b. LoggerMonad e a -> (a -> LoggerMonad e b) -> LoggerMonad e b
    comp >>= mf = LoggerMonad $
                    \s -> let (va,s') = runLogger comp s -- 1. getting v::a from under monad chaining state
                          in  runLogger (mf va) s' -- 2. applying mf action and chaining state further

                        
-- instance of the MonadState class                          
instance MonadState (LogState e) (LoggerMonad e) where
    -- state :: (LogState e -> (a, LogState e)) -> LoggerMonad e a
    state comp = LoggerMonad comp
