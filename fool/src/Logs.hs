{-# LANGUAGE OverloadedStrings #-}
-- error handling 

module Logs where

import Data.Text as L
import Util.IOLogger  
import Util.PrettyPrinting  
import Control.Monad.IO.Class (liftIO)

-- some info for debugging to attach to initially parsed module expressions
data SourceInfo = SourceInfo {
    lineNum :: !Int, colNum :: !Int, notes :: Text
} | SourceInteractive deriving Eq

data LogPayload = LogPayload {
    linePos :: !Int,
    colPos :: !Int,
    filename :: String,
    message :: String
} deriving Show

-- lambda logger monad is a state monad that lies on top of IO
type LambdaLoggerMonad = LoggerMonadIO LogPayload
    
showAllLogsWSource :: Text -> LambdaLoggerMonad ()
showAllLogsWSource src = do
    logs <- getAllLogs
    -- liftIO $ mapM_ (putStrLn . ppr) logs
    liftIO $ mapM_ (\err -> putStrLn $ showErrorWithSource src err ) logs
    

showErrorWithSource :: Text -> LogMessage LogPayload -> String
showErrorWithSource s err' = L.unpack $ L.unlines [
      "  ",
      "  " <> lineContents,
      "  " <> ((L.replicate col " ") <> "^^^"),
      (L.pack $ ppr err')
    ]
  where
    err = payload err'
    lineContents = (L.lines s) !! line
    line = linePos err - 1
    col  = colPos err - 1

instance Show SourceInfo where
    show (SourceInfo l c note) = "At line " ++ show l ++ ", column " ++ show c ++ ": " ++ L.unpack note

instance PrettyPrint LogPayload where
    ppr (LogPayload lin col fname msg) = "At line " ++ show lin ++ ", column " ++ show col ++ ": " ++ msg