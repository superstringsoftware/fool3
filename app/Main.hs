module Main where

import Parser
import Interpreter

import Control.Monad.Trans
import System.Console.Haskeline

import System.Exit

process :: String -> InterpreterState -> IO InterpreterState
process line st = do
  let res = parseToplevel line
  case res of
    Left err -> print err >> return st
    Right ex -> do
        -- processing parsed input
        putStrLn $ "Received expressions: " ++ (show $ length ex)
        mapM_ print ex -- show what was parsed first
        mapM_ (processExpr st) ex -- processing expressions one by one - need to figure out how to pass STATE properly
        return st

processCommand :: [String] -> InterpreterState ->IO InterpreterState
processCommand (":quit":xs) st = putStrLn "Goodbye." >> exitSuccess
processCommand (":vars":_) st = prettyPrintFT (symTable st) >> return st
processCommand (":functions":_) st = prettyPrintFT (funTable st) >> return st
processCommand _ st = do return st

showHelp :: IO ()
showHelp = do
    putStrLn "Available commands:"
    putStrLn ":quit -- quit"
    putStrLn ":vars -- list all global vars"
    putStrLn ":functions -- list all global functions"

main :: IO ()
main = do
    -- initializing interpreter state
    state <- initializeInterpreter
    -- going into the loop
    runInputT defaultSettings (loop state)
      where
      loop st = do
        minput <- getInputLine "ready> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> case input of
              ([]) -> liftIO showHelp >> (loop st)
              -- if starts with ":" - then it's a command
              (':':_) -> (liftIO $ processCommand (words input) st) >>= loop
              -- otherwise parsing our language input
              otherwise -> (liftIO $ process input st) >>= loop
