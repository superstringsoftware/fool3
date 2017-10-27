module Main where

import Parser
import Interpreter
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline

import System.Directory

import System.Exit

import qualified TermColors as TC

process :: String -> InterpreterState -> IO InterpreterState
process line st = do
  let res = parseToplevel line
  case res of
    Left err -> print err >> return st
    Right ex -> do
        -- processing parsed input
        putStrLn $ (TC.ansifyString [TC.bold, TC.underlined] "Received expressions: ") ++ (show $ length ex)
        mapM_ print ex -- show what was parsed first
        mapM_ (processExpr st) ex -- processing expressions one by one - need to figure out how to pass STATE properly
        return st

processCommand :: [String] -> InterpreterState ->IO InterpreterState
processCommand (":quit":_) st = putStrLn "Goodbye." >> exitSuccess
processCommand (":vars":_) st = prettyPrintST (symTable st) >> return st
processCommand (":functions":_) st = prettyPrintFT (funTable st) >> return st
processCommand (":types":_) st = prettyPrintTT (typeTable st) >> return st
processCommand (":load":xs) st = loadFile (head xs) st >>= return
processCommand (":run":_) st = run st >>= return
processCommand _ st = do return st

loadFile :: String -> InterpreterState -> IO InterpreterState
loadFile nm st = do
    putStrLn $ "Loading file: " ++ nm
    res <- parseToplevelFile nm
    putStrLn $ show res
    case res of
      Left err -> print err
      Right exprs -> mapM_ (processExpr st) exprs 
    return st

run :: InterpreterState -> IO InterpreterState
run st = do
  putStrLn "Running"
  mn <- findMain $ funTable st
  putStrLn $ show mn
  case mn of
    Nothing -> putStrLn "main() does not exist!" >> return st
    Just (Function _ _ f) -> putStrLn ("Found main: " ++ (show f)) >> processExpr st f >>= return

showHelp :: IO ()
showHelp = do
    putStrLn "Available commands:"
    putStrLn ":quit -- quit"
    putStrLn ":vars -- list all global vars"
    putStrLn ":functions -- list all global functions"
    putStrLn ":types -- list all types"
    putStrLn ":load <name> -- load and interpret file <name>"
    putStrLn ":run -- execute main() if it is present"

main :: IO ()
main = do
    -- initializing interpreter state
    state <- initializeInterpreter

    dir <- getCurrentDirectory
    putStrLn dir 

    -- going into the loop
    runInputT defaultSettings {historyFile=Just "./.fool_history"} (loop state)
      where
      loop st = do
        minput <- getInputLine  "fool>"
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> case input of
              ([]) -> liftIO showHelp >> (loop st)
              -- if starts with ":" - then it's a command
              (':':_) -> (liftIO $ processCommand (words input) st) >>= loop
              -- otherwise parsing our language input
              otherwise -> (liftIO $ process input st) >>= loop
