module Main where

import Parser
import Interpreter
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline

import System.Directory

import System.Exit

import qualified TermColors as TC

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state

type InputTState a = InputT (StateT InterpreterState IO) a

process :: String -> IntState ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> liftIO $ print err 
    Right ex -> do
        -- processing parsed input
        liftIO $ putStrLn $ (TC.ansifyString [TC.bold, TC.underlined] "Received expressions: ") ++ (show $ length ex)
        liftIO $ mapM_ print ex -- show what was parsed first
        mapM_ processExpr ex -- processing expressions one by one - need to figure out how to pass STATE properly
        

processCommand :: [String] -> IntState ()
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":vars":_) = get >>= liftIO . prettyPrintST . symTable
processCommand (":functions":_) = get >>= liftIO . prettyPrintFT . funTable
processCommand (":types":_) = get >>= liftIO . prettyPrintTT . typeTable
processCommand (":load":xs) = loadFile (head xs)
processCommand (":run":_) = run
processCommand _ = do return ()

loadFile :: String -> IntState ()
loadFile nm = do
    st <- get
    liftIO $ putStrLn $ "Loading file: " ++ nm
    res <- liftIO $ parseToplevelFile nm
    liftIO $ putStrLn $ show res
    case res of
      Left err -> liftIO $ print err
      Right exprs -> mapM_ processExpr exprs 

run :: IntState ()
run = do
  st <- get
  liftIO $ putStrLn "Running"
  mn <- liftIO $ findMain $ funTable st
  liftIO $ putStrLn $ show mn
  case mn of
    Nothing -> liftIO $ putStrLn "main() does not exist!"
    Just (Function _ _ f) -> processExpr f

showHelp :: IO ()
showHelp = do
    putStrLn "Available commands:"
    putStrLn ":quit -- quit"
    putStrLn ":vars -- list all global vars"
    putStrLn ":functions -- list all global functions"
    putStrLn ":types -- list all types"
    putStrLn ":load <name> -- load and interpret file <name>"
    putStrLn ":run -- execute main() if it is present"

loop :: InputTState ()
loop = do
        minput <- getInputLine  "fool>"
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> case input of
              ([]) -> liftIO showHelp >> loop
              -- if starts with ":" - then it's a command
              (':':_) -> (lift $ processCommand (words input)) >> loop
              -- otherwise parsing our language input
              otherwise -> (lift $ process input) >> loop

main :: IO ()
main = do
    -- initializing interpreter state
    state <- initializeInterpreter
    evalStateT (put state) state

    -- dir <- getCurrentDirectory
    -- putStrLn dir 

    -- going into the loop
    let act = runInputT defaultSettings {historyFile=Just "./.fool_history"} loop
    evalStateT act state
      
