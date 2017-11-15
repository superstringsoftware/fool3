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
import Data.Functor.Identity
import State
-- need this 3-monad stack to make sure Haskeline works with our state monad
type InputTState a = InputT (StateT InterpreterState IO) a

-- needs to go to settings!!!
baseLibPath = "base.fool"


process :: String -> IntState ()
process line = do
  let res = runIdentity $ parseToplevel line
  case res of
    Left err -> liftIO $ print err
    Right ex -> do
        -- processing parsed input
        liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received expressions: " -- ++ (show $ length ex)
        liftIO $ print ex -- show what was parsed first
        processExpr ex -- processing expressions one by one


processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":functions":_) = get >>= liftIO . prettyPrintFT . funTable
processCommand (":types":_) = get >>= liftIO . prettyPrintTT . typeTable
processCommand (":core":"-d":_) = get >>= liftIO . showLS . lambdas
processCommand (":core":_) = get >>= liftIO . prettyPrintLS . lambdas
processCommand (":all":_) = do
  st <- get
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Types:"
  liftIO $ prettyPrintTT $ typeTable st
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Functions:"
  liftIO $ prettyPrintFT $ funTable  st
processCommand (":load":xs) = loadFile (head xs)
processCommand (":run":_) = run

processCommand (":q":_) = processCommand [":quit"]
processCommand (":h":_) = processCommand [":help"]
processCommand (":c":"-d":_) = processCommand [":core","-d"]
processCommand (":c":_) = processCommand [":core"]
processCommand (":a":_) = processCommand [":all"]
processCommand (":l":xs) = processCommand (":load":xs)

processCommand _ = liftIO $ print "Unknown command. Type :h[elp] to show available list."

loadFile :: String -> IntState ()
loadFile nm = do
   st <- get
   liftIO $ putStrLn $ "Loading file: " ++ nm
   res <- liftIO $ parseToplevelFile nm
   -- liftIO $ print res
   case res of
     Left err -> liftIO ( putStrLn $ "There were " ++ TC.as [TC.red] "errors:") >> liftIO (print err)
     Right exprs -> mapM_ processExpr exprs >> liftIO (putStrLn "... successfully loaded.")

run :: IntState ()
run = do
  st <- get
  liftIO $ putStrLn "Running"
  mn <- liftIO $ findMain $ funTable st
  liftIO $ print mn
  case mn of
    Nothing -> liftIO $ putStrLn "main() does not exist!"
    Just (Function _ _ f) -> processExpr f

showHelp :: IO ()
showHelp = do
    putStrLn $ TC.as [TC.bold, TC.underlined] "Available commands:"
    putStrLn ":h[elp]           -- this message"
    putStrLn ":q[uit]           -- quit"
    putStrLn ":a[ll]            -- list everything"
    putStrLn ":c[ore]           -- list everything in core format"
    putStrLn ":l[oad] <name>    -- load and interpret file <name>"
    putStrLn ":functions        -- list all global functions"
    putStrLn ":types            -- list all types"
    putStrLn ":run              -- execute main() if it is present"

-- Haskeline loop stacked into 3-monad stack
loop :: InputTState ()
loop = do
        minput <- getInputLine  (TC.as [TC.bold] "λfool3. ")
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> case input of
              [] -> liftIO showHelp >> loop
              -- if starts with ":" - then it's a command
              (':':_) -> lift (processCommand (words input)) >> loop
              -- otherwise parsing our language input
              _ -> lift (process input) >> loop

runInterpreter :: InputTState ()
runInterpreter = do
  liftIO $ putStrLn "Loading base library..."
  lift $ loadFile baseLibPath
  lift $ processCommand [":core"]
  loop

main :: IO ()
main = do
    greetings
    -- setting up Haskeline loop
    -- getting to the right monad in our crazy monad stack
    initializeInterpreter >>= evalStateT (runInputT defaultSettings {historyFile=Just "./.fool_history"} runInterpreter)

greetings = do
  putStrLn "Welcome to Functional Object Oriented Low-Level Language (FOOL3)"
  putStrLn "Version 0.0.1"
  putStrLn "(c) Copyright 2016-2017 by J X-Ray Ho\n"
  putStrLn "Type :help for help on commands or :load a file.\n"
