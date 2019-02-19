module Main where

import Interpreter
import Control.Monad.Trans
import System.Console.Haskeline
import System.Directory
import System.Exit
import qualified TermColors as TC
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Data.Functor.Identity
import State
import Control.Monad (zipWithM_, void, when)

import DotNet.Syntax
import DotNet.Parser

import DotNet.Translator

-- need this 3-monad stack to make sure Haskeline works with our state monad
type InputTState a = InputT (StateT InterpreterState IO) a

-- needs to go to settings!!!
baseLibPath = "base.fool.hs"

processNew :: String -> IntState ()
processNew line = do
  res <- parseToplevel line
  case res of
    Left err -> liftIO $ print err
    Right ex -> do
        -- processing parsed input
        liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received expressions: " -- ++ (show $ length ex)
        liftIO $ print ex -- show what was parsed first
        processSurfaceExpr ex -- processing expressions one by one


processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":functions":_) = get >>= liftIO . prettyPrintFT . funTable
processCommand (":types":_) = get >>= liftIO . prettyPrintTT . typeTable
-- processCommand (":core":"-d":_) = get >>= liftIO . showLS . lambdas
-- processCommand (":core":_) = get >>= liftIO . prettyPrintLS . lambdas
processCommand (":compile":_) = (liftIO $ putStrLn "Compiling...") >> compile
processCommand (":all":"-d":_) = do
  st <- get
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Type classes:"
  liftIO $ prettyPrintTCT $ typeClassTable st
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Types:"
  liftIO $ prettyPrintTT $ typeTable st
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Functions:"
  liftIO $ prettyPrintFT $ funTable  st
processCommand (":all":_) = do
  st <- get
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Types:"
  liftIO $ prettyPrintTT' $ typeTable st
  liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Functions:"
  liftIO $ prettyPrintFT' $ funTable  st  
processCommand (":load":xs) = loadFileNew (head xs)
processCommand (":set":s:xs) = processSet s xs
processCommand (":env":_) = do
  fl <- gets currentFlags
  liftIO $ print fl

processCommand (":q":_) = processCommand [":quit"]
processCommand (":h":_) = processCommand [":help"]
processCommand (":c":"-d":_) = processCommand [":core","-d"]
processCommand (":c":_) = processCommand [":core"]
processCommand (":a":_) = processCommand [":all"]
processCommand (":l":xs) = processCommand (":load":xs)
processCommand (":s":xs) = processCommand (":set":xs)
processCommand (":e":xs) = processCommand (":env":xs)

processCommand _ = liftIO $ putStrLn "Unknown command. Type :h[elp] to show available list."

-- various environment settings
-- processSet :: String -> IntState ()
processSet "strict" _ = do
  modify (\st -> st { currentFlags = (currentFlags st) { strict = True} } )
  liftIO $ putStrLn $ "Set interpretation mode to " ++ TC.as [TC.bold] "strict"

processSet "lazy" _ = do
  modify (\st -> st { currentFlags = (currentFlags st) { strict = False} } )
  liftIO $ putStrLn $ "Set interpretation mode to " ++ TC.as [TC.bold] "lazy"

processSet "pretty" _ = do
  modify (\st -> st { currentFlags = (currentFlags st) { pretty = True} } )
  liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "pretty printing on"

processSet "show" _ = do
  modify (\st -> st { currentFlags = (currentFlags st) { pretty = False} } )
  liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "pretty printing off"

processSet "tracing" (x:_)
  | x == "on" =   do modify (\st -> st { currentFlags = (currentFlags st) { tracing = True} } )
                     liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "tracing on"
  | x == "off" =  do modify (\st -> st { currentFlags = (currentFlags st) { tracing = False} } )
                     liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "tracing off"




processSet _ _ = liftIO $ putStrLn "Unknown :set command. Type :h[elp] to show available list."

loadFileNew :: String -> IntState ()
loadFileNew nm = do
   st <- get
   liftIO $ putStrLn $ "Loading file: " ++ nm
   res <- parseToplevelFile nm
   -- liftIO $ print res
   case res of
     Left err -> liftIO ( putStrLn $ "There were " ++ TC.as [TC.red] "errors:") >> liftIO (print err)
     -- desugaring on the first pass
     Right exprs -> mapM_ (processSurfaceExpr . desugar) exprs >> liftIO (putStrLn "... successfully loaded.")



showHelp :: IO ()
showHelp = do
    putStrLn $ TC.as [TC.bold, TC.underlined] "Available commands:"
    putStrLn ":h[elp]           -- this message"
    putStrLn ":q[uit]           -- quit"
    putStrLn ":a[ll]            -- list everything"
    putStrLn ":c[ore]           -- list everything in core format"
    putStrLn ":l[oad] <name>    -- load and interpret file <name>"
    putStrLn ":s[et] <command>  -- set environment flags (:s strict or :s lazy)"
    putStrLn ":e[nv]            -- show current environment"
    putStrLn ":functions        -- list all global functions"
    putStrLn ":types            -- list all types"
    putStrLn ":compile          -- compile currently loaded program"

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
              _ -> lift (processNew input) >> loop

runInterpreter :: InputTState ()
runInterpreter = do
  liftIO $ putStrLn "Loading base library..."
  lift $ loadFileNew baseLibPath
  lift $ processCommand [":all"]
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
  putStrLn "(c) Copyright 2016-2018 by Anton Antich (a@s3.ag)\n"
  putStrLn "Type :help for help on commands or :load a file.\n"
