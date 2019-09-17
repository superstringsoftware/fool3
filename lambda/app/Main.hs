module Main where

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

import Lambda.Syntax
import Lambda.Parser


-- need this 3-monad stack to make sure Haskeline works with our state monad
type InputTState a = InputT (StateT InterpreterState IO) a

-- needs to go to settings!!!
-- baseLibPath = "prog1.fool.hs" -- "base.fool.hs"
baseLibPath = "base.lambda.hs"

processNew :: String -> IntState ()
processNew line = do
    res <- parseToplevel line
    case res of
        Right ex -> do
            -- processing parsed input
            liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received expressions: " -- ++ (show $ length ex)
            liftIO $ print ex -- show what was parsed first
            -- processSurfaceExpr ex -- processing expressions one by one
        Left err -> do
            -- it's not a top level expression, trying interactive expression
            res1 <- parseExpr line
            case res1 of
                Left err1 -> liftIO (print err) >> liftIO (print err1)
                Right ex1 -> do
                    liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received interactive expression: " -- ++ (show $ length ex)
                    liftIO $ print ex1 -- show what was parsed first
                    -- now processing it in interpreter immediately


showHelp :: IO ()
showHelp = do
    putStrLn $ TC.as [TC.bold, TC.underlined] "Available commands:"
    putStrLn ":h[elp]           -- this message"
    putStrLn ":q[uit]           -- quit"
    putStrLn ":a[ll]            -- list everything, -d - in core format"
    putStrLn ":l[oad] <name>    -- load and interpret file <name>"
    putStrLn ":s[et] <command>  -- set environment flags (:s strict or :s lazy)"
    putStrLn ":e[nv]            -- show current environment"
    putStrLn ":functions        -- list all global functions"
    putStrLn ":types            -- list all types"
    putStrLn ":compile          -- compile currently loaded program"
    putStrLn ":r[un] <f name>   -- execute expression with a given name that's already loaded. 'main' by detault."
        

processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":load":xs) = loadFileNew (head xs)
processCommand (":set":s:xs) = processSet s xs
processCommand (":env":_) = do
    fl <- gets currentFlags
    liftIO $ print fl

processCommand (":q":_) = processCommand [":quit"]
processCommand (":h":_) = processCommand [":help"]
processCommand (":a":"-d":_) = processCommand [":all","-d"]
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
        Right exprs -> liftIO (putStrLn "... successfully loaded.")


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
    putStrLn "Welcome to Tea-Lambda Language!"
    putStrLn "Version 0.0.1"
    putStrLn "(c) Copyright 2016-2019 by Anton Antich (a@s3.ag)\n"
    putStrLn "Type :help for help on commands or :load a file.\n"
    