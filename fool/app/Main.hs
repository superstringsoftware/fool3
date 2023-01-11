{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.Haskeline
import System.Directory
import System.Exit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Data.Functor.Identity
import State
import Control.Monad (zipWithM_, void, when)

import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Core
import Pipeline
import Interpreter
import Logs (SourceInfo(..) )
import Util.PrettyPrinting as TC

import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

import Parser as Lambda

-- need this 4-monad stack to make sure Haskeline works with our state monad
type InputTState = InputT IntState

-- needs to go to settings!!!
-- baseLibPath = "prog1.fool.hs" -- "base.fool.hs"
--baseLibPath = "base.thask.hs"
baseLibPath = "base.fool"
-- baseLibPath = "parsertests.fool"

processNew :: T.Text -> IntState ()
processNew line = do
    res <- parseToplevel line
    case res of
        Right ex -> do
            -- processing parsed input
            liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received expressions: " -- ++ (show $ length ex)
            liftIO $ putStrLn (show ex) -- show what was parsed first
            traceExpr ex
            addBinding ex
            showAllLogs
            clearAllLogs
            -- processSurfaceExpr ex -- processing expressions one by one
        Left err -> do
            -- it's not a top level expression, trying interactive expression
            liftIO $ putStrLn $ "trying interactive expression"
            res1 <- parseExpr line
            case res1 of
                Left err1 -> liftIO (print err) >> liftIO (print err1)
                Right ex1 -> do
                    trace $ TC.as [TC.bold, TC.underlined] "Received interactive expression: " -- ++ (show $ length ex)
                    trace (show ex1) -- show what was parsed first
                    showAllLogs
                    clearAllLogs
                    processInteractive ex1
                    

                    


showHelp :: IO ()
showHelp = do
    putStrLn $ TC.as [TC.bold, TC.underlined] "Available commands:"
    putStrLn ":h[elp]           -- this message"
    putStrLn ":q[uit]           -- quit"
    putStrLn ":a[ll]            -- list everything that was parsed, -d - in core format"
    putStrLn ":l[oad] <name>    -- load and interpret file <name>"
    putStrLn ":s[et] <command>  -- set environment flags (:s strict, :s lazy, :s trace on/off)"
    putStrLn ":e[nv]            -- show current environment"
    putStrLn ":list <types, functions, constructors> [-d] -- list all global functions / types / constructors"
    putStrLn ":i[nfo] <name>    -- find and show a top-level binding with <name>"
    putStrLn ":types            -- list all types"
    putStrLn ":compile          -- compile currently loaded program"
    putStrLn ":r[un] <f name>   -- execute expression with a given name that's already loaded. 'main' by detault."
        

_pprSomethingEnv sel str = do
    liftIO $ putStrLn str
    smth <- get >>= \s -> pure ( (sel . currentEnvironment) s)
    let tkeys = Map.keys smth
    liftIO $ mapM_ (fenv1 smth) tkeys
    where fenv1 ts tk = do 
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":")) ++ "\n  " ++ (ppr tt)
 

processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":load":xs) = loadFileNew (head xs)
processCommand (":set":s:xs) = processSet s xs
processCommand (":compile":_) = compile2JSpass
processCommand (":list":"types":"-d":_) = do
    liftIO $ putStrLn "\n--------------- TYPES ----------------"
    types <- get >>= \s -> pure ( (types . currentEnvironment) s)
    liftIO $ mapM (\t -> pPrint t) types
    return ()
processCommand (":list":"functions":"-d":_) = do
    liftIO $ putStrLn "\n--------------- LAMBDAS ----------------"
    lambdas <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    liftIO $ mapM (\t -> pPrint t) lambdas
    return ()
processCommand (":list":"constructors":"-d":_) = do
    liftIO $ putStrLn "\n--------------- CONSTRUCTORS ----------------"
    cons <- get >>= \s -> pure ( (constructors . currentEnvironment) s)
    liftIO $ mapM (\t -> pPrint t) cons
    return ()
processCommand (":env":"-d":_) = do
    fl <- gets currentFlags
    liftIO $ print fl
    processCommand (":list":"types":"-d":[])
    processCommand (":list":"constructors":"-d":[])
    processCommand (":list":"functions":"-d":[])

processCommand (":list":"types":_) = 
    _pprSomethingEnv types "\n--------------- TYPES ----------------"
processCommand (":list":"functions":_) = 
    _pprSomethingEnv topLambdas "\n--------------- LAMBDAS ----------------"    
processCommand (":list":"constructors":_) = do
    liftIO $ putStrLn "\n--------------- CONSTRUCTORS ----------------"
    res <- get >>= \s -> pure ( (constructors . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    where fenv1 ts tk = do 
                        let (Just (tt,constag)) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ "(" ++ show constag ++ "):")) ++ "\n  " ++ (ppr tt)

processCommand (":clm":_) = do
    liftIO $ putStrLn "\n--------------- CLM LAMBDAS ----------------"
    res <- get >>= \s -> pure ( (clmLambdas . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    where fenv1 ts tk = do 
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":")) ++ "\n  "
                        pPrint tt


processCommand (":env":_) = do
    fl <- gets currentFlags
    liftIO $ print fl
    processCommand (":list":"types":[])
    processCommand (":list":"constructors":[])
    processCommand (":list":"functions":[])
    
processCommand (":all":"-d":_) = do 
    mod <- get >>= \s -> pure (parsedModule s)
    liftIO (mapM_ (\(ex,_) -> pPrint ex ) mod )
    --processCommand ([":types"]) >> processCommand ([":functions"])
processCommand (":all":_) = do
    mod <- get >>= \s -> pure (parsedModule s)
    liftIO (mapM_ (\(ex,_) -> (putStrLn . ppr) ex ) mod )
    
   

{- 
liftIO $ putStrLn "\n--------------- JS REALM ----------------"
    jsp <- get >>= \s -> pure ( (outProgram . currentEnvironment) s)
    liftIO $ mapM_ putStrLn jsp
-}

processCommand (":q":_) = processCommand [":quit"]
processCommand (":h":_) = processCommand [":help"]
processCommand (":a":"-d":_) = processCommand [":all","-d"]
processCommand (":a":_) = processCommand [":all"]
processCommand (":l":xs) = processCommand (":load":xs)
processCommand (":i":xs) = processCommand (":info":xs)
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

processSet "trace" ("on":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { tracing = True} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "tracing on"
    
processSet "trace" ("off":_) = do
    modify (\st -> st { currentFlags = (currentFlags st) { tracing = False} } )
    liftIO $ putStrLn $ "Set " ++ TC.as [TC.bold] "tracing off"
    


processSet _ _ = liftIO $ putStrLn "Unknown :set command. Type :h[elp] to show available list."

loadFileNew :: String -> IntState ()
loadFileNew nm = do
    liftIO $ putStrLn $ "Loading file: " ++ nm
    fileText <- liftIO (T.readFile nm)
    res <- parseWholeFile fileText nm
    -- liftIO $ putStrLn $ T.unpack fileText
    -- liftIO $ print res
    st <- get
    -- liftIO $ print (newParsedModule st)
    put $ st { currentSource = fileText }
    -- liftIO $ print st
    case res of
        Left err -> liftIO ( putStrLn $ "There were " ++ TC.as [TC.red] "parsing errors:") >> liftIO (putStrLn $ showSyntaxError fileText err)
        -- desugaring on the first pass
        Right exprs -> do
                -- liftIO (mapM_ (putStrLn . show) exprs) 
                liftIO (putStrLn "... successfully loaded.")
                liftIO (putStrLn $ "Received " ++ show (length (parsedModule st)) ++ " statements.")
                liftIO (putStrLn $ "Executing pass 0: " ++ TC.as [TC.bold, TC.underlined] "after parser desugaring")
                afterparserPass
                showAllLogsWSource
                clearAllLogs
                processCommand ([":e"])
                liftIO (putStrLn $ "Executing pass 1: " ++ TC.as [TC.bold, TC.underlined] "initial top level environment building")
                buildEnvPass
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 2: " ++ TC.as [TC.bold, TC.underlined] "initial optimizations")
                caseOptimizationPass
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 3: " ++ TC.as [TC.bold, TC.underlined] "Lambdas to CLM")
                lamToCLMPass
                showAllLogsWSource
                processCommand ([":e"])
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 4: " ++ TC.as [TC.bold, TC.underlined] "javascript code generation")
                -- compile2JSpass
                -- showAllLogsWSource
                -- clearAllLogs
                -- mod <- get >>= \s -> pure (parsedModule s)
                -- liftIO (mapM_ (\(ex,_) -> (putStrLn . show) ex ) mod )


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
                _ -> lift (processNew $ T.pack input) >> loop

runInterpreter :: InputTState ()
runInterpreter = do
    -- liftIO $ putStrLn "Building primitive environment..."
    -- lift $ loadPrimitiveEnv
    liftIO $ putStrLn "Loading base library..."
    lift $ loadFileNew baseLibPath
    -- lift $ processCommand [":all"]
    loop

main :: IO ()
main = do
    greetings
    -- setting up Haskeline loop
    -- getting to the right monad in our crazy monad stack
    initializeInterpreter >>= (runIntState (runInputT defaultSettings {historyFile=Just "./.fool_history"} runInterpreter))

greetings = do
    putStrLn "Welcome to the Ultimate Fool!"
    putStrLn "Version 0.0.9"
    putStrLn "(c) Copyright 2016-2023 by Anton Antich (a@s3.ag)\n"
    putStrLn "Type :help for help on commands or :load a file.\n"
    