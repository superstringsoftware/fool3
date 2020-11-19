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

import Core.Syntax
import Core.Pipeline
import Core.Environment
import Core.Interpreter
import Core.Primitives
import Logs (SourceInfo(..) )
import Util.PrettyPrinting as TC

import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

-- depending on which parser we keep, that language will get compiled.
import SurfaceLanguage.Lambda.Parser as Lambda
--import qualified SurfaceLanguage.Thask.Parser as Thask


-- need this 4-monad stack to make sure Haskeline works with our state monad
type InputTState = InputT IntState

-- needs to go to settings!!!
-- baseLibPath = "prog1.fool.hs" -- "base.fool.hs"
--baseLibPath = "base.thask.hs"
baseLibPath = "base.lambda.hs"

processNew :: T.Text -> IntState ()
processNew line = do
    res <- parseToplevel line
    case res of
        Right ex -> do
            -- processing parsed input
            liftIO $ putStrLn $ TC.as [TC.bold, TC.underlined] "Received expressions: " -- ++ (show $ length ex)
            liftIO $ putStrLn (show ex) -- show what was parsed first
            let ex' = afterparse ex
            liftIO $ putStrLn (show ex')
            buildEnvironmentM (ex', SourceInfo 0 0 "")
            showAllLogs
            clearAllLogs
            -- processSurfaceExpr ex -- processing expressions one by one
        Left err -> do
            -- it's not a top level expression, trying interactive expression
            res1 <- parseExpr line
            case res1 of
                Left err1 -> liftIO (print err) >> liftIO (print err1)
                Right ex1 -> do
                    trace $ TC.as [TC.bold, TC.underlined] "Received interactive expression: " -- ++ (show $ length ex)
                    trace (show ex1) -- show what was parsed first
                    let ex2 = afterparse ex1
                    trace (show ex2)                    
                    showAllLogs
                    clearAllLogs
                    res <- interpretExpr ex2
                    liftIO $ putStrLn (ppr res)                    

                    


showHelp :: IO ()
showHelp = do
    putStrLn $ TC.as [TC.bold, TC.underlined] "Available commands:"
    putStrLn ":h[elp]           -- this message"
    putStrLn ":q[uit]           -- quit"
    putStrLn ":a[ll]            -- list everything that was parsed, -d - in core format"
    putStrLn ":l[oad] <name>    -- load and interpret file <name>"
    putStrLn ":s[et] <command>  -- set environment flags (:s strict, :s lazy, :s trace on/off)"
    putStrLn ":e[nv]            -- show current environment"
    putStrLn ":functions        -- list all global functions"
    putStrLn ":i[nfo] <name>    -- find and show a top-level binding with <name>"
    putStrLn ":types            -- list all types"
    putStrLn ":compile          -- compile currently loaded program"
    putStrLn ":r[un] <f name>   -- execute expression with a given name that's already loaded. 'main' by detault."
        

processCommand :: [String] -> IntState ()
processCommand (":help":_) = liftIO showHelp
processCommand (":quit":_) = liftIO $ putStrLn "Goodbye." >> exitSuccess
processCommand (":load":xs) = loadFileNew (head xs)
processCommand (":set":s:xs) = processSet s xs
-- processCommand (":compile":_) = compile2JSpass
processCommand (":env":"-d":_) = do
    fl <- gets currentFlags
    liftIO $ print fl
    liftIO $ putStrLn "\n--------------- TYPES ----------------"
    types <- get >>= \s -> pure ( (types . currentEnvironment) s)
    let tkeys = Map.keys types
    liftIO $ mapM_ (fenv1 types) tkeys
    liftIO $ putStrLn "\n--------------- LAMBDAS ----------------"
    res <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    liftIO $ putStrLn "\n--------------- JS REALM ----------------"
    jsp <- get >>= \s -> pure ( (jsProgram . currentEnvironment) s)
    liftIO $ mapM_ putStrLn jsp
    where fenv1 ts tk = do 
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ "\n" ++ (TC.as [bold,green] (tk ++ ":"))
                        pPrint tt

processCommand (":env":_) = do
    fl <- gets currentFlags
    liftIO $ print fl
    liftIO $ putStrLn "\n--------------- TYPES ----------------"
    types <- get >>= \s -> pure ( (types . currentEnvironment) s)
    let tkeys = Map.keys types
    liftIO $ mapM_ (fenv1 types) tkeys
    liftIO $ putStrLn "\n--------------- LAMBDAS ----------------"
    res <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    let fkeys = Map.keys res
    liftIO $ mapM_ (fenv1 res) fkeys
    liftIO $ putStrLn "\n--------------- CLASS FUNCS LOOKUP TABLE ----------------"
    cfs <- get >>= \s -> pure ( (classFuncs . currentEnvironment) s)
    let cfkeys = Map.keys cfs
    liftIO $ mapM_ (fenv1 cfs) cfkeys
    liftIO $ putStrLn "\n--------------- JS REALM ----------------"
    jsp <- get >>= \s -> pure ( (jsProgram . currentEnvironment) s)
    liftIO $ mapM_ putStrLn jsp
    where fenv1 ts tk = do 
                        let (Just tt) = Map.lookup tk ts
                        putStrLn $ (TC.as [bold,green] (tk ++ ":")) ++ "\n  " ++ (ppr tt)

processCommand (":all":"-d":_) = do 
    mod <- get >>= \s -> pure (parsedModule s)
    liftIO (mapM_ (\(ex,_) -> pPrint ex ) mod )
    --processCommand ([":types"]) >> processCommand ([":functions"])
processCommand (":all":_) = do
    mod <- get >>= \s -> pure (parsedModule s)
    liftIO (mapM_ (\(ex,_) -> (putStrLn . ppr) ex ) mod )
    
processCommand (":types":"-d":_) = do
    types <- get >>= \s -> pure ( (types . currentEnvironment) s)
    liftIO $ mapM_ pPrint types
processCommand (":types":_) = do
    types <- get >>= \s -> pure ( (types . currentEnvironment) s)
    liftIO $ mapM_ (putStrLn . ppr) types
    -- liftIO $ mapM_ print (Map.keys types)

processCommand (":functions":"-d":_) = do
    res <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    let out = foldrWithKey (\n l a -> a ++ n ++ " = " ++ (TL.unpack $ pShow l) ++ "\n") "" res
    liftIO $ putStrLn out
    --liftIO $ mapM_ pPrint res
processCommand (":functions":_) = do
    res <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    let out = foldrWithKey (\n l a -> a ++ (showLambdaAsLambda n l) ++ "\n") "" res
    liftIO $ putStrLn out
    -- liftIO $ mapM_ (putStrLn . ppr) res
-- find a specifically named binding
processCommand (":info":name:_) = do
    res <- get >>= \s -> pure ( (topLambdas . currentEnvironment) s)
    let lam = Map.lookup name res
    let action = maybe (putStrLn "Nothing is found with such name.") (\l -> ( (putStrLn $ showLambdaAsLambda name l) >> pPrint l)) lam
    liftIO action

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
                liftIO (putStrLn $ "Executing pass 1: " ++ TC.as [TC.bold, TC.underlined] "initial top level environment building")
                buildEnvPass
                showAllLogsWSource
                clearAllLogs
                liftIO (putStrLn $ "Executing pass 2: " ++ TC.as [TC.bold, TC.underlined] "javascript code generation")
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
    liftIO $ putStrLn "Building primitive environment..."
    lift $ loadPrimitiveEnv
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
    putStrLn "Welcome to Tea-Lambda Language!"
    putStrLn "Version 0.0.5"
    putStrLn "(c) Copyright 2016-2020 by Anton Antich (a@s3.ag)\n"
    putStrLn "Type :help for help on commands or :load a file.\n"
    