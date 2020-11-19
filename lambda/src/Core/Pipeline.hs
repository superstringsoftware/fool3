{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

{-|
Main Compiler pipeline

Rough order:
- Parsing, with parse errors and some obvious things we can catch while parsing 
    (e.g., # of pattern match arguments mismatch etc).
- First "desugaring" pass - converting operator calls to function applications, initial rewriting of types from vars
    to tuples WITHOUT any checks, reversing the lines from the parsed order (as it reverses the file since we use a list)

- Then, we need to construct our typing and functional environment before doing type checks. This includes:

    * Reconstructing types from data constructor lambdas, converting constructor tags to ints from Strings etc
    * Setting functions environment with Maps from names to lambdas
    * Reconstructing typeclass hierarchy for efficient handling of instances
    * Processing typeclass instances, generating specialized functions in compile environment, so that we can emit
        specific functions based on type in compiled code (unlike GHC which uses a dictionary at runtime as well)
    * Reconstructing subtyping for Record types (not in the initial version?)

- Then comes The Typechecker
- If the program typechecks, we run various optimizations
- Then we generate code
-}

module Core.Pipeline where

import State
import Core.Syntax    
import Logs    
import Core.Environment    

-- import Util.IOLogger

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T
import qualified Data.Text.Lazy as TL

import Util.PrettyPrinting as TC
import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

-- import Realms.JS.Compiler as CompilerJS

--------------------------------------------------------------------------------
-- PASS 0: initial desugaring etc after parsing
--------------------------------------------------------------------------------
-- this needs to be run right after parsing:
-- desugaring (changing op calls to App calls etc)
-- fixing remaining tags for Tuples in data constructors (what wasn't fixed during parsing)
-- reversing the order
afterparserPass :: IntState ()
afterparserPass = do
    s <- get
    let mod = runExprPassAndReverse afterparse (parsedModule s)
    put (s { parsedModule = mod } )
    return ()

-- the only reason we need this is because parser reverses the order of the program while parsing,
-- so we need to reverse it again first before we start mapping stuff    
runExprPassAndReverse :: (Expr -> Expr) -> LTProgram -> LTProgram
runExprPassAndReverse f l = rev f l []
    where rev f [] a = a
          rev f ((ex, srci):xs) a = rev f xs ( (f ex, srci):a )

--------------------------------------------------------------------------------
-- PASS 1: building initial typing and top-level lambdas environment
-- This is a pretty important pass - it establishes top-level lambdas, 
-- moves constructor functions from inside Type declarations to the top level,
-- and processes instances of typeclasses (so, applications of typeclass functions to specific types) - 
-- which involves some basic typechecking etc already.
--------------------------------------------------------------------------------

-- Only VarDefinition, Binding and PatternMatch should be seen at the top level
buildEnvironmentM :: (Expr, SourceInfo) -> IntState ()
buildEnvironmentM x@(e,si) = do
    s <- get
    let env = currentEnvironment s
    env' <- processBinding x env
    put s{currentEnvironment = env'}
    -- either (\err -> logWarning err { linePos = (lineNum si), colPos = (colNum si) } )
           
processBinding :: (Expr, SourceInfo) -> Environment -> IntState Environment
-- top level binding
-- putting ONLY RETURN TYPE to the Lambda sig - it's DIFFERENT from the VarDefinition case, can be confusing!!!
processBinding (ex@(Binding v@(Var n t) lam), si) env = 
    case result of
        (Right e)  -> return e
        (Left err) -> (logError err) >> return env
    where result = 
            let e = case t of 
                        SmallType -> addLambda n (lam { sig = t }) (addManyLambdas (extractConstructors ex)   env) -- adding type declaration and exctracting constructors
                        TClass    -> addLambda n (lam { sig = t }) (addClassFuncs  (extractRecord $ body lam) env) -- adding typeclass and extracting function defs
                        _         -> addLambda n (lam { sig = t }) env
                         
            in  maybe (Right e) 
                    -- name conflict - need BETTER ERROR MESSAGING! (line numbers etc)
                    (const $ Left $ LogPayload 
                        (lineNum si) (colNum si) ""
                        ("Tried to add an identifier named " ++ ppr v ++ " but it has already been defined before!")) 
                    (lookupLambda n env)

-- This is where it gets interesting - processing pattern match, that encodes both function definitions, class instances and type functions!
-- If it's a Type  application - need to create another specific type
-- If it's a Class application - need to instantiate all functions and update function lookup tables for specific types of this class
-- If it's a function application - either add a new function to the environment, or update pattern match cases for the existing function. 
processBinding ( pm@(PatternMatch app@(App (VarId name) args) ex), si) env = do
    case (lookupLambda name env) of
        -- no function with such name yet - creating a function with Patterns as a body and types to figure out
        Nothing -> do
            let lam = Lambda {
                      params = emptyRecordFromList args
                    , body   = Patterns[pm]
                    , sig    = ToDerive
                    , preds  = []
                    } 
            return $ addLambda name lam env
        -- Processing function with an empty body
        Just lam@(Lambda _ EMPTY _ _)  -> return $ addLambda name (lam { body = Patterns[pm] }) env
        -- Processing function with already existing patterns - adding the new found one
        Just lam@(Lambda _ (Patterns ps) _ _) -> return $ addLambda name (lam { body = Patterns (ps ++ [pm]) }) env
        -- remaining cases are TYPECLASSES - need to implement yet
        -- Typeclass processing - Application of a class to variables, so defining an INSTANCE
        Just lam@(Lambda _ (Rec funcs) TClass preds) -> do
            liftIO $ putStrLn $ "Found Class in a pattern match:\n" ++ (ppr pm) ++ "\n" ++ (TL.unpack $ pShow pm)
            -- let env' = addClassFuncs funcs env -- this is WRONG!!! We need to add class functions to the top-level when we initially process typeclasses!
            let funcNamePrefix = fullyQualifiedName app
            liftIO $ putStrLn $ "Generated name: " ++ funcNamePrefix
            let env' = env
            return env'
        Just l -> (logError $ LogPayload (lineNum si) (colNum si) ""
                                        ("Unknown pattern match:\n" ++ name ++ " = " ++ show l)) >> return env
            

-- Generic case, adding a warning (it's internal, in production compiler should not be happening at all):
processBinding (ex, si) env = do 
    let lpl = LogPayload 
                (lineNum si) (colNum si) ""
                ("Cannot add the following expression to the Environment during initial Environment Building pass:\n" 
                    ++ (ppr ex) ++ "\n" 
                    -- ++ (TL.unpack (pShow ex))
                    ++ "\nThe expression is parsed and stored in LTProgram, but is not in the Environment.")
    logWarning lpl { linePos = (lineNum si), colPos = (colNum si) } 
    return env

{-
PatternMatch 
    ( App ( VarId "Group" ) [ VarId "Int" ] ) 
    ( Rec 
        [ Field 
            { fieldName = "+" 
            , fieldType = ToDerive
            , fieldValue = Lam 
                ( Lambda 
                    { params = []
                    , body = VarId "primMinus#" 
                    , sig = ToDerive
                    , preds = []
                    } 
                )
            } 
        ]
    )

-}
-- applyClass 

-- The pass itself   
buildEnvPass :: IntState ()
buildEnvPass = buildPrimitivePass >> get >>= pure . parsedModule >>= mapM_ buildEnvironmentM

-- killing primitive bindings for now to test iterative compilation
primBindings = []    
{-
primBindings = [
        Let [(Var "+" ToDerive, Prim PPlus)] EMPTY,
        Let [(Var "-" ToDerive, Prim PMinus)] EMPTY,
        Let [(Var "*" ToDerive, Prim PMul)] EMPTY,
        Let [(Var "/" ToDerive, Prim PDiv)] EMPTY
    ]
-}

buildPrimitivePass :: IntState ()
buildPrimitivePass = mapM_ (\b -> buildEnvironmentM (b, SourceInfo 0 0 "")) primBindings

-------------------------------------------------------------------------------
-- Important helper functions
-------------------------------------------------------------------------------

-- Processing lambda that is a Type and moving its constructors to the top level bindings, so that calls such as Just 4 were able to resolve "Just" correctly

--------------------------------------------------------------------------------
-- PASS n: compilation passes
--------------------------------------------------------------------------------
{-
compile2JSpass :: IntState ()
compile2JSpass = do
    env <- get >>= \s -> pure (currentEnvironment s)
    -- env { lambdas = Map.insert n (v,e) (lambdas env) }
    let res = lambdas env -- <- get >>= \s -> pure ( (lambdas . currentEnvironment) s)
    let fkeys = Map.keys res
    mapM_ fenv1 fkeys
    where fenv1 tk = do 
                        env <- get >>= \s -> pure (currentEnvironment s)
                        let (Just tt) = Map.lookup tk (lambdas env)
                        -- iterating over all bindings and converting them to js
                        let str = "" -- CompilerJS.binding2text tt
                        let env' = env { jsProgram = Map.insert tk str (jsProgram env)}
                        modify' (\s -> s {currentEnvironment = env'})

-}                
                        