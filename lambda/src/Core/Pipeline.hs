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

import Util.PrettyPrinting as TC

import Data.HashMap.Strict as Map

import Realms.JS.Compiler as CompilerJS

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
--------------------------------------------------------------------------------
buildEnvironmentM :: (Expr, SourceInfo) -> IntState ()
buildEnvironmentM x@(e,si) = get >>= \s->
    either (\err -> logWarning err { linePos = (lineNum si), colPos = (colNum si) } )
           (\env' -> put s{currentEnvironment = env'})
           (processOneBinding x (currentEnvironment s))

-- Let [(Var,Expr)] Expr    
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

--------------------------------------------------------------------------------
-- PASS n: compilation passes
--------------------------------------------------------------------------------

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

                        
                        