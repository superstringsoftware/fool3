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

module Lambda.Pipeline where

import State
import Lambda.Syntax    
import Lambda.Environment    

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Data.Text as T

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
-- PASS 1: building initial typing etc environment
--------------------------------------------------------------------------------
-- addDataConstructor :: Expr -> Environment -> Either Error Environment
-- monadic
addDataConstructorM :: Expr -> SourceInfo -> IntState()
addDataConstructorM e si = do
    s <- get
    let env = currentEnvironment s
    let res = addDataConstructor e env
    liftIO $ putStrLn ("Processing " ++ ppr e)
    either (\err -> logError si{notes=T.pack err}) (\env' -> put s{currentEnvironment = env'}) res

-- Let [(Var,Expr)] Expr    
typeEnvPass :: IntState ()
typeEnvPass = do
    s <- get
    let mod = parsedModule s
    let env = currentEnvironment s
    mapM_ fn mod
    where fn ( Let ( (_, e@(Lam vars (Tuple constag exs typ) typlam preds) ) :[]) _ ,si) = addDataConstructorM e si
          fn ( Let ( (_, e@(Tuple constag exs typ) ) :[]) _ ,si) = addDataConstructorM e si
          fn _ = return ()