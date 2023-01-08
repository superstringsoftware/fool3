{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Pipeline
where

import State
import Core
import Logs

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List.Index

import Util.PrettyPrinting as TC
import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

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
    mod <- (runExprPassAndReverse afterparse (parsedModule s))
    put (s { parsedModule = mod } )
    -- now for more interesting stuff, initial optimizations with error checks
    return ()

-- reversal plus a bunch of initial error checks
runExprPassAndReverse :: (Expr -> IntState Expr) -> LTProgram -> IntState LTProgram
runExprPassAndReverse f l = rev f l []
    where rev f [] a = pure a
          rev f ((ex, srci):xs) a = f ex >>= \ttt -> rev f xs ( (ttt, srci):a )

-- initial quick checks and optimizations
afterparse :: Expr -> IntState Expr
-- sum type - need to map over all constructors and expand them to the full signature form
afterparse (SumType lam@(Lambda typName typArgs (Constructors cons) typTyp)) = 
    pure $ SumType lam { body = Constructors $ Prelude.map fixCons cons }
    where fixCons lam@(Lambda nm args ex typ) = if (ex /= UNDEFINED) then lam else lam { body = Tuple $ Prelude.map (\v -> Id $ name v) args}

-- function with pattern match - check arity etc
{- 
afterparse (Function lam@(Lambda nm args (PatternMatches pms) tp)) = do
    pms' <- mapM (handlePM lam args) pms
    return $ Function (lam { body = PatternMatches pms'} )
-}
afterparse e = pure e



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
-- function definition
processBinding (Function lam, si) env = pure $ addLambda (lamName lam) lam env
-- treating actions the same way - they will have some proper type eventually anyway
processBinding (Action lam, si) env = pure $ addLambda (lamName lam) lam env

-- now extracting constructors from SumTypes, body is guaranteed to be
-- a list of Lambdas under Constructors constructor
processBinding ( tp@(SumType (Lambda typName typArgs (Constructors cons) typTyp)), si) env = do
    pure $ addManyNamedLambdas cons (addNamedSumType tp env)

processBinding (ex, si) env = do 
    let lpl = LogPayload 
                (lineNum si) (colNum si) ""
                ("Cannot add the following expression to the Environment during initial Environment Building pass:\n" 
                    ++ (ppr ex) ++ "\n" 
                    -- ++ (TL.unpack (pShow ex))
                    ++ "\nThe expression is parsed and stored in LTProgram, but is not in the Environment.")
    logWarning lpl { linePos = (lineNum si), colPos = (colNum si) } 
    return env



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

--------------------------------------------------------------------------------
-- PASS 2: Preliminary Optimizations and basic sanity checks
-- now that we have built the environment (so top level lambda and types bidnings)
-- we can start the optimizations
-- This pass includes proper formation of the Case pattern matches inside
-- functions
-- Question: should we forget about the original source program now
-- and start working with the environment??? If not, we'll need to update
-- the environment as well as the source program, which seems counterproductive?
-- For now, choosing to work with the environment.
--------------------------------------------------------------------------------
caseOptimizationPass :: IntState()
caseOptimizationPass = do
    s <- get
    let lambdas = topLambdas $ currentEnvironment s
    lambdas' <- traverseWithKey f lambdas
    put s{currentEnvironment = (currentEnvironment s) {topLambdas = lambdas'} }
    where f k lam@(Lambda nm args (PatternMatches exs) tp) = do
                exs' <- mapM (expandCase lam) exs
                return lam {body = PatternMatches exs'}
          f k e = return e

expandCase :: Lambda -> Expr -> IntState Expr 
-- we need to map over all of the case x of val statements,
-- check if val has a top level binding and if yes - keep it,
-- if not - means it's a variable substitution and we need to do a
-- beta reduction
expandCase lam cs@(CaseOf recs ex si) = do
    -- liftIO $ putStrLn $ "Analyzing: " ++ ppr cs
    (recs', ex') <- t recs ex
    return $ CaseOf recs' ex' si
    where 
        t [] expr = return ([], expr)
        t (v@(Var nm tp val):xs) expr = 
            case val of 
                (Id name) -> do 
                    -- lookup the name in environment, if it exists, keep the case,
                    -- if not - betaReduce by making the switch
                    -- eventually need to also add a check for literals
                    -- liftIO $ putStrLn $ "found Id name case: " ++ ppr v
                    s <- get
                    let env = currentEnvironment s
                    let mlam = lookupLambda name env
                    case mlam of
                        Nothing -> do
                            -- nothing found in the environment, let's
                            -- beta reduce:
                            -- f (x,y) = case x of Z, y of n -> n
                            -- in the 2nd case, changing all occurences of n
                            -- to y
                            -- liftIO $ putStrLn $ "Didnt find in the environment: " ++ name
                            -- liftIO $ putStrLn $ "Beta reducing!"
                            -- liftIO $ putStrLn $ "orig expr: " ++ show expr
                            let vt = Var name UNDEFINED (Id nm)
                            -- liftIO $ putStrLn $ "Var to reduce to: " ++ show vt
                            let expr' = betaReduce vt expr
                            -- liftIO $ putStrLn $ "reduced expr: " ++ show expr'
                            (lst, expr'') <- t xs expr'
                            -- EXCLUDING this var from the case array:
                            return (lst,expr'')
                        Just lambda -> do
                            -- found a lambda with a given name, 
                            -- keeping everything as is
                            (lst, expr') <- t xs expr
                            return (v:lst,expr')
                val' -> do
                    -- otherwise, simply keep everything as is
                    (lst, expr') <- t xs expr
                    return (v:lst,expr')

        
expandCase lam e = pure e


--------------------------------------------------------------------------------
-- PASS 3: Type checking
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 4: Further optimizations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 5: Compilation - JS
--------------------------------------------------------------------------------
mkConsName :: String -> String -> String
mkConsName typName consName = "cons_" ++ typName ++ "_" ++ consName

compileExprToJS :: Expr -> [String]
compileExprToJS (SumType lam@(Lambda typName typArgs (Constructors cons) typTyp)) = 
    imap (compileConstructorToJS ("cons_" ++ typName ++ "_") ) cons
compileExprToJS (Function lam) = [compileFunctionToJS "" lam]
compileExprToJS e = ["/* NOT SUPPORTED:\n" ++ ppr e ++ "\n*/"]

argsToString :: Record -> String
argsToString args = showListRoBr f args
    where f (Var nm tp vl) = nm

argsToTupleFields :: Record -> String
argsToTupleFields args = showListPlainSep f ", " args
    where f (Var nm tp vl) = nm ++ ": " ++ nm

compileConstructorToJS :: String -> Int -> Lambda -> String
compileConstructorToJS pref i (Lambda nm args ex tp) = "function " ++ pref++nm ++
                         argsToString args 
                         ++ " { return { __consTag: " ++ (show i) ++ ", "
                         ++ argsToTupleFields args ++ " } } "                         
    
compileFunctionToJS :: String -> Lambda -> String
compileFunctionToJS pref (Lambda nm args ex tp) = "function " ++ pref++nm ++
                         argsToString args 
                         ++ funBodyToString ex

-- compiling pattern matches is the most complicated thing as we need to 
-- consult the environment about the order of constructors etc
funBodyToString :: Expr -> String
funBodyToString (Id x) = "{ return " ++ x ++ "; }"
funBodyToString e = " { /* NOT IMPLEMENTED:\n" ++ ppr e ++ "\n*/ }" 
