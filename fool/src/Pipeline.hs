{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Pipeline
where

import State
import Core
import Logs

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T hiding (intercalate)
import qualified Data.Text.Lazy as TL
import Data.List.Index
import Data.List (intercalate)

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

-- choose which function to run
localMaybeAlt :: Maybe a -> IntState b -> (a -> IntState b) -> IntState b
localMaybeAlt Nothing  f g = f
localMaybeAlt (Just x) f g = g x

-- this function is a mouthful and needs to be refactored A LOT
expandCase :: Lambda -> Expr -> IntState Expr 
-- First, we expand nested Constructor applications in the pattern
-- matches into the flat list, left to right 
-- Then, we map over all of the case x of val statements,
-- check if val has a top level binding and if yes - keep it,
-- if not - means it's a variable substitution and we need to do a
-- beta reduction
expandCase lam cs@(CaseOf recs ex si) = do
    -- liftIO $ putStrLn $ "Analyzing: " ++ ppr cs
    (recs', ex') <- t recs ex
    return $ CaseOf recs' ex' si
    where 
        -- first beta-reduction step
        t [] expr = return ([], expr)
        -- case nm of val
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
                    localMaybeAlt mlam
                        (do
                            let vt = Var name UNDEFINED (Id nm)
                            let expr' = betaReduce vt expr
                            (lst, expr'') <- t xs expr'
                            -- EXCLUDING this var from the case array:
                            return (lst,expr''))
                        (\lambda -> do
                            -- found a lambda with a given name, 
                            -- check if it's a constructor and then
                            -- expand it to the proper call
                            if (isLambdaConstructor lambda)
                            then do
                                (lst, expr') <- t xs expr
                                return ((Var nm tp (App (Id name) [])):lst,expr')
                            else do
                                -- this means it's a name of some function
                                -- or top level binding - does it even 
                                -- make sense inside a pattern match???
                                (lst, expr') <- t xs expr
                                return (v:lst,expr'))
                    
                -- case nm of vval, first level constructor application:
                -- case x of Succ(n) -> App (Id Succ) [Id n]
                -- here we need to go deep and beta reduce
                -- n = tupleField(0,x)
                vval@(App (Id cons) ex5) -> do
                    -- ok now we need to map over ex5 and run beta-reductions
                    -- inside expr while doing that, while also amending
                    -- the arguments list with new cases
                    (casesToAdd, expr') <- ff ex5 expr
                    (lst, expr'') <- t xs expr'
                    return (v:Prelude.concat [casesToAdd,lst],expr'')
                    where ff [] e8 = pure $ ([], e8)
                          ff ((Id newvar):ks) e8 = do
                              -- liftIO $ putStrLn $ "Running case: " ++ ppr vval ++ ", id: " ++ newvar
                              -- todo: build a sub newvar = tupleField(i,nm) and run beta reduce
                              let vt = Var newvar UNDEFINED (App (Id "tupleField") [Id nm] )
                              let e8' = betaReduce vt e8
                              ff ks e8'
                              pure $ ([], e8')
                          ff smth@(k:ks) e8 = do
                              -- liftIO $ putStrLn $ "Running case: " ++ ppr vval ++ ", but its: " ++ ppr smth
                              -- error: only Ids are currently supported
                              -- in ConApp pattern match on the 1st level
                              let lpl = LogPayload 
                                            (lineNum si) (colNum si) ""
                                            ("Only 1st level is currently supported in pattern matches:\n" 
                                                ++ (ppr vval) ++ "\n")
                              logError lpl { linePos = (lineNum si), colPos = (colNum si) } 
                              ff ks e8
                              pure $ ([], e8)

                val' -> do
                    -- otherwise, simply keep everything as is
                    -- TODO: check proper constructor application!!!
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
compile2JSpass :: IntState()
compile2JSpass = do
    s <- get
    let lambdas = topLambdas $ currentEnvironment s
    let allTypes = types $ currentEnvironment s
    funcsJS <- (traverseWithKey g lambdas)
    typesJS <- (traverseWithKey f allTypes)
    let prog = typesJS `union` funcsJS
    put s{currentEnvironment = (currentEnvironment s) {outProgram = prog} }
    return ()
    where f k expr = pure $ compileExprToJS' expr
          g k lam  = pure $ (compileFunctionToJS "") lam


mkConsName :: String -> String -> String
mkConsName typName consName = "cons_" ++ typName ++ "_" ++ consName

compileExprToJS' :: Expr -> String
compileExprToJS' e = intercalate ("\n" :: String) (compileExprToJS e) 

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

-- taking out prefix in names for now
compileConstructorToJS :: String -> Int -> Lambda -> String
compileConstructorToJS pref i (Lambda nm args ex tp) = "function " ++nm ++
                         argsToString args 
                         ++ " { return { __consTag: " ++ (show i) ++ ", "
                         ++ argsToTupleFields args ++ " } } "                         
    
compileFunctionToJS :: String -> Lambda -> String
compileFunctionToJS pref lam@(Lambda nm args ex tp) = 
    if (isLambdaConstructor lam) then ""
    else "function " ++ pref++nm ++ argsToString args ++ funBodyToString ex

-- compiling pattern matches is the most complicated thing as we need to 
-- consult the environment about the order of constructors etc
funBodyToString :: Expr -> String
funBodyToString (Id x) = "{ return " ++ x ++ "; }"
funBodyToString (App ex exs) = "{ return " 
    ++ (exprOnlyToString ex) 
    ++ showListRoBr exprOnlyToString exs
    ++ "; }"
funBodyToString (PatternMatches cs) = "{\n" ++ 
    (showListPlainSep exprOnlyToString ";\n" cs) ++ ";\n}"
funBodyToString e = " { /* NOT IMPLEMENTED:\n" ++ ppr e ++ "\n*/ }" 

exprOnlyToString :: Expr -> String
exprOnlyToString (Id x) = x
exprOnlyToString (App ex exs) = exprOnlyToString ex ++ showListRoBr exprOnlyToString exs
exprOnlyToString (CaseOf args ex _) = caseToIf args ++ " return " 
    ++ exprOnlyToString ex
    where caseToIf vars = "if (" ++ (showListPlainSep ff " and " vars)
            ++ " )"
          ff (Var nm tp val) = "(" ++ nm ++ " == " ++ exprOnlyToString val ++ ")"
          
exprOnlyToString e = "/* NOT IMPLEMENTED: " ++ ppr e ++ "*/"

    
