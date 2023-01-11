{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Pipeline
where

import State
import Core
import CLM
import Logs

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T hiding (intercalate, map)
import qualified Data.Text.Lazy as TL
import Data.List.Index
import Data.List (intercalate)

import Util.PrettyPrinting as TC
import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map

--------------------------------------------------------------------------------
-- PASS 0: initial desugaring and environment building after parsing - 
-- combined two passes into 1 for better performance
--------------------------------------------------------------------------------
-- this needs to be run right after parsing:
-- desugaring (changing op calls to App calls etc)
-- fixing remaining tags for Tuples in data constructors (what wasn't fixed during parsing)
-- reversing the order
afterparserPass :: IntState ()
afterparserPass = do
    -- s <- get
    -- mod <- (runExprPassAndReverse afterparse (parsedModule s))
    -- put (s { parsedModule = mod } )
    -- now for more interesting stuff, initial optimizations with error checks
    return ()


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
-- primitive functions
processBinding (Prim lam, si) env = pure $ addLambda (lamName lam) lam env

-- now extracting constructors from SumTypes, body is guaranteed to be
-- a list of Lambdas under Constructors constructor
processBinding ( tp@(SumType lam@(Lambda typName typArgs (Constructors cons) typTyp)), si) env = do
    let newTp = SumType lam { body = Constructors $ imap fixCons cons }
    pure $ addManyNamedConstructors 0 cons (addNamedSumType newTp env)
    where fixCons i lam@(Lambda nm args ex typ) = if (ex /= UNDEFINED) 
            then lam 
            else lam { body = ConTuple (ConsTag nm i) $ Prelude.map (\v -> Id $ name v) args}
        

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
-- primBindings = []    
primBindings = [
        (Prim $ Lambda "print#" [Var "s" UNDEFINED UNDEFINED] PrimCall UNDEFINED )
    ]
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


maybeEither :: Maybe a -> b -> (a -> b) -> b
maybeEither Nothing  d f = d
maybeEither (Just x) d f = f x

-- this one checks for ids that maybe arg=0 constructor applications
-- and fixes the expression properly
fixEmptyConstructor :: Environment -> Expr -> Expr
fixEmptyConstructor env ex@(Id name) =
    let mcons = lookupConstructor name env in 
    case mcons of
        Nothing -> ex
        Just (cons,i) -> ConTuple (ConsTag name i) []
fixEmptyConstructor env e = e 

fixEmptyConstructors ex = do
    s <- get
    let env = currentEnvironment s
    pure $ traverseExpr (fixEmptyConstructor env) ex

-- this function is a mouthful and needs to be refactored A LOT
expandCase :: Lambda -> Expr -> IntState Expr 
-- First, we expand nested Constructor applications in the pattern
-- matches into the flat list, left to right 
-- Then, we map over all of the case x of val statements,
-- check if val has a top level binding and if yes - keep it,
-- if not - means it's a variable substitution and we need to do a
-- beta reduction
expandCase lam cs@(CaseOf recs ex si) = do
    liftIO $ putStrLn $ "Analyzing: " ++ ppr cs
    -- ex111 <- fixEmptyConstructors ex
    (cases, ex') <- (t recs ex ([]))
    return $ ExpandedCase cases ex' si
    where 
        t [] expr cases = return (cases, expr)
        -- case nm of val
        t (v@(Var nm tp val):xs) expr cases = do
            liftIO $ putStrLn $ "Processing case of: " ++ ppVarCaseOf v
            liftIO $ putStrLn $ "Current expr is: " ++ ppr expr
            s <- get
            let env = currentEnvironment s
            case val of 
                (Id name) -> do 
                    res <- caseTransformIdTop env (Id nm) name expr
                    case res of
                        -- errors are terminating!
                        Left er -> do
                                    let lpl = LogPayload 
                                                (lineNum si) (colNum si) ""
                                                (er    ++ (ppr v) ++ "\n" )
                                    logError lpl { linePos = (lineNum si), colPos = (colNum si) }
                                    return (cases, expr)
                        Right (cases', expr') -> do t xs expr' (Prelude.concat[cases',cases])
                    
                    
                -- case nm of vval, first level constructor application:
                -- case x of Succ(n) -> App (Id Succ) [Id n]
                -- here we need to go deep and beta reduce
                -- n = tupleField(0,x)
                vval@(App (Id cons) ex5) -> do
                    (liftIO $ putStrLn $ "App case in t: \n")
                    liftIO $ pPrint vval

                    res1 <- caseTransformApp1 env (Id nm) cons ex5
                    case res1 of
                        -- error case is terminating!!!
                        Left er -> do
                                        let lpl = LogPayload 
                                                    (lineNum si) (colNum si) ""
                                                    (er    ++ (ppr v) ++ "\n" )
                                        logError lpl { linePos = (lineNum si), colPos = (colNum si) }
                                        return (cases, expr)
                        Right cs -> do
                            let newBoundVarExpr = (Id nm)
                            liftIO $ putStrLn $ "Created field access on top: " ++ ppr newBoundVarExpr
                            -- launching next level of recursion:
                            (cases',expr', errs') <- caseTransformApp2 0 False env newBoundVarExpr cons ex5 expr [] []
                    
                            mapM_ (\er -> do
                                            let lpl = LogPayload 
                                                        (lineNum si) (colNum si) ""
                                                        (er    ++ (ppr v) ++ "\n" )
                                            logError lpl { linePos = (lineNum si), colPos = (colNum si) })
                                errs'  
                            t xs expr' (Prelude.concat[cases,cs,cases'])
                            -- t (i+1) xs expr' (Prelude.concat[cases,cases'])
                    

        
expandCase lam e = pure e

-- Case conversion for the pattern match is a sort of "double-recursive"
-- function that does the following:
-- when top-level pattern match check finds a constructor application 
-- Con (a1, a2, Con2(b1, b2...)) it takes its arguments, existing cases
-- array, and iterates from left to right doing:
-- 1) beta-reduce when encountering Id n
-- 2) when encountering App (so a new cons application) - 
--    - add a new case analysis into that array
--    - call itself to go down the tree

-- In all of these functions we are working with 2 arguments:
-- 1) list of consTag checks or equality checks that must be
--    folded in order with logical "and" and produce True in order for
-- 2) the right hand expression to be executed

-- First, helper pure functions: 
-- case boundVarName of Id name -- difference between top level 
-- and next level is only in how we make the beta-reduce, so
-- this one gets passed different functions as the first argument and that's it
caseTransformId :: (Expr->Expr) -> Environment -> Expr -> Name -> Expr -> IntState (Either String ([Expr],Expr))
caseTransformId f env boundVarExpr name expr = do
    liftIO $ putStrLn $ "Inside caseTransformId: name = " ++ name ++ " boundVar = " ++ ppr boundVarExpr
    maybeEither (lookupConstructor name env)
            (let vt = Var name UNDEFINED (f boundVarExpr)
                 expr' = betaReduce vt expr
             in  do 
                    -- liftIO $ putStrLn $ "after beta reduce: " ++ ppr expr'
                    return $ Right ([], expr'))
            -- ^^^ nothing found in the environment, making beta-reduce
            -- and returning NOTHING in place of the old case
            -- otherwise checking if it's a constructor and if it is,
            -- returning a correct new "case" expression
            (\(lambda, i) -> pure $ Right ([ExprConsTagCheck (ConsTag name i) boundVarExpr ],expr))
                

-- for top level id, we are passing id as a function
caseTransformIdTop = caseTransformId id
-- for next level, we need to build a proper field access:
caseTransformIdInternal i = caseTransformId (mkTupleFieldAccessExpr i)
-- case boundVarName of Id name -- processing id inside constructor applications
-- example:
-- { Cons(a1,a2) } -> g(a1,a2)
-- once we are inside Cons we process a1 as tupleField(0,boundExpr)
-- where boundExpr in our case will be simply Id boundVarName
-- but as we go deeper it will build up as corresponding calls

-- now for the more complicated case of App ...
-- it is recursive in itself + requires some additional error checks
-- env: environment, boundVarName - 
-- case boundVar name of (App (Id name) ex) -> expr
-- so name is a constructor name. We need to also do an error check
-- if it's not found in the environment!!!

-- so, this first function simply transforms the case statement to the 
-- constructor check, plus checks for errors if there's no such constructor
-- in the environment. Thus we don't need to deal with the RHS of this case here.
caseTransformApp1 :: Environment -> Expr -> Name -> [Expr] -> IntState (Either String [Expr])
caseTransformApp1 env boundVarExpr name ex = do
    liftIO $ putStrLn $ "Inside caseTransformApp1m name: " ++ name
    liftIO $ putStrLn $ "Bound var: " ++ ppr boundVarExpr
    maybeEither (lookupConstructor name env)
            (return $ Left ("Error: constructor " ++ name ++ " is not found in the environment"))
            -- ^^^ nothing found in the environment, it's an ERROR!
            -- otherwise
            (\(lambda, i) -> 
                if (arity lambda == Prelude.length ex)  
                then return $ Right [ExprConsTagCheck (ConsTag name i) boundVarExpr ]
                else return $ Left ("Error: constructor " ++ name ++ " application expects " ++ show (arity lambda) ++ " arguments and was given " ++ show (Prelude.length ex)))
                

-- case boundVar name of (App (Id name) ex) -> exp)r
-- this one falls inside the "ex" (e.g., Con (a1,a2,...) )
-- and iterates through it while building 
caseTransformApp2 :: Int -> Bool -> Environment -> Expr -> Name -> [Expr] -> Expr -> [Expr] -> [String] -> IntState ([Expr], Expr, [String])
caseTransformApp2 i isTop env boundVarExpr name []          expr cases errs = return (cases,expr, errs)
-- case with ids is pretty straightforward
caseTransformApp2 i isTop env boundVarExpr name ((Id x):xs) expr cases errs = do
    (liftIO $ putStrLn $ "Id case in caseTransformApp2: " ++ x ++ " name: " ++ name) 
    (liftIO $ putStrLn $ "Bound var expr: " ++ ppr boundVarExpr) 
    res <- if isTop then caseTransformIdTop env boundVarExpr x expr
           else caseTransformIdInternal i env boundVarExpr x expr
    case (res) of
            -- error case is terminating!!!
            Left er -> return (cases, expr, er:errs)
            Right (cs, ex) -> caseTransformApp2 (i+1) isTop env boundVarExpr name xs ex (Prelude.concat[cases,cs]) errs
-- case with App is complex
caseTransformApp2 i isTop env boundVarExpr name ((App (Id cons) exs):xs) expr cases errs = do
    liftIO $ putStrLn $ "App case in caseTransformApp2: " ++ cons
    liftIO $ putStrLn $ "Bound var expr: " ++ ppr boundVarExpr
    -- first, run basic sanity check plus optional cases array expansion:
    let bv = if isTop then boundVarExpr else mkTupleFieldAccessExpr i boundVarExpr
    res1 <- caseTransformApp1 env bv cons exs
    case res1 of
            -- error case is terminating!!!
            Left er -> return (cases, expr, er:errs)
            -- in case all seems good and we got additional case expressions
            -- things get trickier - now we need to launch NEW
            -- caseTransformApp2 and go down a level, gather all 
            -- expression changes and additional cases, and only then move on
            -- so it's a recursion inside a recursion. Will it even work???
            -- for example, we have a case:
            -- {case x of Cons (a1, Cell (b1, b2) )}
            -- we moved past a1 and encountered Cell application
            Right cs -> do
                let newBoundVarExpr = mkTupleFieldAccessExpr i boundVarExpr
                -- liftIO $ putStrLn $ "Case #: " ++ show i ++ ", Created field access: " ++ ppr newBoundVarExpr
                -- launching next level of recursion:
                (cases',expr', errs') <- caseTransformApp2 0 False env newBoundVarExpr cons exs expr [] []
                -- once we got results of the next level of recursion in ' vars, continue our current recursion:
                caseTransformApp2 (i+1) isTop env boundVarExpr name xs expr' (Prelude.concat[cases,cs,cases']) (Prelude.concat[errs',errs])
-- the rest is errors, so terminating
caseTransformApp2 i isTop env boundVarExpr name eee expr cases errs = 
    return (cases,expr, ("Error: only constructor applications or ids are allowed inside pattern matches on the left side, and we encountered " ++ ppr eee):errs)


--------------------------------------------------------------------------------
-- PASS 3: Type checking
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 4: Further optimizations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 5: Conversion to CLM
--------------------------------------------------------------------------------
lamToCLMPass :: IntState()
lamToCLMPass = do
    s <- get
    let env = currentEnvironment s
    let lambdas = topLambdas env
    let clms = Map.mapWithKey (\n l -> lambdaToCLMLambda env l) lambdas
    let env' = env { clmLambdas = clms }
    let s' = s {currentEnvironment = env'}
    put s'
    

varToCLMVar e v = (name v, exprToCLM e (val v))

varsToCLMVars e vs = Prelude.map (varToCLMVar e) vs

consTagCheckToCLM e (ExprConsTagCheck ct ex) = (ct, exprToCLM e ex)

exprToCLM :: Environment -> Expr -> CLMExpr
exprToCLM _ UNDEFINED = CLMEMPTY
exprToCLM env (Binding v) = CLMBIND (name v) (exprToCLM env $ val v)
exprToCLM env (Statements exs) = CLMPROG (Prelude.map (exprToCLM env) exs)
exprToCLM env (RecFieldAccess ac e) = CLMFieldAccess ac (exprToCLM env e)
exprToCLM env (ExpandedCase cases ex si) = CLMCASE (Prelude.map (consTagCheckToCLM env) cases) (exprToCLM env ex)
exprToCLM env PrimCall = CLMPRIMCALL
exprToCLM env (ConTuple cs exs) = CLMCON cs (Prelude.map (exprToCLM env) exs)
-- have to check for a case when Id in fact refers to an 0-arg constructor call
-- since we need to change it for a corresponding tuple
exprToCLM env (Id n) = 
    case (lookupConstructor n env) of
        Just (cons, i) ->
            if ((params cons) == [] )
            then CLMCON (ConsTag n i) []
            else CLMID n              
        Nothing -> CLMID n
-- application of func or cons to an expression: doing a bunch of checks
-- while converting
exprToCLM env e@(App (Id nm) exs) = 
    let newArgs = Prelude.map (exprToCLM env) exs
        mcons = lookupConstructor nm env
    in  case mcons of
            Just (cons, i) -> 
                  if (Prelude.length (params cons) /= (Prelude.length newArgs) )
                  then CLMERR $ "ERROR: wrong number of arguments in constructor application: " ++ show e
                  else CLMCON (ConsTag nm i) newArgs
            Nothing -> 
                let mfun = lookupLambda nm env
                in  case mfun of
                        Just fun -> 
                            if (Prelude.length (params fun) > (Prelude.length newArgs) )
                            then CLMPAP (CLMID nm) newArgs
                            else if (Prelude.length (params fun) == (Prelude.length newArgs) )
                                 then CLMAPP (CLMID nm) newArgs
                                 else CLMERR $ "ERROR: function is given more arguments than it can handle: " ++ show e
                        Nothing -> CLMERR $ "ERROR: applied unknown function or constructor: " ++ show e
            
    
exprToCLM env (App ex exs) = CLMAPP (exprToCLM env ex) (Prelude.map (exprToCLM env) exs)
exprToCLM _ e = CLMERR $ "ERROR: cannot convert expr to CLM: " ++ show e

lambdaToCLMLambda :: Environment -> Lambda -> CLMLam
lambdaToCLMLambda env (Lambda nm params (PatternMatches exs) tp) = 
    CLMLamCases (varsToCLMVars env params) (Prelude.map (exprToCLM env) exs)
lambdaToCLMLambda env (Lambda nm params body tp) = 
    CLMLam (varsToCLMVars env params) (exprToCLM env body)

--------------------------------------------------------------------------------
-- PASS 6: Compilation - JS
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

    
