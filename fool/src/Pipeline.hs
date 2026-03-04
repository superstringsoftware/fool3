{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Pipeline
where

import State
import Surface
import CLM
import Logs

import Control.Monad.Trans.State.Strict
import Control.Monad
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
    s <- get
    let mod = (runExprPassAndReverse (traverseExpr afterparse) (parsedModule s))
    put (s { parsedModule = mod } )
    -- now for more interesting stuff, initial optimizations with error checks
    return ()

-- the only reason we need this is because parser reverses the order of the program while parsing,
-- so we need to reverse it again first before we start mapping stuff    
runExprPassAndReverse :: (Expr -> Expr) -> LTProgram -> LTProgram
runExprPassAndReverse f l = rev f l []
    where rev f [] a = a
          rev f ((ex, srci):xs) a = rev f xs ( (f ex, srci):a )


afterparse :: Expr -> Expr
afterparse (BinaryOp n e1 e2) = App (Id n) ( e1:e2:[])
afterparse (UnaryOp n e) = App (Id n) ( e:[])
-- if/then/else desugars to a 1-arg lambda with expanded pattern match on Bool
-- We use a fresh variable name "__cond" and produce ExpandedCase directly
-- so it doesn't need to go through caseOptimizationPass
afterparse (IfThenElse cond thenE elseE) =
    App (Function (Lambda "" [Var "__cond" (Id "Bool") UNDEFINED]
        (PatternMatches
            [ ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "__cond")]  thenE SourceInteractive
            , ExpandedCase [ExprConsTagCheck (ConsTag "False" 1) (Id "__cond")] elseE SourceInteractive
            ]) UNDEFINED)) [cond]
-- let/in desugars to nested lambda application
afterparse (LetIn [(v, val)] bdy) =
    App (Function (Lambda "" [v] bdy UNDEFINED)) [val]
afterparse (LetIn ((v,val):rest) bdy) =
    App (Function (Lambda "" [v] (afterparse (LetIn rest bdy)) UNDEFINED)) [val]
-- Law desugaring: law declarations become functions returning PropEqT proof terms
-- Curry-Howard: propositions are types, proofs are programs
--   law name(params) = lhs === rhs
--     ==> function name(params) : PropEqT(_, lhs, rhs) = Refl
--   law name(params) = P ==> lhs === rhs
--     ==> function name(params, __proof0: PropEqT(_, P, True)) : PropEqT(_, lhs, rhs) = Refl
afterparse (Law lam lawBody) = desugarLaw lam lawBody
afterparse e = e

-- Desugar a law declaration into a function returning a PropEqT proof term
desugarLaw :: Lambda -> Expr -> Expr
desugarLaw lam lawBody =
    let (premises, conclusion) = collectImplies lawBody
        (retLhs, retRhs) = case conclusion of
            PropEq l r -> (l, r)
            other      -> (other, Id "True")  -- bare expr treated as === True
        proofParams = imap mkProofParam premises
        returnType = App (Id "PropEqT") [UNDEFINED, retLhs, retRhs]
        allParams = params lam ++ proofParams
    in Function (lam { params = allParams, body = Id "Refl", lamType = returnType })

-- Collect premises from nested Implies: P ==> Q ==> R  ->  ([P, Q], R)
collectImplies :: Expr -> ([Expr], Expr)
collectImplies (Implies premise rest) =
    let (prems, concl) = collectImplies rest
    in (premise : prems, concl)
collectImplies e = ([], e)

-- Create a proof parameter from a premise expression
-- If premise is PropEq l r, proof type is PropEqT(_, l, r)
-- If premise is a bare expression, proof type is PropEqT(_, expr, True)
mkProofParam :: Int -> Expr -> Var
mkProofParam i premise =
    let (lhs, rhs) = case premise of
            PropEq l r -> (l, r)
            other      -> (other, Id "True")
        proofType = App (Id "PropEqT") [UNDEFINED, lhs, rhs]
    in Var ("__proof" ++ show i) proofType UNDEFINED


-- Resolve spread fields (..Name) in a constructor lambda's params
-- by looking up the source record/type's constructor fields from the environment
resolveSpreadFields :: Environment -> Lambda -> Lambda
resolveSpreadFields env lam@(Lambda nm args ex tp) =
    let args' = Prelude.concatMap resolveField args
    in  lam { params = args' }
    where
        resolveField v@(Var fieldNm _ _)
            | Prelude.take 2 fieldNm == ".." =
                let srcTypeName = Prelude.drop 2 fieldNm
                in  case lookupType srcTypeName env of
                        Just (SumType (Lambda _ _ (Constructors cons) _)) ->
                            -- take fields from the first (or matching) constructor
                            case cons of
                                (Lambda _ fields _ _ : _) -> fields
                                _ -> [v]  -- couldn't resolve, keep marker
                        _ -> [v]  -- type not found, keep marker
            | otherwise = [v]

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
    -- resolve any spread fields (..Name) in constructor params
    let cons' = Prelude.map (resolveSpreadFields env) cons
    let newCons = imap fixCons cons'
    let newTp = SumType lam { body = Constructors newCons }
    pure $ addManyNamedConstructors 0 newCons (addNamedSumType newTp env)
    where fixCons i lam@(Lambda nm args ex typ) = if (ex /= UNDEFINED)
            then lam
            else lam { body = ConTuple (ConsTag nm i) $ Prelude.map (\v -> Id $ name v) args}

-- so, structures (typeclasses) are very interesting.
-- what we do here is extract all the functions inside the typeclass
-- and turn them into functions dependent on type which they are
-- then we instantiate type from implicit arguments (type of the arguments)
-- and ask environment for a specific function.
-- When there's instance of some typeclass - our functions in the env
-- are updated with functions for a certain type!
-- E.g., for Eq class (==) is really is a func:
{- (==) (a:Type, x:a, y:a) : Bool = {
   {Bool} -> function(x,y) = ...,
   {Nat} -> function(x,y) = ...
},
i.e. it can be represented as 
CaseOf [(Var "a") (Id "Bool")] Lam $ Lambda {...}
then, we encounter something like 4 == plus(x,y) and go through:
lookup ==: (==) (a,x:a,y:a) (4:Int, plus(x,y):Int) ->
    deduce from this that a = Int
    apply (==) (Int) to hopefully get a function
    if we get it - all ok, if we don't - ERROR

Another consideration: for compilation we will actually want
separate functions for each concrete type, so when a structure is 
instanctiated to a type, we need to create all the needed functions
on the top level and then look them up in 2 steps
-}
processBinding ( st@(Structure lam sinfo), si) env = do
    -- Validate param count for algebra/morphism (warning only)
    case structKind sinfo of
        SAlgebra  -> when (Prelude.length (params lam) /= 1) $
            logWarning (LogPayload (lineNum si) (colNum si) ""
                ("algebra " ++ lamName lam ++ " should have exactly 1 type parameter, has "
                    ++ show (Prelude.length (params lam)) ++ "\n"))
        SMorphism -> when (Prelude.length (params lam) < 2) $
            logWarning (LogPayload (lineNum si) (colNum si) ""
                ("morphism " ++ lamName lam ++ " should have 2+ type parameters, has "
                    ++ show (Prelude.length (params lam)) ++ "\n"))
        SGeneral  -> pure ()

    -- Resolve extends: inherit parent functions and laws
    lam' <- resolveExtends env lam (structExtends sinfo)

    -- Register inheritance
    let parentNames = [n | ext <- structExtends sinfo, let n = extractStructRefName ext, n /= ""]
    let env0 = registerInheritance (lamName lam') parentNames env

    -- Validate requires
    validateRequires env0 (structRequires sinfo) si

    let st' = Structure lam' sinfo
    let env' = addNamedStructure st' env0
    case (body lam') of
        Tuple exs -> do
            -- going over all members of a structure and making needed
            -- bindings / transformations
            env'' <- foldM fixStr env' exs
            pure env''
        _ -> do
                let lpl = LogPayload
                            (lineNum si) (colNum si) ""
                            ("Encountered wrong Structure expressions:\n" ++ (ppr st') ++ "\n")
                logError lpl { linePos = (lineNum si), colPos = (colNum si) }
                pure env'
    where fixStr env1 (Function l@(Lambda nm args body tp)) = do
            let body' = CaseOf [] (Function l) SourceInteractive
            let res = Lambda nm (params lam) (PatternMatches [body']) (Function l)
            let env1' = addNamedLambda res env1
            return env1'
          fixStr env1 (Law _ _) = pure env1  -- skip law declarations
          fixStr env1 _ = pure env1
    

-- instance declaration processing
-- instance Eq(Nat) = { function (==)(x:Nat,y:Nat):Bool = eq(x,y) }
processBinding (Instance structName typeArgs impls reqs, si) env = do
    -- Validate requires
    validateRequires env reqs si
    -- extract the type name from the first type arg (e.g., Id "Nat")
    let typeName = case typeArgs of
            (Id nm : _) -> nm
            _           -> ""
    if typeName == ""
    then do
        let lpl = LogPayload (lineNum si) (colNum si) ""
                ("Instance declaration has no valid type argument: "
                    ++ structName ++ "\n")
        logError lpl
        pure env
    else do
        -- for each function in the instance, store a specialized lambda
        env' <- foldM (addInstanceFunc typeName) env impls
        -- propagate instance functions to parent structures
        env'' <- propagateToParent env' structName typeName impls si
        pure env''
    where
        addInstanceFunc typeNm env1 (Function lam) = do
            let funcNm = lamName lam
            pure $ addInstanceLambda funcNm typeNm lam env1
        addInstanceFunc _ env1 e = do
            let lpl = LogPayload (lineNum si) (colNum si) ""
                    ("Invalid expression inside instance declaration, expected function: "
                        ++ ppr e ++ "\n")
            logWarning lpl
            pure env1

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

-- Extract structure name from a structure reference (Id "Eq" or App (Id "Eq") [Id "a"])
extractStructRefName :: Expr -> Name
extractStructRefName (Id nm)       = nm
extractStructRefName (App (Id nm) _) = nm
extractStructRefName _             = ""

-- Resolve extends: inherit parent functions and laws into child structure
resolveExtends :: Environment -> Lambda -> [Expr] -> IntState Lambda
resolveExtends _   lam [] = pure lam
resolveExtends env lam extends = do
    parentMembers <- Prelude.concat <$> mapM getParentMembers extends
    case body lam of
        Tuple childMembers -> do
            -- Only add parent members that child doesn't override
            let childNames = [lamName l | Function l <- childMembers]
                            ++ [lamName l | Law l _ <- childMembers]
            let newMembers = Prelude.filter (notOverridden childNames) parentMembers
            pure lam { body = Tuple (newMembers ++ childMembers) }
        _ -> pure lam
  where
    getParentMembers ref = do
        let parentName = extractStructRefName ref
        case lookupType parentName env of
            Just (Structure parentLam _) -> case body parentLam of
                Tuple exs -> pure exs
                _         -> pure []
            _ -> do
                logWarning (LogPayload 0 0 ""
                    ("extends: parent structure " ++ parentName ++ " not found in environment\n"))
                pure []
    notOverridden childNames (Function l) = lamName l `Prelude.notElem` childNames
    notOverridden childNames (Law l _)    = lamName l `Prelude.notElem` childNames
    notOverridden _ _                     = True

-- When instance Child(T) is declared, also register functions for parent structures
propagateToParent :: Environment -> Name -> Name -> [Expr] -> SourceInfo -> IntState Environment
propagateToParent env structName typeName impls si = do
    let allParents = getAllParents structName env
    -- filter out the structure itself from parents list
    let parents = Prelude.filter (/= structName) allParents
    if Prelude.null parents then pure env
    else do
        -- For each parent, for each impl function, check if function belongs to parent
        foldM (propagateOne impls) env parents
  where
    propagateOne impls' env1 parentName = do
        -- Look up the parent structure to find its function names
        case lookupType parentName env1 of
            Just (Structure parentLam _) -> case body parentLam of
                Tuple exs -> do
                    let parentFuncNames = [lamName l | Function l <- exs]
                    -- For each impl that's a parent function, also add to parent
                    foldM (addIfParent parentFuncNames) env1 impls'
                _ -> pure env1
            _ -> pure env1
    addIfParent parentFuncNames env1 (Function lam)
      | lamName lam `Prelude.elem` parentFuncNames =
          pure $ addInstanceLambda (lamName lam) typeName lam env1
      | otherwise = pure env1
    addIfParent _ env1 _ = pure env1

-- Validate that required structures exist in the environment
validateRequires :: Environment -> [Expr] -> SourceInfo -> IntState ()
validateRequires _   []   _  = pure ()
validateRequires env reqs si = mapM_ checkReq reqs
  where
    checkReq ref = do
        let reqName = extractStructRefName ref
        case lookupType reqName env of
            Just (Structure _ _) -> pure ()
            _ -> logWarning (LogPayload (lineNum si) (colNum si) ""
                    ("requires: structure " ++ reqName ++ " not found in environment\n"))

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
    let env = currentEnvironment s
    let lambdas = topLambdas env
    lambdas' <- traverseWithKey f lambdas
    -- also optimize instance lambdas
    let instLambdas = instanceLambdas env
    instLambdas' <- traverseWithKey f instLambdas
    put s{currentEnvironment = env {topLambdas = lambdas', instanceLambdas = instLambdas'} }
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
    -- liftIO $ putStrLn $ "Analyzing: " ++ ppr cs
    -- ex111 <- fixEmptyConstructors ex
    (cases, ex') <- (t recs ex ([]))
    return $ ExpandedCase cases ex' si
    where 
        t [] expr cases = return (cases, expr)
        -- case nm of val
        t (v@(Var nm tp val):xs) expr cases = do
            -- liftIO $ putStrLn $ "Processing case of: " ++ ppVarCaseOf v
            -- liftIO $ putStrLn $ "Current expr is: " ++ ppr expr
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
                    -- (liftIO $ putStrLn $ "App case in t: \n")
                    -- liftIO $ pPrint vval

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
                            -- liftIO $ putStrLn $ "Created field access on top: " ++ ppr newBoundVarExpr
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
    -- liftIO $ putStrLn $ "Inside caseTransformId: name = " ++ name ++ " boundVar = " ++ ppr boundVarExpr
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
    -- liftIO $ putStrLn $ "Inside caseTransformApp1m name: " ++ name
    -- liftIO $ putStrLn $ "Bound var: " ++ ppr boundVarExpr
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
    -- (liftIO $ putStrLn $ "Id case in caseTransformApp2: " ++ x ++ " name: " ++ name) 
    -- (liftIO $ putStrLn $ "Bound var expr: " ++ ppr boundVarExpr) 
    res <- if isTop then caseTransformIdTop env boundVarExpr x expr
           else caseTransformIdInternal i env boundVarExpr x expr
    case (res) of
            -- error case is terminating!!!
            Left er -> return (cases, expr, er:errs)
            Right (cs, ex) -> caseTransformApp2 (i+1) isTop env boundVarExpr name xs ex (Prelude.concat[cases,cs]) errs
-- case with App is complex
caseTransformApp2 i isTop env boundVarExpr name ((App (Id cons) exs):xs) expr cases errs = do
    -- liftIO $ putStrLn $ "App case in caseTransformApp2: " ++ cons
    -- liftIO $ putStrLn $ "Bound var expr: " ++ ppr boundVarExpr
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
    -- also convert instance lambdas to CLM
    let instLams = instanceLambdas env
    let clmInsts = Map.mapWithKey (\n l -> lambdaToCLMLambda env l) instLams
    let env' = env { clmLambdas = clms, clmInstances = clmInsts }
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
                            if (hasImplicit fun)
                            then CLMIAP (CLMID nm) newArgs
                            else
                                if (Prelude.length (params fun) > (Prelude.length newArgs) )
                                then CLMPAP (CLMID nm) newArgs
                                else if (Prelude.length (params fun) == (Prelude.length newArgs) )
                                    then CLMAPP (CLMID nm) newArgs
                                    else CLMERR $ "ERROR: function is given more arguments than it can handle: " ++ show e
                        Nothing -> CLMERR $ "ERROR: applied unknown function or constructor: " ++ show e
            
    
exprToCLM env (App ex exs) = CLMAPP (exprToCLM env ex) (Prelude.map (exprToCLM env) exs)
exprToCLM env (Function lam) = CLMLAM $ lambdaToCLMLambda env lam
exprToCLM env (Lit l) = CLMLIT l
exprToCLM _ (U n) = CLMU n
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

    
