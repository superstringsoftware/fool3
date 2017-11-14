{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U

import qualified Data.HashTable.IO as H

import DependentTypes.Core
import Syntax
import Parser

import TermColors
import Data.Char (isUpper)

import Control.Monad (zipWithM_)

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)

import State
import DependentTypes.Eval

-- resolves a symbol by name starting with local scope and going up
resolveSymbol :: Name -> IntState FlExpr
resolveSymbol name = do
    st <- get
    loc <- liftIO $ H.lookup (localSymTable st) name
    if loc == Nothing then do
        glob <- liftIO $ H.lookup (symTable st) name
        if glob == Nothing then return $ ERROR $ "Couldn't resolve symbol " ++ name
        else do
            let (Just x) = glob
            return x
    else do
        let (Just x) = loc
        return x

-- resolve a function in a global scope (should we have it in local as well, lambdas for instance?)
resolveFunction :: Name -> IntState FlExpr
resolveFunction name = do
    st <- get
    fun <- liftIO $ H.lookup (funTable st) name
    if (fun == Nothing) then return $ ERROR $ "Couldn't resolve function " ++ name
    else do
        let (Just x) = fun
        return x


-- resolve a function in a global scope (should we have it in local as well, lambdas for instance?)
resolveType :: Name -> IntState FlExpr
resolveType name = do
    st <- get
    fun <- liftIO $ H.lookup (typeTable st) name
    if (fun == Nothing) then return $ ERROR $ "Couldn't resolve type " ++ name
    else do
        let (Just x) = fun
        return x


-- initializing starting state with tables etc
initializeInterpreter :: IO InterpreterState
initializeInterpreter = do
    ft <- H.new
    st <- H.new
    lt <- H.new
    tt <- H.new
    lam <- H.new
    return InterpreterState {
                funTable = ft,
                typeTable = tt,
                symTable = st,
                localSymTable = lt,
                logs = [],
                lambdas = lam
             }

-- process a single expression and alter the interpreter state correspondigly
processExpr :: FlExpr -> IntState ()
-- binding functions and ops
processExpr e@(Function name _ _) = do
    st <- get
    liftIO $ H.insert (funTable st) name e
    liftIO $ H.insert (lambdas st) name (foolToCore e)
-- processExpr st e@(BinaryDef _ _ _) = addExpression e (funTable st) >> return st
-- processExpr st e@(UnaryDef _ _ _) = addExpression e (funTable st) >> return st
processExpr e@(TypeDef name vars cons) = do
    st <- get
    liftIO $ H.insert (typeTable st) name e
    liftIO $ H.insert (lambdas st) name (foolToCore e)
    -- mapM_ (fn (lambdas st)) cons
    -- where fn ls cs@(Constructor nm _) = liftIO $ H.insert ls nm (foolToCore cs)
{-
processExpr e@(Record name _ _) = do
    st <- get
    liftIO $ H.insert (typeTable st) name e
-}
-- executing binary op
processExpr e@(BinaryOp name _ _) = processExprGeneric True e
-- executing function call
processExpr e@(FlApp _ _) = processExprGeneric True e

processExpr _ = do return ()

processExprGeneric b e = do
    let e1 = foolToCore e
    liftIO $ putStrLn $ as [bold, underlined] "Converted to:"
    liftIO $ putStrLn $ prettyPrint e1
    res <- evalExpr b e1
    liftIO $ putStrLn $ prettyPrint res



-- adds function or operator definition to the table
addExpression :: FlExpr -> ExpressionTable -> IO () -- FunctionTable
addExpression e@(Function name _ _) ft = H.insert ft name e
-- addExpression e@(BinaryDef name _ _) ft = H.insert ft ("operator"++name) e
-- addExpression e@(UnaryDef name _ _) ft = H.insert ft ("operator"++name) e
addExpression e@(TypeDef name _ _) tt = H.insert tt name e
addExpression _ ft = return ()

-- add a variable binding to a table, String is a
addBinding :: ExpressionTable -> String -> FlExpr -> IO ()
addBinding st sym ex =
    case ex of
        (Function _ _ _)  -> errf
        (Var _)           -> errf
        otherwise -> do H.insert st sym ex
    where errf = do putStrLn "[ERROR] Can't bind a Function / Operator or a Var declaration!"

ftoList :: ExpressionTable -> IO [(Name,FlExpr)]
ftoList = H.toList

-- process one module (file) and building top-level operator and function table
loadModule :: [FlExpr] -> InterpreterState -> IO InterpreterState
loadModule exs st = do
    let ft = funTable st
    mapM_ (flip addExpression $ ft) exs
    return st

-- getting main function from the function table
findMain :: ExpressionTable -> IO (Maybe FlExpr)
findMain ft = H.lookup ft "main"

{-
evalStep :: FlExpr -> IntState FlExpr
-- basic terminals first
evalStep e@(PInt _) = return e
evalStep e@(PFloat _) = return e

-- primitive operators (what can't be defined via functions)
evalStep e@(BinaryOp "+" e1 e2) =
    case (e1, e2) of
        (PInt x1, PInt x2) -> return $ PInt $ x1 + x2
        (PInt x1, PFloat x2) -> return $ PFloat $ (fromIntegral x1) + x2
        (PFloat x1, PInt x2) -> return $ PFloat $ (fromIntegral x2) + x1
        (PFloat x1, PFloat x2) -> return $ PFloat $ x1 + x2
        otherwise -> do
            e1' <- evalStep e1
            e2' <- evalStep e2
            evalStep (BinaryOp "+" e1' e2')



-- evaluate variable - check bindings, if there are - substituting, if not - returning as is
evalStep e@(Var n) = do
    res <- resolveSymbol (varName n)
    case res of
        (ERROR _) -> return e
        otherwise -> return res


-- the most important - executing a call
evalStep e@(Call fname vals') = do
    -- try resolving vals in case they are variables (need it for stacked calls)
    vals <- mapM evalStep vals'
    fn <- resolveFunction fname
    case fn of
        f@(Function _ vars body) -> do
            state <- get

            -- first, bind vals from (Call) to vars in Function
            liftIO $ zipWithM_ (addBinding $ localSymTable state) vars vals
            liftIO $ putStrLn $ "Binding variables in a call of " ++ (show f)
            liftIO $ prettyPrintST (localSymTable state)
            -- now, evaluating the body with variables bound
            liftIO $ putStrLn $ "Evaluating body " ++ (show body)
            res <- evalStep body
            -- now clearing local symtable
            liftIO $ mapM_ (H.delete $ localSymTable state) vars
            return res
        otherwise -> return fn

evalStep e = return $ ERROR ("Not implemented eval: " ++ (show e))
-}


-- evalTrace :: FlExpr -> IO()
-- evalTrace e = case e of

nameToOp :: String -> (forall a. Num a => a->a->a)
nameToOp "*" = (*)
nameToOp "+" = (+)
nameToOp "-" = (-)
-- nameToOp "/" = (/)

isPrimitive (PInt _) = True
isPrimitive (PFloat _) = True
isPrimitive _ = False


-- evalBinaryOp :: FlExpr -> FlExpr
execPrimitiveBinaryOp :: (forall a. Num a => a->a->a)->FlExpr->FlExpr->FlExpr
execPrimitiveBinaryOp op (PInt x1) (PInt x2) = PInt (op x1 x2)
execPrimitiveBinaryOp op (PFloat x1) (PFloat x2) = PFloat (op x1 x2)
execPrimitiveBinaryOp op (PInt x1) (PFloat x2) = PFloat (op (fromIntegral x1) x2)
execPrimitiveBinaryOp op (PFloat x1) (PInt x2) = PFloat (op x1 (fromIntegral x2))
execPrimitiveBinaryOp op e1 e2 = ERROR ("Not implemented op: (" ++ (show e1) ++ ", " ++ (show e2) ++ ")")

{-
evalExprStep :: FlExpr -> FunctionTable -> IO()
evalExprStep e@(Function _ _ _) ft = funTable >>= addFunction e
-}

-- print types
prettyPrintTT :: ExpressionTable -> IO ()
prettyPrintTT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ prettyPrint v

-- print functions
prettyPrintFT :: ExpressionTable -> IO ()
prettyPrintFT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ prettyPrint v

-- print symbols
prettyPrintST :: ExpressionTable -> IO ()
prettyPrintST ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ (k ++ " ≡ " ++ (show v))


prettyPrintLS :: CoreExpressionTable -> IO ()
prettyPrintLS ls = H.mapM_ f ls where
    f (k,v) = putStrLn $ clr k ++ " = " ++ prettyPrint v
              where clr s = if isUpper (head s) then as [bold, red] s else as [bold, green] s

showLS :: CoreExpressionTable -> IO ()
showLS ls = H.mapM_ f ls
          where f (k,v) = putStrLn $ clr k ++ " = " ++ show v
                          where clr s = if isUpper (head s) then as [bold, red] s else as [bold, green] s
