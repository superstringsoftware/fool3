{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U

import qualified Data.HashTable.IO as H

import Core
import Syntax
import Parser

import Control.Monad (zipWithM_)

import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)

type IntState a = StateT InterpreterState IO a

type HashTable k v = H.BasicHashTable k v
type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    funTable :: ExpressionTable, -- global function and operator table
    symTable :: ExpressionTable, -- global symbol table for variable bidnings
    localSymTable :: ExpressionTable, -- local table in the current scope (e.g., when processing a function call)
    typeTable :: ExpressionTable,
    logs     :: [String]
} deriving Show

-- resolves a symbol by name starting with local scope and going up
resolveSymbol :: Name -> IntState Expr
resolveSymbol name = do
    st <- get
    loc <- liftIO $ H.lookup (localSymTable st) name
    if (loc == Nothing) then do
        glob <- liftIO $ H.lookup (symTable st) name
        if (glob == Nothing) then return $ ERROR $ "Couldn't resolve symbol " ++ name
        else do
            let (Just x) = glob
            return x
    else do
        let (Just x) = loc
        return x

-- resolve a function in a global scope (should we have it in local as well, lambdas for instance?)
resolveFunction :: Name -> IntState Expr
resolveFunction name = do
    st <- get
    fun <- liftIO $ H.lookup (funTable st) name
    if (fun == Nothing) then return $ ERROR $ "Couldn't resolve function " ++ name
    else do
        let (Just x) = fun
        return x


-- resolve a function in a global scope (should we have it in local as well, lambdas for instance?)
resolveType :: Name -> IntState Expr
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
    return $ InterpreterState {
                funTable = ft,
                typeTable = tt,
                symTable = st,
                localSymTable = lt,
                logs = []
             }

-- process a single expression and alter the interpreter state correspondigly
processExpr :: Expr -> IntState ()
-- binding functions and ops
processExpr e@(Function name _ _) = do
    st <- get
    liftIO $ H.insert (funTable st) name e
-- processExpr st e@(BinaryDef _ _ _) = addExpression e (funTable st) >> return st
-- processExpr st e@(UnaryDef _ _ _) = addExpression e (funTable st) >> return st
processExpr e@(TypeDef name _ _) = do
    st <- get
    liftIO $ H.insert (typeTable st) name e
{-
processExpr e@(Record name _ _) = do
    st <- get
    liftIO $ H.insert (typeTable st) name e
-}
-- executing binary op
processExpr e@(BinaryOp name _ _) = do
    res <- evalStep e
    -- putStrLn "Evaluation:"
    liftIO $ putStrLn $ show res

-- executing function call
processExpr e@(Call _ _) = do
    liftIO $ putStrLn "Evaluation:"
    res <- evalStep e
    liftIO $ putStrLn $ show res

-- binding a global variable
processExpr (GlobalVar name ex) = do
    st <- get
    liftIO $ addBinding (symTable st) name ex

processExpr _ = do return ()



-- adds function or operator definition to the table
addExpression :: Expr -> ExpressionTable -> IO () -- FunctionTable
addExpression e@(Function name _ _) ft = H.insert ft name e
-- addExpression e@(BinaryDef name _ _) ft = H.insert ft ("operator"++name) e
-- addExpression e@(UnaryDef name _ _) ft = H.insert ft ("operator"++name) e
addExpression e@(TypeDef name _ _) tt = H.insert tt name e
addExpression _ ft = return ()

-- add a variable binding to a table, String is a
addBinding :: ExpressionTable -> String -> Expr -> IO ()
addBinding st sym ex =
    case ex of
        (Function _ _ _)  -> errf
        (Var _)           -> errf
        otherwise -> do H.insert st sym ex
    where errf = do putStrLn "[ERROR] Can't bind a Function / Operator or a Var declaration!"

ftoList :: ExpressionTable -> IO [(Name,Expr)]
ftoList = H.toList

-- process one module (file) and building top-level operator and function table
loadModule :: [Expr] -> InterpreterState -> IO InterpreterState
loadModule exs st = do
    let ft = funTable st
    mapM_ (flip addExpression $ ft) exs
    return st

-- getting main function from the function table
findMain :: ExpressionTable -> IO (Maybe Expr)
findMain ft = H.lookup ft "main"

evalStep :: Expr -> IntState Expr
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
    res <- resolveSymbol (getVarName n)
    case res of
        (ERROR _) -> return e
        otherwise -> return res

{-
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
-}
evalStep e = return $ ERROR ("Not implemented eval: " ++ (show e))

-- evalTrace :: Expr -> IO()
-- evalTrace e = case e of

nameToOp :: String -> (forall a. Num a => a->a->a)
nameToOp "*" = (*)
nameToOp "+" = (+)
nameToOp "-" = (-)
-- nameToOp "/" = (/)

isPrimitive (PInt _) = True
isPrimitive (PFloat _) = True
isPrimitive _ = False


-- evalBinaryOp :: Expr -> Expr
execPrimitiveBinaryOp :: (forall a. Num a => a->a->a)->Expr->Expr->Expr
execPrimitiveBinaryOp op (PInt x1) (PInt x2) = PInt (op x1 x2)
execPrimitiveBinaryOp op (PFloat x1) (PFloat x2) = PFloat (op x1 x2)
execPrimitiveBinaryOp op (PInt x1) (PFloat x2) = PFloat (op (fromIntegral x1) x2)
execPrimitiveBinaryOp op (PFloat x1) (PInt x2) = PFloat (op x1 (fromIntegral x2))
execPrimitiveBinaryOp op e1 e2 = ERROR ("Not implemented op: (" ++ (show e1) ++ ", " ++ (show e2) ++ ")")

{-
evalExprStep :: Expr -> FunctionTable -> IO()
evalExprStep e@(Function _ _ _) ft = funTable >>= addFunction e
-}

-- print types
prettyPrintTT :: ExpressionTable -> IO ()
prettyPrintTT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ show v

-- print functions
prettyPrintFT :: ExpressionTable -> IO ()
prettyPrintFT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ show v

-- print symbols
prettyPrintST :: ExpressionTable -> IO ()
prettyPrintST ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ (k ++ " ≡ " ++ (show v))

testPrg = "def id(x) x; def binary --> 2 (x y) x*y*x;\
\ def fg(x y) x+(y*y);\
\ def main() 4 --> fg(4,17.0) - id(4);"
