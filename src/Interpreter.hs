{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U

import qualified Data.HashTable.IO as H

import Syntax
import Parser

type HashTable k v = H.BasicHashTable k v
type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    funTable :: ExpressionTable, -- global function and operator table
    symTable :: ExpressionTable, -- global symbol table for variable bidnings
    logs     :: [String]
} deriving Show

-- initializing starting state with tables etc
initializeInterpreter :: IO InterpreterState
initializeInterpreter = do
    ft <- H.new
    st <- H.new
    return $ InterpreterState {
                funTable = ft,
                symTable = st,
                logs = []
             }

-- process a single expression and alter the interpreter state correspondigly
processExpr :: InterpreterState -> Expr -> IO InterpreterState
-- binding functions and ops
processExpr st e@(Function _ _ _) = addExpression e (funTable st) >> return st
processExpr st e@(BinaryDef _ _ _) = addExpression e (funTable st) >> return st
processExpr st e@(UnaryDef _ _ _) = addExpression e (funTable st) >> return st

-- executing binary op
processExpr st e@(BinaryOp name _ _) = do
    res <- evalStep e (funTable st)
    -- putStrLn "Evaluation:"
    putStrLn $ show res
    return st

-- binding a global variable
processExpr st (GlobalVar name ex) = do
    let stable = symTable st
    addBinding name ex stable
    return st

processExpr st _ = return st



-- adds function or operator definition to the table
addExpression :: Expr -> ExpressionTable -> IO () -- FunctionTable
addExpression e@(Function name _ _) ft = H.insert ft name e
addExpression e@(BinaryDef name _ _) ft = H.insert ft ("operator"++name) e
addExpression e@(UnaryDef name _ _) ft = H.insert ft ("operator"++name) e
addExpression _ ft = return ()

-- add a variable binding to a table, String is a
addBinding :: String -> Expr -> ExpressionTable -> IO ()
addBinding sym ex st =
    case ex of
        (BinaryDef _ _ _) -> errf
        (UnaryDef _ _ _)  -> errf
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

evalStep :: Expr -> ExpressionTable -> IO Expr
evalStep e@(BinaryOp op e1 e2) ft
    | isPrimitive e1 && isPrimitive e2 =
        do
            let res = execPrimitiveBinaryOp (nameToOp op) e1 e2
            putStrLn $ "[Primitive][" ++ op ++ "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")]: " ++ (show res)
            return res
    | otherwise = do
        putStrLn $ "[Going deeper in the call of][ " ++ op ++ "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")]"
        e1' <- evalStep e1 ft
        e2' <- evalStep e2 ft
        let res = execPrimitiveBinaryOp (nameToOp op) e1' e2'
        return res

evalStep e@(PInt _) _ = return e
evalStep e@(PFloat _) _= return e
evalStep e@(Call fname vars) ft = do
    return e

evalStep e _ = return $ ERROR ("Not implemented eval: " ++ (show e))

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

prettyPrintFT :: ExpressionTable -> IO ()
prettyPrintFT ft = H.mapM_ f ft where
    f (k,v) = putStrLn $ show v

testPrg = "def id(x) x; def binary --> 2 (x y) x*y*x;\
\ def fg(x y) x+(y*y);\
\ def main() 4 --> fg(4,17.0) - id(4);"
