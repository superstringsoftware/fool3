{-# LANGUAGE RankNTypes #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U

import qualified Data.HashTable.IO as H

import Syntax
import Parser

type HashTable k v = H.BasicHashTable k v
type FunctionTable = HashTable Name Expr

newFunTable :: IO FunctionTable
newFunTable = H.new

getFunction :: Name -> FunctionTable -> IO (Maybe Expr)
getFunction nm ft = H.lookup ft nm

addFunction :: Expr -> FunctionTable -> IO () -- FunctionTable
addFunction e@(Function name _ _) ft = H.insert ft name e
addFunction e@(BinaryDef name _ _) ft = H.insert ft ("operator"++name) e
addFunction _ ft = return ()

ftoList :: FunctionTable -> IO [(Name,Expr)]
ftoList = H.toList

{-
initBuiltinOps :: IO FunctionTable
initBuiltinOps = do
    ft <- H.new
    addFunction (BinaryDef "*" )
-}

-- process one module (file) and building top-level operator and function table
loadModule :: [Expr] -> IO FunctionTable
loadModule exs = do
    ft <- H.new
    mapM_ (flip addFunction $ ft) exs
    return ft

-- getting main function from the function table
findMain :: FunctionTable -> IO (Maybe Expr)
findMain ft = H.lookup ft "main"

-- evalStep :: Expr -> FunctionTable -> Expr
-- evalStep (BinaryOp "*" e1 e2)

-- evalTrace :: Expr -> IO()
-- evalTrace e = case e of



-- evalBinaryOp :: Expr -> Expr
execPrimitiveBinaryOp :: (forall a. Num a => a->a->a)->Expr->Expr->Expr
execPrimitiveBinaryOp op (PInt x1) (PInt x2) = PInt (op x1 x2)
execPrimitiveBinaryOp op (PFloat x1) (PFloat x2) = PFloat (op x1 x2)
execPrimitiveBinaryOp op (PInt x1) (PFloat x2) = PFloat (op (fromIntegral x1) x2)
execPrimitiveBinaryOp op (PFloat x1) (PInt x2) = PFloat (op x1 (fromIntegral x2))
-- execPrimitiveBinaryOp op e1 e2 =

{-
evalExprStep :: Expr -> FunctionTable -> IO()
evalExprStep e@(Function _ _ _) ft = funTable >>= addFunction e
-}

testPrg = "def id(x) x; def binary --> 2 (x y) x*y*x;\
\ def fg(x y) x+(y*y);\
\ def main() 4 --> fg(4,17.0) - id(4);"
