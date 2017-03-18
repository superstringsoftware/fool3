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

addFunction :: Expr -> FunctionTable -> IO FunctionTable
addFunction e@(Function name _ _) ft = do
    H.insert ft name e
    return ft
addFunction _ ft = return ft

ftoList :: FunctionTable -> IO [(Name,Expr)]
ftoList = H.toList

-- process one module (file)
loadModule :: [Expr] -> IO FunctionTable
loadModule exs = do
    ft <- H.new
    mapM_ (flip addFunction $ ft) exs
    return ft

findMain :: FunctionTable -> IO (Maybe Expr)
findMain ft = H.lookup ft "main"

{-
evalExprStep :: Expr -> FunctionTable -> IO()
evalExprStep e@(Function _ _ _) ft = funTable >>= addFunction e
-}

testPrg = "def id(x) x; def square(x) x*x;\
\ def fg(x y) x+square(y);\
\ def main() 4 + fg(4,17.0) - id(4);"
