{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.List (sortBy)
import Data.Function (on)

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
                lambdas = lam,
                currentFlags = CurrentFlags {
                  strict = True
                }
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

-- executing binary op
processExpr e@(BinaryOp name _ _) = processExprGeneric False e
processExpr e@(FlApp _ _) = processExprGeneric False e
processExpr e@(SymId _) = processExprGeneric False e

processExpr _ = do return ()

processExprGeneric b e = do
    let e1 = foolToCore e
    liftIO $ putStrLn $ as [bold, underlined] "Converted to:"
    liftIO $ putStrLn $ prettyPrint e1
    fl <- gets currentFlags
    evalExpr (strict fl) b e1 -- first True - then strict, otherwise lazy
    -- liftIO $ putStrLn $ prettyPrint res
    return ()


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
prettyPrintLS ls = do
    list <- H.toList ls
    let res = sortBy (compare `on` fst) list
    mapM_ f res where
    f (k,v) = putStrLn $ clr k ++ " = " ++ prettyPrintTopLevel v
              where clr s = if isUpper (head s) then as [bold, red] s else as [bold, green] s

showLS :: CoreExpressionTable -> IO ()
showLS ls = H.mapM_ f ls
          where f (k,v) = putStrLn $ clr k ++ " = " ++ show v
                          where clr s = if isUpper (head s) then as [bold, red] s else as [bold, green] s
