{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module DotNet.Translator where

import Control.Monad.IO.Class (liftIO)
import DotNet.Syntax
-- import Data.Text
import Data.Char

import Control.Monad.Trans.State.Strict

import qualified Data.HashTable.IO as H

import State

-- data Cons = Anon Name [Type] | Record Name [(Name, Type)] deriving (Eq, Ord, Show)
--    | Type Name [Var] [Cons] -- sum type built from product constructors cons

class ToDotNet a where
  compileNet :: a -> String

cap :: String -> String
cap (head:tail) = Data.Char.toUpper head : tail
cap [] = []

verboseExpr e = "// Compiled expr: \n// " ++ show e ++ "\n"

compileExpr :: Expr -> String

-- compiling specifc Type
-- there has to be a better way to do the vars processing...
-- now we are translating sumtype type A a b = C a + B b to 
-- public class A<A,B> {}
-- public class C<A,B> : A<A,B> {...} etc
-- very straightforward just to get a taste 
compileExpr e@(Type tname vars cons) = verboseExpr e 
    ++ "public class " 
    ++ tname
    -- ++ tVars
    ++ "{}\n" {-
    ++ concat (map processCons cons)
    where processVars [] = "" -- processing type variables 
          processVars (x:xs) = "<" ++ (cap $ varName x) ++ processVarsTail xs
          processVarsTail [] = "> "
          processVarsTail (x:xs) = ", " ++ (cap $ varName x) ++ processVarsTail xs
          -- setting baseName for the type
          tVars = processVars vars
          -- processing 1 constructor as child classes
          processCons (Tuple nm fields) = "public class " 
            ++ nm
            ++ tVars
            ++ " : " ++ tname ++ tVars
            ++ " {\n"
            ++ (processFields 0 fields)
            ++ "}\n"
          -- processing fields
          processFields _ [] = ""
          processFields i (f:fs) = typeToClassField ("__F" ++ show i ++ "__") f ++ "\n" ++ processFields (i+1) fs
-}
{-
data Type
    = TVar Name -- TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
    | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the constructor
    | TApp Type Type -- Constructor application - Maybe Int, List a etc
    | TArr Type Type -- Function sig - Maybe a -> String etc
    | TForall [Pred] [TVar] Type
    | ToDerive -- added it to handle initial parsing
    | InsType Expr 
-}

-- takes a field name and type and converts it to .Net field
typeToClassField :: String -> Type -> String
typeToClassField nm (TVar tn) = "public " ++ cap tn ++ " " ++ nm ++ " {get;}"
typeToClassField nm (TCon tn) = "public " ++ tn     ++ " " ++ nm ++ " {get;}"
typeToClassField nm (TApp (TCon tn) (TVar vn)) = "public " ++ tn ++ "<" ++ cap vn ++ ">" ++ " " ++ nm ++ " {get;}"
typeToClassField nm e = "NOT IMPLEMENTED CONVERSION FOR " ++ show e

-- compiling everything we have in the state table now
compile :: IntState ()
compile = do
  ts <- gets typeTable
  liftIO $ H.mapM_ f ts where
    f (k,v) = putStrLn $ compileExpr v


