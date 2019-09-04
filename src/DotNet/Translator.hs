{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module DotNet.Translator where

import Control.Monad.IO.Class (liftIO)
import DotNet.Syntax
-- import Data.Text
import Data.Char

import Control.Monad.Trans.State.Strict

import qualified Data.HashTable.IO as H

import State

import Data.List.Unique

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
    ++ "{}\n" 
    ++ concat (map (processCons tname vars) cons)
    
processCons tname tvars (Lam nm vars (Tuple nm1 fields) tp)= "public class " 
  ++ nm ++ (buildTypeVars $ extractAllTypeVars vars)
  ++ " : " ++ tname
  ++ " {\n"
  -- ++ (processFields 0 fields)
  ++ "}\n"

-- build a generic from TVars - we only take those bound variables in Lambda that have TVar as type!!!
-- filterTypeVars 

-- finding all type variables in a lambda, keeping only unique values
extractAllTypeVars vars = sortUniq $ foldl fn [] vars
  where fn acc (Id _ tp) = concat [acc, (extractTypeVars tp [])]

-- build generic class string from the list of type variables
buildTypeVars [] = ""
buildTypeVars (x:xs) = "<" ++ (cap x) ++ (foldr fn "" xs) ++ ">"
  where fn nm acc = acc ++ ", " ++ (cap nm)

{-
buildTypeVars vars = foldr fn "" vars 
  where fn (Id _ tp) acc = acc ++ ", " ++ (show $ extractTypeVars tp [])
-}

-- converting list of type vars to c# template string 
buildTypeVarsForType [] = ""
buildTypeVarsForType ( (TyVar nm _) : []) = "<" ++ (cap nm) ++ ">"
buildTypeVarsForType ( (TyVar nm _) : xs) = "<" ++ (cap nm) ++ (foldr fn "" xs) ++ ">"
  where fn (TyVar nm1 _) acc = acc ++ ", " ++ (cap nm1)

  -- processing fields
processFields _ [] = ""
processFields i (f:fs) = typeToClassField ("__F" ++ show i ++ "__") f ++ "\n" ++ processFields (i+1) fs
{-
    data Expr 
    = Lit Literal
    | VarId Name
    | Type Name [Var] [Expr] -- sum type built from product constructors, which are Lambdas themselves!
    | BinaryOp Name Expr Expr
    | UnaryOp  Name Expr
    | Tuple Name [Expr] -- polymorphic tuple. 
    | Record Name [(Name, Expr)] -- polymorphic record
    | NewRecord Name [Var] -- polymorphic record
    | App Expr Expr
    | Case Expr [(Expr, Expr)] -- case: which expr we are inspecting, alternatives
    | Lam Name [Var] Expr Type -- typed function (or any typed expression for that matter)
    -- class 
    | Typeclass     Name [Pred] [Var] [Expr] -- Expr here can only be Lam as it's a list of functions basically, where interface is just an empty expression
    | Typeinstance  Name [Pred] Type  [Expr] -- for parsing typelcass instances - Name is typeclass name, Type is the type and [Expr] are all Lambdas
    | EMPTY
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


