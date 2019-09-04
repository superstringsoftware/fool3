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

-- intermediate .Net expression AST -- idea is:
-- optimize Expr AST a bit with some passes, then translate it into
-- this intermediate .Net expression AST, then spit out
-- C#, CLR etc code from this directly

-- only signature for the class
data DotNetClassSignature = DotNetClassSignature {
  className :: Name, -- name of the class
  typeParams :: [Name] -- if it's a generic, list of type variables
} deriving (Show, Eq)

-- field inside the class
data DotNetField = DotNetField {
    fieldName :: Name
  , fieldType :: DotNetClassSignature
} deriving (Show, Eq)

-- method signature
data DotNetMethodSignature = DotNetMethodSignature {
    methodName :: Name
  , methodType :: DotNetClassSignature
  , methodArgs :: [DotNetField]
} deriving (Show, Eq)

-- full class definition
data DotNetClass = DotNetClass {
    classSignature :: DotNetClassSignature
  , fields :: [DotNetField]
  , methods :: [DotNetMethodSignature]
  , inherits :: [DotNetClassSignature]
  , comments :: String
} deriving (Show, Eq)

-- Translation methods Expr -> dot net stuff

translateExprToClass :: Expr -> [DotNetClass]
translateExprToClass e@(Type tname vars cons) = parent : (map (translateConsToClass (classSignature parent)) cons)
    where 
      parent = DotNetClass {
          comments = verboseExpr e
        , classSignature = DotNetClassSignature tname []
        , fields = []
        , methods = []
        , inherits = []
      } 
      translateConsToClass pr (Lam nm vars _ _) = DotNetClass {
          comments = ""
        , classSignature = DotNetClassSignature nm (extractAllTypeVars vars)
        , fields = []
        , methods = []
        , inherits = [pr]
      }

  
-- finding all type variables in a lambda, keeping only unique values
extractAllTypeVars vars = map cap (sortUniq $ foldl fn [] vars)
  where fn acc (Id _ tp) = concat [acc, (extractTypeVars tp [])]



class ToDotNet a where
  compileNet :: a -> String

cap :: String -> String
cap (head:tail) = Data.Char.toUpper head : tail
cap [] = []

verboseExpr e = "// Compiled expr: \n// " ++ show e ++ "\n"

-- Some very basic optimization passes for our naive translation

-- adding default field names to our unnamed datatypes - since we need the names in .Net
renameUnnamed :: Expr -> Expr
renameUnnamed = descend f where
    f (Lam nm vars tup tp) = (Lam nm (fnmap 0 vars) tup tp)
    f e = e
    fnmap _ [] = []
    fnmap acc (v:vs) = (fn acc v):(fnmap (acc+1) vs)
    fn i (Id "" vtype) = Id ("__FIELD__" ++ (show i)) vtype
    fn _ vr = vr


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
    ++ concat (map (processCons tname) cons)
    
-- converting specific constructor function to a child class    
processCons tname (Lam nm vars (Tuple nm1 fields) tp)= "public class " 
  ++ nm ++ (buildTypeVars $ extractAllTypeVars vars)
  ++ " : " ++ tname
  ++ " {\n"
  ++ (processVarsToFields vars)
  ++ "}\n"

-- converting record fields to field names
processVarsToFields [] = ""
processVarsToFields xs = foldr fn "" xs
  where fn (Id nm tp) acc = typeToClassField nm tp ++ "\n" ++ acc


-- build generic class string from the list of type variables
buildTypeVars [] = ""
buildTypeVars (x:xs) = "<" ++ (cap x) ++ (foldr fn "" xs) ++ ">"
  where fn nm acc = acc ++ ", " ++ (cap nm)


-- converting list of type vars to c# template string -- NOT USED NOW
{-
buildTypeVarsForType [] = ""
buildTypeVarsForType ( (TyVar nm _) : []) = "<" ++ (cap nm) ++ ">"
buildTypeVarsForType ( (TyVar nm _) : xs) = "<" ++ (cap nm) ++ (foldr fn "" xs) ++ ">"
  where fn (TyVar nm1 _) acc = acc ++ ", " ++ (cap nm1)

  -- processing fields
processFields _ [] = ""
processFields i (f:fs) = typeToClassField ("__F" ++ show i ++ "__") f ++ "\n" ++ processFields (i+1) fs
-}
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
-- typeToClassField nm (TApp (TCon tn) (TVar vn)) = "public " ++ tn ++ "<" ++ cap vn ++ ">" ++ " " ++ nm ++ " {get;}"
typeToClassField nm (TApp (TCon tn) (TVar vn)) = "public " ++ tn ++ " " ++ nm ++ " {get;}"
typeToClassField nm e = "NOT IMPLEMENTED CONVERSION FOR " ++ show e

-- compiling everything we have in the state table now
compile :: IntState ()
compile = do
  ts <- gets typeTable
  liftIO $ H.mapM_ f ts where
    f (k,v) = do 
      putStrLn $ compileExpr $ renameUnnamed v
      putStrLn $ show $ translateExprToClass v


