{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances, DuplicateRecordFields #-}

module DotNet.Translator where

import Control.Monad.IO.Class (liftIO)
import DotNet.Syntax
-- import Data.Text
import Data.Char

import Control.Monad.Trans.State.Strict

import qualified Data.HashTable.IO as H

import State

import Data.List.Unique
import Data.List.Index

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

----------------------------------------------------------
-- type class for converting stuff into dotnet code - eventually can create directly to CLR etc
----------------------------------------------------------
class ToDotNet a where
  compileNet :: a -> String

-- helper folding function treating zero and many items in the list differently
-- brackets in starting and ending, separates by separator, applies f to each element
specialFold _ _ _ _ [] = ""
specialFold starting ending sep f (x:xs) = starting ++ (f x) ++ (foldl (fn sep f) "" xs) ++ ending
  where fn s g acc el = acc ++ s ++ (g el)

-- helper function converting array of type var names to proper generic <> syntax
typeParamsToDotNet = specialFold "<" ">" ", " id 

{-
typeParamsToDotNet [] = ""
typeParamsToDotNet (x:xs) = "<" ++ x ++ (foldl fn "" xs) ++ ">"
  where fn acc xx = acc ++ ", " ++ xx
-}

instance ToDotNet DotNetClassSignature where
  compileNet DotNetClassSignature{className = cn, typeParams = tp} = cn ++ (typeParamsToDotNet tp)

instance ToDotNet DotNetField where 
  compileNet DotNetField{fieldName = fn, fieldType = ft } = 
    "public " ++ (compileNet ft) ++ " " ++ fn

instance ToDotNet DotNetMethodSignature where
  compileNet = show

instance ToDotNet DotNetClass where
  compileNet DotNetClass{classSignature = cs, fields = fs, methods = ms, inherits = inh, comments = comments} = 
    "public class " ++ (compileNet cs) ++ (specialFold ":" "" ", " compileNet inh)
    ++ "{"
    ++ specialFold "\n" ";\n" ";\n" compileNet fs
    ++ "}"
  

----------------------------------------------------------
-- Translation methods Expr -> dot net stuff
----------------------------------------------------------

translateExprToClass :: Expr -> [DotNetClass]
translateExprToClass e@(Type tname vars cons) = parent : (map (translateConsToClass (classSignature parent)) cons)
    where 
      parent = DotNetClass {
          comments = "Compiled " ++ tname -- verboseExpr e
        , classSignature = DotNetClassSignature tname []
        , fields = []
        , methods = []
        , inherits = []
      } 
      translateConsToClass pr (Lam nm vars _ _) = DotNetClass {
          comments = ""
        , classSignature = DotNetClassSignature nm (extractAllTypeVars vars)
        , fields = map (\(Id nm tp) -> DotNetField nm (typeToClassSignature tp)) vars
        , methods = []
        , inherits = [pr]
      }
      

  
-- finding all type variables in a lambda, keeping only unique values
extractAllTypeVars vars = map cap (sortUniq $ foldl fn [] vars)
  where fn acc (Id _ tp) = concat [acc, (extractTypeVars tp [])]

-- convert type of the expression to class signature
typeToClassSignature :: Type -> DotNetClassSignature
typeToClassSignature (TVar tn) = DotNetClassSignature (cap tn) []
typeToClassSignature (TCon tn) = DotNetClassSignature tn []
typeToClassSignature (TApp (TCon tn) (TVar vn)) = DotNetClassSignature tn []
typeToClassSignature e = DotNetClassSignature ("NOT IMPLEMENTED CONVERSION FOR " ++ show e) []
  

-- stupid utility function for uppercasing strings
cap :: String -> String
cap (head:tail) = Data.Char.toUpper head : tail
cap [] = []

verboseExpr e = "// Compiled expr: \n// " ++ show e ++ "\n"


----------------------------------------------------------
-- Some very basic optimization passes for our naive translation
----------------------------------------------------------

-- adding default field names to our unnamed datatypes - since we need the names in .Net
renameUnnamed :: Expr -> Expr
renameUnnamed = descend f where
    f (Lam nm vars tup tp) = (Lam nm (fnmap 0 vars) tup tp)
    f e = e
    fnmap _ [] = []
    fnmap acc (v:vs) = (fn acc v):(fnmap (acc+1) vs)
    fn i (Id "" vtype) = Id ("__FIELD__" ++ (show i)) vtype
    fn _ vr = vr



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


-- compiling everything we have in the state table now
compile :: IntState ()
compile = do
  ts <- gets typeTable
  liftIO $ H.mapM_ f ts where
    f (k,v) = do 
      let v' = renameUnnamed v
      -- putStrLn $ compileExpr $ renameUnnamed v
      -- putStrLn $ show $ translateExprToClass v'
      mapM_ (putStrLn . compileNet) (translateExprToClass v')
      


