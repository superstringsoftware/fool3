{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
module Syntax where

import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as V

import Core
-- import Data.Text

import TermColors

data Expr
  = PFloat !Double -- primitive values
  | PInt !Int
  | PByte !Word8
  | ListExpr [Expr] -- polymorphic list, just a placeholder for now
  | VInt (U.Vector Int) -- vectors of primitive values
  | VFloat (U.Vector Double)
  | VByte (U.Vector Word8)
  -- concrete specific types are over
  | Var Var -- variable from Core
  | Type Type -- type from Core
  | TypeDef Name [Var] [Expr] -- data Name, then type vars, then constructors: data List a = Cell a (List a) | Nil
  | Constructor Name [Expr] -- constructor only, Name then Types or Vars: Cell a (List a)
  | App Expr Expr -- function call; should we move operator calls here???
  | SymId Name -- again, for Apps
  | NoArgs -- dummy value that is used when we are building App hierarchy when something is called without args. There's probably a better way to do it!
  | Function Name [Var] Expr -- function definition: name, variables names, body expr
  | Extern Name [Name] -- external function declaration
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | Let [Expr] Expr -- changed format, first [Expr] will basically be an array of function definitions, since values *are* functions
  -- | For Name Expr Expr Expr Expr
  -- | Let Name Expr Expr -- let x=1, y=2 in x*y; - bound expression blocks, is it basically lambda??
  -- | BinaryDef Name [Name] Expr -- operator definitions - move to function definition??
  -- | UnaryDef Name [Name] Expr
  -- | GlobalVar Name Expr -- binding for a global var, interpreter only
  | ERROR String -- debugging only?
  deriving (Eq, Ord, Show)

instance PrettyPrint Expr where
  prettyPrint (PFloat x) = as [lmagenta] (show x)
  prettyPrint (PInt x) = as [lmagenta] (show x)
  prettyPrint (VFloat v) = as [bold] "< " ++ V.foldl fn (as [lmagenta] (show $ V.head v)) (V.tail v) ++ as [bold] " >"
                           where fn acc x = acc ++ ", " ++ as [lmagenta] (show x)
  prettyPrint (VInt x) = as [green, bold] (show x)
  prettyPrint (Var v) = prettyPrint v
  -- prettyPrint (Var (Id nm tp)) = nm ++ ":" ++ show tp
  prettyPrint (Function fn v ex) = as [dgray, bold] "func " ++ as [green, bold] fn ++ " " ++
                                   fn1 v ++ " = " ++ prettyPrint ex
                                   where fn1 [] = ""
                                         fn1 (v:vs) = foldl (\acc x -> acc ++ " " ++ prettyPrint x) (prettyPrint v) vs
  prettyPrint (TypeDef tn vs ex) = as [dgray, bold] "type " ++
                                   as [red, bold] tn ++ " " ++
                                   foldl fn "" vs ++ "= " ++ fn1 ex
                                   where fn  acc v = acc ++ prettyPrint v ++ " "
                                         fn1 ex = if null ex then ""
                                                  else foldl fn2 (prettyPrint $ head ex) (tail ex)
                                                  where fn2 acc e = acc ++ as [bold] "| " ++ prettyPrint e
  prettyPrint (Constructor nm ex) = as [red, bold] nm ++ " " ++ foldl fn "" ex
                                    where fn acc e = acc ++ prettyPrint e ++ " "
  prettyPrint e = show e
-- prettyPrint (Var )

{-

data Expr
  = PFloat !Double -- primitive values
  | PInt !Int
  | PByte !Word8
  | ListExpr [Expr] -- polymorphic list, just a placeholder for now
  | VInt (U.Vector Int) -- vectors of primitive values
  | VFloat (U.Vector Double)
  | VByte (U.Vector Word8)
  | Var String String -- variable symbol with a name and a type
  | DataDef Name [Expr] [Expr] -- data Name, then type vars, then constructors: data List a = Cell a (List a) | Nil
  | Constructor Name [Expr] -- constructor only, Name then Types or Vars: Cell a (List a)
  | ParametricType Name [Expr] -- Parametric type call used in constructor definitions, e.g. List a inside Cell
  | Type Name -- Concrete type
  | Call Name [Expr] -- function call; should we move operator calls here???
  | Function Name [Name] Expr -- function definition: name, variable names, expr
  | Extern Name [Name] -- external function declaration
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr -- let x=1, y=2 in x*y; - bound expression blocks, is it basically lambda??
  -- | BinaryDef Name [Name] Expr -- operator definitions - move to function definition??
  -- | UnaryDef Name [Name] Expr
  | GlobalVar Name Expr -- binding for a global var, interpreter only
  | ERROR String -- debugging only?
  -- beginning of types
  | Record Name [(Name,Name)] [Expr] -- simple record, named, [(fieldName, typeName)]
  deriving (Eq, Ord, Show)



instance Show Expr where
    show (PFloat x) = show x
    show (PInt x) = show x
    show (PByte x) = show x
    show (VInt v) = show v
    show (VFloat v) = show v
    show (VByte v) = show v
    show (Var s) = "(Var " ++ s ++ ")"
    show (Call nm ex) = nm ++ (show ex)
    show (Function nm vars defn) = nm ++ (show vars) ++ " ≡ " ++ (show defn)
    show (BinaryOp nm x y) = (show x) ++ nm ++ (show y)
    -- show (BinaryDef nm vars defn) = "operator " ++ nm ++ " " ++ (show vars) ++ " ≡ " ++ (show defn)
    show (Let nm e1 e2) = "let " ++ nm ++ (show e1) ++ (show e2)
    show (ERROR s) = "[ERROR] " ++ s
    show (GlobalVar v def) = "Var " ++ v ++ " ≡ " ++ (show def)
-}


-- next attempt, based on what's in docs/base. For now it's basically incorrect,
-- need to handle the hierarchy of TypeFunction --> TypeConstructor --> Value correctly

-- types in the system
-- data AllTypes = BOTTOM | TOP | EMPTY | UNIT | Type deriving Show

-- record holding generic variable that can be indexing values or types
-- data Variable = Variable String AllTypes deriving Show

-- data TypeFunction = TypeFunction Name [Variable] deriving Show
