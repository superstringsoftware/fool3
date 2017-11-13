{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
module Syntax where

import Data.Word
import qualified Data.Vector.Unboxed as U

import Core
import Data.Text

-- type Name = String
type TypedVar = (String, String) -- simple alias for a typed variable

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
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr -- let x=1, y=2 in x*y; - bound expression blocks, is it basically lambda??
  -- | BinaryDef Name [Name] Expr -- operator definitions - move to function definition??
  -- | UnaryDef Name [Name] Expr
  | GlobalVar Name Expr -- binding for a global var, interpreter only
  | ERROR String -- debugging only?
  deriving (Eq, Ord, Show)

prettyPrint :: Expr -> String
prettyPrint (PFloat x) = show x
prettyPrint (PInt x) = show x
prettyPrint (VFloat x) = show x
prettyPrint (VInt x) = show x
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
