{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
module Syntax where

import Data.Word
import qualified Data.Vector.Unboxed as U

type Name = String

data Expr
  = PFloat !Double -- primitive values
  | PInt !Int
  | PByte !Word8
  | ListExpr [Expr] -- polymorphic list, just a placeholder for now
  | VInt (U.Vector Int) -- vectors of primitive values
  | VFloat (U.Vector Double)
  | VByte (U.Vector Word8)
  | Var String -- variable symbol with a name
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
  deriving (Eq, Ord, Show)

{-
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
