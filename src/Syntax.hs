{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
module Syntax where

import Data.Word
import qualified Data.Vector.Unboxed as U

type Name = String

data Expr
  = PFloat Double
  | PInt Int
  | PByte Word8
  | ListExpr [Expr]
  | VInt (U.Vector Int)
  | VFloat (U.Vector Double)
  | VByte (U.Vector Word8)
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | ERROR String -- debugging only?
  deriving (Eq, Ord)

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
    show (BinaryDef nm vars defn) = "operator " ++ nm ++ " " ++ (show vars) ++ " ≡ " ++ (show defn)
    show (Let nm e1 e2) = "let " ++ nm ++ (show e1) ++ (show e2)
    show (ERROR s) = "[ERROR] " ++ s
