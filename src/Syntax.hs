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
  deriving (Eq, Ord, Show)
