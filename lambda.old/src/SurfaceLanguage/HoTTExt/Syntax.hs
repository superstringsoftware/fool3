{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Surface language syntax, importing Core language and extending it with sugar etc

module SurfaceLanguage.HoTTExt.Syntax
where

import qualified Core.HoTTExt.Core as Core

data Expr = 
    CoreExpr Core.Expr -- wrapping Core and extending it with some missing pieces
  | LetIns [Core.Var] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  | App Expr [Expr] -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
  | PatternMatch Expr Expr -- one pattern match, expression on the left should only be App 
  | Patterns [Expr] -- only PatternMatch should be inside
  | BinaryOp Core.Name Expr Expr
  | UnaryOp Core.Name Expr
  deriving (Show, Eq)
