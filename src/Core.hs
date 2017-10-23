{-# LANGUAGE OverloadedStrings #-}

module Core where

type Name = String

data Var
  = Id Name Type
  | TyVar Name Kind

data Type
  = TVar TVar
  | TCon TyCon
  | TApp Type Type
  | TArr Type Type
  | TForall [Pred] [TVar] Type

data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name