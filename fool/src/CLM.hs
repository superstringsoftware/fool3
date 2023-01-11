{-# LANGUAGE OverloadedStrings, NamedFieldPuns, GADTs #-}

-- This is our PRIMARY CORE LANGUAGE, "Core List Machine", 
-- where we use n-lists for tuples explicitly instead of lambda-applications.
-- NO TYPES, as they must be typechecked by this point fully.

module CLM
where

import Util.PrettyPrinting
import Logs
import Core

type CLMVar = (Name, CLMExpr) 

-- simple lambda:
-- lambda with cases:
-- Expr on the right side may ONLY be CLMCASE!!!
data CLMLam = CLMLam [CLMVar] CLMExpr | CLMLamCases [CLMVar] [CLMExpr]
    deriving (Show, Eq) 

type CLMConsTagCheck = (ConsTag, CLMExpr) -- check if an expression was constructed with a given constructor tag

data CLMExpr = 
    CLMEMPTY
  | CLMERR String
  | CLMID Name
  | CLMLAM CLMLam
  | CLMBIND Name CLMExpr
  | CLMAPP CLMExpr [CLMExpr] -- saturated application first expr to the tuple of exprs
  | CLMPAP CLMExpr [CLMExpr] -- partial application (When we know the types!)
  | CLMCON ConsTag [CLMExpr] -- saturated constructor application, value in a sense
  | CLMFieldAccess (Name, Int) CLMExpr -- accessing a field of an expr by name or number
  | CLMCASE [CLMConsTagCheck] CLMExpr -- list of constructor checks that must all fold to True bound to an expr
  | CLMPROG [CLMExpr] -- list of expressions, for now used for Action but needs to change
  | CLMTYPED CLMExpr CLMExpr -- in case we want to give a type to an expression
  | CLMPRIMCALL -- body of the function that is a primitive call
    deriving (Show, Eq)

