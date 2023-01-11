{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}

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

instance PrettyPrint CLMExpr where
    ppr (CLMERR err) = (as [bold,red] "ERROR: ") ++ err
    ppr (CLMID nm) = nm
    ppr (CLMLAM lam) = ppr lam
    ppr (CLMCASE cschecks ex ) = showListWFormat ppr "{" "}" " && " "{}" cschecks ++ " -> " ++ ppr ex
    ppr (CLMCON (ConsTag nm i) exs) = as [bold,red] nm ++ " "
        ++ showListCuBr ppr exs
    ppr (CLMAPP ex exs) = as [bold] (ppr ex) ++ " " 
        ++ showListRoBr ppr exs
    ppr (CLMFieldAccess ("", i) ex) = ppr ex ++ "." ++ show i
    ppr (CLMFieldAccess (nm, _) ex) = ppr ex ++ "." ++ nm
    ppr (CLMPROG exs) = showListWFormat ppr "{\n" "\n}" ",\n" "{}" exs
    ppr (CLMBIND nm ex) = (as [bold] nm) ++ " = " ++ (ppr ex)
    ppr e = show e

instance PrettyPrint CLMConsTagCheck where
    ppr (ConsTag nm i, e) = ppr e ++ (as [bold,yellow] " is ") ++ nm ++ "(" ++ show i ++ ")"

pprVar1 (nm,ex) = nm

instance PrettyPrint CLMVar where
    ppr (nm,ex) = nm

instance PrettyPrint CLMLam where 
    ppr (CLMLam args ex) = "λ " ++ (showListRoBr ppr args) ++ ". " ++ ppr ex
    ppr (CLMLamCases args exs) = "λ " ++ (showListRoBr ppr args) ++ ". "
        ++ (showListWFormat ppr "{\n" "\n}" ",\n" "{}" exs)

