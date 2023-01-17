{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}

-- This is our PRIMARY CORE LANGUAGE, "Core List Machine", 
-- where we use n-lists for tuples explicitly instead of lambda-applications.
-- NO TYPES, as they must be typechecked by this point fully.

module CLM
where

import Util.PrettyPrinting
import Logs
import Surface

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
  | CLMIAP CLMExpr [CLMExpr] -- application of a function with implicit params,
  -- most often - part of a structure. For further type checking!
  | CLMFieldAccess (Name, Int) CLMExpr -- accessing a field of an expr by name or number
  | CLMCASE [CLMConsTagCheck] CLMExpr -- list of constructor checks that must all fold to True bound to an expr
  | CLMPROG [CLMExpr] -- list of expressions, for now used for Action but needs to change
  | CLMTYPED CLMExpr CLMExpr -- in case we want to give a type to an expression
  | CLMPRIMCALL -- body of the function that is a primitive call
  | CLMLIT Literal
    deriving (Show, Eq)

-- helper function that goes inside all cons tags checks and well checks 
-- if they evaluate to true
evalConsTagChecks :: [CLMConsTagCheck] -> Bool
evalConsTagChecks [] = True
evalConsTagChecks (ct:cts) = if (runConsTagCheck ct) 
    then evalConsTagChecks cts else False

runConsTagCheck (ConsTag nm i, CLMCON (ConsTag nm1 i1) _) = (i == i1)
runConsTagCheck _ = False

resolveCase :: CLMExpr -> Maybe CLMExpr
resolveCase (CLMCASE cts ex) = if evalConsTagChecks cts then Just ex else Nothing
resolveCase e = Nothing

-- this goes through all the cases in a function body and finds the first
-- one that evaluates fully
resolveCases :: CLMLam -> Either String CLMExpr
resolveCases lam@(CLMLamCases vars []) = Left $ "ERROR: No case resolved to an expression in " ++ ppr lam
resolveCases (CLMLamCases vars ((cs@(CLMCASE ctc ex)):cases) ) = 
    case resolveCase cs of
        Nothing -> resolveCases (CLMLamCases vars cases)
        Just e  -> Right e
resolveCases l = Left $ "ERROR: Unexpected expression among the cases in a function " ++ ppr l

-- this function applies a lambda to an array of arguments
-- full application returns body with substituted elements
-- partial application returns new function with new vars and partially substituted body
-- if there are more arguments than vars - return the body applied to the remaining vars
-- do we allow it???
applyCLMLam :: CLMLam -> [CLMExpr] -> CLMExpr
-- terminal case: the same number of args, full application:
applyCLMLam (CLMLam [] body) [] = body
-- terminal case, vars are still left but no more args - partial application:
applyCLMLam (CLMLam (v:vs) body) [] = CLMLAM $ CLMLam (v:vs) body
-- terminal case, no more vars, args remaining - returning body applying to args:
applyCLMLam (CLMLam [] body) (arg:args) = CLMAPP body (arg:args)
-- "normal case", beta-reducing:
applyCLMLam (CLMLam (v:vs) body) (arg:args) = 
    let body' = betaReduceCLM (fst v, arg) body
    in  applyCLMLam (CLMLam vs body') args
-- now the same as above but for more complex "cases" case
-- terminal case full app, returning a lambda unlike last time!!!
applyCLMLam l@(CLMLamCases [] bodies) [] = 
    case (resolveCases l) of
        Left err -> CLMERR err
        Right ex -> ex
applyCLMLam (CLMLamCases (v:vs) bodies) [] = CLMLAM (CLMLamCases (v:vs) bodies)
applyCLMLam (CLMLamCases [] bodies) (arg:args) = CLMAPP (CLMLAM (CLMLamCases [] bodies)) (arg:args)
applyCLMLam (CLMLamCases (v:vs) bodies) (arg:args) = 
    let bodies' = map (betaReduceCLM (fst v, arg)) bodies
    in  applyCLMLam (CLMLamCases vs bodies') args

-- f(x) = expr, x = val, substituting all x appearances in expr for val
betaReduceCLM :: CLMVar -> CLMExpr -> CLMExpr
-- substituting all nm occurences in expr for val
-- no typechecking whatsoever
betaReduceCLM (nm,val) expr = traverseCLMExpr subs expr
  where subs :: CLMExpr -> CLMExpr
        subs e@(CLMID name) = if (nm == name) then val else e
        subs e = e

-- (map f (map (traverseCLMExpr f) exs) )
-- (f $ traverseCLMExpr f ex)
-- we DO NOT traverse left parts of lambda arguments
traverseCLMExpr :: (CLMExpr -> CLMExpr) -> CLMExpr -> CLMExpr
traverseCLMExpr f (CLMLAM (CLMLam args ex)) = (CLMLAM (CLMLam args (f $ traverseCLMExpr f ex)))
traverseCLMExpr f (CLMLAM (CLMLamCases arg exs)) = CLMLAM (CLMLamCases arg (map f (map (traverseCLMExpr f) exs) ))
traverseCLMExpr f (CLMBIND nm ex) = CLMBIND nm (f $ traverseCLMExpr f ex)
traverseCLMExpr f (CLMAPP ex exs) = CLMAPP (f $ traverseCLMExpr f ex) (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMPAP ex exs) = CLMPAP (f $ traverseCLMExpr f ex) (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMCON ct exs) = CLMCON ct (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMFieldAccess ac ex) = CLMFieldAccess ac (f $ traverseCLMExpr f ex)
traverseCLMExpr f (CLMCASE cts ex) = CLMCASE (map (\(ct,e)-> (ct, f $ traverseCLMExpr f e) ) cts ) (f $ traverseCLMExpr f ex)
traverseCLMExpr f (CLMPROG exs) = CLMPROG (map f (map (traverseCLMExpr f) exs) )
traverseCLMExpr f (CLMTYPED ex1 ex2) = CLMTYPED (f $ traverseCLMExpr f ex1) (f $ traverseCLMExpr f ex2)
traverseCLMExpr f e = f e

instance PrettyPrint CLMExpr where
    ppr (CLMERR err) = (as [bold,red] "ERROR: ") ++ err
    ppr (CLMID nm) = nm
    ppr (CLMLAM lam) = ppr lam
    ppr (CLMCASE cschecks ex ) = showListWFormat ppr "{" "}" " && " "{}" cschecks ++ " -> " ++ ppr ex
    ppr (CLMCON (ConsTag nm i) exs) = as [bold,red] nm ++ " "
        ++ showListCuBr ppr exs
    ppr (CLMAPP ex exs) = as [bold] (ppr ex) ++ " " 
        ++ showListRoBr ppr exs
    ppr (CLMIAP ex exs) = as [bold,yellow] (ppr ex) ++ " [?] " 
        ++ showListRoBr ppr exs
    ppr (CLMFieldAccess ("", i) ex) = ppr ex ++ "." ++ show i
    ppr (CLMFieldAccess (nm, _) ex) = ppr ex ++ "." ++ nm
    ppr (CLMPROG exs) = showListWFormat ppr "{\n" "\n}" ",\n" "{}" exs
    ppr (CLMBIND nm ex) = (as [bold] nm) ++ " = " ++ (ppr ex)
    ppr e = show e

instance PrettyPrint CLMConsTagCheck where
    ppr (ConsTag nm i, e) = ppr e ++ (as [bold,yellow] " cons is ") ++ nm ++ "(" ++ show i ++ ")"

pprVar1 (nm,ex) = nm

instance PrettyPrint CLMVar where
    ppr (nm,ex) = nm

instance PrettyPrint CLMLam where 
    ppr (CLMLam args ex) = "λ " ++ (showListRoBr ppr args) ++ ". " ++ ppr ex
    ppr (CLMLamCases args exs) = "λ " ++ (showListRoBr ppr args) ++ ". "
        ++ (showListWFormat ppr "{\n" "\n}" ",\n" "{}" exs)

