{-# LANGUAGE OverloadedStrings #-}

{-|
Compiler to JavaScript

text below is a placeholder, edit!
Rough order:
- Parsing, with parse errors and some obvious things we can catch while parsing 
    (e.g., # of pattern match arguments mismatch etc).
- First "desugaring" pass - converting operator calls to function applications, initial rewriting of types from vars
    to tuples WITHOUT any checks, reversing the lines from the parsed order (as it reverses the file since we use a list)

- Then, we need to construct our typing and functional environment before doing type checks. This includes:

    * Reconstructing types from data constructor lambdas, converting constructor tags to ints from Strings etc
    * Setting functions environment with Maps from names to lambdas
    * Reconstructing typeclass hierarchy for efficient handling of instances
    * Processing typeclass instances, generating specialized functions in compile environment, so that we can emit
        specific functions based on type in compiled code (unlike GHC which uses a dictionary at runtime as well)
    * Reconstructing subtyping for Record types (not in the initial version?)

- Then comes The Typechecker
- If the program typechecks, we run various optimizations
- Then we generate code
-}

module Realms.JS.Compiler where

import State
import Core.Syntax    
import Logs    
import Core.Environment

import Util.PrettyPrinting as TC

-- basic straightforward to test the waters
-- core2text :: Expr -> String

binding2text :: Binding -> String
binding2text (Var nm _, ex) = "function " ++ nm ++ (core2text ex)

core2text :: Expr -> String
core2text (Lam vs ex t p)        = "(" ++ (vs2txt "" vs) ++ ") {\n" 
    ++ (core2text ex) ++ "\n}"
    where vs2txt acc [] = acc
          vs2txt acc ((Var nm tp):xs) = acc ++ nm ++ ", " 
--core2text f (App ex exs)          = f $ App (traverseModify f ex) (map (traverseModify f) exs)
--core2text f (Tuple c exs t)       = f $ Tuple c (map (traverseModify f) exs) t
--core2text f (PatternMatch exs ex) = f $ PatternMatch (map (traverseModify f) exs) (traverseModify f ex)
--core2text f (Patterns exs)        = f $ Patterns (map (traverseModify f) exs)
--core2text f (BinaryOp n e1 e2)    = f $ BinaryOp n (traverseModify f e1) (traverseModify f e2)
--core2text f (UnaryOp n e1)        = f $ UnaryOp n (traverseModify f e1)
--core2text f (Let bnds ex)         = f $ Let (map (fn f) bnds) (traverseModify f ex)
  --  where fn g (v, ex) = (v, traverseModify g ex)
core2text e = ppr e
