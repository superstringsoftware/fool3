-- this is a very straightforward interpreter directly on Expr AST, without any bytecodes etc
-- no typechecks for now, simple lambda machinery

module Lambda.Interpreter where

import Lambda.Environment
import Lambda.Syntax
import Lambda.Logs
import State

import Util.PrettyPrinting
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict

import Data.HashMap.Strict as Map

{-
  | Lam [Var] Expr Type [Pred] 
  | App Expr [Expr] -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
  | Tuple ConsTag [Expr] Type 
  | Let [Binding] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  | PatternMatch [Expr] Expr 
  | Patterns [Expr] -- only PatternMatch should be used inside here, it's only used inside lambda with patterns!!!
-}

interpretExpr :: Expr -> IntState ()
interpretExpr (Lit l) = liftIO $ putStrLn (ppr l)
interpretExpr (VarId n) = do
    env <- currentEnvironment <$> get
    maybe (liftIO $ putStrLn $ "Expression named '" ++ n ++ "' not found.")
          (\e -> liftIO $ putStrLn (ppr e))
          (Map.lookup n (lambdas env))
-- Application - the most interesting (or the only for that matter)
interpretExpr (App (VarId n) args) = do
    env <- currentEnvironment <$> get
    maybe (liftIO $ putStrLn $ "Expression named '" ++ n ++ "' not found.")
          (\(v,e) -> evaluate e args)
          (Map.lookup n (lambdas env))
    where evaluate f as = do
            liftIO $ putStrLn $ (ppr f) ++ " " ++ ppr as
            let res = betaReduce f as
            liftIO $ putStrLn $ ppr res
            -- liftIO $ putStrLn $ show res
            interpretExpr res

interpretExpr e = liftIO $ putStrLn ("Cannot process expression " ++ show e ++ " yet.\n" ++ ppr e)

-- beta reducing lambda application to arguments
betaReduce :: Expr -> [Expr] -> Expr
betaReduce (Lam []   expr t p) []       = expr -- normal value    
betaReduce (Lam []   expr t p) args     = App expr args -- thunk
betaReduce (Lam vars expr t p) []       = Lam vars expr t p -- function value
betaReduce f@(Lam vars expr t p) (a:as) = betaReduce (betaReduceSingle f a) as
betaReduce e args = App e args

betaReduceSingle :: Expr -> Expr -> Expr
betaReduceSingle (Lam vars expr t p) arg = 
    let (Var name _) = head vars
        vars' = tail vars
    in  Lam vars' (replaceVar name expr arg) t p

-- takes a var with name and replaces all it's occurences with an expression    
replaceVar :: Name -> Expr -> Expr -> Expr
replaceVar name expr arg = traverseModify' fn expr
    where fn v@(VarId nm) = if nm == name then arg else v
          fn e = e