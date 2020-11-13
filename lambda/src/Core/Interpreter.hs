-- this is a very straightforward interpreter directly on Expr AST, without any bytecodes etc
-- no typechecks for now, simple lambda machinery

module Core.Interpreter where

import Environment
import Core.Syntax
import Logs
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

interpretExpr :: Expr -> IntState Expr
interpretExpr e@(Lit l) = trace (ppr l) >> pure e
interpretExpr e@(ERROR _) = trace (ppr e) >> pure e
interpretExpr ex@(VarId n) = do
    env <- currentEnvironment <$> get
    maybe ((trace $ "Expression named '" ++ n ++ "' not found.") >> pure ex)
          (\(_,e) -> trace (ppr e) >> pure e)
          (Map.lookup n (lambdas env))
interpretExpr e@(Tuple _ _ _) = (trace $ (ppr e)) >> pure e
-- Application - the most interesting (or the only for that matter)
interpretExpr ex@(App (VarId n) args) = do
    env <- currentEnvironment <$> get
    maybe ( (trace $ "Expression named '" ++ n ++ "' not found.") >> pure ex)
          (\(v,e) -> evaluate e args)
          (Map.lookup n (lambdas env))
    where evaluate f as = do
            trace $ (ppr f) ++ " " ++ ppr as
            let res = betaReduce f as
            trace $ ppr res
            -- liftIO $ putStrLn $ show res
            interpretExpr res

-- primops are strict:
interpretExpr (App (Prim op) ((Lit x):(Lit y):[]) ) = do
    let res = runLitPrimOp x y op
    trace $ ppr res
    pure $ Lit res
interpretExpr (App (Prim op) (x:y:[]) ) = do
    x' <- interpretExpr x 
    trace $ ppr (App (Prim op) (x':y:[]) )
    y' <- interpretExpr y
    trace $ ppr (App (Prim op) (x':y':[]) )
    interpretExpr (App (Prim op) [x',y'] ) 
{-    
interpretExpr (App (Prim op) (arg:(Lit y):[]) ) = 
    interpretExpr arg >>= \arg' -> interpretExpr (App (Prim op) [arg',(Lit y)] ) 
interpretExpr (App (Prim op) ((Lit x):arg:[]) ) = 
    interpretExpr arg >>= \arg' -> interpretExpr (App (Prim op) [(Lit x),arg'] ) 
-}  
interpretExpr e = liftIO $ putStrLn ("Cannot process expression " ++ show e ++ " yet.\n" ++ ppr e) >> pure e

runLitPrimOp :: Literal -> Literal -> PrimOp -> Literal
runLitPrimOp (LInt x) (LInt y) PPlus = LInt (x+y)
runLitPrimOp (LFloat x) (LFloat y) PPlus = LFloat (x+y)
runLitPrimOp (LString x) (LString y) PPlus = LString (x++y)
runLitPrimOp _ _ PPlus = LString "Type error while applying primitive +"
runLitPrimOp (LInt x) (LInt y) PMinus = LInt (x-y)
runLitPrimOp (LFloat x) (LFloat y) PMinus = LFloat (x-y)
runLitPrimOp _ _ PMinus = LString "Type error while applying primitive -"
runLitPrimOp (LInt x) (LInt y) PMul = LInt (x*y)
runLitPrimOp (LFloat x) (LFloat y) PMul = LFloat (x*y)
runLitPrimOp _ _ PMul = LString "Type error while applying primitive *"
runLitPrimOp (LInt x) (LInt y) PDiv = LFloat $ (fromIntegral x) / (fromIntegral  y)
runLitPrimOp (LFloat x) (LFloat y) PDiv = LFloat (x/y)
runLitPrimOp _ _ PDiv = LString "Type error while applying primitive /"



-- beta reducing lambda application to arguments
betaReduce :: Expr -> [Expr] -> Expr
-- data constructor application - can only be fully saturated
betaReduce (Lam vars (Tuple cons expr typ ) t p) args = 
    if (lv == la) then Tuple cons args typ
    else if (lv > la) then ERROR "Constructor applications can only be fully saturated."
         else ERROR "Too many arguments in constructor application."
    where lv = length vars
          la = length args
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