{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
module Syntax where

import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as V

import DependentTypes.Core
-- import Data.Text

import TermColors

-- Surface language AST syntax
data FlExpr
  = PFloat !Double -- primitive values
  | PInt !Int
  | PByte !Word8
  | PString String -- for parsing in the interpreter mostly, will probably go away
  | FlTuple TupleType [FlExpr] -- generic container for parsing Vectors: <1,2,3>, Lists [1,2,3] and actual Tuples {1,Int,"hello"}
  -- concrete specific types are over
  | Var Var -- variable from Core
  | Type Type -- type from Core
  | TypeDef Name [Var] [FlExpr] -- data Name, then type vars, then constructors: data List a = Cell a (List a) | Nil
  | Constructor Name [Var] -- constructor only, Name then Types or Vars: Cell a (List a)
  | FlApp FlExpr FlExpr -- function call; should we move operator calls here???
  | SymId Name -- again, for Apps
  | NoArgs -- dummy value that is used when we are building App hierarchy when something is called without args. There's probably a better way to do it!
  | Function Name [Var] FlExpr -- function definition: name, variables names, body expr
  | Extern Name [Name] -- external function declaration
  | BinaryOp Name FlExpr FlExpr
  | UnaryOp Name FlExpr
  | FlIf FlExpr FlExpr FlExpr
  | FlLet [FlExpr] FlExpr -- changed format, first [Expr] will basically be an array of function definitions, since values *are* functions
  | ERROR String -- debugging only?
  deriving (Eq, Ord, Show)

-- helper type for parsing {}, [], <>
data TupleType = TTVector | TTList | TTTuple deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Converting Fl to Core
-------------------------------------------------------------------------------
{-
foolToCore :: FlExpr -> Expr
foolToCore (Constructor nm vars) = foldr Lam (Tuple nm (map (\x -> VarId "") vars) ) vars
foolToCore (TypeDef nm vars cons) = foldr Lam (Tuple nm consList ) vars
    where consList = map foolToCore cons
foolToCore (Function nm vars ex) = foldr Lam (foolToCore ex) vars
foolToCore (FlApp e1 e2) = App (foolToCore e1) (foolToCore e2)
foolToCore (SymId nm) = VarId nm
foolToCore (PString s) = Lit $ LString s
foolToCore (BinaryOp nm e1 e2) = App (VarId $ "("++nm++")") (App (foolToCore e1) (foolToCore e2))
foolToCore (FlIf e1 e2 e3) = If (foolToCore e1) (foolToCore e2) (foolToCore e3)
foolToCore (FlLet exs e) = foldr fn (foolToCore e) exs -- unwinding List into the tree
    where fn x@(Function nm _ _) = Let nm (foolToCore x)

foolToCore (FlTuple TTTuple exs) = Tuple "" (map foolToCore exs)

foolToCore (PInt x) = Lit $ LInt x
foolToCore (PFloat x) = Lit $ LFloat x
foolToCore e = VarId $ "NOT IMPLEMENTED: " ++ show e

-- foolToCore (TypeDef nm vars cons) =
-}
-------------------------------------------------------------------------------
-- Pretty Print typeclass
-------------------------------------------------------------------------------
instance PrettyPrint FlExpr where
  prettyPrint (PFloat x) = as [lmagenta] (show x)
  prettyPrint (PInt x) = as [lmagenta] (show x)
  prettyPrint (PString s) = as [green] (show s)
{-
  prettyPrint (VFloat v) = as [bold] "< " ++ V.foldl fn (as [lmagenta] (show $ V.head v)) (V.tail v) ++ as [bold] " >"
                           where fn acc x = acc ++ ", " ++ as [lmagenta] (show x)
  prettyPrint (VInt x) = as [green, bold] (show x)
-}
  prettyPrint (Var v) = prettyPrint v
  -- prettyPrint (Var (Id nm tp)) = nm ++ ":" ++ show tp
  prettyPrint (Function fn v ex) = as [dgray, bold] "func " ++ as [green, bold] fn ++ " " ++
                                   fn1 v ++ " = " ++ prettyPrint ex
                                   where fn1 [] = ""
                                         fn1 (v:vs) = foldl (\acc x -> acc ++ " " ++ prettyPrint x) (prettyPrint v) vs
  prettyPrint (TypeDef tn vs ex) = as [dgray, bold] "type " ++
                                   as [red, bold] tn ++ " " ++
                                   foldl fn "" vs ++ "= " ++ fn1 ex
                                   where fn  acc v = acc ++ prettyPrint v ++ " "
                                         fn1 ex = if null ex then ""
                                                  else foldl fn2 (prettyPrint $ head ex) (tail ex)
                                                  where fn2 acc e = acc ++ as [bold] "| " ++ prettyPrint e
  prettyPrint (Constructor nm ex) = as [red, bold] nm ++ " " ++ foldl fn "" ex
                                    where fn acc e = acc ++ prettyPrint e ++ " "
  prettyPrint e = show e
-- prettyPrint (Var )
