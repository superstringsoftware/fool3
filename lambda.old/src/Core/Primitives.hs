{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}


module Core.Primitives
where

import Util.PrettyPrinting

import State
import Core.Syntax    
import Logs    
import Core.Environment    

-- import Util.IOLogger

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T
import qualified Data.Text.Lazy as TL

import Util.PrettyPrinting as TC

import Data.HashMap.Strict as Map

data PrimOp = PPlus | PMinus | PMul | PDiv deriving (Show, Eq)

loadPrimitiveEnv :: IntState ()
loadPrimitiveEnv = do
    s <- get
    let env = currentEnvironment s
    let env' = Prelude.foldl (flip addPrimitiveFunc) env primOps
    put s{currentEnvironment = env'}

addPrimitiveFunc :: Name -> Environment -> Environment
addPrimitiveFunc func env = env { topLambdas = Map.insert func (emptyLambda { body = Prim func }) (topLambdas env) }

primOps = [
    "primPlus#"
  , "primMinus#"
  , "primMul#"
  , "primDiv#"
  , "print#"
  ]