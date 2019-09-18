{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

-- Main Compiler pipeline

module Lambda.Pipeline where

import State
import Lambda.Syntax    

import Control.Monad.Trans.State.Strict

afterparserPass :: IntState ()
afterparserPass = do
    s <- get
    let mod = runExprPassAndReverse afterparse (parsedModule s)
    put (s { parsedModule = mod } )
    return ()

-- the only reason we need this is because parser reverses the order of the program while parsing,
-- so we need to reverse it again first before we start mapping stuff    
runExprPassAndReverse :: (Expr -> Expr) -> LTProgram -> LTProgram
runExprPassAndReverse f l = rev f l []
    where rev f [] a = a
          rev f ((ex, srci):xs) a = rev f xs ( (f ex, srci):a )

