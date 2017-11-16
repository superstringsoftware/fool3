{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module DependentTypes.StrictEval where

import DependentTypes.Core
import DependentTypes.Eval
import State

import qualified Data.HashTable.IO as H
import Control.Monad.Trans.State.Strict -- trying state monad transformer to maintain state
import Control.Monad.IO.Class (liftIO)

{-
Some theory.
If we have f x y = x y, it translates to:
\x. \y. x y
then let's say we call f (1+1) (2 + 2).
Lazy evaluation goes like, left to right:
f (1+1) (2+2)
(\x. \y. x y) (1+1) (2+2)
(\y. (1+1) y) (2+2)
(1+1) (2+2)
2 (2+2)
2 4
When we are strict, we need to evaluate arguments first
(and also make sure it's not a partial application? or not really?)
f (1+1) (2+2)
f (1+1) 4
f 2 4
(\x. \y. x y) 2 4
(\y. 2 y) 4
2 4
What if it's partial?
Lazy:
f (1+1)
(\x. \y. x y) (1+1)
\y. (1+1) y
Strict:
f (1+1)
f 2
(\x. \y. x y) 2
\y. 2 y

So, in our AST (or close enough) it would look like:

-}


-------------------------------------------------------------------------------
-- Strict Eval (kindof?)
-------------------------------------------------------------------------------


evalStepStrict b e = evalStep b e
