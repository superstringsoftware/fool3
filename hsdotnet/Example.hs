{-# LANGUAGE MagicHash, NoImplicitPrelude #-}

module Example where

import GHC.Prim
import GHC.Types (Int(..), Bool(..))

-- Simple primop usage
add :: Int -> Int -> Int
add (I# x) (I# y) = I# (x +# y)

-- Custom list type
data List a = Nil | Cons a (List a)

-- Map over our list
mapL :: (a -> b) -> List a -> List b
mapL _ Nil = Nil
mapL f (Cons x xs) = Cons (f x) (mapL f xs)

-- Length with accumulator
lengthL :: List a -> Int
lengthL Nil = I# 0#
lengthL (Cons _ xs) = add (I# 1#) (lengthL xs)

-- Constructor test
testList :: List Int
testList = Cons (I# 1#) (Cons (I# 2#) (Cons (I# 3#) Nil))

-- Let binding test
letTest :: Int -> Int
letTest n = let x = add n (I# 2#)
                y = add n n
            in add x y
