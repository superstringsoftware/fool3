{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Util.DataStructures
where

import Data.STRef    
import Control.Monad.ST
import Data.Foldable

-- ok this is a VERY IMPERATIVE doubly linked list, but implemented in ST monad so can be used as pure code
-- this is just an exercize in ST and Foldable / Traversable

-- DOESNT WORK THE WAY WE WANTED IT TO WORK

data DoubleListElement a = DoubleListElement {
    cellValue :: a,
    prev :: Maybe (DoubleListElement a),
    next :: Maybe (DoubleListElement a)
}




{-

-- since we have explicit last element we can't have infinite lists
-- use normal list if you want infinite
data DoubleList a = DoubleList {first, last :: (DoubleListElement a)} 
    -- need this otherwise can't handle first / last when it's only 1 element without references
    | DoubleListSingle (DoubleListElement a) 
    | DoubleListEmpty

newList :: a -> DoubleList a
newList el = DoubleListSingle (DoubleListElement el Nothing Nothing)

prepend :: a -> DoubleList a -> DoubleList a
prepend el DoubleListEmpty = newList el
prepend el (DoubleListSingle h) = 
    let newFirst = DoubleListElement el Nothing h 


-- creates new list with just 1 element
newList :: a -> DoubleList s a
newList el = let e = (DoubleListElement el Nothing Nothing) in DoubleList e e

-- adding element in the beginning of a list, O(1) as normal haskell list
prepend :: a -> DoubleList s a -> DoubleList s a
prepend el DoubleListEmpty = newList el
prepend el (DoubleList first last) =
    let newFirst = DoubleListElement {cellValue = el, prev = Nothing, next = Just $ runST (newSTRef first)} 
    in  DoubleList newFirst last
    
-- adding element in the end of a list, also O(1) unlike O(n) for normal haskell
append :: a -> DoubleList s a -> ST s (DoubleList s a)
append el DoubleListEmpty = newList el
append el (DoubleList first last) =
    newSTRef (DoubleListElement {cellValue = el, prev = Just last, next = Nothing}) >>= \ref -> pure $ DoubleList first ref
    

-- instance Foldable (DoubleList s) where
    -- foldr :: (a -> b -> b) -> b -> DoubleList s a -> b

-- foldr f inAcc DoubleListEmpty = inAcc
foldr f inAcc (DoubleList first last) = (readSTRef first >>= \el -> pure $ foldrEl f inAcc el)
    where foldrEl :: (a -> b -> b) -> b -> DoubleListElement s a -> b
          foldrEl g acc (DoubleListElement cell prev Nothing) = g cell acc
          foldrEl g acc (DoubleListElement cell prev next) = 
                runST (readSTRef next >>= \nxt -> pure $ g cell (foldrEl g acc nxt))
-}