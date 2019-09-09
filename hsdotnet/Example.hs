{-# LANGUAGE MagicHash, UnliftedFFITypes, NoImplicitPrelude #-}

module Example where

-- BEGIN TEST PROGRAM FOR .NET COMPILATION
import GHC.Prim

data Int = I# !Int#
(I# x) + (I# y) = I# (x +# y)
(I# x) - (I# y) = I# (x -# y)

data List a = Cons a (List a) | Nil

map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

generate (I# 0#) = Nil
generate n  = Cons n (generate (n- (I# 1#)))

main = map (+(I# 10#)) (generate (I# 1000000#))

-- END TEST PROGRAM FOR .NET COMPILATION


-- foreign import ccall c_exp :: Int -> Int

-- main = putStrLn "Hello World"

-- x = I# 4#
-- y = x + (I# 5#)

{-
class Semigroup a where
    (+) :: a -> a -> a
    fun :: a -> Int

instance Semigroup Int where
    (I# x) + (I# y) = I# (x +# y)
    fun x = x + (I# 5#)
-}

-- plus x y = x + y
-- fact 0 = 1
-- fact n = n * fact (n-1)
--
--import Prelude (Eq(..),Int(..),(*),(-),(+))

-- plus x y = (x+y)

-- func = plus 4

-- l = let g = plus 10 in g 5

-- fact 0 = 1
-- fact n = n*fact(n-1)
-- import Foreign.Ptr (Ptr)
-- import System.IO
-- import Prelude(show, Int, (+))

-- foreign import ccall "foo" foo :: IO ()
-- foreign import ccall c_exp :: Int# -> Int#
-- foreign import ccall c_exp :: Int -> Int

-- data Unit = ConUnit
-- data TestData a = Cons a | Null

-- x = ConUnit

-- data Maybe a = Nothing | Just a

{-
class Functor f where 
    fmap :: (a -> b) -> f a -> f b
-}

--plus :: Int# -> Int# -> Int#
--plus x y = x +# y

--data Int = I# Int#
--(+) (I# x) (I# y) = I# (x +# y)
--(*) (I# x) (I# y) = I# (x *# y)
--(-) (I# x) (I# y) = I# (x -# y)

{-
not True = False
not False = True
-}

{-
instance Functor Maybe where 
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- main = putStrLn (show (plus 2 2))
{-
fact 0# = 1#
fact n = n *# fact (n -# 1#)
-}

data Bool = True | False
data Maybe a = Just a | Nothing
data List a = Cons a (List a) | Nil

-- id :: a -> a
-- id x = x

-- not True = False
-- not False = True

-- add :: Int -> Int -> Int
-- add x y = x + y 
-}