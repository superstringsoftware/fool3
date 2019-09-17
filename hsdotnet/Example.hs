{-# LANGUAGE MagicHash, UnliftedFFITypes, GHCForeignImportPrim, UnboxedTuples, NoImplicitPrelude #-} -- , NoImplicitPrelude

module Example where

import GHC.Prim
-- import Prelude (undefined)
-- import GHC.Float
-- import Foreign.C.String
{-
g x = funcTest x

f = [10, 20, 1]
f1 = f

funcTest :: Int -> Int
funcTest s = let x = s + 2 
             in let y = s * s
                in y - x
-}
{-
letFuncTest s = let y = s + 2
                    z = s * s
                in  y * z


letFuncTest :: forall a. Num a => a -> a
[LclIdX, Arity=2, Unf=OtherCon []] =
    [] \r [$dNum_s1bR s]
        let {
          sat_s1c7 [Occ=Once] :: a
          [LclId] =
              [$dNum_s1bR s] \u [] * $dNum_s1bR s s; } in
        let {
          sat_s1c6 [Occ=Once] :: a
          [LclId] =
              [$dNum_s1bR s] \u []
                  let {
                    sat_s1c5 [Occ=Once] :: a
                    [LclId] =
                        [$dNum_s1bR] \u []
                            let {
                              sat_s1c4 [Occ=Once] :: Integer
                              [LclId] =
                                  CCCS S#! [2#];
                            } in  fromInteger $dNum_s1bR sat_s1c4;
                  } in  + $dNum_s1bR s sat_s1c5;
        } in  * $dNum_s1bR sat_s1c6 sat_s1c7;

-}

-- funnyFunc x b = let t = if x == 0 then True else False in (b && t)

-- repl x = x : repl x
------- BEGIN SMALLEST TEST PROGRAM
-- import Prelude(undefined)
{-
data Int = I# !Int#
(I# x) + (I# y) = I# (x +# y)

i = I# 10#

print# :: a -> ()
print# = print#

main = print# (i + (I# 20#))
-}
------- END SMALLEST TEST PROGRAM

{-
map' _ [] = []
map' f (x:xs) = (f x):(map' f xs)
-}
{-
len :: [a] -> Int#
len [] = 0#
len (_:xs) = 1# +# len xs
-}
-- data Nat = Z | S Nat

-- data Crazy a = Nuts a | Insane

-- fact 0 = 1
{-
fact :: Int -> Int
fact n = if n==0 then 1 else n*fact(n-1)

data Crazy a = Great a | Stupid

true = Stupid
x5 = Great 5

main = print $ fact 10
-}
{-
infixl 6 :+
type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)
-}

--foreign import ccall writeLn :: CString -> IO ()    

{-
-- foreign declaration for prim functions defined elsewhere
-- foreign import prim "int2Integerzh" int2Integer# :: Int# -> (# Int#, ByteArray# #)

-- Apparently we can trick GHC into accepting new dummy "primops" via simply using this trick:
expDoubleStrange# :: Double# -> Double#
expDoubleStrange# = expDoubleStrange#

expD (D# d) = expDoubleStrange# d

-- Actually, this works even without hashes:
expDoubleStrange :: Double -> Double
expDoubleStrange = expDoubleStrange

-- This means we can define .Net classes interface this way and use GHC typechecker to make sure everything works
-}



-- BEGIN TEST PROGRAM FOR .NET COMPILATION

foreign import ccall "exp" exp :: Int# -> Int#

-- primNumTest 0# = 0#
-- primNumTest 10# = 100#
primNumTest x = exp x


data Int = I# !Int#
(I# x) - (I# y) = I# (x -# y)


class Semigroup a where
    (+) :: a -> a -> a
    fun :: a -> Int

instance Semigroup Int where
    (I# x) + (I# y) = I# (x +# y)
    fun x = x + (I# 5#)


plust x = x + (I# 4#)

data List a = Cons a (List a) | Nil

map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

papTest = map (+ (I# 2#))

constructorCheck = Cons 1 Nil

replicate :: a -> List a
replicate x = Cons x (replicate x)

head (Cons x _) = x

foldr f z Nil     = z
foldr f z (Cons x xs) = x `f` foldr f z xs

length Nil = (I# 0#)
length (Cons _ xs) = (I# 1#) + length xs

generate (I# 0#) = Nil
generate n  = Cons n (generate (n- (I# 1#)))

print# :: List Int -> ()
print# = print#

main = print# (map (+(I# 10#)) (generate (I# 1000000#)))


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

{-
\r [ds_sU5 ds_sU6]
         case ds_sU5 of {
           I# x [Occ=Once] ->
               case ds_sU6 of {
                 I# y [Occ=Once] ->
                     case +# [x y] of sat_sUb [Occ=Once] { __DEFAULT -> I# [sat_sUb]; };
               };
         };,

[CLOSURE][]\r[ds_sU5, ds_sU6] . [Case][App]ds_sU5 [] [bound to] wild_sU7 of: 
I#[binders][x] -> [Case][App]ds_sU6 [] [bound to] wild_sU9 of: 
I#[binders][y] -> [Case][OpApp]+# [x y] [bound to] sat_sUb of: 
__DEFAULT[binders][] -> [ConApp]I#`1[sat_sUb] [TYPES][Int#]
-}