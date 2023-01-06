-- eventually, types and functions here will need to go to separate files
-- also see README

type Bool = True + False -- functional boolean

-- lifted primitive types - is there a way around it??
-- probably if we make it strict???
type Int = I# int -- lifted primitive int
type Float = F# double -- lifted primitive double
type Byte = B# word8 -- lifted primitive byte

-- Full signature should be Maybe(a:Type) = Just(x:a) + Nothing, but here since it's simple we are omitting this and
-- using Haskell like syntax
-- So, if we call Maybe(Int) we get a new type: Just(x:Int) + Nothing --> a instantiated to Int
-- if we call Just("hello") we get a value of type Maybe(String) --> a instantiated to String
type Maybe a = Just a + Nothing -- Maybe
-- or in lambda calculus:
-- \a:Type. \x:a. Just x - e.g., [a=Int]-> \x:Int. Just x --> [x=4]-> Just 4
-- wow it's like we don't need to distinguish between types and values at all!

-- We do need a functional list
type List a = (:) a (List a) + [] -- monomorphic list defined like in haskell
-- in lambda:
-- \a:Type. \x:a y:List(a).x * y --> [a=String] --> \x:String y:List(String).x*y
-- [x="Hello"] --> \ y:List(String)."Hello"*y --> [y=Nil] --> "Hello" * Nil


-- some typical list functions, will allow to test recursion
head :: List a -> a
head [] = []
head (x:xs) = x
-- \

tail :: List a -> List a
tail [] = []
tail (x:xs) = xs

length :: List a -> Int
length [] = 0
length (x:xs) = 1 + length xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs


-- TODO: THINK THROUGH THIS, SEEMS IMPORTANT!!!
-- Polymorphic list - define 'TOP' as a built in type value that allows ALL types?? using ideas in Subtyping?
type PolyList = (,) x:TOP l:PolyList + {} -- so, can use {1,"hello", 2+2} easily. Is it basically a record???

-- We do need a Vector as it is a basis for String
type Vector a:Type n:Int where
    -- UnboxedVector (a in Unboxed, n) =  array#(n) of a * n -- allocating primitive array of size n and type a
    -- pattern matched constructors based on the type of vector we are creating
    UnboxedVector Int n = int_array#(n) * n
    UnboxedVector Float n = double_array#(n) * n
    UnboxedVector Byte n = byte_array#(n) * n

    -- \a:Type n:Int.if a=Int then UnboxedVector int_array#(n) * n else if a=Float then UnboxedVector double_array#(n) * n -- etc.

-- need to define interface functions
length :: Vector a n -> Int
length v = snd v -- simply accessing second element of a tuple that contains vectors size

map :: (a -> b) -> Vector a n -> Vector b n
map f v = <∀x ∊ (fst v) f(x)> --??? built in???

-- is this a better syntax? with named vars? Then the order of vars doesn't matter we match either by name or by type
(*) :: (scalar:a in Num, vector:Vector a n)->Vector a n
(*) (scalar, vector) = map (*scalar) vector -- more readable definition?
-- also then can do things like:
-- 4 * <1,2,3> --> <4,8,12> or <4,0,1>*2 --> <8,0,2>: we simply match by type and make sure named arguments are there 


-- String, unicode right away
-- first defining different encodings
type StringEncoding = UTF16 + UTF8 + ASCII

-- returning a record of a byte vector plus encoding
-- interesting syntax since we are putting default value here for the encoding
type String n:Int (e = UTF16):StringEncoding = String (Vector word8 n) * StringEncoding
