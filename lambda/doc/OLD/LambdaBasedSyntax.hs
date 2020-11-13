-- the idea here is to maximally follow typed lambda calculus in syntaxis
-- plus tuples - give access directly

----------------------------------------------------
-- even newer iteration based on 2 below
----------------------------------------------------
-- Everything is centered around FUNCTIONS that make no distinction between types and values

-- Values: either unboxed (int, double, byte, pointer) or Tuple {x1 ... xn} or Record (named fields tuple basically)
-- Tuples: constructor tag, tuple with values
IP {1 2} : IPType
Weird {"hello" 18 (Cons {1 Nil}) } : WeirdType
-- should we give ability to access fields via index???

-- Classic ADTs first
-- Via Unit type
True = {} : Bool
False = {} : Bool
-- shorter syntaxis
True, False : Bool

-- Type functions are functions
Nothing = {} :     Maybe a
Just = \x:a. {_} : Maybe a -- anonymous tuple
-- Shorter syntaxis:
Nothing, Just a : Bool -- this whole confusion about a meaning _:a here... need to get away from it

AnPair = \x:a y:b. {_ _} : AnPair a b -- anonymous pair
Pair   = \x:a y:b. {fst snd} : Pair a b -- named pair, gives us fst and snd functions "for free"
-- how about type of the function in the beginning for consistency with "normal" functions?
Pair : Pair a b = \x:a y:b. {fst snd}


-- record: type Record b = Record test:Int * typed:b;
Record = \x:b y:Int . {typed test} : Record b -- named fields record with one type parameter

-- List and data families - become trivial, as we treat EVERY type as a data family (open, can be extended)
([]) = {} : List a
(::) = \x:a xs:(List a). {head tail} : List a -- again, 2 functions "for free"
-- extending it at any point
ListUnit = \n:Int. {n} : List {}

-- HA, QUESTION - how do we distinguish constructor functions from regular functions that return a specific type???
-- only via the fact that constructor functions only return {} tuples? But what about dependent types stuff, e.g. the 
-- vector from idris:
Z, S (Nat) : Nat
Nil  = {} : Vect Z a
(::) = \head:a tail:(Vect k a). {head tail} : Vect (S k) a

{-
Idris concat of the lists:
(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil       ys = ys
(++) (x :: xs) ys = x :: xs ++ ys
-}
(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) = \v1:(Vect n a) v2:(Vect m a). v1 ?
    Nil     -> v2
  | x :: xs -> x :: xs ++ ys
-- Typechecking this is tricky, requires running a symbolic computation with recursion???

-- Types are 1st class, so functions make no distinction between them and "real" values, e.g. function returns a Type!
tr : Int -> Type
tr = \i:Int . i?
    0 -> Int
    1 -> Bool
    2 -> Maybe a

-- Moreover, we'll NEED such functions to be able to do this complicated typechecking

-- Some "normal" functions
fact : Int = \n:Int . n ?
    0 -> 1
    _ -> n * fact (n - 1);

id : a = \x:a. x

-- Ok, Typeclasses
{-
class Eq a =
    (==):Bool x:a y:a = not (x /= y), -- default implementation
    (/=):Bool x:a y:a = not (x == y);   
-}
Eq : Typeclass = \a:*. {
    (==):Bool = \x:a y:a. not (x /= y)
  , (/=):Bool = \x:a y:a. not (x == y) 
}

Functor : Typeclass = \f:(*->*) {
    fmap : f b = \g:(a->b) x:(f a). -- no body for the function, means needs to be defined
}

-- Functor Maybe gives us fmap : Maybe b = \g:(a->b) x:(Maybe a).
-- What syntaxis to give to definitions? Record like since we are treating it as a tuple?
(Functor Maybe).fmap = \g x.   x? Nothing -> Nothing | (Just y) -> Just (g y);
(Functor  List).fmap = \g xs. xs? [] -> [] | y::ys -> (g y) :: (map g ys);

-- then how do we find a function?
fmap (+2) (Just 5) -- a,b = int, f = Maybe
fmap (+2) [1,2,3]  --            f = List
-- can get value for f from the arguments, then what we really need at runtime is a function
-- fmap = \f. ... which returns our needed fmap instance. This means, application of 
-- Functor Maybe needs to update function fmap that takes f and returns a needed function:
fmap = \f:(*->*). lookup f -- or something similar, b/c lookup f has to return correct fmap
-- challenge - there are NO TYPES at runtime??? - so all this looking up business has to happen at compile time???
-- THINK TROUGH!

{-
Associated type family example:
class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
  empty                  = GMapInt Data.IntMap.empty
  lookup k   (GMapInt m) = Data.IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)
-}
-- translates to, where since we don't discriminate between functions we simply add a Constructor function GMap k
GMapKey : Typeclass = \k:*. {
    -- GMap k : * -> *
    GMap : * = \k v:*. -- type constructor taking 2 arguments, 1 of which is bound via typeclass definition!!!
  , empty : GMap k v
  , lookup : Maybe v  = \i:k cx:(GMap k v).
  , insert : GMap k v = \i:k x:v cx:(GMap k v).
}
-- and then:
(GMapKey Int).(GMap Int) = \x:v. GMapInt (Data.IntMap.IntMap x)
(GMapKey Int).empty = GMapInt Data.IntMap.empty
(GMapKey Int).lookup = \i   cx. cx ? (GMapInt m) -> Data.IntMap.lookup k m
(GMapKey Int).insert = \i x cx. cx ? (GMapInt m) -> GMapInt (Data.IntMap.insert k x m)
-- alternative syntaxis, since we are operating on records anyway:
GMapKey Int = {
    GMap Int = ...
  , empty = ...
}

-- ok the only thing left to define are constraints


-- THE BELOW OLDER THINKING LEADS US TO REVISE THE SYNTAXIS AND STOP TRYING TO COMBINE SUM TYPES INTO ONE AND RATHER FOCUS
-- ON EXTENSIBLE TYPES (FAMILIES!!!), and thus CONSTRUCTOR FUNCTIONS!!!!!!!
-- THIS SHOULD BE CLEANER!!!
True : Bool = {}
False : Bool = {}

-- alternatively and shorter
True, False : Bool

-- then
Nothing : Maybe a = {}
Just : Maybe a = \x:a. {x}

(Nothing, Just = \x:a. {x}) : Maybe a
-- not very clean, because we do need to substitute the type variable somewhere :( - can say we do it implicitly
-- from x argument, but cleaner and more math like approach would be:

-- first, showing maybe signature
Maybe : (* -> *)
-- or like this, which reads as "Maybe of type CONCRETE TYPE" is a type function
Maybe:* = /\a:*.
-- then just is:
Just : (Maybe a:*) = /\a:*. \x:a. {x} 
-- or:
Just = /\a:*. \x:a. {x}:(Maybe a) 
-- this one is probably cleanest, as it shows type of our resulting tuple explicitly
-- then nothing is not necesserily concrete, but since we can do variance / covariance this should typecheck!!!
Nothing = {} : Maybe a

-- with list, demonstrating FAMILIES or extensibility WITHOUT explicit families:
List : * -> *
([]) = {} : List a
(::) = /\a:*. \head:a tail:(List a). {head tail} : List a -- again, clearly showing type of the tuple, very good!!!
-- now, if somebody wants to later expand our List a family to the Unit instance from Haskell example, they simply write:
ListUnit = \n:Int. {n} : (List {}) -- so we have a tuple containing an Int of type List {} 
-- this means, List {} is separate and won't typecheck where List a is needed, as it's more specific!
-- This is easy to understand since map defined for list won't work here at all.
-- Also, it breaks typeclasses, let's look at Functor
Functor = //\f:(*->*).
    fmap : (f b) = \func:(a->b) val:(f a).; -- only interface

-- btw, fmap in reality is a more complicated lambda:
fmap = \a:* b:*. \func:(a->b) val:(f a). : (f b)
-- interestingly, this maps EXACTLY to C# interfaces (well, apart from f itself), and explains
-- while we have to write fmap<A,B>(...) explicitly!!! - cool :)


-- BTW, Functor is also a function that returns tuple of function interfaces with *optional* default implementations
-- so it's again all about tuples.

map : (List b) = \f:(a->b) list:(List a). list ?
    []      -> []
  | x::xs   -> (f x) :: (map f xs);

-- interesting, if we apply Functor List we get the following, by applying beta-reduction:
Functor List = \f=List ... = { fmap : (List b) = \func:(a->b) val:(List a). }
-- so we get a function signature, but no body. 
-- NEED SOME BETTER SYNTAXIS TO DEFINE INSTANCES!!!
fmap = map

-- Ok, then the problem comes when we try to do something with ListUnit instances, e.g.
f : {} -> Int
f _ = 0
fmap f (ListUnit 5) -- what's going to happen?
-- let's see:
ls = ListUnit 5 = {5} : List {}
fmap f ... = \a={}, b=Int \func = f val = (ls : List {}) : List Int -- typechecks, should return List Int, what happens is:
-- inspecting ls:
ls ? -- and ls is tagged ListUnit
[] - no
(::) ... - no
FAIL!!!

-- That might be ok, if somebody extends a closed type family, it's their responsibility to provide instances
-- but we can make it an option in the compiler? e.g., look for more specific function first, then try generic

-- From this, an IDEA on how to implement typeclasses in C#
fmap = \a:* b:* \func:(a->b) c:(f a)  : f b -- type parameters are explicit anyway, f only relates to function interface

-- this means we can use Typeclass instantiation as Generating Specific Interfaces for a given class:
Functor f -- generating function
Functor List : gives us (if we do via namespaces in clr directly)
class Functor.List {
    static List<B> fmap<A,B> (Func<A,B> f, List<A> l) ...
}
Functor Maybe:
class Functor.Maybe {
    static Maybe<B> fmap<A,B> (Func<A,B> f, Maybe<A> val) ...
}
so this maps pretty closely to C# !!!!!
Very buono!
So we have a namespace for each typeclass, and all types have classes there to implement their respective functions.
Since everything is a specific type at runtime anyway, we then simply give needed function:

Functor.List.fmap<Int,Int>( (x) => {x + 2}, [1,2,3]);

-- how do we select a function?
fmap (+2):(Int->Int) (Just 5):(Maybe Int)
fmap (+2):(Int->Int) [1,2,3]:(List Int)

--ok, in reality it's even more complicated, fmap is a 3-level lambda:
fmap = \f:(*->*). \a:* b:*. \g:(a->b) x:(f a).  :  f b
-- AHA!!!! It simply has to return SPECIFIC FUNCTIONS when certain parameters are instantiated!!!!!
-- So it's basically a huge lookup table, ghc implements it at runtime, we can optimize away and put
-- specific functions in the generated code.

Algorithm is:
- find fmap function in the global table
- instantiate whatever bound variables we can FROM TYPE of the arguments given
- get a function that operates on our arguments
- execute
- if we can't find such a function - ERROR message.
- how to treat the extensible List case??? For any List except unit it has to return normal map,
    for Unit ({}) - some other function, but lookup differs only in "a" type variable
    In other words, how do we specify that we have to try in order - first (f=List a=Int) - not found,
    then only (f=List) etc. That's WAY TOO SLOW AT RUNTIME, so only during compilation.

    It can be implemented as a hashtable at runtime in CLR as well btw - f-> maps to specific fmap<A,B> that
    we can easily do. Ha, how about we implement typeclass in pure C#? :)))



-- OLD STUFF -------------------------------------------------------------------------------

-- types are big lambdas, no params here
-- full syntaxis given, may want to optimize eventually
Bool = /\. True = \.{} + False = \.{};

-- with type params
Maybe = /\a:*. Just = \x:a.{x} + Nothing = \.{};
-- simplified version
Maybe = /\a:*. Just = \:a.{_} + Nothing = {}; -- no lambda as there are no vars? probably better / cleaner than lambda with vars
-- even more -- confusing?
Maybe = /\a. Just = \:a. + Nothing;

-- list
List = /\a:*. (::) = \head:a tail:(List a).{head tail} + ([]) = \.{};

-- two params
Pair = /\a:* b:*. Pair = \x:a y:b.{x y};
Either = /\a:* b:*. Left = \x:a.{x} + Right = \y:b.{y};

-- DO WE EVEN NEED TWO LAMBDAS IF TYPES ARE FIRST CLASS???
-- E.g., with dependent types:

Vector = /\a:* n:Nat. MkVector = \data:(IORef primarray# a). {data size=n} -- named field syntax?

-- fully functional Vect, taken from Idris:
data Vect : Nat -> Type -> Type where
    Nil  : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

-- translates to, starting with Nat definition first:
Nat = /\. Z = \.{} + S = \n:Nat .{n}

Vect = /\n:Nat a:*. 
    Nil  : (Vect Z a)     = {} -- still a Unit
  + (::) : (Vect (S k) a) = \head:a tail:(Vect k a). {head tail} -- tuple is still the same

-- TYPES are different for these constructors!!!
-- From here we see that Lambdas also work on type signatures

-- what do we do with typeclasses here???
class Functor f where
    fmap :: (a->b) -> f a -> f b

Functor = //\f:(*->*).
    fmap : (f b) = \func:(a->b) val:(f a).; -- only interface

-- some functions
-- factorial with unicoded constraint
∀a ∃ Num a => fact : a = \n:a. n ?
    0         -> 1
  | otherwise -> n * fact (n - 1);

map : (List b) = \f:(a->b) list:(List a). list ?
    []      -> []
  | x::xs   -> (f x) :: (map f xs);

-- defining functor instance:
Functor List = 
    fmap = map;

-- DATA FAMILIES ------------
-- data families are basically extensible GADT
-- Haskell example:
-- Declare a list-like data family
data family XList a
-- Declare a list-like instance for Char
data instance XList Char = XCons !Char !(XList Char) | XNil
-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int

-- Translates into:
XList = /\a:*.; -- interface only, no body for defining constructors inside the type
-- very similar to how we approach type class above!!
XList Char = 
    XCons = \head:Char tail:(XList Char). {head tail}
  + XNil = {}
-- unit instance 
XList () = XListUnit = \n:Int. {n} -- storing length.

-- SHOULD WE TREAT ALL TYPES AS FAMILIES AUTOMATICALLY?????
-- E.g., for general list that was defined above - we can treat it as a function for
-- ALL types EXCEPT those for which there's more specific definition, so then 
-- we can write anywhere later:
List () = ListUnit = \n:Int. {n}

