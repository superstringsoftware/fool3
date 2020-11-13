class Eq a where #
-- The Eq class defines equality (==) and inequality (/=). All the basic datatypes exported by the Prelude are instances of Eq, and Eq may be derived for any datatype whose constituents are also instances of Eq.
-- Minimal complete definition: either == or /=.

  (==) :: a -> a -> Bool -- infix 4 #
  (/=) :: a -> a -> Bool -- infix 4

class Eq a => Ord a where #

{- The Ord class is used for totally ordered datatypes.
Instances of Ord can be derived for any user-defined datatype whose constituent types are in Ord.
The declared order of the constructors in the data declaration determines the ordering in derived Ord instances.
The Ordering datatype allows a single comparison to determine the precise ordering of two objects.

Minimal complete definition: either compare or <=. Using compare can be more efficient for complex types. -}

-- Minimal complete definition
-- compare | (<=)

  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool infix 4 #
  (<=) :: a -> a -> Bool infix 4 #
  (>) :: a -> a -> Bool infix 4 #
  (>=) :: a -> a -> Bool infix 4 #
  max :: a -> a -> a #
  min :: a -> a -> a

  -- | The 'Bounded' class is used to name the upper and lower limits of a
  -- type.  'Ord' is not a superclass of 'Bounded' since types that are not
  -- totally ordered may also have upper and lower bounds.
  --
  -- The 'Bounded' class may be derived for any enumeration type;
  -- 'minBound' is the first constructor listed in the @data@ declaration
  -- and 'maxBound' is the last.
  -- 'Bounded' may also be derived for single-constructor datatypes whose
  -- constituent types are in 'Bounded'.

  class  Bounded a  where
      minBound, maxBound :: a

  -- | Class 'Enum' defines operations on sequentially ordered types.
  --
  -- The @enumFrom@... methods are used in Haskell's translation of
  -- arithmetic sequences.
  --
  -- Instances of 'Enum' may be derived for any enumeration type (types
  -- whose constructors have no fields).  The nullary constructors are
  -- assumed to be numbered left-to-right by 'fromEnum' from @0@ through @n-1@.
  -- See Chapter 10 of the /Haskell Report/ for more details.
  --
  -- For any type that is an instance of class 'Bounded' as well as 'Enum',
  -- the following should hold:
  --
  -- * The calls @'succ' 'maxBound'@ and @'pred' 'minBound'@ should result in
  --   a runtime error.
  --
  -- * 'fromEnum' and 'toEnum' should give a runtime error if the
  --   result value is not representable in the result type.
  --   For example, @'toEnum' 7 :: 'Bool'@ is an error.
  --
  -- * 'enumFrom' and 'enumFromThen' should be defined with an implicit bound,
  --   thus:
  --
  -- >    enumFrom     x   = enumFromTo     x maxBound
  -- >    enumFromThen x y = enumFromThenTo x y bound
  -- >      where
  -- >        bound | fromEnum y >= fromEnum x = maxBound
  -- >              | otherwise                = minBound
  --
  class  Enum a   where
      -- | the successor of a value.  For numeric types, 'succ' adds 1.
      succ                :: a -> a
      -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
      pred                :: a -> a
      -- | Convert from an 'Int'.
      toEnum              :: Int -> a
      -- | Convert to an 'Int'.
      -- It is implementation-dependent what 'fromEnum' returns when
      -- applied to a value that is too large to fit in an 'Int'.
      fromEnum            :: a -> Int

      -- | Used in Haskell's translation of @[n..]@.
      enumFrom            :: a -> [a]
      -- | Used in Haskell's translation of @[n,n'..]@.
      enumFromThen        :: a -> a -> [a]
      -- | Used in Haskell's translation of @[n..m]@.
      enumFromTo          :: a -> a -> [a]
      -- | Used in Haskell's translation of @[n,n'..m]@.
      enumFromThenTo      :: a -> a -> a -> [a]

      succ                   = toEnum . (+ 1)  . fromEnum
      pred                   = toEnum . (subtract 1) . fromEnum
      enumFrom x             = map toEnum [fromEnum x ..]
      enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
      enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
      enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

  -- Default methods for bounded enumerations
  boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
  boundedEnumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound `asTypeOf` n)]

  boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
  boundedEnumFromThen n1 n2
    | i_n2 >= i_n1  = map toEnum [i_n1, i_n2 .. fromEnum (maxBound `asTypeOf` n1)]
    | otherwise     = map toEnum [i_n1, i_n2 .. fromEnum (minBound `asTypeOf` n1)]
    where
      i_n1 = fromEnum n1
      i_n2 = fromEnum n2


-- NUMERIC CLASSES --------------------------------------------------------

class  Num a  where
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a

    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    x - y               = x + negate y
    negate x            = 0 - x

-- | the same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract :: (Num a) => a -> a -> a
subtract x y = y - x


--------------------------------------------------------------
-- Standard numeric classes
--------------------------------------------------------------

class  (Num a, Ord a) => Real a  where
    -- | the rational equivalent of its real argument with full precision
    toRational          ::  a -> Rational

-- | Integral numbers, supporting integer division.
class  (Real a, Enum a) => Integral a  where
    -- | integer division truncated toward zero
    quot                :: a -> a -> a
    -- | integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    rem                 :: a -> a -> a
    -- | integer division truncated toward negative infinity
    div                 :: a -> a -> a
    -- | integer modulus, satisfying
    --
    -- > (x `div` y)*y + (x `mod` y) == x
    mod                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    quotRem             :: a -> a -> (a,a)
    -- | simultaneous 'div' and 'mod'
    divMod              :: a -> a -> (a,a)
    -- | conversion to 'Integer'
    toInteger           :: a -> Integer

    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE div #-}
    {-# INLINE mod #-}
    n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d

-- | Fractional numbers, supporting real division.
class  (Num a) => Fractional a  where
    {-# MINIMAL fromRational, (recip | (/)) #-}

    -- | fractional division
    (/)                 :: a -> a -> a
    -- | reciprocal fraction
    recip               :: a -> a
    -- | Conversion from a 'Rational' (that is @'Ratio' 'Integer'@).
    -- A floating literal stands for an application of 'fromRational'
    -- to a value of type 'Rational', so such literals have type
    -- @('Fractional' a) => a@.
    fromRational        :: Rational -> a

    {-# INLINE recip #-}
    {-# INLINE (/) #-}
    recip x             =  1 / x
    x / y               = x * recip y
