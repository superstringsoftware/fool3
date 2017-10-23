-- from scratch definition attempt, bottom up
-- starting with primitive types basis - int, float, byte, ref and their arrays (vectors?)

-- we don't have Bool initially and we need it:
data Boolean = True + False

-- comprehensions on collections, changing example from README somewhat:
-- generic container, from which we should be able to derive both Vector and List:
map (f, X) ≡ {∀x ∊ X f(x) | x mod 2 is True}
-- the way to read it is we apply f(x) to all elements of X *if* the condition after | is true and return the same container type as X
-- possible complication with dependent types: if we have Vector Int 4 - so specific size -
-- and we do something like the above, it means we'll have to return the Vector of a different size - HOW TO HANDLE THAT?
-- through some sort of type classes probably

-- without unicode:
def map(f, X) = {forall x in X f(x) | x mod 2 is True}
-- forall x in X is basically an iterator, so type of X has to support some sort of iteration over elements
-- not sure how to handle it now; for vectors it's easy:
range (k,n,X) = <∀i ∊ {k..n} xᵢ>
-- or
range (k,n,X) = <forall i in {k..n} x_i>

-- DATA STRUCTURES -------------------------------------------------------
-- List
-- List is good for where we need to pre-pend values often, as this operation is O(1)
data List a = Cons a (List a) | Nil -- standard functional list
map (f, X::List a) ≡ [∀x ∊ X f(x)] -- using Square Brackets to denote list as a container
-- example:
l = [1,2,3] -- binding list to a label l, by doing Cons 1 (Cons 2 (Cons 3 Nil))

-- Vector
map (f, X::Vector a) ≡ <∀x ∊ X f(x)> -- using Angle Brackets to denote Vector as a container
-- example:
v = <1,2,3> -- binding vector to a label v, by allocating an array of size 3 on the heap (stack?) and instantiating values
-- how to do it functionally? or does it have to be a built-in mechanism that's doing it under the hood??
-- since v is immutable, doing something like
v<1> = 4 -- returns a COPY of the old vector with the changed 2nd element (should it behave like that?)

-- mutable references:
w := <1,2,3> -- creating a Reference w to a mutable vector, then can do:
!w<0> -- returns 1, - dereferencing
w<1> := 4 -- change w<1> from 2 to 4

-- ok, let's start with dependent Vectors, at the same time fleshing out data definition syntax
-- need to define: data structure to hold values
-- constructor function(s)
-- functions on the type
type Vector a:Type n:Int where
  data = _data :: Array a * _size :: Int -- product type, defining data structure
  -- constructor function(s)
  Vector arr n =
