-- from scratch definition attempt, bottom up
-- starting with primitive types basis - int, float, byte, ref and their arrays (vectors?)

-- Tuple:
tuple = (x1, x2, x3...) -- xi can be any value, *including* types!
record = (n1:t1, n2:t2, n3:t3...)

-- data constructors
F (x1:t1, x2:t2...) = (x1, x2, ...) -- returns a tuple
Just (x:a) = x
Nothing () = ()

Cell (x:a, l:List (a)) = (x, l) -- returns a tuple
Nil = ()

Point (x:Float, y:Float) = (x, y)
Rectangle (p:Point, w, h: Float) = p * w * h

Vector (n:Int, a:Type) = (size = n, data = newArray(n,a)):Vector(n,a)
Vector (4, Float) = (4, newArray(4, Float)):Vector(4, Float)

 


-- Product type (record, named or unnamed):
rec = name:String * age:Int * dob:Date -- record with named fields
tuple = Int * Int -- unnamed fields
typeTuple = a:Type * b:Type * v:F(Type, Int) -- tuple with type variables and a type constructor that takes Type and Int

-- what if we want to do something like:
-- defining data fields and then a getter function that returns full name  
data Person = Person {fname, lname:String; age:Int; dob:Date; name = fname + " " + lname; } 
data Company = Company {name:String; address: Address}

data Entities = Person + Company -- SumType meaning that both Person and Company are parts of it. Can we do it?

data Subsidiary = Company * parent:Company -- how about this? Extending via ProductType?
data ExtPerson = Person * siblings:List Person

data Strange a = Strange (Int * String * List a) + End

data List a = Cell (:a * :List a) + Nil
data Maybe a = Just :a + Nothing 

-- record syntax - same keywords as unnamed tuples, the only difference - tuples don't have field names
data Person = Person (
      fname:String
    * lname:String
    * age:Int
    * dob:Date
    * siblings:[Person]
  )

-- syntactic sugar for same type variables
data Person = Person (
      fname, lname:String
    * age:Int
    * dob:Date
    * siblings:<Person>
  )

data List a:* = Cell :a :(List a) + Nil
 

-- then we can do function calls with named parameters e.g.:
square (length:Float, width:Float) = length * width
-- then we can do
square 4 5
-- or
sq10 = square (width=10) -- partial application with the 2nd parameter

-- collections:
-- {} - generic Set, simply a bunch of different elements together
-- <> - Vector, array based hi-performance
-- [] - List, classic functional list

-- we don't have Bool initially and we need it:
data Boolean = True + False

-- some basic stuff
data Maybe (a:Type) = Just(x:a) + Nothing -- translates into:
type Maybe (a:Type) where
  Just(x:a) = x
  Nothing(x:a) = () -- empty type

data List (a:Type) = Cons (x:a * y:List(a:Type)) | Nil -- translates into:
type List(a:Type) where
  Cons(x:a, y:List(a:Type)) = x * y -- simply returning a new record
  Nil = () -- returning empty type

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

-- ok, let's start with dependent Vectors, at the same time fleshing out data definition syntax
-- need to define: data structure to hold values
-- constructor function(s)
-- functions on the type
type Vector a:Type n:Int where
  data = _data :: Array a * _size :: Int -- product type, defining data structure
  -- constructor function(s)
  UnboxedVector (a in {Num, Byte} n:Int) = array#(n) of a * n -- allocating array of type a and size n via primop
  BoxedVector (a:Type, n:Int) = array#(n) of ref# * n -- allocating array of references to any type
-- how do we hide the internal representation of Vector and expose only interface? Hide constructors and use generator functions!

-- public interface for different vectors
length :: Vector a n -> int
length v = v._size -- accessing size value directly, can also do
length (UnboxedVector _ n) = n -- via pattern matching etc

-- now, need to define mapping over both Lists and Vectors
-- let's use Functor for this
class Functor f where
  fmap :: (a->b) -> f a -> f b

-- recursively defined fmap for the List
List (a:Type) instance of Functor where
  fmap( f(x:a):b, Cons(y:a, l:List(a:Type))) = Cons(f(y), fmap(f, l))
  fmap( f(x:a):b, Nil) = Nil

-- now same for the vector
Vector (a:Type, n:Int) instance of Functor where
  fmap (f(x:a):b, UnboxedVector (arr, m) ) = if m != n then ERROR
    otherwise -- construct a new array, iterate over its' elements by applying f(arrᵢ)
    -- now, this is an interesting problem - since we'll be using for loop here internally anyway, should we expose it
    -- to the programmer as well???