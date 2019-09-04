-- another attempt at surface language syntaxis

type Bool = True + False
-- product types
type SimplePair = SimplePair :Int * :String
-- type functions
type Maybe a:* = Nothing + Just :a
-- type definition with operator like constructor
type List a:* = ([]) + (::) :a * :List a
-- type function with parametric sum
type Either a:* b:* = Left :a + Right :b

-- Records: simply named product types
type Person = Employer 
    name, lastName : String 
  * age, income : Int
  * strange : List String
  
-- sum types for records
type Shape = 
  Circle
    name : String
  * radius : Double
  +
  Rectangle
    name : String
  * width, height : Double

-- product types plus extension for records (modeled via subclassing in .Net)
type BetterShape = BetterShape pos : Point
type Circle = Circle BetterShape * radius : Double
type Rectangle = Rectangle BetterShape * width, height : Double

-- better option to handle sums and products:
type Circle = name : String * radius : Double -- define a record type with constructor Circle
type Rectangle = name : String * width, height : Double -- ...
type Shape = Circle + Rectangle
-- now, this definition is more or less equivalent to the one type Shape = Circle name:... + Rectangle name ... etc
-- what we need, is a more complex case with subtyping, that's where we have:
type Shape = pos : Point * name : String -- base type with only a position defined
type Circle = Shape * radius : Double -- automatically say that Circle is a shape
type Rectangle = Shape * width, height : Double -- automatically say that Rectangle is a shape

-- we CANNOT do multiplication for sum types
-- Multiple inheritance is easy as well:
type Persistent a = data:a * index:Int
type PersistentShape a = Shape * Persistent a * someField : String

-- or something like that!

-- mutable records:
type Widget = rec:Rectangle * texture : IORef Texture
-- then can do stuff like
w = Widget pos "myWidget" 500 900 (new IORef Texture)
w.texture := createTexture ... -- only inside IO etc
w.texture.resize -- if resize is a method from .Net

-- list with named fields
type List a:* = 
    ([]) 
    +
    (::) head:a * tail:List a
    
-- Can still pattern match as x::xs, but at the same time can do ls.head ls.tail if needed - better error messages + 
-- automatic head and tail

type Maybe a:* = Nothing + Just value:a

-- dependent vectors based on prim arrays
-- so, this is also just a tuple with 2 fields - data of type primarray of type a and int size - 
-- that gets set at construction time and defines the type!!! (important for type checks etc)
type Vector a:* n:Int = Vector data:primarray# a * size:Int = n
-- now clear why we need type families :)

-- ok taking the above into account here's the program we'd like to compile and run:
-- do we want to give an ability to define new classes??????? encapsulated state???
type List a:* = ([]) + (::) head:a * tail:List a

map:(List b) func:(a->b) list:List a = 
    list ?
        [] -> []
        otherwise -> (func list.head) :: (map func list.tail)

-- alternative syntaxis, haskell like
map : (a->b) -> List a -> List b
map _ [] = []
map f (x::xs) = (f x) :: (map f xs)

-- haskell is definitely cleaner!

main = do
    ls = 1::2::3::[]
    Console#.WriteLine ls -- using '#' to denote C# / .Net objects!!!
    ls1 = map (+5) ls
    Console#.WriteLine ls1
