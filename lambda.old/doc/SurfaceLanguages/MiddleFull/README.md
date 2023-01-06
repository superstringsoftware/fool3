## New Iteration: Middle Level Full Syntax Language

Based on additional research into Type Theory, as well as some playing around with Ur/Web and Coq. We arrive at a consistent language that will allow us to express everything in our language using just 3 concepts: Types, Functions, Tuples. This language can be both used as a surface language (even though it's a bit verbose), but mostly as an illustration of the concept while we would need more concise and expressive surface languages which will be "desugared" into this representation as the first pass of the compiler.

### Tuples

The most basic building block of everything in the language. *Everything is a Tuple*.

Record is a Tuple (name1 : type1, ... nameN : typeN). It defines the *structural* type, similar to typescript. *Value* Tuple is a tuple of triples: ( (name1, type1, value1), ..., (nameN, typeN, valueN) ).

We treat it as tuple of Expressions: (Expr1, ..., ExprN). Then, we can check the type of each expression and check whether it fits with the type of the tuple.

We have some magical operators and functions to manipulate the records:

```haskell
Tuple : Type = (
    (()) : Tuple,
    (:::) (field : ANY, tail : Tuple)
)
```

*Still needs thinking through. We have to distinguish between our typed records "name:type" and everything else. Also, how do we introduce ANY type???*. 

### Functions

Next, is that everything is a function, producing some tuple as a result:

`<Identifier> [vo1:to1, ..., voN:toN] (v1:t1, ..., vN:tN) :t`

The above is a signature. Identifier, optional (implicit) arguments (usually types), required arguments, result type. 

First some gradually increasing typing examples.

```haskell
-- Closed type
Bool : Type = (True : Bool, False : Bool)
-- For such simple cases we can omit the type signatures for the constructors and simply write
Bool : Type = (True, False)
-- Type functions:
Maybe (a:Type) : Type = (Just(:a), Nothing)
List  (a:Type)  : Type = ( ([]), (::) (x:a, xs:List(a)) )

-- Now, to some more complex stuff:
-- GADTs, based on the Haskell example:
data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

-- same in our language, note the EXPLICIT type signature for constructors!!
Expr (a:Type) : Type = (
    I (:Int) : Expr(Int)
    B (:Bool): Expr(Bool)
    Add (:Expr(Int), :Expr(Int)):Expr Int
    -- etc
)

-- we can do the same as open data family:
Expr (a:Type) : Type -- declaration of a type only!
-- now, constructors:
I (:Int) : Expr(Int)
B (:Bool): Expr(Bool)
-- etc

-- another DATA FAMILY example:
-- Declare a list-like data family
data family XList a
-- Declare a list-like instance for Char
data instance XList Char = XCons !Char !(XList Char) | XNil
-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int

-- in our language:
XList(a:Type):Type
-- then, constructors:
XCons(:Char, :XList(Char)) : XList(Char)
XNil : XList(Char)
XListUnit(:Int) : XList (())

-- One-constructor records, introducing type without declaring it previously:
Person(fname, lname:String, age:Int):Person

-- type functions can also have named 
Either(a,b:Type) = (Left (:a), Right (:b))
```

As you can see, so far we have type declarations, type assignments for closed types via tuples, and constructor functions -- all in the same format! Let's now look at typeclasses and some dependent functions.

*Will we be able to distinguish between constructor functions and simply a function signature???*
Should probably use **cons** or something to mark constructors explicitly?

```haskell
-- simple typeclasses
Show(a:Type):Sigma = (
    show (x:a) : String
)

Semigroup(a:Type):Sigma = (
    (•) (:a, :a) :a,
    forall x,y,z:a. x•(y•z) == (x•y)•z
)

Monoid (a:Type, ∃ Semigroup(a)):Sigma = (
    E0 : a,
    forall x:a. (x•E0 == x, E0•x == x)
)

-- example of Haskells multiparam with type synonim:
class Add a b where
    type SumTy a b :: *
    add :: a -> b -> SumTy a b

Add (a, b: Type) : Sigma = (
    SumTy (a, b:Type) : Type, -- how do we tell it's NOT a constructor?!?!? it is a FUNCTION!!!
    add (:a, :b) : SumTy(a,b)
)

Add (Int,Double) = (
    SumTy = Double,
    add (x,y) = fromIntegral x + y
)
```

### Dependent functions and types

In the "mainstream" understanding - types depending on values.

```haskell
fact (n:Int) : Int = if n == 0 then 0 else n * fact (n-1)

length (lst:List(a)) : Int
length [] = 0
length (_::xs) = 1 + length (xs)
```

### What do we need to keep track of inside the system?

- Types: constructors, functions, constraints (laws)
- Functions: on the top level, including those we get from applying the type functions

*Internally, should we transform everything to simple-typed lambda calculus?!*

So, what does an application of a complex type function (a typeclass mostly) to a type looks like? And how do we take into account the fact, that we can't really write `Maybe (Int)` - it would imply a result of `(Just (:Int), Nothing)` - but we can (and should):

```haskell
Semigroup (Int) = (
    (•) = primPlus# 
    forall x,y,z:Int. x•(y•z) == (x•y)•z
)
```

The first application would generate a function `Just [Int] (x:Int) = Just (x) : Maybe (Int)` explicitly, while the second one would do the same for `(•) [Int] (x,y:Int) = x primPlus# y`. Question is, why can we have the first one implicit, and the second needs to be explicit? Why can't we generate it on the fly? --> Very difficult to keep track of the dependencies inside the typeclasses.

So, apparently the conclusion is - constructors should receive a special treatment, unlike all other functions, even dependent ones.

### C-like surface language?

```
def Bool : Type
    constructors True, False end

def Maybe (a:Type) : Type
    constructors
        Just (:a),
        Nothing
    end

def Eq (a:Type) : Type 
    functions
        (==) (x,y:a) : Bool = not (x /= y),
        (/=) (x,y:a) : Bool = not (x == y),
        (≠) = (/=)
    end
    required = (==) || (/=)

def Add (a,b:Type) : Type {
    functions
        SumTy (a,b:Type) : Type
        -- doesn't work because of this. What is SumTy? Is it a function or a type definition? Can it be both, depending on arguments???
        add (x:a, y:b) : SumTy(a,b)
}
```

So, ∏-types are generalization of function (->) types, while ∑-types - are generalization of product types, or our tuples (a,b,...)! This is freaking confusing.
