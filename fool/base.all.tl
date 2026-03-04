Bool:Set = { True, False }

type Nat = {
  Z,
  Succ {:Nat}
}

function plus(x:Nat, y:Nat) : Nat = {
  plus (x, Z) = x,
  plus (Z, x) = x,
  plus (Succ(n), x) = Succ (plus(n,x))
}

// or omitting function name? ->
function plus(x:Nat, y:Nat) : Nat = {
  (x, Z) -> x,
  (Z, x) -> x,
  (Succ(n), x) -> Succ (plus(n,x))
}


type List (a:Type) = {
  (::) {x:a, xs:List(a)},
  ([])
}


// List with a length - dependent type!
type ListN (a:Type, n:Nat) = {
  (:::) {x:a, xs:List(a,m)} : List(a, Succ(m)), // explicit type sig!
  Nil : List(a,Z)
}

// now, let's define typeclasses
structure Eq (a:Type) = {
  function (==) (x,y:a) : Bool = not x != y,
  function (!=) (x,y:a) : Bool = not x == y,
  mandatory = { (==) or (!=) }
}

structure Functor( f:Type->Type) = {
  function fmap(g:a->b, el:f(a) ) : f(b) 
}

Functor (List) = {
  fmap(g, lst) = {
    (_, []) -> [],
    (g, x :: xs) -> g(x) :: fmap(g, xs)
  }
}

// Now, how do we make ListN instance of Functor?????

function mapN(g:a->b, lst:ListN(a, n)) : ListN(b, n) = {
  (_, Nil) -> Nil : ListN(b, Z)
  (g, (x:a) ::: (xs:ListN(a, m) ) -> ( g(x) ::: mapN(g, xs) ) : ListN(b, Succ(m))
}