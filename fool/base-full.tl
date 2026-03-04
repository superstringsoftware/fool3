// we need to start with functions that allow us to manipulate types
// and tuples in general

type Nat = {
  Z,
  Succ (n:Nat)
};

type ConstructorTag = { ConstructorTag (n:Nat) };

type Bool = { True, False };

function f(x) = [x,18,29];

structure Eq (a:Type) = {
  function (==) (x,y:a) : Bool = not (x != y),
  function (!=) (x,y:a) : Bool = not (x == y)
  // mandatory = { (==) or (!=) }
};



/*
instance Eq(Bool) = {
  (==) = {
    {True,True} -> True,
    {False,False} -> True,
    {_,_} -> False
  }
};
*/
/*
type ForF = { Cons(a1:t,a2:t), Cell(a1:t,a2:t) };


function f(x) = {
  { Cons (a1, Cell (b1, b2) ) } -> g (a1,b1,b2)
}; // x is Cons, x.1 is Cell  g (x.0, x.1.0, x.1.1)



function f(x,y,z, h) = {
  { n, Z, Cons (a1, Cell (b1, b2) ), Succ(Succ(Z)) } -> g (n,a1,b1,b2)
}; // -> g (x, z.0, z.1.0, z.1.1)
*/


function eq(x:Nat,y:Nat):Bool = {
  {Z,Z} -> True,
  {Z,n} -> False,
  {n,Z} -> False,
  {Succ(m),Succ(n)} -> eq(m,n)
};


function plus (x:Nat,y:Nat):Nat = {
  {Z, n} -> n, 
  {Succ(n), m} -> Succ (plus(n, m)) 
};



/*
function deep(x:a):b = {
  {Succ(Succ(n))} -> f(n),
  {y} -> f(y)
};
*/




// function id (x:a):a = x;

function not (b:Bool) : Bool = {
    {True}  -> False,
    {False} -> True
};






//function plus2 (x:Nat):Nat = plus(x, Succ(Succ(Z)) );
/*
action main = {
  one = Succ(Z),
  three = Succ(Succ(one)),
  res = plus(three,one),
  print# (res)
};
*/



