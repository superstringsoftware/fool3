// this is used to test all possible parsing cases for correct parsing,
// which means the statements themselves are not necesserily gramatically
// much less "typically" correct

// SUM TYPES ======================================================
// easiest:
// type Bool = { True, False };
// one untyped argument
// type Bool (a) = { True, False };
// several untyped arguments
// type Bool (a,b,c) = { True, False };
// one typed argument
type Bool (a:Type) = { True, False };
// several typed arguments
type Bool (a:Type,b:Int,c:Person) = { True, False };
// mixed
type Bool (a:Type, b,   c:Person) = { True, False };

// MORE COMPLEX: Constructors with arguments on the right side:
type Nat = {
  Z,
  Succ (n:Nat)
};

function id (x:a):a = x;

function g(x) = f(x,y);
function g1(x) = f();
function g2(x,y) = x+y;

function g(x) = (f(x,y)) (z, a+b);



/*
function not (b:Bool) : Bool = {
    True -> False,
    False -> True
}

type Nat = {
  Z,
  Succ (:Nat)
}

function plus (x:Nat,y:Nat):Nat = {
  (Z, x) -> x,
  (Succ(n), x) -> Succ (plus(n,x))  
}
*/