plus : Int = \x:Int y:Int . x + y;

I# : Int    = \x:Int#.    {_};

False : Bool = {};
True : Bool = {};

Nothing : (Maybe a) = {};
Just : (Maybe a) = \x:a. {_};

Nil : (List a) = {};
Cons : (List a) = \x:a xs:(List a). {head tail};

plus : Int = \x:Int y:Int . x +# y;

Vector : (Vector a n) = \arr:(primarray# a n) n:Int . {_ _};

Nil  : (Vect 0 a) = {};
Cons : (Vect (k+1) a) = \head:a tail:(Vect k a). {head tail};

fact : Int = \n:Int;
fact 0 = 1;
fact n = n * fact (n - 1);


