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


