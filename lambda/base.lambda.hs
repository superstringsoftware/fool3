I# : Int    = \x:Int#.    {_}

False : Bool = {}
True : Bool = {}

Nothing : Maybe a = {}
Just : Maybe a = \x:a. {_}

List : Type = \a:*. {
    Nil = {}
  , Cons = \x:a xs:(List a). {head tail}
}

plus : Int = \x:Int y:Int . x +# y

fact : Int -> Int
fact 0 = 0
fact n = n * fact (n-1)

