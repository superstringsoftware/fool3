{-
Minimal compilation - factorial
-}

fact:Int = \n:Int . {
    0 -> 1;
    n -> n * fact(n-1)
};

plus2:Int = \x:Int . x + 2;
