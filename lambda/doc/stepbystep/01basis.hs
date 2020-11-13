{-
Step 1: What we want to be able to compile here:
- List datatype
- length function
- calculating and printing length
-}

Nil  : (List a) = {};
Cons : (List a) = \ :a :(List a);

length:Int = \ ls:(List a) . {
    Nil -> 0;
    (Cons _ xs) -> 1 + length xs
};

ls = Cons 1 (Cons 2 Nil);

main = print# (length ls);