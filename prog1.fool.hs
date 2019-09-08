-- trying

type List a = Nil + (::) head:a * tail:(List a);
type Maybe a = Nothing + Just :a;

{-
type Person = Person name:String * age:Int;
type Either a b = Left :a + Right :b;
type Pair a b = P :a * :b;
type Test a = Test :(List a); 
-}

map:(List b) f:(a->b) l:(List a) = l ? 
    Nil -> Nil 
  | (x :: xs) -> (f x)::(map f xs);

ls:(List Int) = [1,2,3,4,5];

-- plus2:Int x:Int = x + 2;

fact:Int n:Int = n ?
    0 -> 1
  | otherwise -> n * fact (n - 1);

main = show (map fact ls);
-- main = show (map (+2) ls);
-- main = show fact 5;

-- main = Console.WriteLine "hello";