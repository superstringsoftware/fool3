-- trying

type List a = Nil + Cons head:a * tail:(List a);
type Maybe a = Nothing + Just :a;
type Person = Person name:String * age:Int;
type Either a b = Left :a + Right :b;
type Pair a b = P :a * :b;
type Test a = Test :(List a); 

map:(List b) f:(a->b) l:(List a) = l ? 
    Nil -> Nil 
  | (Cons x xs) -> Cons (f x) (map f xs);

ls = [1,2,3,4,5];

plus2:Int x:Int = x + 2;

main = show (map plus2 ls);

-- main = Console.WriteLine "hello";