public class List{}
public class Nil:List{
public  Nil() {
}
}
public class Cons<A>:List{
public A head;
public List tail;

public  Cons(A head, List tail) {
this.head = head;
this.tail = tail;
}
}
public class Maybe{}
public class Nothing:Maybe{
public  Nothing() {
}
}
public class Just<A>:Maybe{
public A __FIELD__0;

public  Just(A __FIELD__0) {
this.__FIELD__0 = __FIELD__0;
}
}