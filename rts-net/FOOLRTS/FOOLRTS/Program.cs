using System;
using System.Collections.Generic;

namespace FOOLRTS
{

    public class Maybe<A>
    {

    }

    public class Functor 
    {
        static List<B> fmap<A,B>(Func<A,B> f, List<A> val)
        {
            return null;
        }
        static Maybe<B> fmap<A, B>(Func<A, B> f, Maybe<A> val)
        {
            return null;
        }
    }

    public class List {
        public virtual string show()
        {
            return "Shouldn't happen: base List!!!";
        }
        
    }

    public class Nil : List
    {
        public List Value
        {
            get { return null; }
        }

        public override string show()
        {
            //Console.WriteLine("Nil : List a");
            return "[]";
        }

    }
    public class Cons<A> : List
    {
        private A _head;
        private List _tail;

        public A head
        {
            get { return _head; }
        }
        public List tail
        {
            get { return _tail; }
        }

        public Cons(A x, List xs)
        {
            _head = x;
            _tail = xs;
        }

        public override string show()
        {
            //Console.WriteLine("Cons : List a");
            return head.ToString() + " :: " + tail.show();

        }
    }

    public class Functions
    {
        public static List map<A,B>(Func<A,B> f, List ls)
        {
            Cons<A> cs = ls as Cons<A>;
            if (cs != null)
            {
                return new Cons<B>(f(cs.head), Functions.map<A, B>(f, cs.tail));
            }
            else
            {
                return new Nil();
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");

            List l1 = new Cons<int>(1, new Cons<int>(2, new Cons<int>(3, new Nil())));
            Console.WriteLine(l1.show());

            Func<int, int> f = x => x * x;
            List l2 = Functions.map<int, int>(f, l1);
            Console.WriteLine(l2.show());

            Console.WriteLine(l1.GetType());

        }
    }
}
