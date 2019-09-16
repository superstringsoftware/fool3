using System;
namespace rts.Idea3
{
    public class Closure
    {
        public virtual Closure ENTER()
        {
            return this;
        }
    }

   

    // Values are represented as normal .Net classes!!!
    // (well, with some optimizations)
    public class List<A> : Closure where A: Closure
    {
        int __constag;
        protected List(int i) { __constag = i; }

        public static List<A> Nil = new List<A>(1);
        public static List<A> Cons(A x, List<A> xs) 
        {
            return new Cons<A>(x, xs);
        }
        public static List<A> Cons(Closure x, Closure xs) 
        {
            return new Cons<A>(x, xs);
        }
    }

    public class Cons<A> : List<A> where A : Closure
    {
        public A __f0;
        public List<A> __f1;

        // fields if this Cons is still unevaluated
        public Closure[] __thunk;

        // constructor with evaluated values
        public Cons(A x, List<A> xs) : base(2)
        {
            __f0 = x;
            __f1 = xs;
            __thunk = null;
        }

        // constructor with thunks
        public Cons (Closure x, Closure xs) : base(2)
        {
            __thunk = new Closure[] { x, xs };
        }
    }

    // function of 3 arguments
    public class FUN<A,B,C,Res>
        where A: Closure
        where B : Closure
        where C: Closure
        where Res:Closure
    {
        private Func<A, B, C, Res> code;
        public int Arity { get { return 3; } }

        public FUN(Func<A, B, C, Res> f)
        {
            code = f;
        }

        public Res CALL(A x, B y, C z)
        {
            return code(x, y, z);
        }

    }

    public static class Functions
    {
        /*
         * length xs =
         *      case xs of
         *          [] -> 0
         *          x:xs -> 1 + length xs
         */ 
        public static int length_code<A>(List<A> ls) where A:Closure
        {
            var ys = ls.ENTER() as Cons<A>;
            if (ys == null)
            {
                // Nil
                return 0;
            }
            else
            {
                // Cons
                return plus.CALL(1, ) // unfinished!
            }
        }
    }

   
        /*
         * foldl f z []     = z
         * foldl f z (x:xs) = let z' = z `f` x 
                                  in foldl f z' xs
         *
         * foldl f z ls =
         *      case ls of
         *          [] -> z
         *          (x:xs) -> let z' = THUNK (f z x)
         *                        in foldl.CALL (f z' xs)
         */

        //public B foldl_code


    
}
