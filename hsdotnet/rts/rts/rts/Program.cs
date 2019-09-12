using System;

using SuperstringSolutions.HSNet.STG;

namespace rts
{
    // Testing several approaches to represent data values at runtime:
    // 1. Unoptimized mapping 1:1 to .Net types, where Sum types are represented
    // via inheritance hierarchy. Type info is preserved at runtime, sort of in
    // the spirit of CLR?
    // 2. Optimized option 1, where we have NO inheritance for SumTypes with
    // 1 constructor and SumTypes with 2 constructors where one of them is nullary
    // (List a, Maybe a etc) - in this case, we represent nullary constructor
    // as null value
    // 3. "Classic" representation via Tuples with type info erased completely:
    // might be a bit faster (we'll check), but interop with .Net is harder.

    // From the initial experiments it seems that Option 2 is the winner
    // That's probably due to the fact we modelled Tuples really ugly,
    // might look for other options.

    // Option 1.
    public class ValueBase
    {
        public int __CONSTAG__;
        public ValueBase(int ct)
        {
            __CONSTAG__ = ct;
        }
        public ValueBase()
        {
            __CONSTAG__ = 1;
        }
    }
    public class List<A> : ValueBase
    {

    }
    public class Nothing<A> : List<A>
    {

    }
    public class Just<A> : List<A>
    {
        public A _F0_;
        public List<A> _F1_;
        public Just(A x, List<A> xs)
        {
            __CONSTAG__ = 2;
            _F0_ = x;
            _F1_ = xs;
        }
    }

    static class Option1
    {
        // via checking type info
        public static List<B> map1<A,B>(Func<A,B> f, List<A> xs)
        {
            Just<A> ys = xs as Just<A>;
            if (ys == null)
            {
                return new Nothing<B>();
            }
            else
            {
                return new Just<B>(f(ys._F0_), map1<A, B>(f, ys._F1_));
            }            
        }
        // via checking cons tag
        public static List<B> map2<A, B>(Func<A, B> f, List<A> xs)
        {
            switch(xs.__CONSTAG__)
            {
                case 1: return new Nothing<B>();
                default:
                    var ys = xs as Just<A>;
                    return new Just<B>(f(ys._F0_), map1<A, B>(f, ys._F1_));
            }
            
        }

        // functional (recursive) generation function - doesn't work)))
        // stack overflow
        /*
        public static List<int> generate(int n)
        {
            var rnd = new System.Random();
            if (n == 0) return new Nothing<int>();
            else
            {
                return new Just<int>(rnd.Next(), generate(n - 1));
            }
        }*/
        public static List<int> generate(int n)
        {
            var rnd = new System.Random();
            List<int> ls = new Nothing<int>();
            for (int i = 0; i<n; i++)
            {
                ls = new Just<int>(rnd.Next(), ls);
            }
            return ls;
        }

        public static int length1<A>(List<A> ls)
        {
            Just<A> ys = ls as Just<A>;
            if (ys == null)
            {
                return 0;
            }
            else
            {
                return 1 + length1<A> (ys._F1_);
            }

        }
        public static int length2<A>(List<A> ls)
        {
            switch (ls.__CONSTAG__)
            {
                case 1: return 0;
                default:
                    var ys = ls as Just<A>;
                    return 1 + length1<A>(ys._F1_);
            }

        }
    }

    // Option 2
    class ListOpt<A>
    {
        public A _F0_;
        public ListOpt<A> _F1_;
        

        public ListOpt(A x, ListOpt<A> xs)
        {
            _F0_ = x;
            _F1_ = xs;
        }
    }

    static class Option2
    {
        public static ListOpt<int> generate(int n)
        {
            var rnd = new System.Random();
            ListOpt<int> ls = null;
            for (int i = 0; i < n; i++)
            {
                ls = new ListOpt<int>(rnd.Next(), ls);
            }
            return ls;
        }

        public static int length1<A>(ListOpt<A> ls)
        {
            if (ls == null)
            {
                return 0;
            }
            else
            {
                return 1 + length1<A>(ls._F1_);
            }

        }

        public static ListOpt<B> map<A, B>(Func<A, B> f, ListOpt<A> xs)
        {
            switch (xs)
            {
                case null: return null;
                default:
                    return new ListOpt<B>(f(xs._F0_), map<A, B>(f, xs._F1_));
            }

        }

    }

    // Option3
    public class Tuple
    {
        public int __CONSTAG__;
        public Tuple(int ct, Tuple[] val)
        {
            __CONSTAG__ = ct;
            value = val;
        }
        public Tuple(Tuple[] val)
        {
            __CONSTAG__ = 1;
            value = val;

        }
        public Tuple[] value;

    }

    public static class Option3
    {
        public static Tuple Nothing = new Tuple(null);
        public static Tuple Just(Tuple x, Tuple xs)
        {
            return new Tuple(2, new Tuple[] { x, xs });
        }
        public static Tuple generate(int n)
        {
            Tuple ls = Nothing;
            Tuple unit = new Tuple(null);
            for (int i = 0; i < n; i++)
            {
                ls = Just(unit, ls);
            }
            return ls;
        }
        public static int length1(Tuple ls)
        {
            //System.Console.WriteLine("Checking tuple " + ls);
            if (ls.__CONSTAG__ == 1) return 0;            
            return 1 + length1(ls.value[1]);
            

        }
        public static Tuple map(Func<Tuple, Tuple> f, Tuple ls)
        {
            if (ls.__CONSTAG__ == 1) return Nothing;
            return Just(f(ls.value[0]), map(f, ls.value[1]));
        }

        public static Tuple func(Tuple t)
        {
            return new Tuple(t.value);
        }

    }

    // Option 4: what if we just take an Object array where first value is cons tag?
    public static class Option4
    {
        public static Object[] Nothing = { 1 };
        public static Object[] Just(Object[] x, Object[] xs)
        {
            return new object[] { 2, x, xs };
        }
        public static Object[] generate(int n)
        {
            Object[] ls = Nothing;
            var rnd = new System.Random();
            for (int i = 0; i < n; i++)
            {
                ls = Just(new object[] { 1, rnd.Next()}, ls);
            }
            return ls;
        }
        public static int length(Object[] ls)
        {
            if (((int)ls[0]) == 1) return 0;
            return 1 + length(ls[2] as Object[]);


        }
        public static Object[] map(Func<Object[], Object[]> f, Object[] ls)
        {
            if (((int)ls[0]) == 1) return Nothing;
            return Just(f(ls[1] as Object[]), map(f, ls[2] as Object[]));
        }

        public static Object[] func(Object[] t)
        {
            return new Object[] { 1, (int)t[1] + 10 };
        }

    }


    


    class Program
    {
        // mapping function
        static int f(int x)
        {
            return x + 10;
        }
        static void Main(string[] args)
        {
            /*
            Console.WriteLine("Hello World!");

            var watch = System.Diagnostics.Stopwatch.StartNew();
            var ls = Option1.generate(50000);
            watch.Stop();
            double tGen = watch.ElapsedTicks / 10000.0;
            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls10 = Option1.map1<int, int>(f, ls);
            var l1 = 0;// Option1.length1<int>(ls);
            double tOpt11 = watch.ElapsedTicks / 10000.0;
            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls20 = Option1.map2<int, int>(f, ls);
            var l2 = 0;// Option1.length2<int>(ls);
            double tOpt12 = watch.ElapsedTicks / 10000.0;

            Console.WriteLine("Test results Option 1: - Class based Full");
            Console.WriteLine("Generation,    ms: " + tGen);
            Console.WriteLine("Via classes,   ms: " + tOpt11 + " (length = " + l1 + ")");
            Console.WriteLine("Via cons tags, ms: " + tOpt12 + " (length = " + l2 + ")");

            
            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls1 = Option2.generate(50000);
            watch.Stop();
            tGen = watch.ElapsedTicks / 10000.0;
            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls11 = Option2.map<int, int>(f, ls1);
            //l1 = Option2.length1<int>(ls1);
            tOpt11 = watch.ElapsedTicks / 10000.0;
            
            Console.WriteLine("Test results Option 2: - Class based Optimized");
            Console.WriteLine("Generation,    ms: " + tGen);
            Console.WriteLine("Via classes,   ms: " + tOpt11 + " (length = " + l1 + ")");

            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls2 = Option3.generate(50000);
            watch.Stop();
            tGen = watch.ElapsedTicks / 10000.0;
            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls13 = Option3.map(Option3.func, ls2);
            //l1 = Option3.length1(ls2);
            tOpt11 = watch.ElapsedTicks / 10000.0;

            Console.WriteLine("Test results Option 3: - Tuple based");
            Console.WriteLine("Generation,    ms: " + tGen);
            Console.WriteLine("Via tuples,    ms: " + tOpt11 + " (length = " + l1 + ")");

            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls3 = Option4.generate(50000);
            watch.Stop();
            tGen = watch.ElapsedTicks / 10000.0;
            watch = System.Diagnostics.Stopwatch.StartNew();
            var ls14 = Option4.map(Option4.func, ls3);
            //l1 = Option3.length1(ls2);
            tOpt11 = watch.ElapsedTicks / 10000.0;

            Console.WriteLine("Test results Option 4: - Object[] based");
            Console.WriteLine("Generation,    ms: " + tGen);
            Console.WriteLine("Via Object[],    ms: " + tOpt11 + " (length = " + l1 + ")");

            */
            //var lp = Test.generate.Call(new CLOSURE[] { new CONPRIM<int>(3) });
            var lp = Test.generateIO(3);
            Console.WriteLine("STG TESTS");
            Console.WriteLine(lp);
            Test.map.Call(new CLOSURE[] { Test.showIO, lp });


        }
    }
}
