using System;
namespace SuperstringSolutions.HSNet.STG
{

    // Heap objects - as per Push/Enter vs Eval/Apply paper

    // FUN - function closure
    public class FUN : CLOSURE
    {
        private Func<CLOSURE[], CLOSURE> code;
        private int arity;

        public int Arity
        {
            get
            {
                return arity;
            }
        }

        public FUN(Func<CLOSURE[], CLOSURE> f, int ar)
        {
            code = f;
            arity = ar;
        }

        public CLOSURE Call(CLOSURE[] xs)
        {
            return code(xs);
        }

        public override string ToString()
        {
            return "[FUN]" + code.ToString();
        }
    }

    // partial application of the function to a number of arguments
    public class PAP : CLOSURE
    {
        protected FUN func;
        protected CLOSURE[] args;

        public PAP(FUN f, CLOSURE[] xs)
        {
            func = f;
            args = xs;
        }

        public CLOSURE Call(CLOSURE[] xs)
        {
            var z = new CLOSURE[args.Length + xs.Length];
            args.CopyTo(z, 0);
            xs.CopyTo(z, args.Length);
            return func.Call(z);
        }

        public override string ToString()
        {
            return "[PAP]" + func.ToString();
        }

    }

    // saturated constructor
    public class CON : CLOSURE
    {
        public int __CONSTAG__ = 1;
        public CLOSURE[] Vals { get; }

        public CON(int constag, CLOSURE[] vals)
        {
            __CONSTAG__ = constag;
            Vals = vals;
        }

        // EVAL for constructor evaluates its' arguments to normal form (???)
        public override CLOSURE EVAL()
        {
            for (int i = 0; i<Vals.Length; i++)
            {
                Vals[i] = Vals[i].EVAL();
            }
            return this;
        }

        public override string ToString()
        {
            string vls = "{";
            if (Vals != null)
            {
                foreach (var v in Vals)
                {
                    vls += v.ToString() + " ";
                }
            }
            vls += "}";
            return "[CON" + __CONSTAG__ + "] " + vls;
        }
    }

    // CON object for primitive types
    public class CONPRIM<A> : CLOSURE
    {
        public int __CONSTAG__ = 1;
        public A Val { get; }

        public CONPRIM(A v)
        {
            Val = v;
        }

        public override string ToString()
        {
            return Val.ToString();
        }
    }

    // Extending PAP because we treat THUNKs as simply a suspended function application
    // (even though in reality they are more complex so may need to rethink this.
    public class THUNK : CLOSURE
    {
        private CLOSURE f;
        private CLOSURE[] args;
        public THUNK(CLOSURE f, CLOSURE[] xs) {
            this.f = f;
            args = xs;
            val = null;
        }
        private CLOSURE val;
        public override CLOSURE ENTER
        {
            get
            {
                if (val == null)
                {
                    /* -- this is true for call by value
                    foreach (var v in args)
                    {
                        var t = v.ENTER;
                    }*/
                    var func = f.ENTER as FUN;
                    val = func.Call(args);
                }
                return val;
            }
        }

        /*
         * Ok, this is the main eval method which has to check how we perform
         * function application based on Eval / Apply or Push / Enter -
         * challenge is as usual with understanding function arity and calling it
         * with the right number of arguments to reduce the graph in steps.
         *
         * Let's start with implementation that assumes we only have EXACT function applications.
         */
        public override CLOSURE EVAL()
        {
            return base.EVAL(); 
        }

        public override string ToString()
        {
            string vls = "{";
            foreach (var v in args)
            {
                vls += v.ToString() + " ";
            }
            vls += "}";
            return "[THUNK]" + f.ToString() + " " + vls;
        }

    }

    public class CLOSURE
    {
        // ENTER simply returns this for value types (FUN, PAP, CONs)
        // and has to update the thunk for THUNKs
        public virtual CLOSURE ENTER {
            get {
                return this;
            }
        }

        // EVAL is doing an actual evaluation / graph reduction -
        // unlike what we tried to do initially, it is NOT inside FUN Call methods,
        // they are *supposed to* return WHNFs
        // Theoretically, EVAL has to produce a Normal Form value?
        // EVAL for FUN and PAP simply returns the object, similar to ENTER
        // for CONs - EVALs its' arguments
        // THUNKs are the most interesting, as it is where application takes place
        public virtual CLOSURE EVAL()
        {
            return this;
        }
    }

    public class List
    {
        public static CLOSURE Nil = new CON(1, null);
        public static CLOSURE Cons (CLOSURE x, CLOSURE xs)
        {
            return new CON(2, new CLOSURE[] { x, xs});
        }

    }

    public class Maybe
    {
        public static CLOSURE Nothing = new CON(1, null);
        public static CLOSURE Just (CLOSURE x)
        {
            return new CON(2, new CLOSURE[] { x });
        }
    }

    public class Test
    {

        public static FUN mmap = new FUN(mmap_code, 2);
        public static CLOSURE mmap_code(CLOSURE[] args)
        {
            // mmap f mx
            // case mx of ...
            var xs = args[1].ENTER as CON;
            switch (xs.__CONSTAG__)
            {
                // Nothing -> Nothing
                case 1: return Maybe.Nothing;
                // Just x ->
                case 2:
                    // let y = {f, x} \u {}. f x
                    var y = new THUNK(args[0], new CLOSURE[] { xs.Vals[0] });
                    // let z = Just y
                    var z = Maybe.Just(y);
                    // in z
                    return z;
            }
            throw new Exception("Fallen out of case switch - this shouldn't happen!");
        }

        public static FUN plus = new FUN(plus_code, 2);
        public static CLOSURE plus_code(CLOSURE[] args)
        {
            var xs = args[0].ENTER as CONPRIM<int>;
            var ys = args[1].ENTER as CONPRIM<int>;
            return new CONPRIM<int>(xs.Val + ys.Val);
        }

        public static FUN map = new FUN(map_code, 2);
        public static CLOSURE map_code(CLOSURE[] args)
        {
            // map f xs
            // case xs of ...
            var xs = args[1].ENTER as CON;
            switch (xs.__CONSTAG__) 
            {
                // Nil -> Nil
                case 1: return List.Nil;
                // Cons y ys
                case 2:
                    // let h = {f, y} \u {}. f y
                    var h = new THUNK(args[0], new CLOSURE[] { xs.Vals[0] });
                    // let t = {map, f, ys} \u {}. map f ys
                    var t = new THUNK(map, new CLOSURE[] { args[0], xs.Vals[1] });
                    // let r = Cons h t
                    var r = List.Cons(h, t);
                    // in r
                    return r;
            }
            throw new Exception("Fallen out of case switch - this shouldn't happen!");
        }

        public static FUN foldr = new FUN(foldr_code, 3);
        public static CLOSURE foldr_code(CLOSURE[] args)
        {
            // foldr f z xs
            // case xs of ...
            var xs = args[2].ENTER as CON;
            switch (xs.__CONSTAG__)
            {
                // Nil -> z
                case 1: return args[2];
                // Cons y ys
                case 2:
                    // let h = {f z ys} \u {}. foldr f z ys
                    var h = new THUNK(args[0], new CLOSURE[] { args[1], xs.Vals[1] });
                    // in f x h
                    return new THUNK(args[0], new CLOSURE[] { xs.Vals[0], h });
            }
            throw new Exception("Fallen out of case switch - this shouldn't happen!");
        }

        public static FUN length = new FUN(length_code, 1);
        public static CLOSURE length_code(CLOSURE[] args)
        {
            var xs = args[1].ENTER as CON;
            switch (xs.__CONSTAG__)
            {
                case 1: return new CONPRIM<int>(0);
                case 2: return new CONPRIM<int>(0);
            }
            throw new Exception("Fallen out of case switch - this shouldn't happen!");
        }

        public static FUN generate = new FUN(generate_code, 1);
        public static CLOSURE generate_code(CLOSURE[] args)
        {
            var n = (args[0] as CONPRIM<int>).Val;
            if (n == 0) return List.Nil;
            return List.Cons(new CONPRIM<int>(n), new THUNK(generate, new CLOSURE[] { new CONPRIM<int>(n - 1) }));
        }

        public static FUN showIO = new FUN(showIO_code, 1);
        public static CLOSURE showIO_code(CLOSURE[] args)
        {
            Console.WriteLine(args[0].ENTER.ToString() );
            return null;
        }   

        public static CLOSURE generateIO(int n)
        {
            CLOSURE ls = List.Nil;
            var rnd = new System.Random();
            for (int i = 0; i < n; i++)
            {
                ls = List.Cons(new CONPRIM<int>(rnd.Next()), ls);
            }
            return ls;
            
        }
    }
}
