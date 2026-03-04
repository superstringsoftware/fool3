using System;
namespace SuperstringSolutions.HSNet.STG
{
    // Hand-written helper classes for testing the RTS

    public class List
    {
        public static CLOSURE Nil = new CON(1, CLOSURE.EMPTY);
        public static CLOSURE Cons(CLOSURE x, CLOSURE xs)
        {
            return new CON(2, new CLOSURE[] { x, xs });
        }
    }

    public class Maybe
    {
        public static CLOSURE Nothing = new CON(1, CLOSURE.EMPTY);
        public static CLOSURE Just(CLOSURE x)
        {
            return new CON(2, new CLOSURE[] { x });
        }
    }

    public class Test
    {
        public static FUN map = new FUN(map_code, 2);
        public static CLOSURE map_code(CLOSURE[] args)
        {
            var xs = args[1].ENTER as CON;
            switch (xs.__CONSTAG__)
            {
                case 1: return List.Nil;
                case 2:
                    var h = new THUNK(args[0], new CLOSURE[] { xs.Vals[0] });
                    var t = new THUNK(map, new CLOSURE[] { args[0], xs.Vals[1] });
                    return List.Cons(h, t);
            }
            throw new Exception("Fallen out of case switch");
        }

        public static FUN showIO = new FUN(showIO_code, 1);
        public static CLOSURE showIO_code(CLOSURE[] args)
        {
            Console.WriteLine(args[0].ENTER.ToString());
            return null;
        }

        public static FUN generate = new FUN(generate_code, 1);
        public static CLOSURE generate_code(CLOSURE[] args)
        {
            var n = (args[0] as CONPRIM<int>).Val;
            if (n == 0) return List.Nil;
            return List.Cons(new CONPRIM<int>(n), new THUNK(generate, new CLOSURE[] { new CONPRIM<int>(n - 1) }));
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
