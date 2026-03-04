using System;
using System.Text;

namespace SuperstringSolutions.HSNet.STG
{
    /// <summary>
    /// Primitive operations - corresponds to GHC.Prim
    /// All primops operate on unboxed values (CONPRIM<T>.Val) and return boxed results.
    /// Naming: GHC primop "foo#" becomes "fooHash" via the compiler's funString2String.
    /// </summary>
    public static class PRIMOPS
    {
        // ============================================================
        // Int# arithmetic
        // ============================================================
        public static CLOSURE PlusHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val + ((CONPRIM<int>)b.ENTER).Val);
        }

        public static CLOSURE MinusHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val - ((CONPRIM<int>)b.ENTER).Val);
        }

        public static CLOSURE StarHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val * ((CONPRIM<int>)b.ENTER).Val);
        }

        public static CLOSURE quotIntHash(CLOSURE a, CLOSURE b)
        {
            int divisor = ((CONPRIM<int>)b.ENTER).Val;
            if (divisor == 0)
                throw new Exception("quotInt#: division by zero");
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val / divisor);
        }

        public static CLOSURE remIntHash(CLOSURE a, CLOSURE b)
        {
            int divisor = ((CONPRIM<int>)b.ENTER).Val;
            if (divisor == 0)
                throw new Exception("remInt#: division by zero");
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val % divisor);
        }

        public static CLOSURE negateIntHash(CLOSURE a)
        {
            return new CONPRIM<int>(-((CONPRIM<int>)a.ENTER).Val);
        }

        // ============================================================
        // Int# comparisons — return Int# (0 or 1), not Bool
        // GHC primops return Int# for comparisons
        // ============================================================
        public static CLOSURE EqHashHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val == ((CONPRIM<int>)b.ENTER).Val ? 1 : 0);
        }

        public static CLOSURE GTHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val > ((CONPRIM<int>)b.ENTER).Val ? 1 : 0);
        }

        public static CLOSURE LTHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val < ((CONPRIM<int>)b.ENTER).Val ? 1 : 0);
        }

        public static CLOSURE GTEqHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val >= ((CONPRIM<int>)b.ENTER).Val ? 1 : 0);
        }

        public static CLOSURE LTEqHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<int>(((CONPRIM<int>)a.ENTER).Val <= ((CONPRIM<int>)b.ENTER).Val ? 1 : 0);
        }

        // ============================================================
        // Double# arithmetic
        // ============================================================
        public static CLOSURE PlusHashHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<double>(((CONPRIM<double>)a.ENTER).Val + ((CONPRIM<double>)b.ENTER).Val);
        }

        public static CLOSURE MinusHashHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<double>(((CONPRIM<double>)a.ENTER).Val - ((CONPRIM<double>)b.ENTER).Val);
        }

        public static CLOSURE StarHashHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<double>(((CONPRIM<double>)a.ENTER).Val * ((CONPRIM<double>)b.ENTER).Val);
        }

        public static CLOSURE SlashHashHash(CLOSURE a, CLOSURE b)
        {
            return new CONPRIM<double>(((CONPRIM<double>)a.ENTER).Val / ((CONPRIM<double>)b.ENTER).Val);
        }

        // ============================================================
        // Conversion primops
        // ============================================================
        public static CLOSURE int2DoubleHash(CLOSURE a)
        {
            return new CONPRIM<double>((double)((CONPRIM<int>)a.ENTER).Val);
        }

        public static CLOSURE double2IntHash(CLOSURE a)
        {
            return new CONPRIM<int>((int)((CONPRIM<double>)a.ENTER).Val);
        }

        // ============================================================
        // tagToEnum# — converts Int# tag to a constructor (used for Bool etc.)
        // Returns CON with the given tag and no fields
        // ============================================================
        public static CLOSURE tagToEnumHash(CLOSURE a)
        {
            int tag = ((CONPRIM<int>)a.ENTER).Val;
            // GHC tags are 0-based, our CON tags are 1-based
            return new CON(tag + 1, CLOSURE.EMPTY);
        }

        // ============================================================
        // seq# — forces evaluation, returns value
        // ============================================================
        public static CLOSURE seqHash(CLOSURE a, CLOSURE token)
        {
            var _ = a.ENTER; // force evaluation
            return token;
        }

        // ============================================================
        // String operations
        // ============================================================
        public static CLOSURE unpackCStringHash(string s)
        {
            // Build a Haskell list of Char from a C# string
            CLOSURE result = new CON(1, new CLOSURE[0]); // []
            for (int i = s.Length - 1; i >= 0; i--)
            {
                result = new CON(2, new CLOSURE[] { new CONPRIM<char>(s[i]), result }); // (:)
            }
            return result;
        }

        // ============================================================
        // IO operations
        // ============================================================
        public static CLOSURE putStrHash(CLOSURE s)
        {
            // Walk the Haskell list and print each char
            CLOSURE current = s.ENTER;
            while (current is CON con && con.__CONSTAG__ == 2) // (:)
            {
                var ch = (CONPRIM<char>)con.Vals[0].ENTER;
                Console.Write(ch.Val);
                current = con.Vals[1].ENTER;
            }
            return new CON(1, CLOSURE.EMPTY); // () unit
        }

        public static CLOSURE putStrLnHash(CLOSURE s)
        {
            putStrHash(s);
            Console.WriteLine();
            return new CON(1, CLOSURE.EMPTY); // () unit
        }

        // ============================================================
        // IO-aware primops (receive State# token, return unboxed tuple)
        // ============================================================

        // putStr# with IO threading: State# -> (# State#, () #)
        public static CLOSURE putStrHash_IO(CLOSURE s, CLOSURE state)
        {
            putStrHash(s);
            return new UNBOXED_TUPLE(new CLOSURE[] { STATE_TOKEN.Instance, new CON(1, CLOSURE.EMPTY) });
        }

        // putStrLn# with IO threading
        public static CLOSURE putStrLnHash_IO(CLOSURE s, CLOSURE state)
        {
            putStrLnHash(s);
            return new UNBOXED_TUPLE(new CLOSURE[] { STATE_TOKEN.Instance, new CON(1, CLOSURE.EMPTY) });
        }

        // ============================================================
        // Error
        // ============================================================
        public static CLOSURE errorHash(CLOSURE msg)
        {
            throw new Exception("Haskell error: " + msg.ENTER.ToString());
        }
    }

}
