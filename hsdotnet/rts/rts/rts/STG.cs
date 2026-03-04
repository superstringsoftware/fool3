using System;
namespace SuperstringSolutions.HSNet.STG
{

    // Heap objects - as per Push/Enter vs Eval/Apply paper

    // FUN - function closure
    // Free variables are captured via C# closure semantics.
    // The code delegate receives only call-time arguments.
    public class FUN : CLOSURE
    {
        private Func<CLOSURE[], CLOSURE> code;
        private int arity;

        public int Arity => arity;

        public FUN(CLOSURE[] freeVars, int arity, Func<CLOSURE[], CLOSURE> code)
        {
            // freeVars stored for GC purposes but code accesses them via C# closure capture
            this.arity = arity;
            this.code = code;
        }

        // Backward-compat constructor (no freeVars parameter)
        public FUN(Func<CLOSURE[], CLOSURE> code, int arity)
        {
            this.arity = arity;
            this.code = code;
        }

        public CLOSURE Call(CLOSURE[] xs)
        {
            if (xs.Length < arity)
            {
                // Under-application: create PAP
                return new PAP(this, xs);
            }
            else if (xs.Length == arity)
            {
                // Exact application
                return code(xs);
            }
            else
            {
                // Over-application: call with arity args, apply result to rest
                var exactArgs = new CLOSURE[arity];
                var extraArgs = new CLOSURE[xs.Length - arity];
                Array.Copy(xs, 0, exactArgs, 0, arity);
                Array.Copy(xs, arity, extraArgs, 0, xs.Length - arity);
                CLOSURE result = code(exactArgs);
                // Result must be a function — apply remaining args
                CLOSURE r = result.ENTER;
                if (r is FUN f)
                    return f.Call(extraArgs);
                else if (r is PAP p)
                    return p.Call(extraArgs);
                else
                    throw new Exception("Over-application: result is not a function! Got: " + r.GetType().Name);
            }
        }

        public override string ToString()
        {
            return "[FUN:" + arity + "]";
        }
    }

    // Partial application
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
            var combined = new CLOSURE[args.Length + xs.Length];
            args.CopyTo(combined, 0);
            xs.CopyTo(combined, args.Length);
            return func.Call(combined);
        }

        public override string ToString()
        {
            return "[PAP]" + func.ToString();
        }
    }

    // Saturated constructor
    public class CON : CLOSURE
    {
        public int __CONSTAG__ = 1;
        public CLOSURE[] Vals { get; }

        public CON(int constag, CLOSURE[] vals)
        {
            __CONSTAG__ = constag;
            Vals = vals;
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

    // Boxed primitive type
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

    // THUNK: suspended computation, memoized on first evaluation.
    // Free vars captured via C# closure semantics.
    public class THUNK : CLOSURE
    {
        private Func<CLOSURE> code;
        private CLOSURE val;
        private bool evaluating; // blackhole detection

        // Primary constructor: code thunk (free vars captured by C# closure)
        public THUNK(CLOSURE[] freeVars, Func<CLOSURE> code)
        {
            // freeVars stored for GC; code accesses them via C# closure capture
            this.code = code;
            val = null;
            evaluating = false;
        }

        // Backward-compat constructor: function application thunk
        public THUNK(CLOSURE f, CLOSURE[] args)
        {
            this.code = () =>
            {
                CLOSURE func = f.ENTER;
                if (func is FUN fun)
                    return fun.Call(args);
                else if (func is PAP pap)
                    return pap.Call(args);
                else if (args.Length == 0)
                    return func;
                else
                    throw new Exception("THUNK: cannot apply " + args.Length + " args to " + func.GetType().Name);
            };
            val = null;
            evaluating = false;
        }

        // Simple code thunk (no freeVars parameter)
        public THUNK(Func<CLOSURE> code)
        {
            this.code = code;
            val = null;
            evaluating = false;
        }

        public override CLOSURE ENTER
        {
            get
            {
                if (val != null)
                    return val;

                if (evaluating)
                    throw new Exception("<<loop>> — infinite evaluation detected (blackhole)");

                evaluating = true;
                val = code();
                evaluating = false;

                // Indirection shortcutting
                if (val is THUNK inner)
                    val = inner.ENTER;

                return val;
            }
        }

        public override CLOSURE EVAL()
        {
            return ENTER;
        }

        public override string ToString()
        {
            if (val != null)
                return val.ToString();
            return "[THUNK]";
        }
    }

    public class CLOSURE
    {
        public static readonly CLOSURE[] EMPTY = new CLOSURE[0];

        public virtual CLOSURE ENTER
        {
            get { return this; }
        }

        public virtual CLOSURE EVAL()
        {
            return this;
        }
    }

    // Static STG evaluation and application helpers
    public static class STG
    {
        public static CLOSURE EVAL(CLOSURE c)
        {
            return c.ENTER;
        }

        // Apply a function closure to arguments
        // ENTER the function first (might be a thunk), then call
        public static CLOSURE APPLY(CLOSURE f, CLOSURE[] args)
        {
            CLOSURE func = f.ENTER;
            if (func is FUN fun)
                return fun.Call(args);
            else if (func is PAP pap)
                return pap.Call(args);
            else
                throw new Exception("STG.APPLY: not a function! Got: " + func.GetType().Name);
        }
    }
}
