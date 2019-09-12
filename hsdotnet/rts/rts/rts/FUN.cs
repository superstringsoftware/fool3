using System;
/*
namespace SuperstringSolutions.HSNet.STG
{
    // FUN and PAP are represented by the same set of classes

    // function of 1 argument, cannot be partially applied
    public class FUN<A, Res>
    {

    }

    public class FUN<A,B,Res>
    {
        private Func<A, B, Res> f;
        private A arg;

        public FUN(Func<A, B, Res> f)
        {
            this.f = f;            
        }

        public FUN(Func<A,B,Res> f, A arg)
        {
            this.f = f;
            this.arg = arg;
        }

        public Res Call(B x)
        {
            return f(arg, x);
        }

    }

    public class PAP<A, B, C, Res>
    {
        private Func<A, B, C, Res> f;
        private A x1;
        private B x2;

        public PAP(Func<A, B, C, Res> f, A x1)
        {
            this.f = f;
            this.x1 = x1;            
        }

        public PAP(Func<A, B, C, Res> f, A x1, B x2)
        {
            this.f = f;
            this.x1 = x1;
            this.x2 = x2;
        }

        public void ApplyX2(B x2)
        {
            this.x2 = x2;
        }

        public Res Call(C x)
        {
            return f(x1, x2, x);
        }

    }
}
*/