using System;
namespace rts
{
    /*
     * How do we represent the {x1 .. xn} \<flag> {} -> ... stuff from STG?
     * So, free vars and some function application for instance?
     * It should only be *evaluated* (if we are lazy!!!) when there's a
     * CASE expression applied to it
     */
    public class THUNK
    {
        public THUNK()
        {
        }
    }
}
