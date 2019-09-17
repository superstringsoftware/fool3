collection of base haskell files to try to compile prelude from the ground up into c#

some notes on how to handle this

Here's the GHC.Base headers:

```Haskell

    module GHC.Base
            (
            module GHC.Base,
            module GHC.Classes,
            module GHC.CString,
            module GHC.Magic,
            module GHC.Types,
            module GHC.Prim,        -- Re-export GHC.Prim and [boot] GHC.Err,
                                    -- to avoid lots of people having to
            module GHC.Err,         -- import it explicitly
            module GHC.Maybe
    )
            where

    import GHC.Types
    import GHC.Classes
    import GHC.CString
    import GHC.Magic
    import GHC.Prim
    import GHC.Err
    import GHC.Maybe
    import {-# SOURCE #-} GHC.IO (failIO,mplusIO)

    import GHC.Tuple ()              -- Note [Depend on GHC.Tuple]
    import GHC.Integer ()            -- Note [Depend on GHC.Integer]
    import GHC.Natural ()            -- Note [Depend on GHC.Natural]

    -- for 'class Semigroup'
    import {-# SOURCE #-} GHC.Real (Integral)
    import {-# SOURCE #-} Data.Semigroup.Internal ( stimesDefault
                                                , stimesMaybe
                                                , stimesList
                                                , stimesIdempotentMonoid
                                                )
```                                                

Out of these, we WILL NOT compile Types, CString, Magic, and Prim - need to take some shortcuts and define functions from 
there in RTS.