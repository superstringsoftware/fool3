module TermColors where

    import Data.List
    import Control.Monad (join)
    
    -- some simple functions to apply terminal color to Text
    -- http://misc.flogisoft.com/bash/tip_colors_and_formatting
    reset       = "\ESC[0m"
    bold        = "\ESC[1m"
    dim         = "\ESC[2m"
    underlined  = "\ESC[4m"
    
    black       = "\ESC[30m"
    red         = "\ESC[31m"
    green       = "\ESC[32m"
    yellow      = "\ESC[33m"
    blue        = "\ESC[34m"
    magenta     = "\ESC[35m"
    cyan        = "\ESC[36m"
    lgray       = "\ESC[37m"
    dgray       = "\ESC[90m"
    lred        = "\ESC[91m"
    lgreen      = "\ESC[92m"
    lyellow     = "\ESC[93m"
    lblue       = "\ESC[94m"
    lmagenta    = "\ESC[95m"
    lcyan       = "\ESC[96m"
    white       = "\ESC[97m"
    
    
    
    ansifyString :: [String] -> String -> String
    ansifyString params s = join params ++ s ++ reset
    
    as = ansifyString
    