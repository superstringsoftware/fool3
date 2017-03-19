module TermColors where

import Data.List

-- some simple functions to apply terminal color to Text
-- http://misc.flogisoft.com/bash/tip_colors_and_formatting
reset       = "\ESC[0m"
bold        = "\ESC[1m"
dim         = "\ESC[2m"
underlined  = "\ESC[4m"

black       = "\ESC[30m"
red         = "\ESC[31m"
green       = "\ESC[32m"


ansifyString :: [String] -> String -> String
ansifyString params s = (params >>= id) ++ s ++ reset
