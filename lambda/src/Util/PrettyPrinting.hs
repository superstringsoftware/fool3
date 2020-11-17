{-# LANGUAGE OverloadedStrings #-}
-- this is simply class definition for pretty printing that supports terminal and html, and potentially other formats

module Util.PrettyPrinting where

import Control.Monad (join)

class PrettyPrint a where
    -- | pretty print nicer terminal text with "ansifications", unicode etc
    ppr  :: a -> String
    ppr = pprP
    -- | pretty print *plain* text, no ansi colors etc
    pprP :: a -> String
    pprP = ppr
    -- | pretty print to html
    pprH :: a -> String
    pprH = pprP

-- functions that generically output a list of values 
showListWFormat :: (a -> String) -> String -> String -> String -> String -> [a] -> String
showListWFormat showF beginWith endWith sep empty [] = empty
showListWFormat showF beginWith endWith sep empty (x:xs) = Prelude.foldl fn (beginWith ++ showF x) xs ++ endWith
    where fn acc e = acc ++ sep ++ showF e

showListSqBr fun = showListWFormat fun "[" "]" ", " "[]"
showListRoBr fun = showListWFormat fun "(" ")" ", " "()"     
showListCuBr fun = showListWFormat fun "{" "}" ", " "{}"
showListCuBrSpace fun = showListWFormat fun "{ " " }" ", " "{}"     
showListPlain fun = showListWFormat fun "" "" " " ""
showListRoBrPlain fun = showListWFormat fun "(" ")" " " ""

-- default instances
instance {-# OVERLAPPABLE #-} PrettyPrint a => PrettyPrint [a] where ppr = showListSqBr ppr

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

c256 n      = "\ESC[38;5;" ++ show n ++ "m"
        
ansifyString :: [String] -> String -> String
ansifyString params s = join params ++ s ++ reset

as = ansifyString