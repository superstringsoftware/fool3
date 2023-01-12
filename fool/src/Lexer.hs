{-# LANGUAGE OverloadedStrings #-}
module Lexer where

-- import Text.Parsec.String (Parser)
-- import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)
import Text.Parsec

import Text.Parsec.Token as Tok

import State

import Data.Text as L

-- our parser's user state - remember that parser is ParsecT s u m a
-- and Parsec is ParsecT s u Identity, so we are making u = ParserState
data ParserState = ParserState {
    -- count :: Int,
    parserLog :: [String],
    -- when we are parsing a function, store current arity here - needed for error messages while parsing, e.g. with pattern match mismatch etc.
    currentArity :: !Int,
    currentLambdaName :: Text
} deriving Show

initialParserState = ParserState {
    -- count = 0,
    parserLog = [],
    currentArity = 0,
    currentLambdaName = ""
} 

setCurrentLambdaName :: Text -> Parser ()
setCurrentLambdaName a = modifyState (\s -> s {currentLambdaName = a})
getCurrentLambdaName :: Parser Text
getCurrentLambdaName = getState >>= pure . currentLambdaName

setCurrentArity :: Int -> Parser ()
setCurrentArity a = modifyState (\s -> s {currentArity = a})
getCurrentArity :: Parser Int
getCurrentArity = getState >>= pure . currentArity

addParserLog :: String -> ParserState -> ParserState
addParserLog s ps = ps { parserLog = s : (parserLog ps) }

type Parser = ParsecT Text ParserState IntState
type LanguageDefIS st = Tok.GenLanguageDef Text st IntState
type TokenParserIS st = Tok.GenTokenParser Text st IntState

emptyDef   :: LanguageDefIS st
emptyDef    = Tok.LanguageDef
            { commentStart   = "/*"
            , commentEnd     = "*/"
            , commentLine    = "//"
            , nestedComments = False
            , identStart     = letter <|> char '_'
            , identLetter    = alphaNum <|> oneOf "_'#"
            , opStart        = opLetter emptyDef
            , opLetter       = oneOf (":!#$%&*+./<=>?@\\^|-~" ++ "•§≠∑®†ø©˙∆˚¬…æ≈ç√∫≤≥÷¿˘¯˜◊∏ˇ‰Œ±·°‡›‹€") -- unicode stuff
            , reservedOpNames= []
            , reservedNames  = []
            , caseSensitive  = True
            }


lexer :: TokenParserIS ParserState
lexer = Tok.makeTokenParser style
    where
    ops = [";","=",",",".",":", "->", "=>", "|", "?", "<:", "\\"]
    names = ["type","function","if","then","else","in","action","structure",
            "let", "case", "of", "where", "∃", "∀", "exists", "forall"]
    style = emptyDef {
                Tok.commentStart   = "/*"
                , Tok.commentEnd     = "*/"
                , Tok.commentLine = "//"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = names
                }

integer    = Tok.natural lexer -- ATTN!!! changed here because integer screws up '-' and '+' binary operators!!! Now it's unclear what to do with '-' unary.
float      = Tok.float lexer
braces     = Tok.braces lexer
parens     = Tok.parens lexer
angles     = Tok.angles lexer -- added to parse vectors <1,2,3>
brackets   = Tok.brackets lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
stringLit  = Tok.stringLiteral lexer
symbol     = Tok.symbol lexer
operator   = Tok.operator lexer
    