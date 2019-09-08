module Lexer where

-- import Text.Parsec.String (Parser)
-- import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)
import Text.Parsec

import Text.Parsec.Token as Tok

import State

-- our parser's user state - remember that parser is ParsecT s u m a
-- and Parsec is ParsecT s u Identity, so we are making u = ParserState
data ParserState = ParserState {
  -- count :: Int,
  parserLog :: [String]
} deriving Show

initialParserState = ParserState {
  -- count = 0,
  parserLog = []
}

addParserLog :: String -> ParserState -> ParserState
addParserLog s ps = ps { parserLog = s : (parserLog ps) }

type Parser = ParsecT String ParserState IntState
type LanguageDefIS st = Tok.GenLanguageDef String st IntState
type TokenParserIS st = Tok.GenTokenParser String st IntState

emptyDef   :: LanguageDefIS st
emptyDef    = Tok.LanguageDef
            { commentStart   = "{-"
            , commentEnd     = "-}"
            , commentLine    = "--"
            , nestedComments = False
            , identStart     = letter <|> char '_'
            , identLetter    = alphaNum <|> oneOf "_'"
            , opStart        = opLetter emptyDef
            , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , reservedOpNames= []
            , reservedNames  = []
            , caseSensitive  = True
            }


lexer :: TokenParserIS ParserState
lexer = Tok.makeTokenParser style
  where
    ops = [";","=",",",".",":", "->","<",">", "|", "?", "<:"] -- ["+","*","-","/",";","=",",","<",">","|",":"]
    names = ["instance","extern","if","then","else","in","for"
            ,"binary", "unary", "let", "class", "case", "of", "type", "where", "∃", "∀", "exists"]
    style = emptyDef {
               Tok.commentStart   = "{-"
             , Tok.commentEnd     = "-}"
             , Tok.commentLine = "--"
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
