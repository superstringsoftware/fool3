module Lexer where

-- import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)
import Text.Parsec ((<?>), Parsec, ParsecT)

import qualified Text.Parsec.Token as Tok

-- our parser's user state - remember that parser is ParsecT s u m a
-- and Parsec is ParsecT s u Identity, so we are making u = ParserState
data ParserState = ParserState {
  count :: Int
} deriving Show

initialParserState = ParserState {
  count = 0
}

type Parser = Parsec String ParserState

lexer :: Tok.TokenParser ParserState
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","/",";","=",",",".","|",":", "::","<",">"] -- ["+","*","-","/",";","=",",","<",">","|",":"]
    names = ["def","extern","if","then","else","in","for"
            ,"binary", "unary", "var", "let", "data"]
    style = emptyDef {
               Tok.commentLine = "#"
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

{-
operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef <?> "operator error"
  cs <- many (Tok.opLetter emptyDef) <?> "operator error 1"
  return (c:cs)
-}
