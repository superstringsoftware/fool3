--------------------------------------------------------------------
-- |
-- Module    :  Parser
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Vector.Unboxed as U

import Lexer
import Syntax

int :: Parser Expr
int = PInt <$> fromInteger <$> integer

floating :: Parser Expr
floating = PFloat <$> float

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]]
        -- ,[binary "<" Ex.AssocLeft, binary ">" Ex.AssocLeft]]

expr :: Parser Expr
expr = try vector <|> Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- try (parens $ many identifier) <|> (parens $ commaSep identifier)
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- try (parens $ many identifier) <|> (parens $ commaSep identifier)
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

letins :: Parser Expr
letins = do
  reserved "let"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  body <- expr
  return $ Function o args body

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int <?> "integer: precedence value for the operator definition"
  args <- parens $ many identifier
  body <- expr
  return $ Function o args body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> ifthen
      <|> try letins
      <|> for
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> try unarydef
    <|> try binarydef
    <|> try record
    <|> try globalvar
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s


-- adding new stuff

-- simple (flat) record
record :: Parser Expr
record = do
  reserved "data"
  name <- identifier
  reservedOp "="
  fields <- braces $ semiSep expr
  return $ Record name [] fields

-- global vars - for the interpreter only
globalvar :: Parser Expr
globalvar = do
  reserved "var"
  var <- identifier
  reservedOp "="
  val <- expr
  return $ GlobalVar var val

-- polymorphic list: [x, 2+4, 1.3]
list :: Parser Expr
list = do
  args <- brackets $ commaSep expr
  return $ ListExpr args

-- numeric vector: <1,2,3.4>
vector :: Parser Expr
vector = do
    -- we are checking something is between <>
    -- then separating this input by commas
    -- then for each expression - first trying to read it as a float then if it fails - as an int!
    -- cool and beautiful!
    args <- angles $ commaSep (try floating <|> int)
    let l = map conv args where
        conv (PInt i) = fromIntegral i
        conv (PFloat f) = f
    let v = U.fromList l
    return $ VFloat v
