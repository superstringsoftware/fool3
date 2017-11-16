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
-- import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (foldM)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Char (string)

import qualified Data.Vector.Unboxed as U

import Lexer
import Syntax
import DependentTypes.Core

int :: Parser FlExpr
int = PInt <$> fromInteger <$> integer

floating :: Parser FlExpr
floating = PFloat <$> float

stringVal :: Parser FlExpr
stringVal = PString <$> stringLit

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  -- whitespace
  o <- operator
  -- whitespace
  return o

binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft, binary ">" Ex.AssocLeft]]

-- helper parsers: lower case and upper case
lIdentifier = skipMany space >> lookAhead lower >> identifier
uIdentifier = skipMany space >> lookAhead upper >> identifier

expr :: Parser FlExpr
expr = Ex.buildExpressionParser (binops ++ [[unop],[binop]]) factor
-- expr = try vector <|> Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

-- concrete type or type application
typeAp :: Parser FlExpr
typeAp = do
  name <- uIdentifier
  vars <- many $ try (Type <$> TCon <$> uIdentifier) <|> try ( Type <$> TVar <$> lIdentifier ) <|> (parens typeAp)
  let tcon = TCon name
  if (length vars == 0) then return $ Type tcon -- concrete type
  else return $ Type $ foldl f tcon vars -- type application
  where f acc t = TApp acc (extractType t)

-- concrete type only
concreteType :: Parser FlExpr
concreteType = uIdentifier >>= return . Type . TCon


-- helper function to extract Type and Var from FlExpr
extractType (Type t) = t
extractVar (Var v) = v

-- variable with type
variable :: Parser FlExpr
variable = do
  name <- lIdentifier
  typ <- try (reservedOp ":" *> parens typeAp) <|>
         try (reservedOp ":" *> concreteType) <|>
         try (reservedOp ":" *> typeVariable) <|>
         pure (Type ToDerive)
  return $ Var (Id name (extractType typ))

-- variables in records need to be handled differently
varInRecord :: Parser Var
varInRecord = do
  name <- lIdentifier
  typ <- try (reservedOp ":" *> parens typeAp) <|>
         try (reservedOp ":" *> typeVariable) <|>
         (reservedOp ":" *> typeAp)
  return $ Id name (extractType typ)

-- type variable - Kind is always '*', needs to be adjusted at later stages
typeVariable :: Parser FlExpr
typeVariable = do
  name <- lIdentifier
  return $ Var (TyVar name KStar)

-- this, parametricType and dataDef parses haskell based data hello = Text a b | Nil type of data definitions
constructor :: Parser FlExpr
constructor = do
  name <- uIdentifier
  vars <- many  (try ((Id "") <$> TCon <$> uIdentifier) <|> -- concrete type
                 try ((Id "") <$> TVar <$> lIdentifier) <|> -- type var
                 ((Id "") <$>  extractType <$> (parens typeAp)) -- complex type, like List a
                 <?> "regular constructor failed")
  return $ Constructor name vars

recordConstructor :: Parser FlExpr
recordConstructor = do
  name <- uIdentifier
  whitespace >> char '{' >> whitespace
  vars <- commaSep varInRecord
  whitespace >> char '}' >> whitespace
  return $ Constructor name vars

constructors = try recordConstructor <|> constructor

-- simple ADT
typeDef :: Parser FlExpr
typeDef = do
  reserved "data"
  name <- uIdentifier
  vars <- many (extractVar <$> typeVariable)
  reservedOp "="
  fields <- sepBy1 constructors (char '|')
  return $ TypeDef name vars fields

function :: Parser FlExpr
function = do
  name <- lIdentifier
  args <- many (extractVar <$> variable)   -- (parens $ many identifier) <|> (parens $ commaSep identifier)
  reservedOp "="
  body <- expr
  return $ Function name args body

-- \x -> x + 1 etc
lambda :: Parser FlExpr
lambda = parens $ do
  char '\\'
  args <- many (extractVar <$> variable)
  reservedOp "->"
  body <- expr
  return $ Function "_LAMBDA_" args body

extern :: Parser FlExpr
extern = do
  reserved "extern"
  name <- lIdentifier
  args <- try (parens $ many identifier) <|> (parens $ commaSep identifier)
  return $ Extern name args

symbolId :: Parser FlExpr
symbolId = identifier >>= return . SymId

ifthen :: Parser FlExpr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ FlIf cond tr fl

letins :: Parser FlExpr
letins = do
  reserved "let"
  defs <- commaSep function
  reserved "in"
  body <- expr
  return $ FlLet defs body

unarydef :: Parser FlExpr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- many (extractVar <$> variable)
  reservedOp "="
  body <- expr
  return $ Function o args body

binarydef :: Parser FlExpr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int <?> "integer: precedence value for the operator definition"
  args <- many (extractVar <$> variable)
  reservedOp "="
  body <- expr
  return $ Function o args body


argument :: Parser FlExpr
argument = try vector
      <|> try lambda
      <|> try (parens expr)
      <|> try ifthen
      <|> try floating
      <|> try containers
      <|> try int
      <|> try symbolId
      <|> stringVal

arguments :: Parser FlExpr
arguments = do
  args <- many1 argument
  return $ foldl FlApp (head args) (tail args)


factor :: Parser FlExpr
factor = try letins <|> arguments


-- <|> try variable
-- <|> try for

defn :: Parser FlExpr
defn = try extern
    <|> try typeDef
    <|> try function
    <|> try unarydef
    <|> try binarydef
    <|> expr

-- <|> try record

-- contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r

toplevel :: Parser [FlExpr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

-- parseExpr :: String -> Either ParseError Expr
parseExpr s = runParserT (contents expr) initialParserState "<stdin>" s

--parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = runParserT (contents defn) initialParserState "<stdin>" s

-- parse a given file
parseToplevelFile name = parseFromFile (contents toplevel) name initialParserState

-- parseFromFile :: Parser a -> String -> IO (Either ParseError a)
-- redefining parse from file to work with our state - just a quick and dirty fix
parseFromFile p fname st
    = do input <- readFile fname
         return (runP p st fname input)

-- adding new stuff

-- vector: <1,2,3.4>
-- list: [1,2,3,4]
-- tuple: {1,int, "hello"}
containers :: Parser FlExpr
containers = -- try  (FlTuple TTVector <$> angles   (commaSep expr)) <|>
        try  (FlTuple TTList   <$> brackets (commaSep expr))
        <|>        FlTuple TTTuple  <$> braces   (commaSep expr)

vector :: Parser FlExpr
vector = FlTuple TTVector <$> angles (commaSep arguments) -- doesn't work with expr for some reason
  {-
  st <- getParserState
  reservedOp "<" <?> "parser: " ++ (stateInput st)
  p2 <- commaSep expr <?> "commasep failed"
  reservedOp ">" <?> "right angle failed"
  return $ FlTuple TTVector p2
  -}
