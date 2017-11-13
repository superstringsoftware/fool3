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

import qualified Data.Vector.Unboxed as U

import Lexer
import Syntax
import Core

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
          binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft, binary ">" Ex.AssocLeft]]

-- helper parsers: lower case and upper case
lIdentifier = skipMany space >> lookAhead lower >> identifier
uIdentifier = skipMany space >> lookAhead upper >> identifier

expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[binop]]) factor
-- expr = try vector <|> Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

-- concrete type or type application
typeAp :: Parser Expr
typeAp = do
  name <- uIdentifier
  vars <- many $ try (Type <$> TCon <$> uIdentifier) <|> try ( Type <$> TVar <$> lIdentifier ) <|> (parens typeAp)
  let tcon = TCon name
  if (length vars == 0) then return $ Type tcon -- concrete type
  else return $ Type $ foldl f tcon vars -- type application
  where f acc t = TApp acc (extractType t)

-- concrete type only
concreteType :: Parser Expr
concreteType = uIdentifier >>= return . Type . TCon


-- helper function to extract Type and Var from Expr
extractType (Type t) = t
extractVar (Var v) = v

-- variable with type
variable :: Parser Expr
variable = do
  name <- lIdentifier
  typ <- try (reservedOp ":" *> parens typeAp) <|>
         try (reservedOp ":" *> concreteType) <|>
         try (reservedOp ":" *> typeVariable) <|>


         pure (Type ToDerive)
  return $ Var (Id name (extractType typ))

-- type variable - Kind is always '*', needs to be adjusted at later stages
typeVariable :: Parser Expr
typeVariable = do
  name <- lIdentifier
  return $ Var (TyVar name KStar)

-- this, parametricType and dataDef parses haskell based data hello = Text a b | Nil type of data definitions
constructor :: Parser Expr
constructor = do
  name <- uIdentifier
  vars <- many $ try (Var <$> (Id "") <$> TCon <$> uIdentifier) <|> -- concrete type
                 try ( Var <$> (Id "") <$> TVar <$> lIdentifier) <|> -- type var
                 (Var <$> (Id "") <$>  extractType <$> (parens typeAp)) -- complex type, like List a
  return $ Constructor name vars

recordConstructor :: Parser Expr
recordConstructor = do
  name <- uIdentifier
  whitespace >> char '{' >> whitespace
  vars <- commaSep variable
  whitespace >> char '}' >> whitespace
  return $ Constructor name vars

constructors = try recordConstructor <|> constructor

-- simple ADT
typeDef :: Parser Expr
typeDef = do
  reserved "data"
  name <- uIdentifier
  vars <- many (extractVar <$> typeVariable)
  reservedOp "="
  fields <- sepBy1 constructors (char '|')
  return $ TypeDef name vars fields

function :: Parser Expr
function = do
  name <- lIdentifier
  args <- many (extractVar <$> variable)   -- (parens $ many identifier) <|> (parens $ commaSep identifier)
  reservedOp "="
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- lIdentifier
  args <- try (parens $ many identifier) <|> (parens $ commaSep identifier)
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- many $ try expr <|> parens expr
  let acc = SymId name
  if (length args == 0) then return $ acc
  else let e = foldl f acc args in return e -- type application
  where f acc arg = App acc arg

  -- return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

letins :: Parser Expr
letins = do
  reserved "let"
  defs <- commaSep function
  reserved "in"
  body <- expr
  return $ Let defs body

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- many (extractVar <$> variable)
  reservedOp "="
  body <- expr
  return $ Function o args body

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int <?> "integer: precedence value for the operator definition"
  args <- many (extractVar <$> variable)
  reservedOp "="
  body <- expr
  return $ Function o args body

factor :: Parser Expr
factor = try vector
      <|> try ifthen
      <|> try letins
      <|> (parens expr)
      <|> try floating
      <|> try int
      <|> try call


-- <|> try variable
-- <|> try for

defn :: Parser Expr
defn = try extern
    <|> try typeDef
    <|> try function
    <|> try unarydef
    <|> try binarydef
    -- <|> expr

-- <|> try record

-- contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

-- parseExpr :: String -> Either ParseError Expr
parseExpr s = runParserT (contents expr) initialParserState "<stdin>" s

--parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = runParserT (contents expr) initialParserState "<stdin>" s

-- parse a given file
parseToplevelFile name = parseFromFile (contents toplevel) name initialParserState

-- parseFromFile :: Parser a -> String -> IO (Either ParseError a)
-- redefining parse from file to work with our state - just a quick and dirty fix
parseFromFile p fname st
    = do input <- readFile fname
         return (runP p st fname input)

-- adding new stuff

{-
-- simple (flat) record
record :: Parser Expr
record = do
  reserved "data"
  name <- uIdentifier
  reservedOp "="
  fields <- braces $ semiSep expr
  return $ Record name [] fields

letins :: Parser Expr
letins = do
  reserved "let"
  defs <- semiSep $ do
    var <- lIdentifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs


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

-}

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
