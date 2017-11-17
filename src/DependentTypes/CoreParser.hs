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

module DependentTypes.CoreParser where

import Text.Parsec
-- import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (foldM)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Char (string)

import qualified Data.Vector.Unboxed as U

import Lexer
import DependentTypes.Core

int :: Parser Literal
int = LInt . fromInteger <$> integer

floating :: Parser Literal
floating = LFloat <$> float

stringVal :: Parser Literal
stringVal = LString <$> stringLit

binop = Ex.Infix  (BinaryOp <$> op) Ex.AssocLeft
unop  = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = operator

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
expr = Ex.buildExpressionParser (binops ++ [[unop],[binop]]) factor
-- expr = try vector <|> Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

-- concrete type or type application
typeAp :: Parser Type
typeAp = do
  name <- uIdentifier
  vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> lIdentifier ) <|> parens typeAp
  let tcon = TCon name
  if null vars then return tcon -- concrete type
  else return $ foldl TApp tcon vars -- type application


-- concrete type only
concreteType :: Parser Type
concreteType = TCon <$> uIdentifier


-- variable with type
variable :: Parser Var
variable = do
  name <- lIdentifier
  typ <- try (reservedOp ":" *> parens typeAp) <|>
         try (reservedOp ":" *> concreteType) <|>
         try (reservedOp ":" *> typeVariable) <|>
         pure ToDerive
  return $ Id name typ

-- variables in records need to be handled differently
varInRecord :: Parser Var
varInRecord = do
  name <- lIdentifier
  typ <- try (reservedOp ":" *> parens typeAp) <|>
         try (reservedOp ":" *> typeVariable) <|>
         (reservedOp ":" *> typeAp)
  return $ Id name typ

-- type variable - Kind is always '*', needs to be adjusted at later stages
typeVariable :: Parser Type
typeVariable = do
  name <- lIdentifier
  return $ TVar name

-- this, parametricType and dataDef parses haskell based data hello = Text a b | Nil type of data definitions
constructor :: Parser Expr
constructor = do
  name <- uIdentifier
  vars <- many  (try (Id "" . TCon <$> uIdentifier) <|> -- concrete type
                 try (Id "" . TVar <$> lIdentifier) <|> -- type var
                 (Id "" <$> parens typeAp) -- complex type, like List a
                 <?> "regular constructor failed")
  let t = map (\x -> VarId "") vars
  let sz = length vars
  return $ Lam name SzT { tuple = vars, size = sz } (Tuple SzT { tuple = t, size = sz})

recordConstructor :: Parser Expr
recordConstructor = do
  name <- uIdentifier
  whitespace >> char '{' >> whitespace
  vars <- commaSep varInRecord
  whitespace >> char '}' >> whitespace
  let t = map (\x -> VarId "") vars
  let sz = length vars
  return $ Lam name SzT { tuple = vars, size = sz } (Tuple SzT { tuple = t, size = sz})


constructors = try recordConstructor <|> constructor

-- simple ADT
typeDef :: Parser Expr
typeDef = do
  reserved "data"
  name <- uIdentifier
  vars <- many typeVariable
  let tvars = map (\(TVar n) -> TyVar n KStar) vars
  reservedOp "="
  fields <- sepBy1 constructors (char '|')
  return $ Lam name SzT { tuple = tvars, size = length vars } (Tuple SzT { tuple = fields, size = length fields})

function :: Parser Expr
function = do
  name <- lIdentifier
  args <- many variable   -- (parens $ many identifier) <|> (parens $ commaSep identifier)
  reservedOp "="
  body <- expr
  return $ Lam name SzT { tuple = args, size = length args } body

-- \x -> x + 1 etc
lambda :: Parser Expr
lambda = parens $ do
  char '\\'
  args <- many variable
  reservedOp "->"
  body <- expr
  return $ Lam "" SzT { tuple = args, size = length args } body
{-
extern :: Parser FlExpr
extern = do
  reserved "extern"
  name <- lIdentifier
  args <- try (parens $ many identifier) <|> (parens $ commaSep identifier)
  return $ Extern name args
-}

symbolId :: Parser Expr
symbolId = VarId <$> identifier

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

{-
letins :: Parser Expr
letins = do
  reserved "let"
  defs <- commaSep function
  reserved "in"
  body <- expr
  return $ Let defs body
-}

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  arg <- variable
  reservedOp "="
  body <- expr
  return $ Lam ("("++o++")") SzT { tuple = [arg], size = 1 } body

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int <?> "integer: precedence value for the operator definition"
  arg1 <- variable
  arg2 <- variable
  reservedOp "="
  body <- expr
  return $ Lam ("("++o++")") SzT { tuple = [arg1, arg2], size = 2 } body


argument :: Parser Expr
argument = try lambda
      <|> try containers
      <|> try (parens expr)
      <|> try (parens ifthen)
      <|> try (Lit <$> floating)
      <|> try (Lit <$> int)
      <|> try (Lit <$> stringVal)
      <|> symbolId

arguments :: Parser Expr
arguments = do
  args <- many1 argument
  return $ foldr1 App args

-- so the way we are parsing now is that we can only apply a symbol to a tuple of expressions,
-- moving away from trees to lists
call :: Parser Expr
call = do
  name <- identifier
  args <- many1 argument
  return $ App (VarId name) (Tuple SzT { tuple = args, size = length args })


factor :: Parser Expr
factor = try call <|> ifthen <|> argument -- arguments -- try letins <|>

defn :: Parser Expr
defn =  try typeDef
    <|> try function
    <|> try unarydef
    <|> try binarydef
    <|> expr


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

containers :: Parser Expr
containers = -- try  (FlTuple TTVector <$> angles   (commaSep expr)) <|>
        try  $ do
          args <- brackets (commaSep expr)
          return $ Lit $ LList SzT { tuple = args, size = length args}
        <|> try (braces (commaSep expr) >>= \args -> return (Tuple SzT { tuple = args, size = length args}))
        <|> (angles (commaSep factor)  >>= \args -> return (Lit $ LVec SzT { tuple = args, size = length args}))



{-
vector :: Parser Expr
vector = FlTuple TTVector <$> angles (commaSep arguments)
-}



{-
argument :: Parser Expr
argument = try vector
      <|> try lambda
      <|> try (parens expr)
      <|> try ifthen
      <|> try (Lit <$> floating)
      <|> try containers
      <|> try int
      <|> try symbolId
      <|> stringVal

arguments :: Parser Expr
arguments = do
  args <- many1 argument
  return $ foldr1 FlApp args


factor :: Parser Expr
factor = try letins <|> arguments

defn :: Parser Expr
defn = try extern
    <|> try typeDef
    <|> try function
    <|> try unarydef
    <|> try binarydef
    <|> expr
-}
