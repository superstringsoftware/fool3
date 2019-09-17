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

module Lambda.Parser where

import Text.Parsec
import Text.Parsec.Prim (many)
-- import Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Char (string)

-- import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State


import qualified Data.Vector.Unboxed as U

import State
import Lambda.Lexer
import Lambda.Syntax


----------------------------------------------------
-- PARSER ----------------------------------------------------
----------------------------------------------------
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
-- expr = Ex.buildExpressionParser ([]) factor

---------------------------------------------------------------------------------------------------
-- Parsing typed identifier
---------------------------------------------------------------------------------------------------
-- concrete type or type application
typeAp :: Parser Type
typeAp = do
  tcon <- try (TCon <$> uIdentifier) <|> (TVar <$> lIdentifier)
  vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> lIdentifier ) <|> parens typeAp
  if null vars then return tcon -- concrete type
  else return $ foldl TApp tcon vars -- type application

-- :a->b->String etc
tArr :: Parser Type
tArr = do
  ts <- sepBy1 allTypes (reservedOp "->")
  return $ foldr1 TArr ts

-- concrete type only
concreteType :: Parser Type
concreteType = TCon <$> uIdentifier

allTypes :: Parser Type
allTypes = try typeAp <|> try concreteType <|> typeVar

strictTypeSignature :: Parser Type
strictTypeSignature =
        try (reservedOp ":" *> parens tArr) <|>
        try (reservedOp ":" *> parens typeAp) <|>
        try (reservedOp ":" *> concreteType) <|>
        try (reservedOp ":" *> typeVar)
       
typeSignature = try strictTypeSignature <|> pure ToDerive

-- simply a name of type variable on the right of regular variable
typeVar :: Parser Type
typeVar = do
  name <- lIdentifier
  return $ TVar name

-- variable with type (or any identified for that matter, including function definitions)
variable :: Parser Var
variable = do
  name <- identifier --emptyStringParser -- added unnamed variables for easier record parsing
  typ <- typeSignature
  return $ Var name typ

---------------------------------------------------------------------------------------------------
int :: Parser Literal
int = LInt . fromInteger <$> integer

floating :: Parser Literal
floating = LFloat <$> float

stringVal :: Parser Literal
stringVal = LString <$> stringLit

symbolId :: Parser Expr
symbolId = VarId <$> identifier

-- everything is a function
lambda :: Parser Expr
lambda = do
    var@(Var _ tp) <- variable -- identifier
    args <- (reservedOp "=" *> reservedOp "\\" *> many variable)
    body <- try (reservedOp "." *> expr) <|> pure EMPTY
    let expr = Lam args body tp
    return $ Let [(var, expr)] EMPTY -- top level function, no "in" for LET

argument :: Parser Expr
argument = try containers
    <|> try (parens expr)
    <|> try (Lit <$> floating)
    <|> try (Lit <$> int)
    <|> try (Lit <$> stringVal)
    <|> symbolId

arguments :: Parser Expr
arguments = do
    args <- many1 argument
    return $ foldl1 App args -- need to use left fold here because application is highest precedence
  
containers :: Parser Expr
containers = -- try  (FlTuple TTVector <$> angles   (commaSep expr)) <|>
        try  $ do
            args <- brackets (commaSep expr)
            return $ Lit $ LList args
        <|> try (braces (commaSep expr) >>= \args -> return (Tuple "" args ToDerive))
        <|> (angles (commaSep factor)  >>= \args -> return (Lit $ LVec args))

-- contents :: Parser a -> Parser a
contents p = do
    whitespace
    r <- p
    eof
    return r

factor :: Parser Expr
-- factor = try caseExpression <|> try call <|> try ifthen <|> argument -- arguments -- try letins <|>
factor = arguments -- try caseExpression <|> arguments -- arguments -- try letins <|>

defn :: Parser Expr
defn =  try lambda <|> expr

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
parseToplevelFile :: String -> IntState (Either ParseError [Expr])
parseToplevelFile name = parseFromFile (contents toplevel) name initialParserState

parseFromFile :: Parser a -> String -> ParserState -> IntState (Either ParseError a)
-- redefining parse from file to work with our state - just a quick and dirty fix
parseFromFile p fname st
    = liftIO (readFile fname) >>= runPT p st fname
             
    
    
    
   
    