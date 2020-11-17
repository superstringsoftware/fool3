{-# LANGUAGE OverloadedStrings #-}
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

module SurfaceLanguage.Thask.Parser where

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
import Control.Monad.Trans.State.Strict

import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as L

import qualified Data.Vector.Unboxed as U

import State
import SurfaceLanguage.Thask.Lexer
import Core.Syntax
import Core.Environment
import Logs
import Util.PrettyPrinting


-- used to show syntax errors together with source (first argument)
showSyntaxError :: L.Text -> ParseError -> String
showSyntaxError s err = L.unpack $ L.unlines [
      "  ",
      "  " <> lineContents,
      "  " <> ((L.replicate col " ") <> "^"),
      (L.pack $ show err)
    ]
  where
    lineContents = (L.lines s) !! line
    pos  = errorPos err
    line = sourceLine pos - 1
    col  = fromIntegral $ sourceColumn pos - 1

----------------------------------------------------
-- PARSER ----------------------------------------------------
----------------------------------------------------
binop = Ex.Infix  (BinaryOp <$> op) Ex.AssocLeft
unop  = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc
binaryCustom s assoc = Ex.Infix (op >>= \s -> return (BinaryOp s)) assoc

op :: Parser String
op = operator
    
binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft, binary "/" Ex.AssocLeft, binary "*#" Ex.AssocLeft, binary "/#" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft, binary "-" Ex.AssocLeft, binary "+#" Ex.AssocLeft, binary "-#" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft, binary ">" Ex.AssocLeft]]

    
-- helper parsers: lower case and upper case
lIdentifier = skipMany space >> lookAhead lower >> identifier
uIdentifier = skipMany space >> lookAhead upper >> identifier
    
expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[unop],[binop]] ++ [[binary "==" Ex.AssocLeft]] ) factor
-- expr = Ex.buildExpressionParser ([]) factor

---------------------------------------------------------------------------------------------------
-- Parsing typed identifier
---------------------------------------------------------------------------------------------------
-- concrete type or type application
typeAp :: Parser Type
typeAp = do
  tcon <- try (TCon <$> uIdentifier) <|> (TVar <$> variable )
  vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> variable ) 
                 <|> try (parens typeAp) <|> (TExpr <$> argument)
  if null vars then return tcon -- concrete type
  else return $ TApp tcon vars -- foldl TApp tcon vars -- type application

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
  return $ TVar (Var name ToDerive)

-- variable with type (or any identified for that matter, including function definitions)
-- including operators
variable :: Parser Var
variable = do
  name <- try (parens op) <|> identifier --emptyStringParser -- added unnamed variables for easier record parsing
  typ <- typeSignature
  return $ Var name typ

anonVariable :: Parser Var
anonVariable = do
    typ <- strictTypeSignature
    return $ Var "" typ
  

typedId :: Parser Expr
typedId = do
  name <- try (parens op) <|> identifier --emptyStringParser -- added unnamed variables for easier record parsing
  typ <- strictTypeSignature
  return $ Let (Field name typ EMPTY)
---------------------------------------------------------------------------------------------------
int :: Parser Literal
int = LInt . fromInteger <$> integer

floating :: Parser Literal
floating = LFloat <$> float

stringVal :: Parser Literal
stringVal = LString <$> stringLit

symbolId :: Parser Expr
symbolId = do 
    s <- (try (parens op) <|> identifier)
    return $ VarId s


binding :: Parser Expr
binding = do
    var@(Var name tp) <- variable -- identifier
    body <- (reservedOp "=" *> expr)
    return $ Let (Field name tp body) -- top level function, no "in" for LET

argument :: Parser Expr
argument = try containers
    <|> try (parens expr)
    -- <|> try typedId
    <|> try (Lit <$> floating)
    <|> try (Lit <$> int)
    <|> try (Lit <$> stringVal)
    <|> symbolId
    <?> "container, literal, symbol id or parenthesized expression"

arguments :: Parser Expr
arguments = do
    args@(f:xs) <- many1 argument
    --return $ foldl1 App args -- need to use left fold here because application is highest precedence
    let er = if (xs == []) then f else (App f xs)
    return er
  
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
    whitespace
    eof
    return r

factor :: Parser Expr
-- factor = try caseExpression <|> try call <|> try ifthen <|> argument -- arguments -- try letins <|>
factor = try {- patternsInsideLambda <|> try typeclass <|>  try consTuple <|> -} arguments -- try caseExpression <|> arguments -- arguments -- try letins <|>


defn :: Parser Expr
defn =  do
    ex <- try binding -- WRONG, PLACEHOLDER!!! -- try lambda
            -- <|> try anonConstructor
            --try binding
            -- <|> patternMatch
            <|> expr
            <?> "lambda, binding, pattern match or expression"
    return ex
        
    

toplevel :: Parser [Expr]
toplevel = many $ do
    pos <- getPosition
    def <- defn
    --endOfLine
    reservedOp ";"
    --crlf
    -- try newline
    --spaces
    ints <- lift get 
    let pm = (def, SourceInfo (sourceLine pos) (sourceColumn pos) ""):(parsedModule ints)
    lift $ put ints {parsedModule = pm}    
    return def
    
-- parseExpr :: String -> Either ParseError Expr
parseExpr s = runParserT (contents expr) initialParserState "<stdin>" s

--parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = runParserT (contents defn) initialParserState "<stdin>" s

-- give a text and then parse it - need to store source for error reporting
parseWholeFile s fn = runParserT (contents toplevel) initialParserState fn s

-- parse a given file
parseToplevelFile :: String -> IntState (Either ParseError [Expr])
parseToplevelFile name = parseFromFile (contents toplevel) name initialParserState

parseFromFile :: Parser a -> String -> ParserState -> IntState (Either ParseError a)
-- redefining parse from file to work with our state - just a quick and dirty fix
parseFromFile p fname st
    = liftIO (T.readFile fname) >>= runParserT p st fname
             
    
    
    
   
    