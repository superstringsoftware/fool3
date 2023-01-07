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

module Parser where

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
import Lexer
import Core
import Logs
import Util.PrettyPrinting

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL

dbg msg = liftIO $ putStrLn msg

-- sum type:
-- type Bool = {True, False}
pSumType :: Parser Expr
pSumType = do
    reserved "type"
    name <- identifier
    args <- try pVars <|> pure []
    ex   <- reservedOp "=" *> (braces (sepBy1 (pConstructor (Id name)) (reservedOp ",") ))
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = Tuple ex
     , lamType    = Type 
    }
    return $ SumType lam

-- To properly parse the type definition we need to properly parse
-- CONSTRUCTORS inside the sum type
-- For now, simple constructors as in haskell (eventually for dependent types we'll need others)
-- passing the name of the SumType to set type of the constructors --
-- eventually we'll need to parse the type (for GADT support etc)
pConstructor :: Expr -> Parser Expr
pConstructor tp = do
    name <- identifier
    args <- try pVars <|> pure []
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = UNDEFINED
     , lamType    = tp 
    }
    return $ Lam lam

-- Variable with optional type signature, to be used in DEFINITIONS!!!
-- (as opposed to function calls, as there it can be any expression)
-- TODO: eventually needs to parse (= EXPR) part
pVar :: Parser Var
pVar = do
  name <- identifier --emptyStringParser -- added unnamed variables for easier record parsing
  typ  <- typeSignature
  return $ Var name typ UNDEFINED

-- variables in parenthesis in function / type etc definitions:
-- Maybe (a:Type) etc
pVars :: Parser Record
pVars = parens (sepBy pVar (reservedOp ",") )

strictTypeSignature :: Parser Expr
strictTypeSignature =
        -- try (reservedOp ":" *> tArr) <|>
        -- try (reservedOp ":" *> parens tArr) <|>
        -- try (reservedOp ":" *> parens typeAp) <|>
        -- try (reservedOp ":" *> spaces *> string "Type" *> spaces *> return SmallType) <|> -- built in "Type" parsing as SmallType right away
        try (reservedOp ":" *> concreteType) -- <|>
        -- try (reservedOp ":" *> typeVar)
       
typeSignature = try strictTypeSignature <|> pure UNDEFINED

concreteType :: Parser Expr
concreteType = do
    nm <- uIdentifier
    return $ Id nm


{- 
=====================================================================================
-}
-- Building expression parser
pExpr :: Parser Expr
pExpr = Ex.buildExpressionParser (binops ++ [[unop],[binop]] ++ [[binary "==" Ex.AssocLeft]] ) pFactor

pFactor :: Parser Expr
pFactor = try pSumType <?> "arguments in pFactor failed?"


-- Building top level parsers
pDef :: Parser Expr
pDef =  do
    expr <- try pFactor
            <?> "lambda, binding, pattern match or expression"
    return expr
          

pToplevel :: Parser [Expr]
pToplevel = many $ do
    pos <- getPosition
    def <- pDef
    reservedOp ";"
    ints <- lift get 
    let pm = (def, SourceInfo (sourceLine pos) (sourceColumn pos) ""):(parsedModule ints)
    lift $ put ints {parsedModule = pm}    
    return def
    
--parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = runParserT (contents pDef) initialParserState "<stdin>" s

-- give a text and then parse it - need to store source for error reporting
parseWholeFile s fn = runParserT (contents pToplevel) initialParserState fn s

-- testing parsers
hlpp p s = runParserT (contents p) initialParserState "<stdin>" s
testParser p str = liftIO $ runIntState (hlpp p (L.pack str)) emptyIntState

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




-- contents :: Parser a -> Parser a
contents p = do
    whitespace
    r <- p
    eof
    return r

    
-- parseExpr :: String -> Either ParseError Expr
parseExpr s = runParserT (contents pExpr) initialParserState "<stdin>" s

--parseToplevel :: String -> Either ParseError [Expr]
-- parseToplevel s = runParserT (contents defn) initialParserState "<stdin>" s

-- give a text and then parse it - need to store source for error reporting
-- parseWholeFile s fn = runParserT (contents toplevel) initialParserState fn s

-- parse a given file
parseToplevelFile :: String -> IntState (Either ParseError [Expr])
parseToplevelFile name = parseFromFile (contents pToplevel) name initialParserState

parseFromFile :: Parser a -> String -> ParserState -> IntState (Either ParseError a)
-- redefining parse from file to work with our state - just a quick and dirty fix
parseFromFile p fname st
    = liftIO (T.readFile fname) >>= runParserT p st fname
             
    
    
    
   
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