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
import Control.Monad.Trans.State.Strict


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
  tcon <- try (TCon <$> uIdentifier) <|> (TVar <$> lIdentifier)
  vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> lIdentifier ) 
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
  return $ TVar name

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
  let v = Var name typ
  return $ Let [(v,EMPTY)] EMPTY
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

-- everything is a function
lambda :: Parser Expr
lambda = do
    var@(Var _ tp) <- variable -- identifier
    reservedOp "="
    preds <- try predicates <|> pure []
    args <- (reservedOp "\\" *> many1 variable )
    setCurrentArity (length args)
    body <- try (reservedOp "." *> expr) <|> pure EMPTY
    let ex = Lam args body tp preds
    setCurrentArity 0
    return $ Let [(var, ex)] EMPTY -- top level function, no "in" for LET

anonConstructor :: Parser Expr
anonConstructor = do
    var@(Var _ tp) <- variable -- identifier
    reservedOp "="
    preds <- try predicates <|> pure []
    args <- (reservedOp "\\" *> many1 anonVariable)
    body <- try (reservedOp "." *> expr) <|> pure EMPTY
    let ex = Lam args body tp preds
    return $ Let [(var, ex)] EMPTY -- top level function, no "in" for LET
    
predicate :: Parser Pred
predicate = do
    tcon <- (TCon <$> uIdentifier) <?> "type class name" 
    vars <- many1 $ (TVar <$> lIdentifier)                
    return $ Exists $ TApp tcon vars -- foldl TApp tcon vars -- type application

predicates :: Parser [Pred]
predicates = do
    try (reserved "exists") <|> reserved "∃"
    preds <- try (parens (sepBy1 predicate (reservedOp ",") )) <|> (predicate >>= \p -> pure [p])
    reservedOp "=>"
    return preds

typeclass :: Parser Expr    
typeclass = do
    s <- braces (sepBy1 defn (reservedOp ";")) <?> "error while parsing typeclass"
    return $ Tuple "" s ToDerive

-- pattern match inside a function    
lambdaPatternMatch :: Parser Expr
lambdaPatternMatch = do
    h <- argumentsOnly
    a <- getCurrentArity
    reservedOp "->"
    t <- expr
    if ( (length h) /= a ) 
        then fail ("function has arity " ++ show a ++ " but pattern match has " ++ show (length h) ++ " arguments")             
        else return $ PatternMatch h t

argumentsOnly :: Parser [Expr]
argumentsOnly = do
    args <- many1 argument
    return args
    

-- function with pattern matches defined inside
{-
length:Int = \ ls:(List a) . {
    Nil -> 0;
    (Cons _ xs) -> 1 + length xs
};
-}    
patternsInsideLambda :: Parser Expr
patternsInsideLambda = do
    s <- braces (sepBy1 lambdaPatternMatch (reservedOp ";")) <?> "error while parsing patterns inside lambda"
    return $ Patterns s


-- top-level pattern match, like in Haskell
patternMatch :: Parser Expr
patternMatch = do
    h <- arguments
    reservedOp "="
    t <- expr
    return $ PatternMatch [h] t


-- simple top-level binding of a symbol to expression, without lambdas
binding :: Parser Expr
binding = do
    var@(Var _ tp) <- variable -- identifier
    body <- (reservedOp "=" *> expr)
    return $ Let [(var, body)] EMPTY -- top level function, no "in" for LET

-- Tuple that data constructors return, can only contain names or "_" symbols    
consTuple :: Parser Expr
consTuple = braces (many symbolId) >>= \args -> return (Tuple "" args ToDerive)


argument :: Parser Expr
argument = try containers
    <|> try (parens expr)
    <|> try typedId
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
    eof
    return r

factor :: Parser Expr
-- factor = try caseExpression <|> try call <|> try ifthen <|> argument -- arguments -- try letins <|>
factor = try patternsInsideLambda <|> try typeclass <|> try consTuple <|> arguments -- try caseExpression <|> arguments -- arguments -- try letins <|>

defn :: Parser Expr
defn =  do
    expr <- try lambda
            <|> try anonConstructor
            <|> try binding
            <|> patternMatch
            -- <|> expr
            <?> "lambda, binding, pattern match or expression"
    return expr
        
        

toplevel :: Parser [Expr]
toplevel = many $ do
    pos <- getPosition
    def <- defn
    reservedOp ";"
    ints <- lift get 
    let pm = (def, SourceInfo (sourceLine pos) (sourceColumn pos) ""):(parsedModule ints)
    lift $ put ints {parsedModule = pm}    
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
             
    
    
    
   
    