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

module SurfaceLanguage.Lambda.Parser where

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
import SurfaceLanguage.Lambda.Lexer
import Core.Syntax
import Core.Environment
import Logs
import Util.PrettyPrinting

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL

{-
Ok, so in the new concept we only need to parse the following syntaxis:
[<predicate> => ] <name>:<type> { name1:type1 [= <expr>], ... , namen:typen [=<expr>] }

alternatively, we can combine several fields of the same type inside of a tuple:
{ n1,n2,n3:t1, n4:t2, n5,n6:t3 } 

Will we be able to distinguish between that and pattern matched format though?
map _ Nil = Nil
map f (x::xs) = (f x)::(map f xs)
and:
map { func, ls } = {
    _ Nil -> Nil,
    f (x::xs) -> (f x)::(map f xs)
}
How do we say that above is a function definition instead of a pattern match on a record with fields func and ls? Only because of constructor functions?

REQUIRE type signatures when writing out functions in this full format? alternatively, 
REQUIRE pattern matches only within the Tuple?
------------------------

This leads to the following logic for the top-level:
- Expression: id:type arguments:record body:expr or record
- Expr: constant (literal), list, vector, App, operator App, Record of Expr
Parsing:
- optional predicate
- identifier with optional type signature
- optional arguments record:
    - identifier[[:type] = expr] - split with ","
- = <expr> - optional.
That's it! Should be very clean.
-}

dbg msg = liftIO $ putStrLn msg
-- Starting with Predicates parser:
pPreds :: Parser [Pred]
pPreds = do
    --dbg "pPreds started"
    try (reserved "exists") <|> reserved "∃"
    preds <- try (parens (sepBy1 pPred (reservedOp ",") )) <|> (pPred >>= \p -> pure [p])
    reservedOp "=>"
    -- dbg "pPreds finished"
    return preds

pPred :: Parser Pred
pPred = do
    tcon <- (TCon <$> uIdentifier) <?> "type class name (should start from the upper case)." 
    vars <- many1 $ (TVar <$> variable)                
    return $ Exists $ TApp tcon vars -- foldl TApp tcon vars -- type application

-- Next, top level identifier: name and optional type signature
pVar :: Parser Var
pVar = do
  name <- try (parens op) <|> identifier --emptyStringParser -- added unnamed variables for easier record parsing
  typ <- typeSignature
  return $ Var name typ

pAnonVar :: Parser Var
pAnonVar = do
    typ <- strictTypeSignature
    return $ Var "" typ

-- x:t [= <expr>] -- single field parser
pField :: Parser Field
pField = do
    bind <- try pTopLevelBinding <|> pure EMPTY
    if (bind == EMPTY) then do
            (Var nm tp) <- (try pVar <|> pAnonVar)
            ex  <- try (reservedOp "=" *> pExpr) <|> pure EMPTY
            return $ Field nm tp ex
    else return $ binding2field bind

-- parsing a record within {}
pFields :: Parser Record
pFields = braces (sepBy pField (reservedOp ",") )


-- Parser doing the top-level expression parsing for most of the definitions, excluding pattern match
pBindingWithArgs :: Parser Expr
pBindingWithArgs = do
    -- dbg "pTopLevelBinding started"
    preds <- try pPreds <|> pure [] -- optional predicates
    var   <- pVar -- mandatory identificator
    args  <- pFields -- arguments record
    ex    <- try (reservedOp "=" *> pExpr) <|> pure EMPTY -- trying to parse the body
    -- dbg "pTopLevelBinding ended"
    return $ Binding var (Lambda args ex ToDerive preds)

pBindingNoArgs :: Parser Expr
pBindingNoArgs = do
    var   <- pVar -- mandatory identificator
    ex    <- (reservedOp "=" *> pExpr) -- parse the body
    return $ Binding var (Lambda [] ex ToDerive [])


pTopLevelBinding :: Parser Expr
pTopLevelBinding = try pBindingWithArgs <|> pBindingNoArgs 

-- top-level pattern match, like in Haskell
pTopLevelPatternMatch :: Parser Expr
pTopLevelPatternMatch = do
    --dbg "pPatternMatch started"
    h <- pArgs <?> "pArgs in pattern match"
    reservedOp "="
    t <- expr
    --dbg "pPatternMatch ended"
    return $ PatternMatch [h] t

-- Building expression parser
pExpr :: Parser Expr
pExpr = Ex.buildExpressionParser (binops ++ [[unop],[binop]] ++ [[binary "==" Ex.AssocLeft]] ) pFactor

pFactor :: Parser Expr
pFactor = try pArgs <?> "arguments in pFactor failed?"

pContainers :: Parser Expr
pContainers = -- try  (FlTuple TTVector <$> angles   (commaSep expr)) <|>
        try  $ do
            args <- brackets (commaSep expr)
            return $ Lit $ LList args
        <|> (try pFields >>= \args -> return (Rec args))
        -- <|> try (braces (commaSep pAnonVar) >>= \args -> return $ Rec $ vars2record args)
        <|> try (braces (commaSep pExpr) >>= \args -> return $ Rec $ recordFromExprs args)
        <|> (angles (commaSep factor)  >>= \args -> return (Lit $ LVec args))

pArg :: Parser Expr
pArg = try pContainers
    <|> try (parens pExpr)
    -- <|> try typedId
    <|> try (Lit <$> floating)
    <|> try (Lit <$> int)
    <|> try (Lit <$> stringVal)
    <|> symbolId
    <?> "container, literal, symbol id or parenthesized expression"

pArgs :: Parser Expr
pArgs = do
    args@(f:xs) <- many1 pArg
    --return $ foldl1 App args -- need to use left fold here because application is highest precedence
    let er = if (xs == []) then f else (App f xs)
    return er

-- Building top level parsers
pDef :: Parser Expr
pDef =  do
    expr <- try pTopLevelBinding
            -- <|> try anonConstructor
            -- <|> try binding
            <|> try pTopLevelPatternMatch
            <|> (VarDefinition <$> pVar)
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
    
expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[unop],[binop]] ++ [[binary "==" Ex.AssocLeft]] ) factor
-- expr = Ex.buildExpressionParser ([]) factor

---------------------------------------------------------------------------------------------------
-- Parsing typed identifier
---------------------------------------------------------------------------------------------------
-- concrete type or type application
typeAp :: Parser Type
typeAp = do
  tcon <- try concreteType <|> (TVar <$> variable)
  vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> variable ) 
                 <|> (parens typeAp) -- <|> (TExpr <$> argument)
  if null vars then return tcon -- concrete type
  else return $ TApp tcon vars -- foldl TApp tcon vars -- type application

-- :a->b->String etc
tArr :: Parser Type
tArr = do
  ts <- sepBy1 allTypes (reservedOp "->")
  return $ foldr1 TArr ts

-- concrete type only
concreteType :: Parser Type
concreteType = do
    nm <- uIdentifier
    return $ if (nm == "Type") then SmallType else TCon nm 

allTypes :: Parser Type
allTypes = try typeAp <|> try concreteType <|> typeVar

strictTypeSignature :: Parser Type
strictTypeSignature =
        try (reservedOp ":" *> tArr) <|>
        try (reservedOp ":" *> parens tArr) <|>
        try (reservedOp ":" *> parens typeAp) <|>
        -- try (reservedOp ":" *> spaces *> string "Type" *> spaces *> return SmallType) <|> -- built in "Type" parsing as SmallType right away
        try (reservedOp ":" *> concreteType) <|>
        try (reservedOp ":" *> typeVar)
       
typeSignature = try strictTypeSignature <|> pure ToDerive

-- simply a name of type variable on the right of regular variable
-- we are putting the type to SmallType (so, any regular type), but in the presence of predicates it will need to change
typeVar :: Parser Type
typeVar = do
  name <- lIdentifier
  return $ TVar $ Var name SmallType

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
  return $ Let $ Field name typ EMPTY
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
-- this is a huge and entangled method doing a lot, needs REFACTORING and SIMPLIFICATION!!!
lambda :: Parser Expr
lambda = do
    pos <- getPosition
    var@(Var name tp) <- variable -- identifier
    reservedOp "="
    preds <- try predicates <|> pure []
    args <- try (reservedOp "\\" *> many1 variable ) <|> (reservedOp "\\" *> many1 anonVariable) <?> "list of variables or anonymous variables. You can't mix both!"
    setCurrentArity (length args)
    setCurrentLambdaName $ L.pack name
    body' <- try (reservedOp "." *> expr) <|> pure EMPTY
    setCurrentArity 0
    setCurrentLambdaName ""
    incl <- getInsideTypeclass
    -- if we are not inside typeclass and body is empty - this must be a data constructor,
    -- so changing empty to the tuple of the correct size
    let body = if ((body' == EMPTY) && (not incl)) then Tuple name (map (\a -> VarId "_") args) tp else body'
    let ex = LamOld (vars2record args) body tp preds
    -- checking whether it's a data constructor and then whether it has type specified:
    -- this may change if we change the syntaxis for type definition!!!
    case body of
        Tuple _ _ _ -> 
            if (isTConOrTApp tp) then return $ Let $ Field name tp ex
            else do
                let s = if (tp == ToDerive) then " has no type" else " has type " ++ (TL.unpack $ pShow tp) 
                lift $ logError $ LogPayload 
                        (sourceLine pos) 
                        (sourceColumn pos)  ""
                        (("data constructor " ++ name ++ s
                                ++ " but it can only be a concrete type or type application." 
                                ++ "\nIf you intend to define a type class - you should write " ++ name ++ " : Class = \\ ..."))
                return $ Let $ Field name tp ex
        _           -> return $ Let $ Field name tp ex -- top level function, no "in" for LET

predicate :: Parser Pred
predicate = do
    tcon <- (TCon <$> uIdentifier) <?> "type class name" 
    vars <- many1 $ (TVar <$> variable)                
    return $ Exists $ TApp tcon vars -- foldl TApp tcon vars -- type application

predicates :: Parser [Pred]
predicates = do
    try (reserved "exists") <|> reserved "∃"
    preds <- try (parens (sepBy1 predicate (reservedOp ",") )) <|> (predicate >>= \p -> pure [p])
    reservedOp "=>"
    return preds

typeclass :: Parser Expr    
typeclass = do
    setInsideTypeclass True
    s <- braces (sepBy1 defn (reservedOp ";")) <?> "error while parsing typeclass"
    setInsideTypeclass False
    return $ Tuple "" s ToDerive

-- pattern match inside a function    
lambdaPatternMatch :: Parser Expr
lambdaPatternMatch = do
    pos <- getPosition
    h <- argumentsOnly
    a <- getCurrentArity
    reservedOp "->"
    t <- expr
    if ( (length h) == a ) 
        then return $ PatternMatch h t
        else do
            nm <- getCurrentLambdaName
            lift $ logError $ LogPayload (sourceLine pos) 
                                         (sourceColumn pos) "" 
                                         (("function " ++ L.unpack nm ++  " has arity " ++ show a ++ " but pattern match has " ++ show (length h) ++ " arguments"))
            return $ PatternMatch h t

-- Tuple that data constructors return, can only contain names or "_" symbols    
consTuple :: Parser Expr
consTuple = do
    pos <- getPosition
    a <- getCurrentArity
    args <- braces (many symbolId) 
    if ( (length args) == a ) then return (Tuple "" args ToDerive)
    else do
        nm <- getCurrentLambdaName
        lift $ logError $ LogPayload (sourceLine pos) 
                    (sourceColumn pos) "" 
                    (("constructor " ++ L.unpack nm ++  " has arity " ++ show a 
                             ++ " but it's tuple has " ++ show (length args) ++ " arguments"))
        return (Tuple "" args ToDerive)


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
    var@(Var name tp) <- variable -- identifier
    reservedOp "="
    let bodyAction = if (tp /= SmallType) then expr else expr
    body <- bodyAction
    return $ Let $ Field name tp body -- top level function, no "in" for LET


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
            -- <|> try anonConstructor
            <|> try binding
            <|> try patternMatch
            <|> (VarDefinition <$> variable)
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
-- parseToplevel s = runParserT (contents defn) initialParserState "<stdin>" s

-- give a text and then parse it - need to store source for error reporting
-- parseWholeFile s fn = runParserT (contents toplevel) initialParserState fn s

-- parse a given file
parseToplevelFile :: String -> IntState (Either ParseError [Expr])
parseToplevelFile name = parseFromFile (contents toplevel) name initialParserState

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