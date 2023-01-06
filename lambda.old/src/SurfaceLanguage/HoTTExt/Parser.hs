{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    :  Pure Parser for HoTTExt surface language
-- Copyright :  (c) Anton Antich, 2018-2021
-- License   :  MIT
-- Maintainer:  anton.antich@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module SurfaceLanguage.HoTTExt.Parser where

import Text.Parsec
import Text.Parsec.Prim (many)
-- import Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Char (string)

-- import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

import qualified Data.Text as L

import qualified Data.Vector.Unboxed as U

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL

import SurfaceLanguage.HoTTExt.Lexer
import SurfaceLanguage.HoTTExt.Syntax
import qualified Core.HoTTExt.Core as Core

{-
Ok, so in the new concept we only need to parse the following syntaxis:
1. Definition:
[<predicate> => ] <Identifier> [xo1:to1, ..., xon:ton] 
  (name1:type1 [= <expr>], ... , namen:typen [=<expr>] ) : <Type> = <expr>

alternatively, we can combine several fields of the same type inside of a tuple:
(n1,n2,n3:t1, n4:t2, n5,n6:t3)

2. Pattern match:
<Identifier> [xo1,...,xon] (x1, ..., xm) = <expr> OR
<Identifier> [xo1,...,xon] (x1 = e1, ..., xm = em) = <expr>

These 2 cases are a bit difficult to distinguish, as the only difference is the absense of type 
signatures, but that's exactly what we would want in many cases, to have type inference. 
So, what if we defined fact without the type?

fact (n) = n ? (
    0 -> 1,
    n -> n * fact (n-1)
)

fact (0) = 1
fact (n) = n * fact (n-1)

they would be parsed into something like

Lambda {
    implParams = []
  , explParams = [Lit (LInt 0)]
  , body       = 1
  , lamType    = UNDEFINED
} and

Lambda {
    implParams = []
  , explParams = [Id "n"]
  , body       = App (*) ... etc
  , lamType    = UNDEFINED
}

and they'd need to be bound to the same name, which means we'll have to create a pattern match 
for the function.

length ( lst:List(a) ) = lst ? (
    [] -> 0,
    x :: xs -> 1 + length (xs)
)

This syntaxis doesn't work well for many variables though :(

Ack (m, n: Int) : Int
Ack (0, n) = n + 1
Ack (m, 0) = Ack (m-1, 1)
Ack (m, n) = Ack (m-1, Ack (m, n-1))

So, apparently we have to require function header, even if it's untyped.

OK, the SOLUTION IS: REQUIRE ALL TOP LEVEL EXPRESSIONS TO BE A LAMBDA.
Then we sort it out in the conversion stage.


=========== OLD STUFF BELOW =============
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

------------------------------- NEW STUFF ------------------------------------

----------------------------------------------------
-- Operator tables + top level parsing structure 
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

-- Building expression parser (RHS!?)
pExpr :: Parser Expr
pExpr = Ex.buildExpressionParser (binops ++ [[unop],[binop]] ++ [[binary "==" Ex.AssocLeft]] ) pFactor

pFactor :: Parser Expr
pFactor = try pArgs <?> "arguments in pFactor failed?"

-- list of arguments WITHOUT brackets
pArgs :: Parser Expr
pArgs = do
    res <- sepBy1 pArg (reservedOp ",")
    return (CoreExpr $ Core.Tuple res)

pArg :: Parser Core.Expr
pArg = --try pContainers
    -- <|> try pRecordAccess
    try (parens pExpr)
    -- <|> try typedId
    <|> try (Core.Lit <$> floating)
    <|> try (Core.Lit <$> int)
    <|> try (Core.Lit <$> stringVal)
    <|> symbolId
    <?> "container, literal, symbol id or parenthesized expression"



------------------------------- Literals ------------------------------------
int :: Parser Core.Literal
int = Core.LInt . fromInteger <$> integer

floating :: Parser Core.Literal
floating = Core.LFloat <$> float

stringVal :: Parser Core.Literal
stringVal = Core.LString <$> stringLit

symbolId :: Parser Core.Expr
symbolId = do 
    s <- (try (parens op) <|> identifier)
    return $ Core.Id s


{-
-- Starting with Predicates parser:
pPreds :: Parser [Core.Pred]
pPreds = do
    --dbg "pPreds started"
    try (reserved "exists") <|> reserved "∃"
    preds <- try (parens (sepBy1 pPred (reservedOp ",") )) <|> (pPred >>= \p -> pure [p])
    reservedOp "=>"
    -- dbg "pPreds finished"
    return preds

pPred :: Parser Core.Pred
pPred = do
    tcon <- (Core.Id <$> uIdentifier) <?> "Sigma type name should start from the upper case." 
    vars <- many1 $ (TVar <$> pVar)                
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
    ex    <- reservedOp "=" *> pExpr -- parse the body
    return $ Binding var (Lambda [] ex ToDerive [])


pTopLevelBinding :: Parser Expr
pTopLevelBinding = try pBindingWithArgs <|> pBindingNoArgs 

-- top-level pattern match, like in Haskell
pTopLevelPatternMatch :: Parser Expr
pTopLevelPatternMatch = do
    --dbg "pPatternMatch started"
    h <- pApp <?> "pArgs in pattern match"
    reservedOp "="
    --dbg "pPatternMatch ended"
    PatternMatch h <$> (try (braces pClassPatternMatches) <|> pExpr)

pClassPatternMatches :: Parser Expr
pClassPatternMatches = Patterns <$> (try (commaSep pTopLevelBinding) <|> (commaSep pClassPatternMatch))

pClassPatternMatch :: Parser Expr
pClassPatternMatch = do 
    h <- pApp <?> "pArgs in pattern match"
    reservedOp "="
    PatternMatch h <$> pExpr

pContainers :: Parser Expr
pContainers = -- try  (FlTuple TTVector <$> angles   (commaSep expr)) <|>
        try (brackets (commaSep pExpr) >>= return . Lit . LList)
        <|> (try pFields >>= \args -> return (Rec args))
        -- <|> try (braces (commaSep pAnonVar) >>= \args -> return $ Rec $ vars2record args)
        <|> try (braces (commaSep pExpr) >>= \args -> return $ Rec $ recordFromExprs args)
        -- <|> try (braces pClassPatternMatches)
        <|> (angles (commaSep pFactor)  >>= \args -> return (Lit $ LVec args))


-- Clear function application
pApp :: Parser Expr
pApp = do
    func <- symbolId
    args <- many pArg
    let er = if args == [] then func else App func args
    return er


pRecordAccess :: Parser Expr
pRecordAccess = do
    arg <- try (parens pExpr) <|> (VarId <$> identifier)
    char '.'
    args <- sepBy1 (try (parens pExpr) <|> VarId <$> identifier) (char '.' )
    return $ RecordAccess (arg:args)

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

-}

---------------------------------------------------------------------------------------------------
-- Parsing typed identifier
---------------------------------------------------------------------------------------------------
{-
-- concrete type or type application
typeAp :: Parser Type
typeAp = do
  tcon <- try concreteType <|> (TVar <$> pVar)
  vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> pVar ) 
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
    return $ if (nm == "Type") then SmallType 
             else if (nm == "Class") then TClass else TCon nm 

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

-}
---------------------------------------------------------------------------------------------------
-- contents :: Parser a -> Parser a
contents p = do
    whitespace
    r <- p
    eof
    return r
   
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