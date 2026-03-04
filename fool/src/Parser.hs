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
import Surface
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
    name <- uIdentifier
    args <- try pVars <|> pure []
    -- Default return type for constructors: Name(arg1, arg2, ...) or just Name
    let defaultTp = if null args
            then Id name
            else App (Id name) (map (\v -> Id (Surface.name v)) args)
    ex   <- reservedOp "=" *> (braces (sepBy1 (pConstructor defaultTp) (reservedOp ",") ))
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = Constructors ex
     , lamType    = Type
    }
    return $ SumType lam

-- To properly parse the type definition we need to properly parse
-- CONSTRUCTORS inside the sum type
-- For now, simple constructors as in haskell (eventually for dependent types we'll need others)
-- passing the name of the SumType to set type of the constructors --
-- eventually we'll need to parse the type (for GADT support etc)
-- Constructor parser with optional GADT return type annotation
-- e.g. VNil : Vec(a, Z)  or  VCons(head:a, tail:Vec(a,n)) : Vec(a, Succ(n))
pConstructor :: Expr -> Parser Lambda
pConstructor defaultTp = do
    name <- uIdentifier
    args <- try pVars <|> pure []
    -- GADT: optional explicit return type  "Con(args) : ReturnType"
    tp <- try (reservedOp ":" >> concreteType) <|> pure defaultTp
    return Lambda {
       lamName    = name
     , params = args
     , body       = UNDEFINED
     , lamType    = tp
    }

-- STRUCTURES ---------------------------------------------------------
pStructureWith :: String -> StructKind -> Parser Expr
pStructureWith keyword kind = do
    reserved keyword
    name <- identifier
    args <- try pVars <|> pure []
    tp <- typeSignature
    extends <- parseExtends
    requires <- parseRequires
    -- making structure arguments Implicit for easier manipulation during
    -- pipeline stages
    let args' = map (\x@(Var nm tp val) -> Var nm (Implicit tp) val) args
    let str = Lambda {
       lamName    = name
     , params = args'
     , body       = UNDEFINED
     , lamType    = tp
    }
    reservedOp "="
    exs <- braces (sepBy (try pSumType <|> try pLaw <|> try pFuncOrDecl <|> pAction ) (reservedOp ",") )
    let si = StructInfo {
        structKind      = kind,
        structExtends   = extends,
        structRequires  = requires,
        structMandatory = []
    }
    return $ Structure (str{body = Tuple exs}) si

pStructure :: Parser Expr
pStructure = pStructureWith "structure" SGeneral

pAlgebra :: Parser Expr
pAlgebra = pStructureWith "algebra" SAlgebra

pTrait :: Parser Expr
pTrait = pStructureWith "trait" SAlgebra

pMorphism :: Parser Expr
pMorphism = pStructureWith "morphism" SMorphism

pBridge :: Parser Expr
pBridge = pStructureWith "bridge" SMorphism

-- Parse extends clause: extends Parent1(a), Parent2(a, b)
parseExtends :: Parser [Expr]
parseExtends = try (do
    reserved "extends"
    sepBy1 pStructRef (reservedOp ",")
    ) <|> pure []

-- Parse requires clause: requires Struct1(a), Struct2(a, b)
parseRequires :: Parser [Expr]
parseRequires = try (do
    reserved "requires"
    sepBy1 pStructRef (reservedOp ",")
    ) <|> pure []

-- Parse a structure reference like Eq(a) or Monoid(a)
pStructRef :: Parser Expr
pStructRef = do
    name <- identifier
    args <- try (parens (sepBy1 concreteType (reservedOp ","))) <|> pure []
    return $ if null args then Id name else App (Id name) args

-- RECORDS ----------------------------------------------------------
-- record Point = { x:Nat, y:Nat };
-- record Pair(a:Type, b:Type) = { fst:a, snd:b };
-- Desugars to single-constructor sum type
pRecord :: Parser Expr
pRecord = do
    reserved "record"
    recName <- uIdentifier
    tparams <- try pVars <|> pure []
    reservedOp "="
    fields <- braces (sepBy1 pRecordField (reservedOp ","))
    -- Desugar: record Foo = { x:A, y:B }  =>  type Foo = { Foo(x:A, y:B) }
    -- Spread fields (..Name) are stored as Var "..Name" UNDEFINED UNDEFINED
    -- and resolved in Pass 1 when the environment is available
    let vars = map (\(nm, tp) -> Var nm tp UNDEFINED) fields
    let consLam = Lambda recName vars UNDEFINED (Id recName)
    let lam = Lambda {
       lamName = recName
     , params = tparams
     , body = Constructors [consLam]
     , lamType = Type
    }
    return $ SumType lam

pRecordField :: Parser (Name, Expr)
pRecordField =
    try pSpreadField <|> pNormalField

pSpreadField :: Parser (Name, Expr)
pSpreadField = do
    reservedOp ".."
    nm <- uIdentifier
    return (".." ++ nm, UNDEFINED)  -- marker: name starts with ".."

pNormalField :: Parser (Name, Expr)
pNormalField = do
    nm <- identifier
    reservedOp ":"
    tp <- concreteType
    return (nm, tp)

-- INSTANCES ---------------------------------------------------------
pInstance :: Parser Expr
pInstance = do
    reserved "instance"
    name <- identifier
    targs <- parens (sepBy1 concreteType (reservedOp ","))
    reqs <- parseRequires
    reservedOp "="
    exs <- braces (sepBy (try pFunc) (reservedOp ","))
    return $ Instance name targs exs reqs

-- FUNCTIONS ---------------------------------------------------------
pFuncHeader :: Parser Lambda
pFuncHeader = do
    reserved "function"
    name <- try identifier <|> (parens operator)
    args <- try pVars <|> pure []
    tp <- typeSignature
    return Lambda {
       lamName    = name
     , params = args
     , body       = UNDEFINED
     , lamType    = tp 
    }

pFuncL :: Parser Lambda
pFuncL = do
    lam <- pFuncHeader
    reservedOp "="
    ex <- try (PatternMatches <$> braces (sepBy (pPatternMatch lam) (reservedOp ",") ) ) <|> pExpr
    return $ lam { body = ex }

-- Function declaration: with body (= expr) or without (abstract, for structures)
pFuncDecl :: Parser Lambda
pFuncDecl = do
    lam <- pFuncHeader
    mbody <- optionMaybe (reservedOp "=" >> (try (PatternMatches <$> braces (sepBy (pPatternMatch lam) (reservedOp ",") ) ) <|> pExpr))
    case mbody of
        Just ex -> return $ lam { body = ex }
        Nothing -> return lam  -- abstract: body stays UNDEFINED

pFunc :: Parser Expr
pFunc = Function <$> pFuncL

-- pFuncOrDecl: allows bodyless function declarations (for use inside structures)
pFuncOrDecl :: Parser Expr
pFuncOrDecl = Function <$> pFuncDecl

-- LAW declarations (inside structures) ---------------------------------
-- law reflexivity(x:a) = (x == x) === True
pLaw :: Parser Expr
pLaw = do
    reserved "law"
    name <- identifier
    args <- try pVars <|> pure []
    tp <- typeSignature
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = UNDEFINED
     , lamType    = tp
    }
    reservedOp "="
    lawBody <- pLawExpr
    return $ Law lam lawBody

-- Parse law body expressions with === and ==> operators
-- ==> binds loosest (right-associative), === binds tighter
pLawExpr :: Parser Expr
pLawExpr = do
    parts <- sepBy1 pLawAtom (reservedOp "==>")
    return $ foldr1 Implies parts

pLawAtom :: Parser Expr
pLawAtom = try pParenLawAtom <|> pPlainLawAtom

-- Parenthesized law atom: (expr === expr) — only matches if === is inside the parens
pParenLawAtom :: Parser Expr
pParenLawAtom = parens $ do
    lhs <- pExpr
    reservedOp "==="
    rhs <- pExpr
    return (PropEq lhs rhs)

pPlainLawAtom :: Parser Expr
pPlainLawAtom = do
    lhs <- pExpr
    mRhs <- optionMaybe (reservedOp "===" >> pExpr)
    case mRhs of
        Nothing  -> return lhs
        Just rhs -> return (PropEq lhs rhs)

-- eventually to be expanded to literals etc
-- but in general only Ids and constructor applications Apps should be allowed
-- in the left part of pattern matches
pInsideLeftPattern :: Parser Expr
pInsideLeftPattern = try pApp <|> symbolId <?> "constructor application or a value"

-- pattern match Expr -> Expr
-- Lambda is given for external context to do some basic error checking,
-- e.g. # of arguments correspondence etc
pPatternMatch :: Lambda -> Parser Expr
pPatternMatch lam = do
    pos <- getPosition
    ex1 <- braces (sepBy1 pInsideLeftPattern (reservedOp ","))
    if (length (params lam) /= (length ex1) ) 
    then parserFail $
            "\nWrong number of arguments in a pattern match in a function:\n\n"
            ++ (ppr lam) ++ "\n\nThe function expects "
            ++ show (length (params lam)) ++ " arguments "
            ++ "but was given " ++ show (length ex1) 
            ++ " in the pattern match shown above."
    else 
      do 
        reservedOp "->"
        ex2 <- pExpr
        -- ok so now we have function variables in (params lam) and 
        -- respective pattern matches, we need to convert the expression
        -- to CaseOf [Var] Expr
        -- Since Parser has no context, we make a straightforward conversion:
        -- f(x:t1,y:t2) = { {a,b} -> expr } gets converted into
        -- CaseOf [Var "x" t1 a, Var "y" t2 b] expr
        let bnd = zipWith (\arg pat -> arg {val = pat} ) (params lam) ex1
        return $ CaseOf bnd ex2 (SourceInfo (sourceLine pos) (sourceColumn pos) "") 

    
    
    
-- ACTIONS =====================================================
pBinding :: Parser Expr
pBinding = do
    name <- identifier
    tp <- typeSignature
    reservedOp "="
    ex <- pExpr
    return $ Binding $ Var name tp ex

pAction :: Parser Expr    
pAction = do
    reserved "action"
    name <- identifier
    args <- try pVars <|> pure []
    tp <- typeSignature
    reservedOp "="
    ex <- braces (sepBy1 (try pDef <|> pApp ) (reservedOp ",") ) 
    return $ Action $ Lambda {
       lamName    = name
     , params = args
     , body       = Statements ex
     , lamType    = if (tp /= UNDEFINED) then tp else Id "Action"
    }
    


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

-- Full type expression parser: handles application, arrows, parens, universes
-- Examples: Nat, Vec(a, Succ(n)), a -> b, (a -> b) -> c, Type
concreteType :: Parser Expr
concreteType = pTypeArrow

-- Arrow types: a -> b -> c (right-associative)
pTypeArrow :: Parser Expr
pTypeArrow = do
    lhs <- pTypeApp
    rest <- optionMaybe (reservedOp "->" >> pTypeArrow)
    case rest of
        Nothing  -> return lhs
        Just rhs -> return $ ArrowType lhs rhs

-- Type application: Vec(a, n), Maybe(a), or bare name/universe
pTypeApp :: Parser Expr
pTypeApp = try (do
    nm <- identifier
    case nm of
      "Type"  -> return (U 0)
      "Type0" -> return (U 0)
      "Type1" -> return (U 1)
      "Type2" -> return (U 2)
      "Type3" -> return (U 3)
      _ -> do
        margs <- optionMaybe (parens (sepBy1 concreteType (reservedOp ",")))
        case margs of
            Nothing   -> return $ Id nm
            Just args -> return $ App (Id nm) args
    )
    <|> try (Lit . LTuple <$> braces (sepBy1 concreteType (reservedOp ",")))  -- tuple types {a, b}
    <|> parens concreteType


int :: Parser Literal
int = LInt . fromInteger <$> integer

floating :: Parser Literal
floating = LFloat <$> float

stringVal :: Parser Literal
stringVal = LString <$> stringLit


pContainers :: Parser Expr
pContainers = 
        try (brackets (commaSep pExpr) >>= return . Lit . LList)
        <|> try (braces (commaSep pExpr) >>= return . Lit . LTuple)
        <|> (angles (commaSep pExpr) >>= return . Lit . LVec)

{- 
=====================================================================================
-}
-- Building expression parser - for RIGHT HAND SIDE ONLY!!!
pExpr :: Parser Expr
pExpr = try (Lit . LVec <$> angles (commaSep pFactor))  -- vector literals <1, 2, 3> (use pFactor to avoid > consumed as operator)
    <|> Ex.buildExpressionParser (binops ++ [[unop],[binop]] ++ [[binary "==" Ex.AssocLeft]] ) pFactor

pIfThenElse :: Parser Expr
pIfThenElse = do
    reserved "if"
    cond <- pExpr
    reserved "then"
    e1 <- pExpr
    reserved "else"
    e2 <- pExpr
    return $ IfThenElse cond e1 e2

pLetIn :: Parser Expr
pLetIn = do
    reserved "let"
    bindings <- sepBy1 pLetBinding (reservedOp ",")
    reserved "in"
    bdy <- pExpr
    return $ LetIn bindings bdy

pLetBinding :: Parser (Var, Expr)
pLetBinding = do
    nm <- identifier
    tp <- typeSignature
    reservedOp "="
    val <- pExpr
    return (Var nm tp UNDEFINED, val)

pFactor :: Parser Expr
pFactor = try pIfThenElse
    <|> try pLetIn
    <|> try pApp
    <|> try (parens pExpr)
    <|> try (Id <$> parens operator)   -- (op) as value: (+), (!=), etc.
    <|> try symbolId
    <|> try (Lit <$> floating)
    <|> try (Lit <$> int)
    <|> try (Lit <$> stringVal)
    <|> pContainers
    <?> "if/then/else, let/in, container, literal, symbol id or parenthesized expression"

symbolId :: Parser Expr
symbolId = do 
    s <- identifier
    return $ Id s

-- Clear function application
pApp :: Parser Expr
pApp = do
    func <- try (parens pExpr) <|> try (Id <$> parens operator) <|> (Id <$> identifier)
    args <- parens (sepBy pExpr (reservedOp ",") )
    return $ App func args

-- Building top level parsers
pDef :: Parser Expr
pDef =  try pSumType
        <|> try pRecord
        <|> try pAlgebra
        <|> try pTrait
        <|> try pMorphism
        <|> try pBridge
        <|> try pStructure
        <|> try pInstance
        <|> try pFunc
        <|> try pAction
        <|> pBinding
        -- <?> "lambda, binding, pattern match or expression"
    
          

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
        ]

    
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
    line = fromIntegral $ sourceLine pos - 1
    col  = fromIntegral $ sourceColumn pos - 1