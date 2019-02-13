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

module DotNet.Parser where

    import Text.Parsec
    -- import Text.Parsec.String (parseFromFile)
    import Control.Applicative ((<$>), liftA2)
    import Control.Monad (foldM)
    import Control.Monad.IO.Class (liftIO)
    
    import qualified Text.Parsec.Expr as Ex
    import qualified Text.Parsec.Token as Tok
    import Text.Parsec.Char (string)
    
    import qualified Data.Vector.Unboxed as U
    
    import Lexer
    import DotNet.Syntax
    
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
             -- try (reservedOp ":" *> typeVariable) <|>
             pure ToDerive
      return $ Id name typ
    
    
    -- type variable - Kind is either '*' or a concrete type, needs to be adjusted at later stages
    -- need to add kind parsing for complex applications!
    typeVariable :: Parser Var
    typeVariable = do
      name <- lIdentifier
      typ <- try (KTerm <$> (reservedOp ":" *> concreteType)) <|>
             pure KStar -- default to *
      return $ TyVar name typ
    
    -- this, parametricType and dataDef parses haskell based data hello = Text a b | Nil type of data definitions
    constructor :: Parser Cons
    constructor = do
      name <- uIdentifier
      vars <- sepBy  (try (TCon <$> (reservedOp ":" *> uIdentifier)) <|> -- concrete type
                     try (TVar <$> (reservedOp ":" *> lIdentifier)) <|> -- type var
                     (reservedOp ":" *> parens typeAp) -- complex type, like List a
                     <?> "regular constructor failed") (symbol "*")
      return $ Anon name vars
    
    
    constructors = {- try recordConstructor <|> -} constructor
    
    -- simple ADT
    typeDef :: Parser Expr
    typeDef = do
      reserved "type"
      name <- uIdentifier
      vars <- many typeVariable
      modifyState (addParserLog $ "Parsing typeDef for" ++ name)
      reservedOp "="
      fields <- sepBy1 constructors (char '+')
      return $ Type name vars fields
    
    
    
    symbolId :: Parser Expr
    symbolId = VarId <$> identifier
    
    
    {-
    unarydef :: Parser Expr
    unarydef = do
      o <- parens op
      arg <- variable
      reservedOp "="
      body <- expr
      return $ Lam ("("++o++")") [arg] body
    
    -- precedence is not stored anywhere now and is not handled at all
    binarydef :: Parser Expr
    binarydef = do
      o <- parens op
      prec <- int <?> "integer: precedence value for the operator definition"
      arg1 <- variable
      arg2 <- variable
      reservedOp "="
      body <- expr
      return $ Lam ("("++o++")") [arg1, arg2] body
    -}
    {-
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
      return $ Lam ("("++o++")") [arg1, arg2] body
    
    unarydef :: Parser Expr
    unarydef = do
      reserved "def"
      reserved "unary"
      o <- op
      arg <- variable
      reservedOp "="
      body <- expr
      return $ Lam ("("++o++")") [arg] body
    
    
    fieldAccess :: Parser Expr
    fieldAccess = do
      fields <- sepBy1 intOrStr (reservedOp ".")
      return $ foldl1 RecAccess fields
    
    intOrStr :: Parser Expr
    intOrStr = try (Index <$> integer) <|> (VarId <$> identifier)
    
    -}

    argument :: Parser Expr
    argument = {-try lambda
          <|>-} try containers
          <|> try (parens expr)
          -- <|> try (parens ifthen)
          <|> try (Lit <$> floating)
          <|> try (Lit <$> int)
          <|> try (Lit <$> stringVal)
          -- <|> try fieldAccess
          <|> symbolId
    
    
    arguments :: Parser Expr
    arguments = do
      args <- many1 argument
      return $ foldr1 App args
    
    
    factor :: Parser Expr
    -- factor = try caseExpression <|> try call <|> try ifthen <|> argument -- arguments -- try letins <|>
    factor = argument -- arguments -- try letins <|>
    

    defn :: Parser Expr
    defn =  try typeDef
        -- <|> try caseFunction
        -- <|> try function
        -- <|> try unarydef
        -- <|> try binarydef
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
             let res = (runP p st fname input)
             --st <- (lift getState)
             --print st
             return res
    
    -- adding new stuff
    
    -- vector: <1,2,3.4>
    -- list: [1,2,3,4]
    -- tuple: {1,int, "hello"}
    

    containers :: Parser Expr
    containers = -- try  (FlTuple TTVector <$> angles   (commaSep expr)) <|>
            try  $ do
              args <- brackets (commaSep expr)
              return $ Lit $ LList args
            <|> try (braces (commaSep expr) >>= \args -> return (Tuple "" args))
            <|> (angles (commaSep factor)  >>= \args -> return (Lit $ LVec args))

    
    
   
    