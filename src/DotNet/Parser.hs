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
      tcon <- try (TCon <$> uIdentifier) <|> (TVar <$> lIdentifier)
      vars <- many $ try (TCon <$> uIdentifier) <|> try ( TVar <$> lIdentifier ) <|> parens typeAp
      if null vars then return tcon -- concrete type
      else return $ foldl TApp tcon vars -- type application
    
    -- :a->b->String etc
    tArr :: Parser Type
    tArr = do
      ts <- sepBy1 allTypes (reservedOp "->")
      return $ foldr1 TArr ts

    -- :*->*  etc
    kArr :: Parser Kind
    kArr = do
      ks <- sepBy1 allKinds (reservedOp "->")
      -- let ks' = map (\x -> KStar) ks
      return $ foldr1 KArr ks

    allKinds :: Parser Kind
    allKinds = (symbol "*") *> pure KStar

    -- concrete type only
    concreteType :: Parser Type
    concreteType = TCon <$> uIdentifier
    
    allTypes :: Parser Type
    allTypes = try typeAp <|> try concreteType <|> typeVar

    -- variable with type
    variable :: Parser Var
    variable = do
      name <- lIdentifier
      typ <- typeSignature
      return $ Id name typ

    typeSignature :: Parser Type
    typeSignature =
            try (reservedOp ":" *> parens tArr) <|>
            -- try (reservedOp ":" *> tArr) <|>
            try (reservedOp ":" *> parens typeAp) <|>
            try (reservedOp ":" *> concreteType) <|>
            try (reservedOp ":" *> typeVar) <|>
            pure ToDerive
      
  
    -- simply a name of type variable on the right of regular variable
    typeVar :: Parser Type
    typeVar = do
      name <- lIdentifier
      return $ TVar name
    
    -- type variable - Kind is either '*' or a concrete type, needs to be adjusted at later stages
    -- need to add kind parsing for complex applications!
    -- for analyzing type definitions
    typeVariable :: Parser Var
    typeVariable = do
      name <- lIdentifier
      typ <- try (KTerm <$> (reservedOp ":" *> concreteType)) 
             <|> try (reservedOp ":" *> parens kArr) 
             <|> try (reservedOp ":" *> kArr) 
             <|> pure KoDerive
             <?> "correct kind signature for " ++ name
      return $ TyVar name typ
    
    -- single constructor inside type definition
    -- returns a lambda
    constructor :: Type -> Parser Expr
    constructor tp = do
      name <- try uIdentifier <|> spaces *> parensOp <?> "either Constructor name or operator syntax"
      vars <- sepBy  (try (TCon <$> (reservedOp ":" *> uIdentifier)) <|> -- concrete type
                     try (TVar <$> (reservedOp ":" *> lIdentifier)) <|> -- type var
                     (reservedOp ":" *> parens typeAp) -- complex type, like List a
                     <?> "simple constructor expected") (symbol "*")
      let tupl = map (\_ -> VarId "") vars
      let vars' = map (\x -> Id "" x) vars
      return $ Lam name vars' (Tuple name tupl) tp
    
    parensOp :: Parser String
    parensOp = do 
      n <- parens op
      return $ "(" ++ n ++ ")"

    constructors = {- try recordConstructor <|> -} constructor
    
    -- simple ADT
    typeDef :: Parser Expr
    typeDef = do
      reserved "type"
      name <- uIdentifier
      vars <- many typeVariable
      modifyState (addParserLog $ "Parsing typeDef for" ++ name)
      reservedOp "="
      -- converting type name to actual type to put it as a type to functions
      let tp = foldl TApp (TCon name) (map (\(TyVar n _)-> TVar n) vars)
      fields <- sepBy1 (constructors tp) (char '+')
      return $ Type name vars fields
    
    symbolId :: Parser Expr
    symbolId = VarId <$> identifier
    
    -- parses both empty and non-empty functions (for typeclasses empty are needed)
    function :: Parser Expr
    function = do
      fn <- variable
      let (Id name typ) = fn
      args <- many variable   -- (parens $ many identifier) <|> (parens $ commaSep identifier)
      body <- try (reservedOp "=" *> expr) <|> pure EMPTY
      return $ Lam name args body typ

    -- typeclass. Kind signatures aren't working for some reason yet.
    typeClass :: Parser Expr
    typeClass = do
      reserved "class"
      name <- uIdentifier
      vars <- many typeVariable <?> "correct type variable signature"
      subtypes <- try (reservedOp "<:" *> (parens $ sepBy1 predicate (symbol ","))) 
                  <|> try (reservedOp "<:" *> (predicate >>= \x-> return [x])) <|> pure []
      reservedOp "="
      fs <- many1 (try function <|> binarydef)
      return $ Typeclass name subtypes vars fs

    -- subtyping predicates for typeclasses
    predicate = do
      name <- uIdentifier
      vnames <- many lIdentifier
      return $ IsIn vnames (TClass name)

    -- class instance is somewhat tricky - we parse it simply as a function
    -- with a compound name <ClassName>.<TypeName>.<functionName>
    -- however, when compiling - this function needs to be stored more efficiently
    -- so that we can retrieve it as needed based on the type parameter
    -- returns array of Lam's
    classInstance :: Parser Expr
    classInstance = do
      reserved "instance"
      name <- uIdentifier
      tp <- try (parens typeAp) <|> concreteType <?> ("correct type in instance " ++ name ++ " definition")
      reservedOp "="
      fs <- many1 (try function <|> binarydef)
      return $ Typeinstance name tp fs

    
    -- one case like | x == 0 -> 1
    oneCase :: Parser (Expr, Expr)
    oneCase = do
      left <- expr
      reservedOp "->"
      right <- expr
      return (left, right)

    caseExpression :: Parser Expr
    caseExpression = do
      inspect <- try (parens expr) <|> symbolId <?> "caseExpression failed in inspect"
      reservedOp "?"
      cases <- sepBy1 oneCase (reservedOp "|")
      return $ Case inspect cases

    
    unarydef :: Parser Expr
    unarydef = do
      o <- parens op
      arg <- variable
      reservedOp "="
      body <- expr
      return $ Lam ("("++o++")") [arg] body ToDerive
    
    -- precedence is not stored anywhere now and is not handled at all
    binarydef :: Parser Expr
    binarydef = do
      o <- parens op
      prec <- try int <|> pure (LInt 0) -- <?> "integer: precedence value for the operator definition"
      typ <- typeSignature
      arg1 <- variable
      arg2 <- variable
      body <- try (reservedOp "=" *> expr) <|> pure EMPTY
      return $ Lam ("("++o++")") [arg1, arg2] body typ
    
    {-
    
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
    
    -- so the way we are parsing now is that we can only apply a symbol to a tuple of expressions,
    -- moving away from trees to lists
    call :: Parser Expr
    call = do
      name <- argument
      args <- try (parens $ many1 argument) <|> many1 argument
      return $ App name (Tuple "" args)
    
    arguments :: Parser Expr
    arguments = do
      args <- many1 argument
      return $ foldr1 App args
    
    
    factor :: Parser Expr
    -- factor = try caseExpression <|> try call <|> try ifthen <|> argument -- arguments -- try letins <|>
    factor = try caseExpression <|> arguments -- arguments -- try letins <|>
    

    defn :: Parser Expr
    defn =  try typeDef
        <|> try typeClass
        <|> try classInstance
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

    
    
   
    