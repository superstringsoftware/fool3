{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module SurfaceLanguage.MF.Syntax where

type Name = String
type ConsTag = (Name, Int)

-- Any variable, can be a type variable or "real" variable, they all differ in their type only
data Var = Var {
    varName  :: Name
  , varType  :: Type
  , varValue :: Expr
 } deriving (Show, Eq)

-- data Tuple = TypedTuple [Type] | NamedTuple [(Name, Type)] | ExprTuple [Expr]

type TypedTuple = [Type]
type NamedTuple = [(Name, Type)]
type FullRecord = [Var]
data Record = Record {
          rec     :: FullRecord
        , recType :: Type
        , consTag :: ConsTag 
      } deriving (Show, Eq)

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] deriving (Eq, Show)

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Var -- TVar -- a:U etc, type with constraints 
  deriving (Show, Eq)

data Expr
  = EMPTY_EXPR
  | VarId Name
  | Lit Literal
  | App {
      func :: Expr,
      implParams :: [Expr],
      explParams :: [Expr]
  } -- application of expression "func" to implicit [..] and explicit (...) parameters
  | Lam Lambda
  | Tuple [Expr]
  | TypedExpr Expr Type -- as any expression can be annotated
  | Binding Var Lambda
  | VarDefinition Var
  
  deriving (Show, Eq)

data Lambda = Lambda {
    implParams :: NamedTuple, -- [a1:to1, ...] - optional parameters, usually types
    explParams :: FullRecord -- (x1:t1 = v1, ..., xn:tn = vn) - supporting default values
  , body       :: Expr -- whatever our lambda is bound to
  , lamType    :: Type -- return type of the function
} deriving (Show, Eq)

{-
Bool : Type = (True : Bool, False : Bool)
-- For such simple cases we can omit the type signatures for the constructors and simply write
Bool : Type = (True, False)

Binding 
    (Var "Bool" "Type" EMPTY)
    Lambda {
        implParams = []
        explParams = []
        body = 
    }

-- Type functions:
Maybe (a:Type) : Type = (Just(:a), Nothing)
List  (a:Type)  : Type = ( ([]), (::) (x:a, xs:List(a)) )

-}