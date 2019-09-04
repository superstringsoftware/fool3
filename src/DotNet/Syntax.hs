{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

module DotNet.Syntax where

import TermColors
import Data.Char (isUpper)

import Data.Functor.Identity
import Control.Monad.IO.Class

type Name = String

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
    = TVar Name -- TVar -- a:* - careful, we CANNOT use 'Id' constructor from Var here
    | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the constructor
    | TApp Type Type -- Constructor application - Maybe Int, List a etc
    | TArr Type Type -- Function sig - Maybe a -> String etc
    | TForall [Pred] [TVar] Type
    | ToDerive -- added it to handle initial parsing
    | TClass Name -- for initial typeclass parsing, might change this
    | InsType Expr -- this is probably a workaround - when we are beta-reducing Type lambdas for variables like \a. x:a
    -- "a" needs to be able to become any kind of expression (since we are applying lambdas to expressions).
    -- Type checking etc will fix this.
    deriving (Show, Eq, Ord)

extractTypeVars :: Type -> [Name] -> [Name]
extractTypeVars (TVar nm) acc = (nm:acc)
extractTypeVars (TApp t1 t2) acc = extractTypeVars t2 acc
extractTypeVars _ acc = acc

-- traversing Type AST with a function and gather results in a list
flattenType :: (Type -> a) -> Type -> [a] -> [a]
flattenType f (TApp t1 t2) acc = concat [acc, (flattenType f t1 acc), (flattenType f t2 acc)]
flattenType f (TArr t1 t2) acc = concat [acc, (flattenType f t1 acc), (flattenType f t2 acc)]
flattenType f t acc = concat [acc, [f t]]

-- traversing Type AST with a modifying function
traverseType :: (Type -> Type) -> Type -> Type
traverseType f (TApp t1 t2) = f (TApp (traverseType f t1) (traverseType f t2))
traverseType f (TArr t1 t2) = f (TArr (traverseType f t1) (traverseType f t2))
-- traverseType 
traverseType f t = f t


-- Since we are doing dependent types, we need to be able to do both the standard:
-- Maybe :: * -> * as well as something for Vector a:* n:Int which would look like:
-- Vector :: * -> Int -> *
-- We are using KTerm constructor to describe it - just need to be careful, since valid value there is only
-- *Concrete* type!
data Kind
    = KStar
    | KArr Kind Kind
    | KPrim
    | KVar Name
    | KTerm Type
    | KoDerive
    deriving (Show, Eq, Ord)
  
data TyCon
    = AlgTyCon { tyId :: Name }
    | PrimTyCon { tyId :: Name }
    deriving (Show, Eq, Ord)

data Pred
    = IsIn [Name] Type | Exists Type [Name] -- class Eq a => Ord a, Eq a goes to IsIn ['a'] TClass 'Eq'
    deriving (Show, Eq, Ord)

data Var = Id Name Type | TyVar Name Kind
    deriving (Show, Eq, Ord)
  
type TVar = Var -- type synonim to handle Forall predicates


data Literal = LInt !Int | LFloat !Double | LChar !Char |
               LString !String | LBool !Bool | LList [Expr] | LVec [Expr]
               deriving (Eq, Ord, Show)

-- we need to handle dependent types, this is first attempt by storing specific value var
{-
data TupleField 
    = TypedField Type -- regular field with a type
    | ConstField 
-}
-- product type constructor
-- data Cons = Anon Name [Type] | CRecord Name [(Name, Type)] deriving (Eq, Ord, Show)

-- Surfance language AST type to handle both lazy and strict hopefully a bit more efficiently
-- So, no currying
data Expr 
    = Lit Literal
    | VarId Name
    | Type Name [Var] [Expr] -- sum type built from product constructors, which are Lambdas themselves!
    | BinaryOp Name Expr Expr
    | UnaryOp  Name Expr
    | Tuple Name [Expr] -- polymorphic tuple. 
    | Record Name [(Name, Expr)] -- polymorphic record
    | NewRecord Name [Var] -- polymorphic record
    | App Expr Expr
    | Case Expr [(Expr, Expr)] -- case: which expr we are inspecting, alternatives
    | Lam Name [Var] Expr Type -- typed function (or any typed expression for that matter)
    -- class 
    | Typeclass     Name [Pred] [Var] [Expr] -- Expr here can only be Lam as it's a list of functions basically, where interface is just an empty expression
    | Typeinstance  Name [Pred] Type  [Expr] -- for parsing typelcass instances - Name is typeclass name, Type is the type and [Expr] are all Lambdas
    | EMPTY
    deriving (Eq, Ord, Show)

-- function that is a memeber of typeclass:
-- maps concrete types for which a typeclass is implemented to Lambda that implements it
-- using a list of types to support multiparameter typeclasses
-- E.g., fmap for List would be ([TCon "List"], Lam 'fmap' ...)
type ClassFunPair = ([Type], Expr) 

-- Holder for all interfaces and implementations for one typeclass    
data Typeclass = MkTypeclass {
    className       :: String -- name of the typeclass
  , vars            :: [TVar] -- type variables
  , superclasses    :: [Pred] -- immediate superclasses (Exists Type [Name] - e.g., Ord a where ∃ Eq a)
  , funDictionary   :: [(Name, [ClassFunPair])] -- mapping from the function name to it's implementations for different
  -- instances. So, when we do instance Class Type - we add functions here to the dictionnary.
  -- Need to implement a good lookup function.
  -- interface for the function is in [(Name, ([], Lam ...))]
} deriving (Eq, Ord, Show)

{-
data Expr
  = Lit Literal
  | VarId Name -- for bound variables and functions???
  | Lam   Name [Var] Expr -- named function. To be non-partially applied, needs all arguments (size of Var list)
  | Tuple Name [Expr] -- polymorphic tuple: main thing for values generated by ADTs
  | App Expr Expr -- e1 can only be Lam or VarId, e2 can only be Tuple - so there should be a better way to handle this
  | If  Expr Expr Expr -- will get rid of this once case patterns are in, since we can model it with a function
  | Let Name Expr Expr -- ok, need to figure out how GHC does it - here we are binding first Expr to symbol Name in 2nd Expr
  -- Case, for instance: case x of Nothing -> Nothing; (Just x) -> Just x*2 translates to something like:
  -- Case "x" [ (Nothing, Nothing), (cons x == Just, Just (x*2)) ]
  | Case [(Expr, Expr)]
  -- whatever goes below is for interpreter / initial parsing
  | Index !Integer -- used for Tuple element access and maybe some other stuff?
  | RecAccess Expr Expr -- first try, first expr is either VarId or Index - named field - second expr is to what we apply
  -- so, f.a would be RecAccess (VarId "a") (VarId "f")
  -- f.a.b: RecAccess (varId "b") (RecAccess (VarId "a") (VarId "f"))
  | BinaryOp Name Expr Expr
  | UnaryOp  Name Expr
  | FAIL String -- some invalid expression
  deriving (Eq, Ord, Show)
-}

-- mkTuple x = Tuple "" [x]
-- mk2Tuple x y = Tuple "" [x, y]

arity :: Expr -> Int
arity (Type _ v _) = length v
arity (Lam _ v _ _) = length v


varName (Id n _) = n
varName (TyVar n _) = n
varType (Id _ t) = t


-- Expr traversals with modifying function f::Expr->Expr
-- I think THIS IS WRONG!!! -- we are not traversing or not modifying...    
traverseExpr :: (Expr -> Expr) -> Expr -> Expr
traverseExpr f (Lam nm v e t) = Lam nm v (f e) t
traverseExpr f (Tuple nm ex) = Tuple nm (map f ex)
traverseExpr f (App e1 e2) = App (f e1) (f e2)
traverseExpr f (BinaryOp nm e1 e2) = BinaryOp nm (f e1) (f e2)
traverseExpr f (UnaryOp nm e) = UnaryOp nm (f e)
traverseExpr f (Type nm v ex) = Type nm v (map f ex)
traverseExpr f (Record nm nex) = Record nm (map (\(n, e) -> (n, f e)) nex)  -- nex = [(Name, Expr)]
traverseExpr f (Case e nex) = Case (f e) (map (\(e1, e2) -> (f e1, f e2)) nex) -- nex = [(Expr, Expr)]
traverseExpr f (Typeclass    nm p v ex) = Typeclass     nm p v (map f ex)
traverseExpr f (Typeinstance nm p t ex) = Typeinstance  nm p t (map f ex)
traverseExpr f (NewRecord nm vrs) = NewRecord nm vrs
traverseExpr f e = f e

-- Monadic traversals - in case we need to lookup/ modify state along the way etc
descendM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
descendM f e = case e of
    App  a b        -> App <$> descendM f a <*> descendM f b
    VarId  a        -> VarId <$> pure a
    Lam nm v e t    -> Lam <$> pure nm <*> pure v <*> descendM f e <*> pure t
    Lit  n          -> Lit <$> pure n
    BinaryOp n a b  -> BinaryOp <$> pure n <*> descendM f a <*> descendM f b
    UnaryOp n a     -> UnaryOp <$> pure n <*> descendM f a
    Tuple nm ex     -> Tuple <$> pure nm <*> mapM (descendM f) ex
    Case e exs      -> Case <$> descendM f e <*> mapM (fn f) exs
                                  where fn f1 (e1, e2) = do
                                                            e1' <- descendM f1 e1
                                                            e2' <- descendM f1 e2
                                                            return (e1', e2')
    Record n nex    -> Record <$> pure n <*> mapM (fn f) nex 
                                    where fn f1 (n, e) = do
                                                            e' <- descendM f1 e
                                                            return (n, e')
    Typeclass    n p v ex -> Typeclass      <$> pure n <*> pure p <*> pure v <*> mapM (descendM f) ex
    Typeinstance n p t ex -> Typeinstance   <$> pure n <*> pure p <*> pure t <*> mapM (descendM f) ex
    Type n v ex     -> Type <$> pure n <*> pure v <*> mapM (descendM f) ex
    r@(NewRecord nm vrs) -> pure r
    EMPTY           -> pure EMPTY                                                            
    >>= f -- to apply actual change function on the way out of the traversal

-- pure traversal in the identity monad
descend :: (Expr -> Expr) -> Expr -> Expr
descend f ex = runIdentity (descendM (return . f) ex)

-- function that performs some initial optimizations - e.g., getting rid of 
-- BinaryOps by turning them into App Tuple, turning function App's from the Tree into App Tuple etc
desugar :: Expr -> Expr
desugar = descend f where
    f (BinaryOp name e1 e2) = App (VarId $ "(" ++ name ++")") (Tuple "" [e1,e2])
    f (UnaryOp nm e) = App (VarId $ "("++nm++")") (Tuple "" [e])
    f e = e

-- ------------------------------------------------------------------------------------------------



class PrettyPrint a where
    prettyPrint :: a -> String
  
class PrettyPrintTyped a where
    prettyPrintTyped :: a -> String

instance PrettyPrint Expr where
    prettyPrint (VarId "") = as [lgray,bold] "_"
    prettyPrint (VarId n) = n
    -- prettyPrint (Lam nm [] (Tuple tnm [])) = clrLam nm
    prettyPrint (Lam nm vars e t) = clrLam nm ++ ":" ++ prettyPrint t ++ " = " ++ as [bold, dgray] "λ" 
        ++ foldr fn "" vars ++ ". " ++ prettyPrint e -- clrLam nm ++ " = " ++
        where fn el acc = prettyPrint el ++ " " ++ acc
    prettyPrint (Tuple nm tpl) = showBracketedList " {" "}" tpl -- clrLam nm ++
    prettyPrint (NewRecord nm vrs) = showBracketedList " {" "}" vrs
    prettyPrint (Lit l) = prettyPrint l
    prettyPrint (App e1 e2) = embrace $ prettyPrint e1 ++ " " ++ prettyPrint e2
    prettyPrint (BinaryOp n e1 e2) = prettyPrint e1 ++ " " ++ n ++ " " ++ prettyPrint e2 -- "("++n++") " ++ 
    prettyPrint (Case e exs) = prettyPrint e ++ " ? " ++  (foldr fn "" exs)
        where fn (e1,e2) acc = "\n\t| " ++ prettyPrint e1 ++ " -> " ++ prettyPrint e2 ++ acc
    prettyPrint (Type nm vrs exs) = clrLam nm ++ " = " ++ as [bold, dgray] "Λ" 
                ++ (foldr (\x y -> prettyPrint x ++ y) "" vrs) 
                ++ (foldr (\x y -> "\n\t" ++ prettyPrint x ++ y) "" exs)
    prettyPrint (Typeclass nm pred vrs fns) = clrLam nm ++ prettyPrint pred ++ " = " ++ as [bold, dgray] "Λ" 
                ++ (foldr (\x y -> prettyPrint x ++ y) "" vrs) 
                ++ (foldr (\x y -> "\n\t" ++ prettyPrint x ++ y) "" fns)
    prettyPrint (Typeinstance nm pred tp fns) = foldr (\x y -> x ++ y) "" (map (fn (nm ++ "." ++ prettyPrint tp)) fns)
        where fn n (Lam n1 x y z) = prettyPrint (Lam (n ++ "." ++ clrLam n1) x y z)
                                        ++ prettyPrint pred ++ "\n" 
    prettyPrint EMPTY = as [magenta, bold] "undefined"
    prettyPrint e = show e
    
clrLam "" = ""
clrLam s = if isUpper (head s) then as [bold, red] s else as [bold, green] s


embrace s = as [bold, blue] "(" ++ s ++ as [bold, blue] ")"
-- additional nicer formatting for output for top level expressions
-- prettyPrintTopLevel (App e1 e2) = embrace (prettyPrint e1) ++ embrace (prettyPrint e2)
prettyPrintTopLevel e = prettyPrint e


showBracketedList l r [] = l ++ r
showBracketedList l r [x]    = l ++ prettyPrint x ++ r
showBracketedList l r (x:xs) = l ++ prettyPrint x ++ foldr fn "" xs ++ r
    where fn el acc = ", " ++ prettyPrint el ++ acc

instance PrettyPrint Pred where
    prettyPrint (IsIn names tp) = prettyPrint tp ++ " " ++ foldr fn "" names 
        where fn el acc = el ++ " " ++ acc
    prettyPrint (Exists tp names) = prettyPrint tp ++ " " ++ foldr fn "" names 
        where fn el acc = el ++ " " ++ acc

instance PrettyPrint [Pred] where
    prettyPrint [] = " "
    prettyPrint (x:[]) = " <: " ++ prettyPrint x
    prettyPrint xs = " <: " ++ showBracketedList "(" ")" xs

instance PrettyPrint Literal where
    prettyPrint (LInt x) = as [magenta] $ show x
    prettyPrint (LFloat x) = as [magenta] $ show x
    prettyPrint (LBool x) = as [magenta] $ show x
    prettyPrint (LString s) = as [green] $ show s
    prettyPrint (LList tuple) = showBracketedList "[" "]" tuple
    prettyPrint (LVec  tuple)  = showBracketedList "<" ">" tuple
    prettyPrint e = show e

instance PrettyPrint Type where
    prettyPrint (TVar nm) = nm
    prettyPrint (TCon nm) = as [yellow, bold] nm
    prettyPrint ToDerive  = "" --as [dgray, bold] "?"
    prettyPrint (TApp t1 t2) = "(" ++ prettyPrint t1 ++ " " ++ prettyPrint t2 ++")"
    prettyPrint (InsType ex) = prettyPrint ex
    prettyPrint (TArr t1 t2) = prettyPrint t1 ++ "->" ++ prettyPrint t2
    prettyPrint (TClass n) = clrLam n
    prettyPrint e = show e
  
  {-
    | TArr Type Type
    | TForall [Pred] [TVar] Type
  -}
-- Λ
instance PrettyPrint Var where
    prettyPrint (Id nm tp)   = nm ++ ":" ++ prettyPrint tp
    prettyPrint (TyVar nm k) = nm ++ ":" ++ prettyPrint k

instance PrettyPrint Kind where
    prettyPrint KStar = as [lgray, bold] "*"
    prettyPrint KoDerive = "" --as [dgray, bold] "?"
    prettyPrint k = show k