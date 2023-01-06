{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- Surface language syntax, Expr is generated directly from the Parser

module Core.Syntax
where

import Util.PrettyPrinting
-- import Core.Syntax

type Name = String
type ConsTag = String

-- Any variable, can be a type variable or "real" variable, they all differ in their type only
data Var = Var Name Type deriving (Show, Eq)
-- binding of Expr to a Variable Var
-- type Binding = (Var, Expr) 

varName v = fst v
varType v = snd v

-- Predicates can and will be extended
data Pred
  = Exists Type -- TApp (TCon name) (TVar name) - e.g., Num a --> TApp (TCon "Num") (TVar "a")
  | Unconstrained -- variables can be anything
  deriving (Show, Eq)

-- anything that can be on the right side of ':' in expressions - so a type or a kind or type function that may depend on values etc
data Type
  = TVar Var -- TVar -- a:U etc, type with constraints 
  | HighVar Name [Pred] -- for rank-n polymorphism, predicates for a specific type variable -- DO WE NEED IT?
  | TCon Name -- TyCon -- Maybe, String, Int etc - just a name of the type
  -- Constructor application - Maybe Int, List a etc; as it should be always saturated tuple is a nobrainer
  -- Ok, this now gets tricky as we can have types dependent on values, so we can apply it not just to the type but to the literals etc
  -- E.g., if we have Vector a:U n:Int and we have a call to some calculation within the type:
  -- Vector Int (4+x) -- means we need to be able to parse expressions inside type applications
  | TApp Type [Type] -- due to the above, we may want to store BOTH types and expressions in ONE type. The only exception would be Arrows and Universe hierarchy?
  | TExpr Expr -- or this would resolve the issue above??? think through.
  -- the above would probably work - we would record Vector Int (4+x) something like - TApp (TCon "Vector") [TCon "Int", TExpr (BinaryOp "+" 2 "x")]
  -- so, should be OK, yes!
  | TArr Type Type -- Function sig - Maybe a -> String etc
  | TFunSig Record Type -- alternative way to store function signatures - {x1:t1, ..., xn:tn} -> Type. 
  -- ^^^ Might be BETTER for our purposes, as we want better documentation etc, and here we are storing variable names as additional information.
  -- converting between TArr and TFunSig should be a no-brainer.
  -- | TForall [Pred] [TVar] Type
  | THasFields [Field] -- special "constraint" type, for situations such as "print r.name + " " + show r.age" - then deduced type will be HasFields (name:String, age:exists Show a)
  | ToDerive -- added it to handle initial parsing
  | SmallType -- synonim for Universe 0, since that's the most common type we'll use
  | Universe Int -- for types, following the Martin-Lof theory - "Universe 0" is "small types", so regular types etc.
  | TClass -- typeclass or type family
  deriving (Show, Eq)

-- helper function that constructs e.g. TApp (TCon "Maybe") [TVar (Var "a" ...)] from Maybe a
constructTypeFunction :: Name -> Record -> Type
constructTypeFunction name []   = TCon name
constructTypeFunction name args = TApp (TCon name) (map (TVar . field2var) args)

-- Record is the representation of any record or tuple (with names being absent) - a list of typed field names, nothing more
data Field = Field {
    fieldName  :: Name, -- name of the field
    fieldType  :: Type, -- type of the field
    fieldValue :: Expr  -- value of the field (can be EMPTY)
} deriving (Show, Eq)
type Record      = [Field]
type TypedRecord = (Record, Type) -- record is the list of Fields and a Type of the record itself
-- Since everything is either a lambda or a Record in our land, we need a separate type for Lambda as well:
data Lambda = Lambda {
    params :: Record -- {x1:t1 = v1, ..., xn:tn = vn} - supporting default values
  , body   :: Expr -- whatever our lambda is bound to
  , sig    :: Type -- full type signature
  , preds  :: [Pred] -- rank-1 predicates (related to the whole type signature)
} deriving (Show, Eq)
-- needed for environment manipulations
type NamedLambda = (Name, Lambda)

emptyLambda :: Lambda
emptyLambda = Lambda [] EMPTY ToDerive []

-- generating a fully qualified name for a top-level binding (now mostly for CLASSes):
fullyQualifiedName :: Expr -> Name
fullyQualifiedName (App (VarId name) args) = Prelude.foldl (\acc v -> acc ++ "." ++ (_argToName v)) name args

_argToName :: Expr -> Name
_argToName (VarId n) = n
_argToName ex = "[ERROR] Called _argToName with argument being: " ++ show ex  

-- Record equality functions - FOR TYPE CHECKING!!!
-- checks if only the TYPE is equal 
fieldTypeEqual :: Field -> Field -> Bool
fieldTypeEqual f1 f2 = (fieldType f1) == (fieldType f2)

recTypeEqual :: Record -> Record -> Bool
recTypeEqual r1 r2 = (length r1 == length r2) && and (zipWith fieldTypeEqual r1 r2)

-- checks if both TYPE and NAME(s) are equal
fieldNameTypeEqual :: Field -> Field -> Bool
fieldNameTypeEqual f1 f2 = (fieldType f1 == fieldType f2) && ( fieldName f1 == fieldName f2)

recNameTypeEqual :: Record -> Record -> Bool
recNameTypeEqual r1 r2 = (length r1 == length r2) && and (zipWith fieldNameTypeEqual r1 r2)

arity :: Lambda -> Int
arity lam = length (params lam)

var2field :: Var -> Field
var2field (Var n t) = Field n t EMPTY

field2var :: Field -> Var
field2var (Field n t _) = Var n t

vars2record :: [Var] -> Record
vars2record vs = map var2field vs

isTConOrTApp (TCon _)   = True
isTConOrTApp (TApp _ _) = True
isTConOrTApp _          = False

binding2field (Binding (Var nm tp) (Lambda [] EMPTY _ _)) = Field nm tp EMPTY
binding2field (Binding (Var nm tp) lam) = Field nm tp (Lam lam)

-- converts a list of expressions to a record - need it for parsing 
recordFromExprs :: [Expr] -> Record
recordFromExprs exs = map (\ex -> Field "" ToDerive ex) exs

emptyRecordFromList :: [a] -> Record
emptyRecordFromList exs = map (\ex -> Field "" ToDerive EMPTY) exs

data Expr = 
    VarId Name
  | Lit Literal
  {-
  Supporting the new logic described in the Parser in CoreLambda.

  Binding - binding of a typed identifier with arguments to an expression. Used to model ALL functions, types, typeclasses, constants etc at the top level.
  KEY type for us as it can represent pretty much anything.

  On the top level, only Binding, PatternMatch and VarDefinition (which is used for declaring type signatures mostly for the functions) should exist!
  -}
  | Binding Var Lambda
  | Lam Lambda
  | Rec Record -- generic record used to pass things around
  | Value Record ConsTag Type -- The ONLY object that is a value object in our land (well, except Lit for primitives)
  | VarDefinition Var -- for standalone type signatures for functions and other symbols, e.g. fact : Int -> Int etc
  | RecordAccess [Expr] -- Accessing fields of a record. r.address.city will be recorded as RecordAccess (VarId "r") ["address", "city"]
  | LetIns [Field] Expr -- bindings "in" Expr; top level function definitions go here as well with EMPTY "in"
  | App Expr [Expr] -- tuple application mechanism (since even haskell eventually gets there!!!): Expr1 (Expr1,...,Exprn)
  -- 1 occurence of pattern match
  -- since we are NOT repeating function name on the left side of the pattern match but put ONLY arguments there,
  -- first [Expr] here is simply a list of expressions to which lambda bound variables need to be evaluated!!!
  -- second expression is the normal expression as usual
  | PatternMatch Expr Expr -- one pattern match, expression on the left should only be App 
  | Patterns [Expr] -- only PatternMatch should be inside
  | Prim Name -- primitive operation or function
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | EMPTY
  | ERROR String -- used only in Interpreter
  deriving (Show, Eq)

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] deriving (Eq, Show)

extractRecord :: Expr -> Record
extractRecord (Rec r) = r
extractRecord ex = fail $ "WHAT??? Tried to extract a record from a non-record expression: " ++ show ex

---------------------------------------------------------------------------------------------------
-- PURE CONVERSION / PROCESSING FUNCTIONS
---------------------------------------------------------------------------------------------------
-- takes a lambda that is a Type declaration and extracts constructors from it, turning it into proper
-- and correct form (all types are correct, all parameters, returning Value from Expr)
-- NB! Can only be run AFTER the afterparser stage!!!
extractConstructors :: Expr -> [NamedLambda]
extractConstructors (Binding v@(Var n SmallType) (Lambda _ (Rec cons) _ _) ) = map (\(Field nm _ (Lam l)) -> (nm, l)) cons
extractConstructors ex = []
-- extractConstructors ex = Left $ "Tried to extract a Constructor function from the Lambda that is likely not a Type declaration: " ++ (ppr ex) 

-- if we have a constructor field, converting it to a named lambda for eventual setting in the environment
fieldToNamedLambda :: Field -> NamedLambda
fieldToNamedLambda (Field n t (Lam val)) = (n, val)

-- Helper method for extractConstructors: converts a single Field in a constructor record to a proper Lambda with correct Type etc
convertFieldToCons :: Type -> Field -> Field
-- Unit constructor (Nil, Nothing etc)
-- Setting it up to return an empty Value record
convertFieldToCons typ (Field name tp EMPTY) = Field {
    fieldName = name 
  , fieldType = chooseType 
  , fieldValue = Lam $ Lambda {
        params = [], 
        body = Value [] name chooseType,
        sig  = chooseType,
        preds = []
      }
  }
  where chooseType = if (tp == ToDerive ) then typ else tp
-- constructor with some parameters - simply setting up a record to return with correct ConsTag and Type:
convertFieldToCons typ (Field name tp (Lam lam)) = Field {
    fieldName = name 
  , fieldType = chooseType 
  , fieldValue = Lam $ lam {
        body = Value (params lam) name chooseType,
        sig  = chooseType
      }
  }
  where chooseType = if (tp == ToDerive ) then typ else tp
-- general case, do nothing
convertFieldToCons _ f = f

-- Builds a Record of fields used for beta reduction using "substituteVariable/Type" functions below
-- Given an expression App (Lambda ...) [exs] - we make exs values inside parameters of the lambda
-- then doing a beta reduction inside the lambda for both total application (returning the body expression)
-- and the partial application (INCORRECT NOW!!! - need to construct a lambda with new arguments)
betaReduceLambda :: Lambda -> [Expr] -> Expr
betaReduceLambda lam exs = 
  let ar1 = length $ params lam
      ar2 = length exs
      (result, args)
        | ar1 == ar2
        = (body lam, 
          zipWith (\ f e -> f {fieldValue = e}) (params lam) exs) -- build an arguments record from arguments names and expressions given
        | ar1 > ar2
        = (body lam, 
          zipWith (\ f e -> f {fieldValue = e}) (take ar2 $ params lam) exs)
        | otherwise = (EMPTY, [])
  in if result == EMPTY then ERROR "Tried to give more arguments to the function than it can take"
     else traverseModify (substituteVariables args) (substituteTypeVariables args) result



-- --------------------------------- TRAVERSAL AND MODIFICATION --------------------------------------------
-- need to define functions that take an Expr->Expr function and modify all our datatypes correctly.
-- Then, beta-reduction will use these functions to work properly. It's a bit more complicated in our case,
-- as bound variables can be used in Type Signatures, where we'll need to make substitution as well.

class ExprTraversable a where
  traverseModify :: (Expr -> Expr) -> (Type -> Type) -> a -> a

instance ExprTraversable a => ExprTraversable[a] where
  traverseModify fe ft exs = map (traverseModify fe ft) exs

instance ExprTraversable Field where
  traverseModify fe ft fld = fld { 
      fieldValue = fe $ traverseModify fe ft (fieldValue fld) 
    , fieldType = ft $ traverseModify fe ft (fieldType fld) 
    }

instance ExprTraversable Lambda where
  traverseModify fe ft lam = lam { 
      params = traverseModify fe ft (params lam)
    , body = fe $ traverseModify fe ft (body lam)
    , sig = ft $ traverseModify fe ft (sig lam) 
    }

instance ExprTraversable Type where
  traverseModify fe ft (TApp t1 ts) = TApp (ft $ traverseModify fe ft t1) (traverseModify fe ft ts)
  traverseModify fe ft (TArr t1 t2) = TArr (ft $ traverseModify fe ft t1) (ft $ traverseModify fe ft t2)
  traverseModify fe ft tex = ft tex

instance ExprTraversable Expr where 
  traverseModify fe ft (Binding v lam) = Binding v (traverseModify fe ft lam)
  traverseModify fe ft (Lam lam) = Lam (traverseModify fe ft lam)
  traverseModify fe ft (Rec r) = Rec (traverseModify fe ft r)
  traverseModify fe ft (Value r c t) = Value (traverseModify fe ft r) c t
  traverseModify fe ft (RecordAccess exs) = RecordAccess (traverseModify fe ft exs)
  traverseModify fe ft (LetIns fs ex) = LetIns (traverseModify fe ft fs) (fe $ traverseModify fe ft ex)
  traverseModify fe ft (App ex exs) = App (fe $ traverseModify fe ft ex) (traverseModify fe ft exs)
  traverseModify fe ft (PatternMatch e1 e2) = PatternMatch (fe $ traverseModify fe ft e1) (fe $ traverseModify fe ft e2)
  traverseModify fe ft (Patterns exs) = Patterns (traverseModify fe ft exs)
  -- not including Ops since they will be desugared into Apps
  traverseModify fe ft ex = fe ex


-- ---------------------------------- Helper function for the variable substitution -----------------------
-- can add type checking eventually
-- it takes a Field (name, type, value) and substitutes a variables in the Expr with the same name to the field value,
-- *including* bound variables with the same name in the type signatures!
-- Theoretically, if we traverse modify with these 2 functions we should get a beta reduction?
substituteVariable :: Field -> Expr -> Expr
substituteVariable fld ex@(VarId n) = if (fieldName fld == n) && (fieldName fld /= "") then fieldValue fld else ex
substituteVariable _ ex = ex

-- substitues ANY variable in the field list with its occurence in Expr
substituteVariables :: [Field] -> Expr -> Expr
substituteVariables (f:fs) ex@(VarId n) = if (fieldName f == n) && (fieldName f /= "") then fieldValue f else substituteVariables fs ex
substituteVariables [] ex = ex
substituteVariables _ ex = ex
-- same but for types
substituteTypeVariable :: Field -> Type -> Type
substituteTypeVariable fld tex@(TVar (Var n t)) = if (fieldName fld == n) && (fieldName fld /= "") then TExpr $ fieldValue fld else tex
substituteTypeVariable _ tex = tex

substituteTypeVariables :: [Field] -> Type -> Type
substituteTypeVariables (f:fs) tex@(TVar (Var n t)) = if (fieldName f == n) && (fieldName f /= "") then TExpr $ fieldValue f else substituteTypeVariables fs tex
substituteTypeVariables [] tex = tex
substituteTypeVariables _ tex = tex

-- the very first pass that we run right after parsing the source file    
-- Some desugaring (Operators to Apps)
-- Setting up initial guesses for Types inside Type Declarations           
afterparse :: Expr -> Expr
-- converting VarDefinition to Binding with an empty lambda and a guess for the type signature
afterparse (VarDefinition v@(Var n t)) = Binding v (Lambda [] EMPTY ToDerive [])
-- Processing Type Declaration: setting up type signatures and constructor function bodies
afterparse (Binding v@(Var n SmallType) (Lambda p ex@(Rec cons) t pr) ) = 
    let typeType = constructTypeFunction n p -- creating a correct type signature for the type declaration
        (Rec cons') = afterparse ex -- walking down the tree to desugar possible values inside the record
    in  (Binding v (Lambda p (Rec $ map (convertFieldToCons typeType) cons') t pr) )
-- walking down the tree
afterparse (Binding v (Lambda p ex t pr) ) = (Binding v (Lambda p (afterparse ex) t pr) )
afterparse (Lam (Lambda p ex t pr)) = Lam (Lambda p (afterparse ex) t pr)
afterparse (Rec recr) = Rec (map fn recr) where fn f@(Field _ _ ex) = f {fieldValue = afterparse ex}
afterparse (RecordAccess exs) = RecordAccess (map afterparse exs)
afterparse (App ex exs) = App (afterparse ex) (map afterparse exs)
afterparse (PatternMatch exs ex) = PatternMatch (afterparse exs) (afterparse ex)
afterparse (Patterns exs) = Patterns (map afterparse exs)
afterparse (BinaryOp n e1 e2) = App (VarId n) ( (afterparse e1):(afterparse e2):[])
afterparse (UnaryOp n e) = App (VarId n) ( (afterparse e):[])
afterparse e = e


-- --------------------------------- PRETTY PRINTING --------------------------------------------

instance PrettyPrint Field where
  ppr (Field fn ft EMPTY) = fn ++ pprTypeOnly ft
  ppr (Field fn ft (Lam lam)) = fn ++ pprTypeOnly ft ++ " " ++ ppr lam
  ppr (Field fn ft fv) = fn ++ pprTypeOnly ft ++ " = " ++ (ppr fv)

instance PrettyPrint Pred where
  ppr Unconstrained = ""
  ppr (Exists typ) = "∃" ++ (ppr typ)  

instance PrettyPrint Lambda where
  ppr (Lambda params body sig pred) = ppr params ++ if (body == EMPTY) then "" else " = " ++ ppr body

showLambdaAsLambda :: String -> Lambda -> String
showLambdaAsLambda nm (Lambda params body sig pred) = ppr pred ++ nm ++ ":" ++ ppr sig ++ ppr params ++ " = " ++ ppr body 

instance PrettyPrint [Field] where
  ppr [] = ""
  ppr fs = (showListCuBrSpace ppr fs)

instance PrettyPrint (Var, Expr) where 
  ppr (v, e) = ppr v ++ " = " ++ (ppr e)

instance PrettyPrint [Pred] where
  ppr [] = ""
  ppr preds = (showListRoBr ppr preds) ++ " => "
  
instance PrettyPrint Literal where
  ppr (LInt i) = as [lmagenta] $ show i
  ppr (LFloat i) = as [lmagenta] $ show i
  ppr (LChar i) = as [lyellow] $ show i
  ppr (LString i) = as [lyellow] $ show i
  ppr (LList []) = "[]" 
  ppr (LList (x:xs)) = foldl fn ("[" ++ ppr x) xs ++ "]"
      where fn acc e = acc ++ ", " ++ ppr e
  ppr (LVec []) = "< >"
  ppr (LVec (x:xs)) = foldl fn ("<" ++ ppr x) xs ++ ">"
      where fn acc e = acc ++ ", " ++ ppr e

instance PrettyPrint Type where
  ppr ToDerive = "(?)"
  ppr (TCon n) = as [bold,lyellow] n
  ppr (TApp con args) = "(" ++ ppr con ++ " " ++ (showListPlain ppr args) ++ ")"
  ppr (TVar n) = ppr n
  ppr (Universe n) = as [bold,lyellow] $ "U" ++ (show n)
  ppr (TArr t1 t2) = ppr t1 ++ "->" ++ ppr t2
  ppr SmallType = as [bold,c256 202] "Type"
  ppr TClass = as [bold,c256 204] "Class"
  ppr e = show e

pprTypeOnly ToDerive = ""
pprTypeOnly e = ":" ++ ppr e

instance PrettyPrint Var where
  ppr (Var n t) = as [bold] n ++ pprTypeOnly t      

instance PrettyPrint Expr where
  ppr (Lit l) = ppr l
  ppr (VarId v) = v
  ppr (Binding v lam) = ppr (preds lam) ++ ppr v ++ " " ++ ppr lam
  ppr (Lam lam) = ppr lam
  ppr (VarDefinition v) = ppr v
  ppr (Rec r) = ppr r
  ppr (Value r ct tp) = as [bold, green] ct ++ ppr r ++ ":" ++ ppr tp
  -- ppr (Let ((v,e):[]) _ ) = ppr v ++ " = " ++ ppr e
  -- ppr (Lam vars expr tp preds) = ppr preds ++ (as [bold,lgray] "λ ") ++ (showListPlain ppr vars) ++ " . " ++ ppr expr
  ppr (BinaryOp n e1 e2) = ppr e1 ++ " " ++ n ++ " " ++ ppr e2
  ppr (UnaryOp n e) = n ++ ppr e
  -- ppr (Tuple cons exprs typ) = cons ++ (showListCuBr ppr exprs) ++ " : " ++ ppr typ
  ppr (App f args) = (ppr f) ++ " " ++ (showListRoBrPlain ppr args)
  ppr (Patterns ps) = showListCuBr ppr1 ps
      where ppr1 (PatternMatch args e2) = ppr args ++ " -> " ++ ppr e2
            ppr1 ex = ppr ex
  ppr (PatternMatch args e2) = ppr args ++ " = " ++ ppr e2
  ppr (ERROR err) = as [bold,red] err
  ppr e = show e
  -- λ  