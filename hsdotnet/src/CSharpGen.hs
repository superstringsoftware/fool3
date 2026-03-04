{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards, DataKinds #-}
module CSharpGen where

import Compiler
import GHC.Stg.Syntax
import GHC.Core
import GHC.Types.Var
import GHC.Core.DataCon
import GHC.Types.Literal

import GHC.Types.RepType (PrimRep(..))
import qualified Data.ByteString.Char8 as BS

import Data.List (isInfixOf)
import Data.List.Index (imapM, ifind)
import Data.Foldable (foldlM)
import Control.Monad (liftM2, foldM)
import Control.Monad.State

-- classes to output different options of the code
-- to .Net CLR IL
class ILable a where
    toIL :: a -> SM String

-- to C#
class CSharpable a where
    toCSharp :: a -> SM String

instance CSharpable a => CSharpable (Maybe a) where
    toCSharp Nothing = return ""
    toCSharp (Just x) = toCSharp x


instance CSharpable DotNetObj where
    toCSharp (STRINGLIT v bs) = do
        ins <- checkTopLevel >>= pure . not
        let prefix = if ins then "var " else ""
        stab (prefix ++ funString2String v ++ " = PRIMOPS.unpackCStringHash(\"" ++ escapeCSharpString (BS.unpack bs) ++ "\");\n")
    toCSharp (CON v n1 tag ar) = do
        ins <- checkTopLevel >>= pure . not
        let prefix = if ins then "var " else ""
        let vn = funString2String v
        case primBoxType n1 of
            Just csType -> do
                s1 <- primBoxArg csType ar
                stab (prefix ++ vn ++ " = " ++ s1 ++ "; /* " ++ funString2String n1 ++ " */\n")
            Nothing -> do
                s1 <- conArgs2CSharpLookup ar
                stab (prefix ++ vn ++ " = new CON(" ++ show tag ++ ", " ++ s1 ++ "); /* " ++ funString2String n1 ++ " */\n")
    toCSharp e@(FUN n fv ar c) = genClosure n fv ar c "FUN"
    toCSharp e@(ONCE n fv ar c) = genClosure n fv ar c "THUNK"  -- ONCE is just THUNK for now
    toCSharp e@(THUNK n fv ar c) = genClosure n fv ar c "THUNK"
-- Code generation for closures (FUN/THUNK).
-- Free vars are captured via C# closure semantics:
--   var __freeVars = new CLOSURE[] { ... };  // declared before closure
--   name = new FUN(__freeVars, arity, (args)=> { body using __freeVars[i] })
--   name = new THUNK(__freeVars, ()=> { body using __freeVars[i] })

-- | Generate a closure (FUN or THUNK), auto-detecting whether return is needed
genClosure :: IdName -> Args -> Args -> DotNetExpr -> String -> SM String
genClosure n fv ar c con = genClosureWithReturn n fv ar c con (not $ isACaseOrLet c)

-- | Generate a closure with explicit return control
genClosureWithReturn :: IdName -> Args -> Args -> DotNetExpr -> String -> Bool -> SM String
genClosureWithReturn n fv ar c con addRet = genClosureFull True n fv ar c con addRet

-- | Full closure generation with all options (addVar controls 'var' prefix)
genClosureFull :: Bool -> IdName -> Args -> Args -> DotNetExpr -> String -> Bool -> SM String
genClosureFull addVar n fv ar c con addRet = do
    ins <- checkTopLevel >>= pure . not
    s0 <- varargs2CSharpLookup fv
    let prefix = if addVar && ins then "var " else ""
    let n' = funString2String n
    let isFun = con == "FUN"
    let arityStr = show (length ar)
    -- Get a unique freeVars variable name to avoid duplicate declarations
    fvName <- if null fv then return "__freeVars" else freshFreeVarsName
    fvDecl <- if null fv then return ""
              else stab ("var " ++ fvName ++ " = " ++ s0 ++ ";\n")
    let fvRef = if null fv then "CLOSURE.EMPTY" else fvName
    -- FUN: new FUN(fvRef, arity, (__args)=> { var x = __args[0]; ... body })
    -- THUNK: new THUNK(fvRef, ()=> { body })
    let ctorOpen = if isFun
            then prefix ++ n' ++ " = new FUN(" ++ fvRef ++ ", " ++ arityStr ++ ", (__args)"
            else prefix ++ n' ++ " = new THUNK(" ++ fvRef ++ ", ()"
    s1 <- stab (ctorOpen ++ "=> {\n")
    incTab
    -- For FUN: unpack __args into named local variables
    argUnpack <- if isFun
        then fmap concat $ imapM (\i v -> stab ("var " ++ funString2String (showGhc v) ++ " = __args[" ++ show i ++ "];\n")) ar
        else return ""
    s2 <- withFreeVars fvName fv (toCSharp c)
    if addRet then do
        r1 <- stab "return "
        decTab
        s3 <- stab "});\n"
        return (fvDecl ++ s1 ++ argUnpack ++ r1 ++ s2 ++ ";\n" ++ s3)
    else do
        decTab
        s3 <- stab "});\n"
        return (fvDecl ++ s1 ++ argUnpack ++ s2 ++ "\n" ++ s3)

-- converting free vars to arguments in a new closure object call
freeVars2CSharp [] = "CLOSURE.EMPTY"
freeVars2CSharp (x:xs) =  (foldl fn ("new CLOSURE[] {" ++ showGhc x) xs) ++ "}"
    where fn acc v = acc ++ ", " ++ showGhc v

-- converting function arguments to proper function call syntax in c#
args2CSharp [] = "()"
args2CSharp (x:xs) =  (foldl fn ("(" ++ showGhc x) xs) ++ ")"
    where fn acc v = acc ++ ", " ++ showGhc v

-- Generic arg lookup: converts a list of items using a converter function,
-- wrapping in the given delimiters (or returning emptyCase for empty list)
lookupArgs :: String -> String -> String -> (a -> SM String) -> [a] -> SM String
lookupArgs emptyCase _open _close _convert [] = pure emptyCase
lookupArgs _emptyCase open close convert (x:xs) = do
    s1 <- convert x
    s2 <- foldlM fn (open ++ s1) xs
    return (s2 ++ close)
    where fn acc v = do
            s1 <- convert v
            return (acc ++ ", " ++ s1)

-- Convert a StgArg (literal or variable) with free var lookup
convertStgArg :: StgArg -> SM String
convertStgArg (StgLitArg lit) = pure $ lit2CSharp lit
convertStgArg (StgVarArg x)  = case knownNullaryCon (showGhc x) of
    Just cs -> pure cs
    Nothing -> lookupFreeVar (showGhc x)

-- | Known nullary constructors that need special C# representation
knownNullaryCon :: String -> Maybe String
knownNullaryCon "[]"    = Just "new CON(1, CLOSURE.EMPTY) /* [] */"
knownNullaryCon "True"  = Just "new CON(2, CLOSURE.EMPTY) /* True */"
knownNullaryCon "False" = Just "new CON(1, CLOSURE.EMPTY) /* False */"
knownNullaryCon "()"    = Just "new CON(1, CLOSURE.EMPTY) /* () */"
knownNullaryCon _       = Nothing

-- Lookup a variable name, resolving to __fvN[i] if it's a captured free var.
-- If not a free var, sanitize the name for C# via funString2String.
lookupFreeVar :: String -> SM String
lookupFreeVar name = do
    freeVars <- readFreeVars
    fvName <- getFreeVarsName
    let fv = ifind (\i el -> name == (showGhc el) ) freeVars
    return $ maybe (funString2String name) ( \(i,_) -> fvName ++ "[" ++ show i ++ "]" ) fv

-- Convert a StgArg to raw (unboxed) C# value — no CONPRIM wrapping for literals
convertStgArgRaw :: StgArg -> SM String
convertStgArgRaw (StgLitArg lit) = pure $ lit2CSharpRaw lit
convertStgArgRaw (StgVarArg x)  = case knownNullaryCon (showGhc x) of
    Just cs -> pure cs
    Nothing -> lookupFreeVar (showGhc x)

-- Primitive boxing constructor args: raw values, comma-separated
primArgs2CSharpLookup :: [StgArg] -> SM String
primArgs2CSharpLookup = lookupArgs "" "" "" convertStgArgRaw

-- Free vars for closure construction: new CLOSURE[] {...} or CLOSURE.EMPTY
varargs2CSharpLookup :: Args -> SM String
varargs2CSharpLookup = lookupArgs "CLOSURE.EMPTY" "new CLOSURE[] {" "}" (\x -> lookupFreeVar (showGhc x))

-- CON constructor args: new CLOSURE[] {...} or CLOSURE.EMPTY
conArgs2CSharpLookup :: [StgArg] -> SM String
conArgs2CSharpLookup = lookupArgs "CLOSURE.EMPTY" "new CLOSURE[] {" "}" convertStgArg

-- Function/thunk call args: (...) or ()
args2CSharpLookup :: [StgArg] -> SM String
args2CSharpLookup = lookupArgs "()" "(" ")" convertStgArg


instance CSharpable DotNetExpr where
    toCSharp (RAWSTG e) = return $ "[NOT IMPLEMENTED] " ++ "(" ++ showGhc e ++ ")\n"
    toCSharp (VAR v) = lookupFreeVar v
    toCSharp (LITERAL l) = return $ lit2CSharp l
    toCSharp (FUNCALL n1 args) = do
        fn <- lookupFreeVar n1
        s <- conArgs2CSharpLookup args
        pure ("STG.APPLY(" ++ fn ++ ", " ++ s ++ ")")
    toCSharp (CONCALL n1 tag args) = do
        let n = funString2String n1
        case primBoxType n1 of
            Just csType -> do
                -- Primitive boxing constructor (I#, C#, D#, F#, W#) → CONPRIM<T>
                -- If the arg is a variable, it's already a CONPRIM — just pass through.
                -- If it's a literal, wrap in new CONPRIM<T>(rawValue).
                s <- primBoxArg csType args
                pure (s ++ " /* " ++ n ++ " */")
            Nothing -> do
                s <- conArgs2CSharpLookup args
                pure ("new CON(" ++ show tag ++ ", " ++ s ++ ") /* " ++ n ++ " */")
    toCSharp (PRIMOP n1 args)   = args2CSharpLookup args >>= \s -> pure ("PRIMOPS." ++ n ++ s) where n = funString2String n1
    toCSharp (PRIMCALL n1 args) = args2CSharpLookup args >>= \s -> pure ("[PRIMCALL]" ++ n ++ ".CALL" ++ s) where n = funString2String n1
    toCSharp (FOREIGNCALL n1 args) = do
        s <- args2CSharpLookup args
        let n = funString2String n1
        case knownForeignCall n of
            Just gen -> pure (gen s)
            Nothing  -> pure ("/* [FOREIGN] " ++ n ++ " */ null")
    toCSharp (UNBOXED_TUPLE args) = do
        s <- conArgs2CSharpLookup args
        pure ("new UNBOXED_TUPLE(" ++ s ++ ")")
    -- let .. in let - is a separate case, don't need "return" there!!!
    toCSharp (LET o e@(LET _ _)) = (liftM2 (++) (toCSharp o) (toCSharp e)) >>= (\s -> pure $ "\n" ++ s)
    -- normal let transforms to a number of var assignments and then a return for "in" expression
    toCSharp (LET o e) = do
        s1 <- (toCSharp e >>= \x -> stab ("return " ++ x ++ ";"))
        s2 <- (toCSharp o)
        return ("\n" ++ s2 ++ s1)
    -- LETREC: forward-declare all bindings as CLOSURE, then assign, then evaluate body
    toCSharp (LETREC bindings e) = do
        -- forward declarations
        decls <- mapM forwardDecl bindings
        -- actual assignments (each binding can reference the others, already declared so no "var")
        assigns <- mapM letrecAssign bindings
        -- body expression
        s1 <- if (isACaseOrLet e)
              then toCSharp e >>= stab
              else toCSharp e >>= \x -> stab ("return " ++ x ++ ";")
        return ("\n" ++ concat decls ++ concat assigns ++ s1)
        where
            forwardDecl obj = stab ("CLOSURE " ++ funString2String (name obj) ++ " = null;\n")
            -- like toCSharp for DotNetObj but without "var" prefix
            letrecAssign (CON v n1 tag ar) = case primBoxType n1 of
                Just csType -> primBoxArg csType ar >>= \s1 -> stab (funString2String v ++ " = " ++ s1 ++ ";\n")
                Nothing -> conArgs2CSharpLookup ar >>= \s1 -> stab (funString2String v ++ " = new CON(" ++ show tag ++ ", " ++ s1 ++ ");\n")
            letrecAssign obj = genClosureFull False (name obj) (freeVars obj) (args obj) (code obj) (conName' obj) (not $ isACaseOrLet (code obj))
            conName' (FUN {}) = "FUN"
            conName' (THUNK {}) = "THUNK"
            conName' (ONCE {}) = "THUNK"
            conName' _ = "CLOSURE"
    -- only one case and it is default - flat code with evaluation and return
    -- Needs TONS AND TONS OF REFACTORING!!!!!
    toCSharp (CASEDEFAULT n e re altType) = do
        s1 <- processCaseEval n e
        s2 <- if (isACaseOrLet re) then
                    do toCSharp re >>= stab
              else do toCSharp re >>= \x -> stab ("return " ++ x ++ ";")
        return ("/* [CASEDEFAULT][" ++ showGhc altType ++ "] */"  ++ "\n" ++ s1 ++ s2)
    toCSharp (CASESIMPLE n e (con, bndrs, re) altType) = do
        s1 <- processCaseEval n e
        s2 <- if (isACaseOrLet re) then
                do toCSharp re >>= stab
              else do toCSharp re >>= \x -> stab ("return " ++ x ++ ";")
        s3 <- altToPatternMatchCon bndrs (funString2String n) altType con >>= pure . (foldl (++) "")
        return ("/* [CASESIMPLE][" ++ showGhc altType ++ "] */" ++ (altConToComment con bndrs) ++ "\n" ++ s1 ++ s3 ++ s2)
    toCSharp (CASE n e def cases altType) = do
        s2 <- processCaseEval n e
        let switchExpr = caseSwitchExpr n altType
        s6 <- stab ("switch (" ++ switchExpr ++ ") {\n")
        incTab
        s3 <- altsToCSharp n cases altType
        -- Default case
        s5 <- case def of
            Nothing -> do
                -- No default — add throw to satisfy C# exhaustiveness check
                s4 <- stab "default:\n"
                incTab
                throwSt <- stab "throw new Exception(\"Non-exhaustive case\");\n"
                decTab
                return (s4 ++ throwSt)
            Just defExpr -> do
                s4 <- stab "default:\n"
                incTab
                defBody <- if (isACaseOrLet defExpr) then
                        toCSharp defExpr >>= stab
                    else toCSharp defExpr >>= \x -> stab ("return " ++ x ++ ";")
                decTab
                return (s4 ++ defBody ++ "\n")
        decTab
        s7 <- stab "}"
        return ("/* [CASE][" ++ showGhc altType ++ "] */\n" ++ s2 ++ s6 ++ s3 ++ s5 ++ s7)

----------------------------------------------------------------------
-- HELPER FUNCTIONS
----------------------------------------------------------------------

-- | Generate the C# switch expression based on the AltType
-- The name `n` is raw from showGhc — sanitize it.
caseSwitchExpr :: String -> AltType -> String
caseSwitchExpr n (AlgAlt _)        = "((CON)" ++ n' ++ ").__CONSTAG__"  where n' = funString2String n
caseSwitchExpr n (PrimAlt IntRep)  = "((CONPRIM<int>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (PrimAlt WordRep) = "((CONPRIM<uint>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (PrimAlt Int64Rep) = "((CONPRIM<long>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (PrimAlt Word64Rep) = "((CONPRIM<ulong>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (PrimAlt DoubleRep) = "((CONPRIM<double>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (PrimAlt FloatRep)  = "((CONPRIM<float>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (PrimAlt _)       = "((CONPRIM<int>)" ++ n' ++ ").Val"  where n' = funString2String n
caseSwitchExpr n (MultiValAlt _)   = funString2String n
caseSwitchExpr n PolyAlt           = funString2String n

-- | Generate case scrutinee evaluation; primops don't need extra EVAL call
-- The name `n` comes from showGhc and needs sanitization.
processCaseEval :: String -> DotNetExpr -> SM String
processCaseEval n e@(PRIMOP _ _) = toCSharp e >>= \x -> stab ("var " ++ funString2String n ++ " = " ++ x ++ ";\n")
processCaseEval n e = toCSharp e >>= \x -> stab ("var " ++ funString2String n ++ " = STG.EVAL(" ++ x ++ ");\n")

-- | Convert pattern match constructor to destructuring assignment of C# datatypes
-- AltType-aware: AlgAlt uses CON.Vals[i], PrimAlt has no destructuring,
-- MultiValAlt uses UNBOXED_TUPLE.Fields[i]
-- For prim boxing constructors (I#, C#, etc.), use CONPRIM<T>.Val
altToPatternMatch :: [String] -> String -> AltType -> SM [String]
altToPatternMatch bndrs vname (AlgAlt _) = imapM (fn vname) bndrs
        where fn vn i b = stab ("var " ++ funString2String b ++ " = ((CON)" ++ vn ++ ").Vals[" ++ show i ++ "];\n")
altToPatternMatch bndrs vname (PrimAlt _) = return [] -- PrimAlt: scrutinee IS the unboxed value, no destructuring
altToPatternMatch bndrs vname (MultiValAlt _) = imapM (fn vname) bndrs
        where fn vn i b = stab ("var " ++ funString2String b ++ " = ((UNBOXED_TUPLE)" ++ vn ++ ").Fields[" ++ show i ++ "];\n")
altToPatternMatch bndrs vname _ = imapM (fn vname) bndrs
        where fn vn i b = stab ("var " ++ funString2String b ++ " = ((CON)" ++ vn ++ ").Vals[" ++ show i ++ "];\n")

-- | Specialized pattern match for prim boxing constructors (I#, C#, etc.)
-- The scrutinee is already a CONPRIM<T> (a CLOSURE), so we just alias it.
-- PRIMOPs will unbox via .Val themselves. This avoids type mismatches.
altToPatternMatchPrimBox :: [String] -> String -> String -> SM [String]
altToPatternMatchPrimBox [b] vname _csType =
    sequence [stab ("var " ++ funString2String b ++ " = " ++ vname ++ ";\n")]
altToPatternMatchPrimBox bndrs vname _ = altToPatternMatch bndrs vname (AlgAlt undefined) -- fallback

-- | Pattern match that checks for prim boxing constructors first
altToPatternMatchCon :: [String] -> String -> AltType -> AltCon -> SM [String]
altToPatternMatchCon bndrs vname altType (DataAlt dc) =
    case primBoxType (showGhc dc) of
        Just csType -> altToPatternMatchPrimBox bndrs vname csType
        Nothing     -> altToPatternMatch bndrs vname altType
altToPatternMatchCon bndrs vname altType _ = altToPatternMatch bndrs vname altType

-- | Generate C# switch cases from a list of case alternatives
-- `n` is the raw scrutinee name (from showGhc), sanitized here.
altsToCSharp :: String -> [(AltCon, [String], DotNetExpr)] -> AltType -> SM String
altsToCSharp n alts altType = foldM (altToCSharp (funString2String n) altType) "" alts

-- | Generate a single C# switch case from a case alternative
-- `n` is already sanitized by altsToCSharp.
altToCSharp :: String -> AltType -> String -> (AltCon, [String], DotNetExpr) -> SM String
altToCSharp n altType acc (altCon, bndrs, ex) = do
    s1 <- stab (altConToCSharp altCon bndrs altType ++ ":\n")
    incTab
    exprRes <- if (isACaseOrLet ex) then
        do toCSharp ex >>= stab
      else do toCSharp ex >>= \x -> stab ("return " ++ x ++ ";")
    let s2 = (exprRes ++ "\n")
    s3 <- altToPatternMatchCon bndrs n altType altCon >>= pure . (foldl (++) "")
    decTab
    return (acc ++ s1 ++ s3 ++ s2)

-- | Convert an AltCon to a C# case label
altConToCSharp :: AltCon -> [String] -> AltType -> String
altConToCSharp (LitAlt lit) _ (PrimAlt _) = "case " ++ lit2CSharpRaw lit
altConToCSharp (LitAlt lit) _ _ = "case " ++ showGhc lit
altConToCSharp (DataAlt dc) bndrs _ = "case /* " ++ showGhc dc ++ show bndrs ++ " */ " ++ show (dataConTag dc)

-- | Convert a literal to a raw C# value (no CONPRIM wrapping) for use in case labels
lit2CSharpRaw :: Literal -> String
lit2CSharpRaw (LitNumber LitNumInt n) = show n
lit2CSharpRaw (LitNumber LitNumInt64 n) = show n ++ "L"
lit2CSharpRaw (LitNumber LitNumWord n) = show n ++ "u"
lit2CSharpRaw (LitNumber LitNumWord64 n) = show n ++ "UL"
lit2CSharpRaw (LitChar c) = "'" ++ [c] ++ "'"
lit2CSharpRaw l = showGhc l

-- | Convert an AltCon to a C# comment for documentation
altConToComment :: AltCon -> [String] -> String
altConToComment (DataAlt dc) bndrs = "/* " ++ showGhc dc ++ show bndrs ++ " */ "
altConToComment (LitAlt lit) _ = "/* " ++ showGhc lit ++ " */"

-- | Map known foreign calls to C# code generators
knownForeignCall :: String -> Maybe (String -> String)
knownForeignCall "hs_free_stable_ptr" = Just $ \_ -> "/* hs_free_stable_ptr: no-op */ null"
knownForeignCall "hs_free_fun_ptr"    = Just $ \_ -> "/* hs_free_fun_ptr: no-op */ null"
knownForeignCall n
    | "unpackCString" `isInfixOf` n = Just $ \s -> "PRIMOPS.unpackCStringHash" ++ s
    | otherwise = Nothing

-- | Escape a string for C# string literal
escapeCSharpString :: String -> String
escapeCSharpString [] = []
escapeCSharpString ('\\':cs) = '\\' : '\\' : escapeCSharpString cs
escapeCSharpString ('"':cs) = '\\' : '"' : escapeCSharpString cs
escapeCSharpString ('\n':cs) = '\\' : 'n' : escapeCSharpString cs
escapeCSharpString ('\r':cs) = '\\' : 'r' : escapeCSharpString cs
escapeCSharpString ('\t':cs) = '\\' : 't' : escapeCSharpString cs
escapeCSharpString ('\0':cs) = '\\' : '0' : escapeCSharpString cs
escapeCSharpString (c:cs) = c : escapeCSharpString cs

-- | Generate code for a primitive boxing constructor (I#, C#, etc.)
-- If the single arg is a literal, wrap in new CONPRIM<T>(rawValue).
-- If the single arg is a variable, it's already a CONPRIM<T> CLOSURE — just pass through.
primBoxArg :: String -> [StgArg] -> SM String
primBoxArg csType [StgLitArg lit] = pure ("new CONPRIM<" ++ csType ++ ">(" ++ lit2CSharpRaw lit ++ ")")
primBoxArg _csType [StgVarArg x] = lookupFreeVar (showGhc x) -- already a CONPRIM, just use it
primBoxArg csType args = do
    -- Fallback for unexpected multi-arg case
    s <- primArgs2CSharpLookup args
    pure ("new CONPRIM<" ++ csType ++ ">(" ++ s ++ ")")

-- | Check if a constructor name is a known primitive boxing constructor.
-- Returns the C# type if so.
primBoxType :: String -> Maybe String
primBoxType "I#" = Just "int"
primBoxType "C#" = Just "char"
primBoxType "D#" = Just "double"
primBoxType "F#" = Just "float"
primBoxType "W#" = Just "uint"
primBoxType _    = Nothing

-- MAIN FUNCTION: compiles STG program to C# program
stgToText :: [CgStgTopBinding] -> TextProgram
stgToText stgp = evalState (mapM toCSharp (stg2DotNet stgp)) initialCompilerState

-- converting illegal function names to proper c#
funChar2String :: Char -> String
funChar2String '+' = "Plus"
funChar2String '-' = "Minus"
funChar2String '*' = "Star"
funChar2String '=' = "Eq"
funChar2String '>' = "GT"
funChar2String '<' = "LT"
funChar2String '.' = "Dot"
funChar2String ':' = "Colon"
funChar2String '$' = "DLR"
funChar2String '#' = "Hash"
funChar2String '[' = "_"
funChar2String ']' = "_"
funChar2String '\'' = "P"
funChar2String c = [c]

-- | Convert a string with illegal C# characters to a valid C# identifier.
-- Also escapes C# reserved keywords with @-prefix.
funString2String :: String -> String
funString2String s = escapeCSharpKeyword $ foldl fn "" s
        where fn acc c = acc ++ funChar2String c

-- | C# reserved keywords that need @-prefix when used as identifiers
csharpKeywords :: [String]
csharpKeywords = ["abstract","as","base","bool","break","byte","case","catch","char",
    "checked","class","const","continue","decimal","default","delegate","do","double",
    "else","enum","event","explicit","extern","false","finally","fixed","float","for",
    "foreach","goto","if","implicit","in","int","interface","internal","is","lock",
    "long","namespace","new","null","object","operator","out","override","params",
    "private","protected","public","readonly","ref","return","sbyte","sealed","short",
    "sizeof","stackalloc","static","string","struct","switch","this","throw","true",
    "try","typeof","uint","ulong","unchecked","unsafe","ushort","using","virtual",
    "void","volatile","while"]

escapeCSharpKeyword :: String -> String
escapeCSharpKeyword s
    | s `elem` csharpKeywords = "@" ++ s
    | otherwise = s

-- Type-aware literal conversion: wraps GHC literals in appropriate CONPRIM<T>
lit2CSharp :: Literal -> String
lit2CSharp (LitChar c) = "new CONPRIM<char>('" ++ [c] ++ "')"
lit2CSharp (LitNumber LitNumInt n) = "new CONPRIM<int>(" ++ show n ++ ")"
lit2CSharp (LitNumber LitNumInt64 n) = "new CONPRIM<long>(" ++ show n ++ "L)"
lit2CSharp (LitNumber LitNumWord n) = "new CONPRIM<uint>(" ++ show n ++ "u)"
lit2CSharp (LitNumber LitNumWord64 n) = "new CONPRIM<ulong>(" ++ show n ++ "UL)"
lit2CSharp (LitFloat r) = "new CONPRIM<float>(" ++ show (fromRational r :: Double) ++ "f)"
lit2CSharp (LitDouble r) = "new CONPRIM<double>(" ++ show (fromRational r :: Double) ++ "d)"
lit2CSharp (LitString bs) = "PRIMOPS.unpackCStringHash(\"" ++ escapeCSharpString (BS.unpack bs) ++ "\")"
lit2CSharp (LitNullAddr) = "null"
lit2CSharp l = "/* unknown literal: " ++ showGhc l ++ " */ null"
