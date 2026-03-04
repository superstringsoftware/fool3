{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards, DataKinds #-}
module CSharpGen where

import Compiler
import GHC.Stg.Syntax
import GHC.Core
import GHC.Types.Var
import GHC.Core.DataCon
import GHC.Types.Literal

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
    toCSharp (CON v n1 tag ar) = do
        ins <- checkTopLevel >>= pure . not
        s1 <- conArgs2CSharpLookup ar
        stab ((if ins then "var " else "") ++ funString2String (fst v) ++ " = new CON(" ++ show tag ++ ", " ++ s1 ++ "); /* " ++ funString2String n1 ++ " */\n")
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
    let n' = funString2String (fst n)
    let isFun = con == "FUN"
    let arityStr = show (length ar)
    -- Get a unique freeVars variable name to avoid duplicate declarations
    fvName <- if null fv then return "__freeVars" else freshFreeVarsName
    fvDecl <- if null fv then return ""
              else stab ("var " ++ fvName ++ " = " ++ s0 ++ ";\n")
    let fvRef = if null fv then "CLOSURE.EMPTY" else fvName
    -- FUN: new FUN(fvRef, arity, (args)=> { body })  -- fvName captured by C# closure
    -- THUNK: new THUNK(fvRef, ()=> { body })          -- fvName captured by C# closure
    let ctorOpen = if isFun
            then prefix ++ n' ++ " = new FUN(" ++ fvRef ++ ", " ++ arityStr ++ ", " ++ args2CSharp ar
            else prefix ++ n' ++ " = new THUNK(" ++ fvRef ++ ", ()"
    s1 <- stab ( ctorOpen ++ if addRet then "=> {\n" else "=> {" )
    incTab
    s2 <- withFreeVars fvName fv (toCSharp c)
    r1 <- stab "return "
    decTab
    s3 <- stab "});\n"
    let body = if addRet then (s1 ++ r1 ++ s2 ++ ";\n" ++ s3) else (s1 ++ s2 ++ "\n" ++ s3)
    return (fvDecl ++ body)

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
convertStgArg (StgVarArg x)  = lookupFreeVar (showGhc x)

-- Lookup a variable name, resolving to __fvN[i] if it's a captured free var
lookupFreeVar :: String -> SM String
lookupFreeVar name = do
    freeVars <- readFreeVars
    fvName <- getFreeVarsName
    let fv = ifind (\i el -> name == (showGhc el) ) freeVars
    return $ maybe name ( \(i,_) -> fvName ++ "[" ++ show i ++ "]" ) fv

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
    toCSharp (VAR v) = lookupFreeVar $ fst v
    toCSharp (LITERAL l) = return $ lit2CSharp l
    toCSharp (FUNCALL (n1,_) args) = do
        s <- conArgs2CSharpLookup args
        pure ("STG.APPLY(" ++ n ++ ", " ++ s ++ ")") where n = funString2String n1
    toCSharp (CONCALL (n1,_) tag args) = do
        s <- conArgs2CSharpLookup args
        pure ("new CON(" ++ show tag ++ ", " ++ s ++ ") /* " ++ funString2String n1 ++ " */")
    toCSharp (PRIMOP (n1,_) args)   = args2CSharpLookup args >>= \s -> pure ("PRIMOPS." ++ n ++ s) where n = funString2String n1
    toCSharp (PRIMCALL (n1,_) args) = args2CSharpLookup args >>= \s -> pure ("[PRIMCALL]" ++ n ++ ".CALL" ++ s) where n = funString2String n1
    toCSharp (FOREIGNCALL (n1,_) args) = args2CSharpLookup args >>= \s -> pure ("[FOREIGN]" ++ n ++ ".CALL" ++ s) where n = funString2String n1
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
            forwardDecl obj = stab ("CLOSURE " ++ funString2String (fst $ name obj) ++ " = null;\n")
            -- like toCSharp for DotNetObj but without "var" prefix
            letrecAssign (CON v n1 tag ar) = conArgs2CSharpLookup ar >>= \s1 -> stab (funString2String (fst v) ++ " = new CON(" ++ show tag ++ ", " ++ s1 ++ ");\n")
            letrecAssign obj = genClosureFull False (name obj) (freeVars obj) (args obj) (code obj) (conName' obj) (not $ isACaseOrLet (code obj))
            conName' (FUN {}) = "FUN"
            conName' (THUNK {}) = "THUNK"
            conName' (ONCE {}) = "THUNK_ONCE"
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
        s3 <- altToPatternMatch bndrs n >>= pure . (foldl (++) "")
        return ("/* [CASESIMPLE][" ++ showGhc altType ++ "] */" ++ (altConToComment con bndrs) ++ "\n" ++ s1 ++ s3 ++ s2)
    toCSharp (CASE n e def cases altType) = do
        s2 <- processCaseEval n e
        s6 <- stab ("switch (" ++ n ++ ") {\n")
        incTab
        s3 <- altsToCSharp n cases
        incTab
        defSt <- toCSharp def
        decTab
        s4 <- stab ("default: " ++ defSt)
        decTab
        s7 <- stab "}"
        let s5 = if (defSt == "") then "" else s4
        return ("/* [CASE][" ++ showGhc altType ++ "] */\n" ++ s2 ++ s6 ++ s3 ++ s5 ++ "\n" ++ s7)

----------------------------------------------------------------------
-- HELPER FUNCTIONS
----------------------------------------------------------------------

-- | Generate case scrutinee evaluation; primops don't need extra EVAL call
processCaseEval :: String -> DotNetExpr -> SM String
processCaseEval n e@(PRIMOP _ _) = toCSharp e >>= \x -> stab ("var " ++ n ++ " = " ++ x ++ ";\n")
processCaseEval n e = toCSharp e >>= \x -> stab ("var " ++ n ++ " = STG.EVAL(" ++ x ++ ");\n")

-- | Convert pattern match constructor to destructuring assignment of C# datatypes
altToPatternMatch :: [String] -> String -> SM [String]
altToPatternMatch bndrs vname = imapM (fn vname) bndrs
        where fn vn i b = stab ("var " ++ b ++ " = ((CON)" ++ vn ++ ").Vals[" ++ show i ++ "];\n")

-- | Generate C# switch cases from a list of case alternatives
altsToCSharp :: String -> [(AltCon, [String], DotNetExpr)] -> SM String
altsToCSharp n alts = foldM (altToCSharp n) "" alts

-- | Generate a single C# switch case from a case alternative
altToCSharp :: String -> String -> (AltCon, [String], DotNetExpr) -> SM String
altToCSharp n acc (altCon, bndrs, ex) = do
    s1 <- stab (altConToCSharp altCon bndrs ++ ":\n")
    incTab
    exprRes <- if (isACaseOrLet ex) then
        do toCSharp ex >>= stab
      else do toCSharp ex >>= \x -> stab ("return " ++ x ++ ";")
    let s2 = (exprRes ++ "\n")
    s3 <- altToPatternMatch bndrs n >>= pure . (foldl (++) "")
    decTab
    return (acc ++ s1 ++ s3 ++ s2)

-- | Convert an AltCon to a C# case label
altConToCSharp :: AltCon -> [String] -> String
altConToCSharp (LitAlt lit) _ = "case " ++ showGhc lit
altConToCSharp (DataAlt dc) bndrs = "case /* " ++ showGhc dc ++ show bndrs ++ " */ " ++ show (dataConTag dc)

-- | Convert an AltCon to a C# comment for documentation
altConToComment :: AltCon -> [String] -> String
altConToComment (DataAlt dc) bndrs = "/* " ++ showGhc dc ++ show bndrs ++ " */ "
altConToComment (LitAlt lit) _ = "/* " ++ showGhc lit ++ " */"

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

-- | Convert a string with illegal C# characters to a valid C# identifier
funString2String :: String -> String
funString2String s = foldl fn "" s
        where fn acc c = acc ++ funChar2String c

-- Type-aware literal conversion: wraps GHC literals in appropriate CONPRIM<T>
lit2CSharp :: Literal -> String
lit2CSharp (LitChar c) = "new CONPRIM<char>('" ++ [c] ++ "')"
lit2CSharp (LitNumber LitNumInt n) = "new CONPRIM<int>(" ++ show n ++ ")"
lit2CSharp (LitNumber LitNumInt64 n) = "new CONPRIM<long>(" ++ show n ++ "L)"
lit2CSharp (LitNumber LitNumWord n) = "new CONPRIM<uint>(" ++ show n ++ "u)"
lit2CSharp (LitNumber LitNumWord64 n) = "new CONPRIM<ulong>(" ++ show n ++ "UL)"
lit2CSharp (LitFloat r) = "new CONPRIM<float>(" ++ show (fromRational r :: Double) ++ "f)"
lit2CSharp (LitDouble r) = "new CONPRIM<double>(" ++ show (fromRational r :: Double) ++ "d)"
lit2CSharp (LitString bs) = "PRIMOPS.unpackCStringHash(\"" ++ show bs ++ "\")"
lit2CSharp (LitNullAddr) = "null"
lit2CSharp l = "/* unknown literal: " ++ showGhc l ++ " */ null"
