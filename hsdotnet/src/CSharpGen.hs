{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards #-}
module CSharpGen where

import Compiler
import StgSyn
import CoreSyn
import Var
import DataCon

import Data.List.Index (imapM, ifind)
import Data.Foldable (foldlM)
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
    toCSharp (CON v n ar) = stab (fst v ++ " = new " ++ n ++ args2CSharp ar ++ ";\n")
    toCSharp e@(FUN n fv ar c) = _scs n fv ar c "FUN"
    toCSharp e@(ONCE n fv ar c) = _scs n fv ar c "THUNK_ONCE"
    toCSharp e@(THUNK n fv ar c) = _scs n fv ar c "THUNK"
-- helper function
-- Ok this is a bit tricky - if an expr inside our closure is a LET or CASE then we process the code inside LET / CASE
-- further down the hierarchy; if it's something else - we return the result (cause it's a var or a function call)
_scs n fv ar c con = _scsR n fv ar c con (not $ isACaseOrLet c)
_scsR n fv ar c con addRet = do
    ins <- checkTopLevel >>= pure . not
    s0 <- varargs2CSharpLookup fv
    s1 <- stab ( (if ins then "var " else "") ++ (fst n) ++ " = new " 
            ++ con ++ "("          
            ++ s0 ++ ", "
            ++ args2CSharp ar ++ if addRet then "=> {\n" else "=> {" )
    incTab
    pushFreeVars fv
    s2 <- toCSharp c
    popFreeVars
    r1 <- stab "return "
    decTab
    s3 <- stab "})\n"
    if addRet then return (s1 ++ r1 ++ s2 ++ ";\n" ++ s3) else return (s1 ++ s2 ++ "\n" ++ s3)

-- converting free vars to arguments in a new closure object call
freeVars2CSharp [] = "CLOSURE.EMPTY"
freeVars2CSharp (x:xs) =  (foldl fn ("new CLOSURE[] {" ++ showGhc x) xs) ++ "}"
    where fn acc v = acc ++ ", " ++ showGhc v

-- converting function arguments to proper function call syntax in c#
args2CSharp [] = "()"
args2CSharp (x:xs) =  (foldl fn ("(" ++ showGhc x) xs) ++ ")"
    where fn acc v = acc ++ ", " ++ showGhc v

-- converting arguments to proper names taking into account current free vars context of the function / thunk
-- (stored in the state monad)
-- this one is used in closure constructions
varargs2CSharpLookup :: Args -> SM String
varargs2CSharpLookup []     = pure "CLOSURE.EMPTY"
varargs2CSharpLookup (x:xs) = do
    s1 <- convertFreeVar x
    s2 <- (foldlM fn ("new CLOSURE[] {" ++ s1) xs)
    return (s2 ++ "}")
    where fn acc v = do
                        s1 <- convertFreeVar v
                        return (acc ++ ", " ++ s1)
          convertFreeVar x = lookupFreeVar (showGhc x)
-- this has to be top level as we use it to lookup single var name as well
lookupFreeVar name = do
    freeVars <- readFreeVars
    -- ok this is terribly inefficient as we are searching var list every time
    let fv = ifind (\i el -> name == (showGhc el) ) freeVars
    return $ maybe name ( \(i,_) -> "__freeVars[" ++ show i ++ "]" ) fv

-- this one is used in function / thunk calls
args2CSharpLookup :: [GenStgArg Var] -> SM String
args2CSharpLookup []     = pure "()"
args2CSharpLookup (x:xs) = do
    s1 <- convertFreeVar x
    s2 <- (foldlM fn ("(" ++ s1) xs)
    return (s2 ++ ")")
    where fn acc v = do
                        s1 <- convertFreeVar v
                        return (acc ++ ", " ++ s1)
          convertFreeVar (StgLitArg  lit) = pure $ showGhc lit
          convertFreeVar (StgVarArg  x) = do
                let name = showGhc x
                freeVars <- readFreeVars
                -- ok this is terribly inefficient as we are searching var list every time
                let fv = ifind (\i el -> name == (showGhc el) ) freeVars
                return $ maybe name ( \(i,_) -> "__freeVars[" ++ show i ++ "]" ) fv


instance CSharpable DotNetExpr where
    toCSharp (RAWSTG e) = return $ "[NOT IMPLEMENTED] " ++ "(" ++ showGhc e ++ ")\n"
    toCSharp (VAR v) = lookupFreeVar $ fst v
    toCSharp (LITERAL l) = return $ showGhc l
    toCSharp (FUNCALL (n,_) args)  = args2CSharpLookup args >>= \s -> pure (n ++ ".CALL" ++ s)
    toCSharp (CONCALL (n,_) args)  = args2CSharpLookup args >>= \s -> pure ("new " ++ n ++ s)
    toCSharp (PRIMOP (n,_) args)   = args2CSharpLookup args >>= \s -> pure ("[PRIMOP]" ++ n ++ ".CALL" ++ s)
    toCSharp (PRIMCALL (n,_) args) = args2CSharpLookup args >>= \s -> pure ("[PRIMCALL]" ++ n ++ ".CALL" ++ s)
    toCSharp (FOREIGNCALL (n,_) args) = args2CSharpLookup args >>= \s -> pure ("[FOREIGN]" ++ n ++ ".CALL" ++ s)
    -- let .. in let - is a separate case, don't need "return" there!!!
    toCSharp (LET o e@(LET _ _)) = (liftM2 (++) (toCSharp o) (toCSharp e)) >>= (\s -> pure $ "\n" ++ s)
    -- normal let transforms to a number of var assignments and then a return for "in" expression
    toCSharp (LET o e) = do
        s1 <- (toCSharp e >>= \x -> stab ("return " ++ x ++ ";"))
        s2 <- (toCSharp o)
        return ("\n" ++ s2 ++ s1)
    toCSharp (LETREC p e) = return ("[REC]" ++ show p ++ "return " ++ show e ++ ";\n")
    -- only one case and it is default - flat code with evaluation and return
    toCSharp (CASEDEFAULT n e re altType) = do
        s1 <- toCSharp e >>= \x -> stab ("var " ++ n ++ " = EVAL(" ++ x ++ ");\n")
        s2 <- if (isACaseOrLet re) then
                    do toCSharp re >>= stab
              else do toCSharp re >>= \x -> stab ("return " ++ x ++ ";")
        return ("/* [CASEDEFAULT][" ++ showGhc altType ++ "] */"  ++ "\n" ++ s1 ++ s2)
    -- toCSharp (CASESIMPLE n e (con, bndrs, re)) = do
    toCSharp (CASESIMPLE n e (con, bndrs, re) altType) = do
        s1 <- toCSharp e >>= \x -> stab ("var " ++ n ++ " = EVAL(" ++ x ++ ");\n")
        s2 <- if (isACaseOrLet re) then
                do toCSharp re >>= stab
              else do toCSharp re >>= \x -> stab ("return " ++ x ++ ";")
        s3 <- altToPatternMatch bndrs n >>= pure . (foldl (++) "")
        return ("/* [CASESIMPLE][" ++ showGhc altType ++ "] */" ++ (altConToComment con bndrs) ++ "\n" ++ s1 ++ s3 ++ s2)
    toCSharp (CASE n e def cases altType) = do
        s1 <- toCSharp e
        s2 <- stab ("var " ++ n ++ " = EVAL(" ++ s1 ++ ");\n")
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

-- converts pattern match constructor application to descructuring assignment of C# datatypes
-- bndrs is a list of var names in Con application
-- vname is a name of variable being destructured
altToPatternMatch bndrs vname = imapM (fn vname) bndrs
        where fn vn i b = stab ("var " ++ b ++ " = " ++ vn ++ ".__f" ++ show i ++ "__;\n")

altsToCSharp n alts = foldM (altToCSharp n) "" alts
altToCSharp n acc (altCon, bndrs, ex) = do
    s1 <- stab (altConToCSharp altCon bndrs ++ ":\n")
    incTab
    -- exprRes <- toCSharp ex
    exprRes <- if (isACaseOrLet ex) then
        do toCSharp ex >>= stab
      else do toCSharp ex >>= \x -> stab ("return " ++ x ++ ";")
    let s2 = (exprRes ++ "\n")
    s3 <- altToPatternMatch bndrs n >>= pure . (foldl (++) "")
    decTab
    return (acc ++ s1 ++ s3 ++ s2)
        
altConToCSharp (LitAlt lit) _ = "case " ++ showGhc lit
altConToCSharp (DataAlt dc) bndrs = "case /* " ++ showGhc dc ++ show bndrs ++ " */ " ++ show (dataConTag dc)

altConToComment (DataAlt dc) bndrs = "/* " ++ showGhc dc ++ show bndrs ++ " */ "
altConToComment (LitAlt lit) _ = "/* " ++ showGhc lit ++ " */"

-- MAIN FUNCTION: compiles STG program to C# program
stgToText :: [GenStgTopBinding Var Var] -> TextProgram
stgToText stgp = evalState (mapM toCSharp (stg2DotNet stgp)) initialCompilerState

