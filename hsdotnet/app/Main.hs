{-# LANGUAGE ScopedTypeVariables #-}

{-
Compilation sequence goes in phases:
https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/ghc-8.6.3/src/DriverPipeline.html

Actual Haskell compilation happens here:
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (RealPhase (Hsc src_flavour)) input_fn dflags0

there's a lot of interesting initializations that's worth understanding and then
    actual compilation happens in hscIncrementalCompile:
    http://hackage.haskell.org/package/ghc-8.6.5/docs/src/HscMain.html


-}

module Main where

import GHC
import GHC.Paths (libdir)

import DynFlags
import Outputable
import HscTypes
import CorePrep
import CoreToStg
import SimplStg
import SimplCore

import CoreSyn
import StgSyn
import TyCon

import Var
import Name (nameStableString)
import Kind
import IdInfo

import Control.Monad.Trans

import Compiler

import Control.Monad.State

banner :: MonadIO m => String -> m ()
banner msg = liftIO $ putStrLn (
  (replicate (fromIntegral n) '=')
  ++
  msg
  ++ 
  (replicate (fromIntegral n) '=')
  )
  where
    n = (76 - length msg) `div` 2

main :: IO ()
main = runGhc (Just libdir) $ do
    env <- getSession
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscAsm {-HscInterpreted, HscC-} 
        -- , ghcLink = LinkInMemory
    }

    target <- guessTarget "Example.hs" Nothing
    addTarget target
    -- setTargets [target]
    load LoadAllTargets
    depanal [] True
    modSum <- getModSummary $ mkModuleName "Example"

    pmod <- parseModule modSum      -- ModuleSummary
    tmod <- typecheckModule pmod    -- TypecheckedSource
    dmod <- desugarModule tmod      -- DesugaredModule
    let coreMod = coreModule dmod      -- CoreModule
    let mod   = ms_mod modSum
    let loc   = ms_location modSum
    let core  = mg_binds coreMod
    let tcs   = filter isDataTyCon (mg_tcs coreMod) -- see note in source code: -- cg_tycons includes newtypes, for the benefit of External Core,
    -- but we don't generate any code for newtypes

    -- env <- getHscEnv
    -- mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
    -- let mod = mg_module guts
    -- let (Just modSum) = mgLookupModule (hsc_mod_graph env) mod
    -- let tcs = filter isDataTyCon (mg_tcs guts)
    -- let loc   = ms_location modSum
    -- prepping core
    let dflags1 = foldl (\acc flag -> gopt_set acc flag) dflags [Opt_StgCSE, 
            Opt_DoEtaReduction,
            Opt_CallArity,
            Opt_FunToThunk,
            Opt_StgStats
            ]
    let dflags' = dflags1 -- dopt_set dflags1 Opt_D_dump_stg
    let env' = env {hsc_dflags = dflags'}
    -- run core2core passes
    guts' <- liftIO $ core2core env' coreMod
    let core' = mg_binds guts' 
    (prep, _) <- liftIO $ corePrepPgm env' mod loc core' tcs
    -- compiling to stg
    -- gopt_set :: DynFlags -> GeneralFlag -> DynFlags
    -- dopt_set :: DynFlags -> DumpFlag -> DynFlags
    let (stg,_) = coreToStg dflags' mod prep
    -- let dflags1 = dopt_set dflags Opt_D_dump_stg
    -- setting stg optimization passes
        -- , Opt_StgStats
    -- setDynFlags dflags'
    stg_binds2 <- liftIO $ stg2stg dflags' stg

    {-

    liftIO $ banner "Parsed Source"
    liftIO $ putStrLn $ showGhc ( parsedSource pmod )

    liftIO $ banner "Renamed Module"
    liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

    liftIO $ banner "Typechecked Module"
    liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

    liftIO $ banner "Typed Toplevel Definitions"
    liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

    liftIO $ banner "Typed Toplevel Exports"
    liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )

    liftIO $ banner "Core Module"
    liftIO $ putStrLn $ showGhc ( mg_binds coreMod )

    liftIO $ banner "Type Declarations"
    liftIO $ mapM_ (putStrLn . processTyThing) ( modInfoTyThings (moduleInfo tmod) )

    -- liftIO $ banner "OUR CORE COMPILATION!!!"
    -- liftIO $ mapM_ (putStrLn . processBind) binds

    liftIO $ banner "Core Module - PREPPED!"
    liftIO $ putStrLn $ showGhc prep

    -}
    liftIO $ banner "STG"
    -- liftIO $ putStrLn $ showPpr dflags1 stgBindings
    liftIO $ mapM_ putStrLn (map showGhc stg_binds2)
    -- liftIO $ mapM_ (putStrLn . stgProcessBind) stgBindings

    

    liftIO $ banner "Class Instances"
    liftIO $ putStrLn $ showGhc ( mg_inst_env guts' )

    liftIO $ banner "Typed Toplevel Definitions"
    liftIO $ mapM_ (putStrLn . showTyCon) (mg_tcs guts')


    liftIO $ banner "OUR STG COMPILATION"
    -- liftIO $ mapM_ putStrLn (stgToText stg_binds2)
    liftIO $ mapM_ (putStrLn . show) (stg2DotNet stg_binds2)



  
  



  -- Mapping Core
{-
NonRec b (Expr b)	 
Rec [(b, Expr b)]	
-}
processBind :: CoreBind -> String
processBind (NonRec b ex) = showVar b ++ " =\n" ++ showExpr ex
processBind (Rec exs)     = "Recursive "

-- nameStableString $ 

showExpr (Var v) = showVar v
showExpr (Lit l) = "Literal " ++ showGhc l
showExpr (App ex arg) = "App [" ++ showExpr ex ++ "(" ++ showExpr arg ++ ")" ++ "]"
showExpr (Lam v ex) = "\\" ++ showVar v ++ " . " ++ showExpr ex
showExpr (Let b ex) = "Let: " ++ processBind b ++ " = " ++ showExpr ex
showExpr (Case ex b tp alts) = "Case: " ++ showExpr ex ++ " var: " ++ showGhc b ++ ":" 
    ++ showGhc tp ++ (foldl (\acc a -> acc ++ showAlt a) "" alts)
showExpr e@(Cast ex coer) = "Cast: " ++ showGhc e
showExpr (Type t) = "Type: " ++ showGhc t
showExpr (Tick tick ex) = "Tick: " ++ showExpr ex
showExpr e@(Coercion coerc) = "Coercion: " ++ showGhc e
-- showExpr e = "NOT IMPLEMENTED: " ++ showGhc e

-- showing Alts
showAlt (con, vars, ex) = showGhc con ++ " vars: " ++ showGhc vars ++ "-->" ++ showExpr ex ++ "\n"

{-

-}

{-
Var Id	 
Lit Literal	 
App (Expr b) (Arg b) infixl 4	 
Lam b (Expr b)	 
Let (Bind b) (Expr b)	 
Case (Expr b) b Type [Alt b]	 
Cast (Expr b) Coercion	 
Tick (Tickish Id) (Expr b)	 
Type Type	 
Coercion Coercion
-}
