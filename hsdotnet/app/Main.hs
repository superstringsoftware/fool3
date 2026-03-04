{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC
import GHC.Paths (libdir)

import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Driver.Env
import GHC.Driver.Ppr (showPprUnsafe)
import GHC.Driver.Backend (ncgBackend)
import GHC.CoreToStg.Prep (corePrepPgm)
import GHC.CoreToStg (coreToStg)
import GHC.Stg.Pipeline (stg2stg)
import GHC.Core.Opt.Pipeline (core2core)

import GHC.Core
import GHC.Stg.Syntax
import GHC.Driver.Ppr (showSDocUnsafe)
import GHC.Core.TyCon

import GHC.Types.Var
import GHC.Types.Name (nameStableString)
import GHC.Types.Id.Info
import GHC.Unit.Module.ModGuts (mg_binds, mg_tcs, mg_inst_env)

import GHC.Driver.Config.CoreToStg.Prep (initCorePrepConfig, initCorePrepPgmConfig)
import GHC.Driver.Config.CoreToStg (initCoreToStgOpts)
import GHC.Driver.Config.Stg.Pipeline (initStgPipelineOpts)

import Control.Monad.Trans

import Compiler
import CSharpGen

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
    setSessionDynFlags $ dflags { backend = ncgBackend
        , ghcLink = LinkInMemory
    }
    target <- guessTarget "Example.hs" Nothing Nothing
    addTarget target
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
    let tcs   = filter isDataTyCon (mg_tcs coreMod)

    -- setting stg optimization flags
    let dflags1 = foldl (\acc flag -> gopt_set acc flag) dflags [Opt_StgCSE,
            Opt_DoEtaReduction,
            Opt_CallArity,
            Opt_StgStats
            ]
    let dflags' = dflags1
    -- Update session with new flags and get fresh env
    setSessionDynFlags dflags'
    env' <- getSession
    let logger = hsc_logger env'

    -- run core2core passes
    guts' <- liftIO $ core2core env' coreMod
    let core' = mg_binds guts'

    -- core prep
    corePrepCfg <- liftIO $ initCorePrepConfig env'
    let corePrepPgmCfg = initCorePrepPgmConfig dflags' []
    prep <- liftIO $ corePrepPgm logger corePrepCfg corePrepPgmCfg mod loc core' tcs

    -- compiling to stg
    let coreToStgOpts = initCoreToStgOpts dflags'
    let (stg, _, _) = coreToStg coreToStgOpts mod loc prep

    -- stg2stg optimization passes
    let stgPipelineOpts = initStgPipelineOpts dflags' False
    (stg_binds2, _) <- liftIO $ stg2stg logger [] stgPipelineOpts mod stg

    liftIO $ banner "STG"
    liftIO $ putStrLn $ showSDocUnsafe $ pprStgTopBindings (StgPprOpts{stgSccEnabled=False}) stg

    liftIO $ banner "Class Instances"
    liftIO $ putStrLn $ showGhc ( mg_inst_env guts' )

    liftIO $ banner "Typed Toplevel Definitions"
    liftIO $ mapM_ (putStrLn . showTyCon) (mg_tcs guts')

    liftIO $ banner "OUR STG COMPILATION"
    liftIO $ mapM_ putStrLn (stgToText stg_binds2)


-- Mapping Core
processBind :: CoreBind -> String
processBind (NonRec b ex) = showVar b ++ " =\n" ++ showExpr ex
processBind (Rec exs)     = "Recursive "

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

-- showing Alts (changed from tuple to Alt constructor in 9.6)
showAlt (Alt con vars ex) = showGhc con ++ " vars: " ++ showGhc vars ++ "-->" ++ showExpr ex ++ "\n"
