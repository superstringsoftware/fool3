{-# LANGUAGE ScopedTypeVariables, RankNTypes, DataKinds #-}
module Plugin (plugin) where

import Control.Monad.Trans
import Control.Monad.State
import GHC.Plugins

import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Driver.Env
import GHC.Driver.Ppr (showPprUnsafe)
import GHC.CoreToStg.Prep (corePrepPgm)
import GHC.CoreToStg (coreToStg)
import GHC.Stg.Pipeline (stg2stg)
import GHC.Core.Opt.Pipeline (core2core)

import GHC.Core
import GHC.Stg.Syntax
import GHC.Core.TyCon

import GHC.Types.Var
import GHC.Types.Name (nameStableString)
import GHC.Types.Id.Info

import GHC.Driver.Config.CoreToStg.Prep (initCorePrepConfig, initCorePrepPgmConfig)
import GHC.Driver.Config.CoreToStg (initCoreToStgOpts)
import GHC.Driver.Config.Stg.Pipeline (initStgPipelineOpts)

import GHC

import Compiler

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
    dflags <- getDynFlags

    let core = mg_binds guts

    env <- getHscEnv
    let mod = mg_module guts
    let (Just modSum) = mgLookupModule (hsc_mod_graph env) mod
    let tcs = filter isDataTyCon (mg_tcs guts)
    let loc   = ms_location modSum
    -- prepping core
    let dflags1 = foldl (\acc flag -> gopt_set acc flag) dflags [Opt_StgCSE,
            Opt_DoEtaReduction,
            Opt_CallArity,
            Opt_StgStats
            ]
    let dflags' = dflags1
    let logger = hsc_logger env

    -- core prep
    corePrepCfg <- liftIO $ initCorePrepConfig env
    let corePrepPgmCfg = initCorePrepPgmConfig dflags' []
    prep <- liftIO $ corePrepPgm logger corePrepCfg corePrepPgmCfg mod loc core tcs

    -- compiling to stg
    let coreToStgOpts = initCoreToStgOpts dflags'
    let (stg, _, _) = coreToStg coreToStgOpts mod loc prep

    -- stg2stg optimization passes
    let stgPipelineOpts = initStgPipelineOpts dflags' False
    (stg_binds2, _) <- liftIO $ stg2stg logger [] stgPipelineOpts mod stg


    liftIO $ banner "Core Module"
    liftIO $ putStrLn $ showPprUnsafe core

    liftIO $ banner "Class Instances"
    liftIO $ putStrLn $ showPprUnsafe ( mg_inst_env guts )

    liftIO $ banner "OUR STG WITH SHOW GHC"
    -- STG display skipped (CgStgTopBinding lacks Outputable in GHC 9.6)

    liftIO $ banner "Typed Toplevel Definitions"

    liftIO $ banner "OUR STG COMPILATION"

    return guts


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
