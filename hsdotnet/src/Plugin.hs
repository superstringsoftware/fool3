{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Plugin (plugin) where

import Control.Monad.Trans
import GhcPlugins

import DynFlags
import Outputable
import HscTypes
import CorePrep
import CoreToStg
import SimplStg

import CoreSyn
import StgSyn
import TyCon

import Var
import Name (nameStableString)
import Kind
import IdInfo

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
    
    let showGhc :: (Outputable a) => a -> String 
        showGhc = showPpr dflags

    let core = mg_binds guts

    liftIO $ banner "Core Module"
    liftIO $ putStrLn $ showGhc core

    liftIO $ banner "Class Instances"
    liftIO $ putStrLn $ showGhc ( mg_inst_env guts )
    
    -- http://hackage.haskell.org/package/ghc-8.6.5/docs/CorePrep.html
    -- (prep, _) <- liftIO $ corePrepPgm env mod loc binds tcs
    --stg <- liftIO $ coreToStg dflags (mg_module core) (mg_binds core)
    -- let (stgBindings,_) = coreToStg dflags (mg_module core) (mg_binds core)
    env <- getHscEnv
    -- mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
    let mod = mg_module guts
    let (Just modSum) = mgLookupModule (hsc_mod_graph env) mod
    let tcs = filter isDataTyCon (mg_tcs guts)
    let loc   = ms_location modSum
    -- prepping core
    (prep, _) <- liftIO $ corePrepPgm env mod loc core tcs
    -- compiling to stg
    let (stg,_) = coreToStg dflags mod prep
    stg_binds2 <- liftIO $ stg2stg dflags stg

    liftIO $ banner "Core Module - PREPPED!"
    liftIO $ putStrLn $ showGhc prep

    liftIO $ banner "STG"
    liftIO $ mapM_ putStrLn (map showGhc stg_binds2)

    liftIO $ banner "OUR STG COMPILATION"
    liftIO $ mapM_ (putStrLn . stgProcessBind) stg_binds2
    
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

