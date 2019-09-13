{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Plugin (plugin) where

import Control.Monad.Trans
import Control.Monad.State
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
-- import StgFVs - doesnt import???

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

    {-
    liftIO $ banner "Core Module"
    liftIO $ putStrLn $ showGhc core
    -}

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
    let dflags1 = foldl (\acc flag -> gopt_set acc flag) dflags [Opt_StgCSE, 
            Opt_DoEtaReduction,
            Opt_CallArity,
            Opt_FunToThunk,
            Opt_StgStats
            ]
    let dflags' = dflags1 -- dopt_set dflags1 Opt_D_dump_stg
    let env' = env {hsc_dflags = dflags'}
    (prep, _) <- liftIO $ corePrepPgm env' mod loc core tcs
    -- compiling to stg
    -- gopt_set :: DynFlags -> GeneralFlag -> DynFlags
    -- dopt_set :: DynFlags -> DumpFlag -> DynFlags
    let (stg,_) = coreToStg dflags' mod prep
    -- let dflags1 = dopt_set dflags Opt_D_dump_stg
    -- setting stg optimization passes
     -- , Opt_StgStats
    -- setDynFlags dflags'
    stg_binds2 <- liftIO $ stg2stg dflags' stg

    
    liftIO $ banner "Core Module"
    liftIO $ putStrLn $ showGhc core
    

    {-
    liftIO $ banner "STG"
    liftIO $ mapM_ putStrLn (map showGhc stg_binds2)
    -}

    liftIO $ banner "Class Instances"
    liftIO $ putStrLn $ showGhc ( mg_inst_env guts )

    -- TODO: http://hackage.haskell.org/package/ghc-8.6.5/docs/TyCon.html#
    -- Inspect type definitions properly, using at the very least:
    -- tyConName :: TyCon -> Name
    -- tyConKind :: TyCon -> Kind
    -- tyConTyVars :: TyCon -> [TyVar]
    -- tyConDataCons :: TyCon -> [DataCon]
    -- We'll probably need it for .Net code generation if we decide to use the type system somewhat.

    liftIO $ banner "OUR STG WITH SHOW GHC"
    liftIO $ putStrLn $ showGhc stg_binds2

    liftIO $ banner "Typed Toplevel Definitions"
    liftIO $ mapM_ (putStrLn . showTyCon) (mg_tcs guts)
    
    {-
    liftIO $ banner "OUR STG BEFORE STG2STG"
    liftIO $ mapM_ (putStrLn . stgProcessBind) stg
    -}

    liftIO $ banner "OUR STG COMPILATION"
    liftIO $ mapM_ (\bind -> putStrLn (evalState (stgProcessBind bind) initialCompilerState)) stg_binds2 

    
    {-
    let cgStg = annTopBindingsFreeVars stg_binds2
    liftIO $ banner "Annotated STG"
    liftIO $ mapM_ (putStrLn . showGhc) cgStg
    -}
    
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

