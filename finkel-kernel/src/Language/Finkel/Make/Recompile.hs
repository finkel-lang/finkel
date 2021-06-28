{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module containing types and functions for object code recompilation check.
module Language.Finkel.Make.Recompile
  ( RecompM(..)
  , RecompState(..)
  , emptyRecompState
  , checkRecompileRequired
  , checkModSummary
  , adjustIncludePaths
  ) where

#include "ghc_modules.h"

-- base
import           Control.Monad                     (forM_, when)
import           Control.Monad.Fail                (MonadFail (..))
import           Control.Monad.IO.Class            (MonadIO (..))
import           Data.Bifunctor                    (first)
import           GHC.Fingerprint                   (getFileHash)
import           System.IO                         (fixIO)

-- filepath
import           System.FilePath                   (dropExtension,
                                                    takeDirectory)

-- ghc
import           GHC_Data_FastString               (FastString)
import           GHC_Driver_Env_Types              (HscEnv (..))
import           GHC_Driver_Phases                 (Phase (..))
import           GHC_Driver_Session                (DynFlags (..),
                                                    HasDynFlags (..))
import           GHC_IfaceToCore                   (typecheckIface)
import           GHC_Iface_Load                    (readIface)
import           GHC_Iface_Recomp                  (RecompileRequired (..),
                                                    checkOldIface)
import           GHC_Iface_Recomp_Binary           (putNameLiterally)
import           GHC_Iface_Recomp_Flags            (fingerprintDynFlags)
import           GHC_Tc_Module                     (getModuleInterface)
import           GHC_Tc_Utils_Monad                (initIfaceLoad)
import           GHC_Types_SrcLoc                  (Located, noLoc, unLoc)
import           GHC_Types_Unique_Set              (UniqSet, addOneToUniqSet,
                                                    elementOfUniqSet,
                                                    emptyUniqSet)
import           GHC_Unit_Finder                   (findObjectLinkableMaybe)
import           GHC_Unit_Home_ModInfo             (HomeModInfo (..), addToHpt,
                                                    lookupHpt)
import           GHC_Unit_Module                   (ModLocation (..),
                                                    ModuleName, mkModuleName,
                                                    moduleName,
                                                    moduleNameString)
import           GHC_Unit_Module_Deps              (Dependencies (..),
                                                    Usage (..))
import           GHC_Unit_Module_ModSummary        (ModSummary (..),
                                                    msHiFilePath, ms_mod_name)
import           GHC_Unit_State                    (LookupResult (..),
                                                    lookupModuleWithSuggestions)
import           GHC_Unit_Types                    (IsBootInterface)
import           GHC_Utils_Exception               (handleIO)
import           GHC_Utils_Fingerprint             (Fingerprint)
import           GHC_Utils_Outputable              (Outputable (..), (<+>))

import qualified GHC_Data_Maybe                    as Maybes

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env                    (hsc_units)
import           GHC.Types.SourceFile              (SourceModified (..))
#else
import           GHC_Driver_Types                  (SourceModified (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Unit_Types                    (GenWithIsBoot (..),
                                                    ModuleNameWithIsBoot)
#endif

#if MIN_VERSION_ghc(8,10,0)
import           GHC_Unit_Module_ModIface          (ModIface, ModIface_ (..),
                                                    mi_flag_hash, mi_mod_hash)
#else
import           HscTypes                          (ModIface (..))
#endif

#if MIN_VERSION_ghc(8,6,0)
import           GHC_Driver_Session                (IncludeSpecs,
                                                    addQuoteInclude)
#endif

-- internal
import           Language.Finkel.Fnk
import           Language.Finkel.Make.Summary
import           Language.Finkel.Make.TargetSource
import           Language.Finkel.Make.Trace


-- ------------------------------------------------------------------------
--
-- Recompilation check for ModSummary
--
-- ------------------------------------------------------------------------

checkModSummary :: MonadIO m => HscEnv -> ModSummary -> m Bool
checkModSummary hsc_env ms = do
  let mb_usages = do
        old_hmi <- lookupHpt (hsc_HPT hsc_env) (ms_mod_name ms)
        return (mi_usages (hm_iface old_hmi))
  maybe (pure False) runUsageFileCheck mb_usages

-- | Simple function to check whther the 'UsageFile' is up to date.
runUsageFileCheck :: MonadIO m => [Usage] -> m Bool
runUsageFileCheck =  go
  -- See: 'MkIface.checkModUsage'.
  where
    go us =
      case us of
        [] -> return True
        u:us' -> do
          ret <- check u
          if ret
             then go us'
             else return False
    check u =
      case u of
        UsageFile {usg_file_path = file
                  ,usg_file_hash = old_hash} ->
          liftIO (handleIO (const (return False))
                           (fmap (== old_hash) (getFileHash file)))
        _ -> return True


-- ------------------------------------------------------------------------
--
-- Recompilation check for interface file
--
-- ------------------------------------------------------------------------

-- | State used for recompilation check.
data RecompState = RecompState
  { rs_hsc_env  :: !HscEnv
  , rs_outdated :: !ModuleNameSet
  }

type ModuleNameSet = UniqSet ModuleName

emptyRecompState :: HscEnv -> RecompState
emptyRecompState hsc_env = RecompState hsc_env emptyUniqSet
{-# INLINABLE emptyRecompState #-}

addOutdated :: ModuleName -> RecompState -> RecompState
addOutdated name rs = rs {rs_outdated = addOneToUniqSet (rs_outdated rs) name}
{-# INLINABLE addOutdated #-}

elemOutdated :: ModuleName -> RecompState -> Bool
elemOutdated name rs = name `elementOfUniqSet` rs_outdated rs
{-# INLINABLE elemOutdated #-}

-- "RecompM a" is same as "FnkT (ExceptT String (State RecompState)) a".

-- | Newtype for recompilation check, state monad combined with Fnk with either
-- value.
newtype RecompM a =
  RecompM {unRecompM :: RecompState -> Fnk (Either String a, RecompState)}

instance Functor RecompM where
  fmap f (RecompM m) = RecompM (fmap (first (fmap f)) . m)
  {-# INLINE fmap #-}

instance Applicative RecompM where
  pure a = RecompM (\st0 -> pure (pure a, st0))
  {-# INLINE pure #-}
  f <*> m = f >>= flip fmap m
  {-# INLINE (<*>) #-}

instance Monad RecompM where
  RecompM m >>= k =
    RecompM (\st0 -> do (et1, st1) <- m st0
                        case et1 of
                          Left why -> st1 `seq` return (Left why, st1)
                          Right a  -> st1 `seq` unRecompM (k a) st1)
  {-# INLINE (>>=) #-}

instance MonadFail RecompM where
  fail e = RecompM (\_ -> Control.Monad.Fail.fail e)
  {-# INLINE fail #-}

instance MonadThrow RecompM where
  throwM e = RecompM (\_ -> throwM e)
  {-# INLINE throwM #-}

instance MonadIO RecompM where
  liftIO io = RecompM (\st -> liftIO io >>= \a -> pure (Right a, st))
  {-# INLINE liftIO #-}

instance HasDynFlags RecompM where
  getDynFlags = RecompM (\st -> pure (Right (hsc_dflags (rs_hsc_env st)), st))
  {-# INLINE getDynFlags #-}

-- | Check whether recompilation is required.
checkRecompileRequired
  :: FnkEnv -> TargetUnit -> RecompM ModSummary
checkRecompileRequired fnk_env tu = do
  hsc_env <- getHscEnv
  ms0 <- mkModSummaryForRecompile hsc_env tu
  checkOutdatedCache (ms_mod_name ms0)
  checkObjDate ms0
  iface0 <- lookupOrLoadIface ms0
  checkUsagePackageModules (mi_usages iface0)
  ms1 <- refillHomeImports fnk_env ms0 iface0
  iface1 <- doCheckOldIface ms1 iface0
  hsc_env1 <- getHscEnv
  hmi <- mkHomeModInfo hsc_env1 ms0 iface1
  addHomeModInfo (ms_mod_name ms0) hmi
  return ms1

getRecompState :: RecompM RecompState
getRecompState = RecompM (\st -> pure (Right st, st))
{-# INLINABLE getRecompState #-}

getHscEnv :: RecompM HscEnv
getHscEnv = RecompM (\st -> pure (Right (rs_hsc_env st), st))
{-# INLINABLE getHscEnv #-}

recomp :: String -> RecompM a
recomp why = RecompM (\st -> pure (Left why, st))
{-# INLINABLE recomp #-}

outdate :: ModuleName -> String -> RecompM a
outdate name why = RecompM (\st0 -> pure (Left why, addOutdated name st0))
{-# INLINABLE outdate #-}

outdateToo :: ModuleName -> RecompM a -> RecompM a
outdateToo name (RecompM r) =
  RecompM $ \st0 -> do
    et_a <- r st0
    case et_a of
      (Left why, st1) -> pure (Left why, addOutdated name st1)
      (Right a, st1)  -> pure (Right a, st1)
{-# INLINABLE outdateToo #-}

addHomeModInfo :: ModuleName -> HomeModInfo -> RecompM ()
addHomeModInfo name hmi =
  RecompM (\rs0 ->
             let hsc_env0 = rs_hsc_env rs0
                 hpt1 = addToHpt (hsc_HPT hsc_env0) name hmi
                 hsc_env1 = hsc_env0 {hsc_HPT = hpt1}
                 rs1 = rs0 {rs_hsc_env = hsc_env1}
             in  pure (Right (), rs1))
{-# INLINABLE addHomeModInfo #-}

checkOutdatedCache :: ModuleName -> RecompM ()
checkOutdatedCache mname = do
  st <- getRecompState
  when (elemOutdated mname st)
       (recomp (moduleNameString mname ++ " in outdated cache"))
{-# INLINABLE checkOutdatedCache #-}

checkObjDate :: ModSummary -> RecompM ()
checkObjDate ms = do
  let hdate = ms_hs_date ms
      name = ms_mod_name ms
      out str = outdate name (unwords [moduleNameString name, "has", str])
  case ms_obj_date ms of
    Just odate | hdate < odate -> return ()
    Just _                     -> out "outdated object code"
    _                          -> out "no object code"
{-# INLINABLE checkObjDate #-}

lookupOrLoadIface :: ModSummary -> RecompM ModIface
lookupOrLoadIface ms = do
  rs <- getRecompState
  case lookupHpt (hsc_HPT (rs_hsc_env rs)) (ms_mod_name ms) of
    Just hmi -> return (hm_iface hmi)
    Nothing  -> loadIface (rs_hsc_env rs) ms
{-# INLINABLE lookupOrLoadIface #-}

-- | Check whether 'UsagePackageModule' elements are up to date or not.
checkUsagePackageModules :: [Usage] -> RecompM ()
checkUsagePackageModules usages = getHscEnv >>= forM_ usages . go
  -- Since RecompM might use a ModSummary without parsing source code, import
  -- declarations of external modules are not filled in the ModSummary. This
  -- function is for manually checking the status of imported modules from
  -- external package.
  where
    -- Checking the Usage for external package modules, to decide whether the
    -- source code file should be parsed or not.
    go hsc_env u =
      case u of
        UsagePackageModule {usg_mod=mdl,usg_mod_hash=old_hash} -> do
          let mname = moduleName mdl
              mname_str = moduleNameString mname
              check_mod_hash = do
                -- External package modules are also stored in outdated cache,
                -- looking up the cache before loading the interface.
                checkOutdatedCache mname
                (_, mb_iface) <- liftIO (getModuleInterface hsc_env mdl)
                case mb_iface of
                  Nothing -> outdate mname (mname_str ++ " iface not found")
                  Just iface ->
                    when (mi_mod_hash' iface /= old_hash)
                         (outdate mname (mname_str ++ " hash changed"))
#if MIN_VERSION_ghc(9,2,0)
              lmws_arg1 = hsc_units
#elif MIN_VERSION_ghc(9,0,0)
              lmws_arg1 = unitState . hsc_dflags
#else
              lmws_arg1 = hsc_dflags
#endif
          case lookupModuleWithSuggestions (lmws_arg1 hsc_env) mname Nothing of
            LookupFound {}    -> check_mod_hash
            LookupMultiple {} -> check_mod_hash
            LookupHidden {}   -> check_mod_hash
#if MIN_VERSION_ghc(8,6,0)
            LookupUnusable {} -> outdate mname (mname_str ++ " unusable")
#endif
            LookupNotFound {} -> outdate mname (mname_str ++ " not found")
        _ -> return ()

-- | Refill 'ms_textual_imps' field with 'UsageHomeModule' in interface.
refillHomeImports :: FnkEnv -> ModSummary -> ModIface -> RecompM ModSummary
refillHomeImports fnk_env ms mi = do
  -- XXX: At the moment cannot find any clue to get textual imports of external
  -- packages from ModIface, recompilation due to changes in external package
  -- modules are done with "checkUsagePackageModules".
  let dmods0 = dep_mods (mi_deps mi)
      dmods1 = map unDeps dmods0
      tr = traceMake fnk_env "refillHomeImports"
      mname = ms_mod_name ms

  tr [ "dep_mods mi_deps of" <+> ppr mname
     , nvc_or_none (map fst dmods1) ]

  -- Marking this module as outdated when any of the mported home package module
  -- was outdated, and at the same time, preserving the state with outdated home
  -- package module.
  imps <- outdateToo mname (mapM (collectOldIface fnk_env) dmods1)
  return (ms {ms_textual_imps=imps})

#if MIN_VERSION_ghc(9,0,0)
unDeps :: ModuleNameWithIsBoot -> (ModuleName, IsBootInterface)
unDeps gwib = (gwib_mod gwib, gwib_isBoot gwib)
#else
unDeps :: a -> a
unDeps = id
#endif
{-# INLINABLE unDeps #-}

-- | Load old interface when usable and not yet loaded.
collectOldIface
  :: FnkEnv -> (ModuleName, IsBootInterface)
  -> RecompM (Maybe FastString, Located ModuleName)
collectOldIface fnk_env (mname, _is_boot) = do
  hsc_env <- getHscEnv
  let tr = traceMake fnk_env "collectOldIface"

  -- Lookup HomeModInfo in current HomePackageTable. If not found, updating
  -- HomeModInfo, so that the later "checkOlfIface" can lookup the interface
  -- files in HomePackageTable. If the interface were not added, fake interface
  -- would be added to PIT by the "checkOldIface" via "LoadIface.loadInterface".
  case lookupHpt (hsc_HPT hsc_env) mname of
    Just _hmi -> do
      tr ["Found iface of" <+> ppr mname <+> "in HPT"]
      return (Nothing, noLoc mname)
    Nothing -> do
      -- Before doing any other check, lookup the outdated cache.
      checkOutdatedCache mname

      -- Checking the existence of the old module, could be deleted.
      tu <- checkTargetUnit (noLoc (moduleNameString mname), Nothing)

      dep_ms <- mkModSummaryForRecompile hsc_env tu
      checkObjDate dep_ms

      -- Comparing the DynFlags hash at this point, to trigger recompilation
      -- with changes in the DynFlag.
      iface <- loadIface hsc_env dep_ms
      checkFlagHash hsc_env dep_ms iface

      -- External packages are not in textual import of ModSummary when reusing
      -- interface, checking now.
      checkUsagePackageModules (mi_usages iface)

      tr ["Collecting old iface of" <+> ppr mname]
      hmi <- mkHomeModInfo hsc_env dep_ms iface
      addHomeModInfo mname hmi

      return (Nothing, noLoc mname)

-- | Check whether recompile is required or not via 'checkOldIface'.
doCheckOldIface :: ModSummary -> ModIface -> RecompM ModIface
doCheckOldIface ms iface0 = do
  -- Delegating the interface test to "checkOldIface", except for the
  -- up-to-date-ness of source code by comparing the timestamps of the source
  -- code file and object code file.
  hsc_env0 <- getHscEnv
  let dflags_with_new_paths = adjustIncludePaths (ms_hspp_opts ms) ms
      hsc_env1 = hsc_env0 {hsc_dflags = dflags_with_new_paths}
      mb_iface0 = Just iface0
      src_modified =
       case ms_obj_date ms of
         Just odate | ms_hs_date ms < odate -> SourceUnmodified
         _                                  -> SourceModified
  (rr, mb_iface1) <- liftIO (checkOldIface hsc_env1 ms src_modified mb_iface0)
  let why = recompileReason rr
      mname = ms_mod_name ms
  case rr of
    UpToDate | Just iface <- mb_iface1 -> return iface
    _                                  -> outdate mname why

checkTargetUnit :: (Located String, Maybe Phase) -> RecompM TargetUnit
checkTargetUnit name_and_mb_phase@(lname, _) = do
  dflags <- hsc_dflags <$> getHscEnv
  let name = unLoc lname
      mname = mkModuleName (asModuleName name)
  mb_tu <- findTargetUnitMaybe dflags name_and_mb_phase
  case mb_tu of
    Nothing -> outdate mname ("Source of " ++ name ++ " not found")
    Just tu -> return tu
{-# INLINABLE checkTargetUnit #-}

checkFlagHash :: HscEnv -> ModSummary -> ModIface -> RecompM ()
checkFlagHash _he ms iface = do
  -- See "checkFlagHash" function in "MkIface".
  let old_hash = mi_flag_hash' iface
      dflags0 = ms_hspp_opts ms
      dflags1 = adjustIncludePaths dflags0 ms
      mdl = mi_module iface
#if MIN_VERSION_ghc(9,2,0)
  new_hash <-
    let he1 = _he {hsc_dflags = dflags1}
    in  liftIO (fingerprintDynFlags he1 mdl putNameLiterally)
#else
  new_hash <- liftIO (fingerprintDynFlags dflags1 mdl putNameLiterally)
#endif
  when (old_hash /= new_hash) (outdate (moduleName mdl) "flag hash changed")
{-# INLINABLE checkFlagHash #-}

-- | Wrapper function to load interface file with 'readIface'.
loadIface :: HscEnv -> ModSummary -> RecompM ModIface
loadIface hsc_env ms = do
  let mdl = ms_mod ms
      mname = moduleName mdl
      mname_str = moduleNameString mname
      load_iface = readIface mdl (msHiFilePath ms)
  read_result <- liftIO (initIfaceLoad hsc_env load_iface)
  case read_result of
    Maybes.Failed _err     -> outdate mname ("no iface for " ++ mname_str)
    Maybes.Succeeded iface -> pure iface

-- | Make 'HomeModInfo' for object code recompilation.
mkHomeModInfo
  :: MonadIO m => HscEnv -> ModSummary -> ModIface -> m HomeModInfo
mkHomeModInfo hsc_env0 ms iface0 = liftIO $ do
  let mdl = ms_mod ms
      mloc = ms_location ms
      -- See Note [Knot-tying typecheckIface] in GhcMake.
      knot_tying hsc_env mname iface =
        fixIO $ \details' -> do
          let hsc_env1 =
                hsc_env {
                  hsc_HPT =
                    let hmi = HomeModInfo iface details' Nothing
                    in  addToHpt (hsc_HPT hsc_env) mname hmi
                }
          initIfaceLoad hsc_env1 (typecheckIface iface)
  details <- knot_tying hsc_env0 (ms_mod_name ms) iface0
  mb_linkable <- findObjectLinkableMaybe mdl mloc
  return $! HomeModInfo iface0 details mb_linkable

-- | Adjust the 'includePaths' field in given 'DynFlags' to prepare for getting
-- flag hash value.
adjustIncludePaths :: DynFlags -> ModSummary -> DynFlags
adjustIncludePaths dflags0 ms =
  -- See: "DriverPipeline.compileOne'", it is doing similar work for updating
  -- the "includePaths" of the "DynFlags" used in "checkOldInterface".
  let new_paths =
        case ml_hs_file (ms_location ms) of
          Just path -> addQuoteInclude' old_paths [current_dir path]
          Nothing   -> old_paths
      old_paths = includePaths dflags0
      current_dir = takeDirectory . dropExtension
      dflags1 = dflags0 {includePaths = new_paths}
  in  dflags1

-- | Show textual representation of 'RecompileRequired'.
recompileReason :: RecompileRequired -> String
recompileReason rr =
  case rr of
    UpToDate          -> "up to date"
    MustCompile       -> "must compile"
    RecompBecause why -> why
{-# INLINABLE recompileReason #-}


-- ------------------------------------------------------------------------
--
-- GHC version compatibility functions
--
-- ------------------------------------------------------------------------

#if MIN_VERSION_ghc(8,6,0)
addQuoteInclude' :: IncludeSpecs -> [String] -> IncludeSpecs
addQuoteInclude' = addQuoteInclude
#else
addQuoteInclude' :: [String] -> [String] -> [String]
addQuoteInclude' = flip (++)
#endif
{-# INLINABLE addQuoteInclude' #-}

mi_mod_hash', mi_flag_hash' :: ModIface -> Fingerprint
#if MIN_VERSION_ghc(8,10,0)
mi_mod_hash' = mi_mod_hash . mi_final_exts
mi_flag_hash' = mi_flag_hash . mi_final_exts
#else
mi_mod_hash' = mi_mod_hash
mi_flag_hash' = mi_flag_hash
#endif
{-# INLINABLE mi_mod_hash' #-}
{-# INLINABLE mi_flag_hash' #-}
