{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Wrapper for Finkel code compilation monad.
module Language.Finkel.Fnk
  ( -- * Finkel compiler monad
    Fnk(..)
  , FnkEnv(..)
  , FnkEnvRef(..)
  , FnkInvokedMode(..)
  , Macro(..)
  , MacroFunction
  , MacroName(..)
  , EnvMacros
  , FlagSet
  , runFnk
  , runFnk'
  , toGhc
  , fromGhc
  , emptyFnkEnv
  , initFnkEnv
  , getFnkEnv
  , putFnkEnv
  , modifyFnkEnv
  , setDynFlags
  , updateDynFlags
  , withTmpDynFlags
  , prepareInterpreter
  , useInterpreter

  -- * Error related functions
  , failFnk
  , finkelSrcError

  -- * GHC library directory
  , getLibDirFromGhc
  , initializeLibDirFromGhc

  -- * Debugging
  , FnkDebugFlag(..)
  , fopt
  , foptSet
  , setFnkVerbosity
  , debugWhen
  , debugWhen'
  , dumpDynFlags
  , dumpHscEnv
  , getFnkDebug

  -- * Macro related functions
  , emptyEnvMacros
  , insertMacro
  , lookupMacro
  , makeEnvMacros
  , mergeMacros
  , addMacro
  , deleteMacro
  , macroNames
  , isMacro
  , macroFunction

  -- * Gensym and UniqSupply
  , gensym
  , gensym'
  , initUniqSupply'

  -- * Re-export from 'exceptions' package
  , MonadCatch(..)
  , MonadThrow(..)
  , MonadMask(..)
  ) where

#include "ghc_modules.h"

-- base
import           Control.Concurrent        (MVar, newMVar, withMVar)
import           Control.Exception         (throw, throwIO)
import           Control.Monad             (mplus, unless, when)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Bifunctor            (first)
import           Data.Bits                 (setBit, testBit, zeroBits)
import           Data.Char                 (isSpace)
import           Data.IORef                (IORef, atomicModifyIORef',
                                            atomicWriteIORef, newIORef,
                                            readIORef)
import           Data.Word                 (Word8)
import           System.Environment        (getProgName, lookupEnv)
import           System.Exit               (exitFailure)
import           System.IO                 (stderr)
import           System.IO.Unsafe          (unsafePerformIO)

#if MIN_VERSION_ghc(9,10,0)
import           Data.Word                 (Word64)
#endif

-- containers
import qualified Data.Map                  as Map

import           System.Directory          (canonicalizePath, doesFileExist,
                                            findExecutable)
import           System.FilePath           (takeDirectory, (</>))

-- exceptions
import           Control.Monad.Catch       (MonadCatch (..), MonadMask (..),
                                            MonadThrow (..))

#if MIN_VERSION_ghc(9,0,0)
import           Control.Monad.Catch       (bracket)
#else
import           GHC_Utils_Exception       (ExceptionMonad (..))
#endif

-- process
import           System.Process            (readProcess)

-- ghc
import           GHC                       (ModSummary (..), runGhc)
import qualified GHC_Data_EnumSet          as EnumSet
import           GHC_Data_FastString       (FastString, fsLit, uniqueOfFS,
                                            unpackFS)
import           GHC_Driver_Env_Types      (HscEnv (..))
import           GHC_Driver_Main           (Messager, batchMsg)
import           GHC_Driver_Monad          (Ghc (..), GhcMonad (..),
                                            Session (..), modifySession)
import           GHC_Driver_Ppr            (showSDocForUser)
import           GHC_Driver_Session        (DynFlags (..), GeneralFlag (..),
                                            GhcLink (..), HasDynFlags (..),
                                            IncludeSpecs (..), gopt, gopt_set,
                                            gopt_unset, opt_P_signature,
                                            picPOpts, ways)
import           GHC_Platform_Ways         (wayGeneralFlags,
                                            wayUnsetGeneralFlags)
import           GHC_Runtime_Context       (InteractiveContext (..))
import           GHC_Settings_Config       (cProjectVersion)
import           GHC_Types_TyThing         (TyThing (..))
import           GHC_Types_Unique_Supply   (MonadUnique (..), UniqSupply,
                                            initUniqSupply, mkSplitUniqSupply,
                                            splitUniqSupply, takeUniqFromSupply)
import           GHC_Types_Var             (varType)
import           GHC_Unit_Home_ModInfo     (pprHPT)
import           GHC_Utils_CliOption       (showOpt)
import           GHC_Utils_Outputable      (Outputable (..), SDoc,
                                            alwaysQualify, defaultErrStyle,
                                            nest, ppr, printSDocLn, sep, text,
                                            vcat, (<+>))
import qualified GHC_Utils_Ppr             as Pretty

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Driver.Backend        (interpreterBackend)
#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Backend        (Backend (..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Env            (hscSetFlags, hsc_HPT, hsc_HUG)
import           GHC.Driver.Hooks          (Hooks (..))
import           GHC.Driver.Make           (ModIfaceCache, newIfaceCache)
import           GHC.Settings              (ToolSettings (..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env            (hsc_units)
#else
import           GHC_Driver_Session        (HscTarget (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Driver_Session        (initSDocContext,
                                            sccProfilingEnabled)
import           GHC_Platform_Ways         (hostFullWays)
#else
import           GHC_Platform_Ways         (interpWays, updateWays)
#endif

-- Internal
import           Language.Finkel.Error
import           Language.Finkel.Exception
import           Language.Finkel.Form


-- ---------------------------------------------------------------------
--
-- Macro and Fnk monad
--
-- ---------------------------------------------------------------------

-- | Macro transformer function.
--
-- A macro in Finkel is implemented as a function. The function takes a located
-- code data argument, and returns a located code data wrapped in 'Fnk'.
type MacroFunction = Code -> Fnk Code

-- | Data type to distinguish user defined macros from built-in special forms.
data Macro
  = Macro MacroFunction
  | SpecialForm MacroFunction

instance Show Macro where
  showsPrec _ m =
    case m of
      Macro _       -> showString "<macro>"
      SpecialForm _ -> showString "<special-form>"

-- | Type synonym to express mapping of macro name to 'Macro' data.
type EnvMacros = Map.Map MacroName Macro

newtype MacroName = MacroName {unMacroName :: FastString}
  deriving (Eq)

instance Ord MacroName where
  compare (MacroName a) (MacroName b) = compare (uniqueOfFS a) (uniqueOfFS b)
  {-# INLINE compare #-}

-- | Data type for debug information.
data FnkDebugFlag
  = Fnk_dump_dflags
  | Fnk_dump_expand
  | Fnk_dump_hs
  | Fnk_dump_session
  | Fnk_trace_expand
  | Fnk_trace_session
  | Fnk_trace_make
  | Fnk_trace_spf
  deriving (Eq, Show, Enum)

-- | Type synonym for holding on/off of 'FnkDebugFlag'.
type FlagSet = Word8 -- Word8 is enough for now.

-- | Data type to hold how the compiler was invoked.
data FnkInvokedMode
  = ExecMode
  -- ^ Standalone executable mode.
  | GhcPluginMode
  -- ^ GHC plugin mode.

instance Outputable FnkInvokedMode where
  ppr im = case im of
    ExecMode      -> "exec"
    GhcPluginMode -> "ghc-plugin"

-- | Environment state in 'Fnk'.
data FnkEnv = FnkEnv
   { -- | Macros accessible in current compilation context.
     envMacros                 :: EnvMacros
     -- | Temporary macros in current compilation context.
   , envTmpMacros              :: [EnvMacros]
     -- | Default set of macros, these macros will be used when
     -- resetting 'FnkEnv'.
   , envDefaultMacros          :: EnvMacros

     -- | Modules to import to context.
   , envContextModules         :: [String]
     -- | The default 'DynFlags', possibly containing settings from command line.
   , envDefaultDynFlags        :: !(Maybe DynFlags)

     -- | Messager used in make.
   , envMessager               :: Messager
     -- | Required home package modules names in current target.
   , envRequiredHomeModules    :: [ModSummary]

     -- | Directory to save generated Haskell source codes.
   , envHsOutDir               :: !(Maybe FilePath)

     -- | Lib directory passed to 'runGhc'.
   , envLibDir                 :: !(Maybe FilePath)

     -- | Whether to use qualified name for primitive functions used in quoting
     -- codes.
   , envQualifyQuotePrimitives :: !Bool

     -- | The 'HscEnv' used by the byte-code interpreter for macro expansion.
   , envSessionForExpand       :: !(Maybe HscEnv)

     -- | The 'UniqSupply' for 'gensym'.
   , envUniqSupply             :: UniqSupply

     -- | Verbosity level for Fnk related messages.
   , envVerbosity              :: {-# UNPACK #-} !Int
     -- | Dump flag settings.
   , envDumpFlags              :: {-# UNPACK #-} !FlagSet

     -- | How the compiler was invoked.
   , envInvokedMode            :: !FnkInvokedMode

     -- | ModIFaceCache used by GHC's load function, for interpreter.
   , envInterpModIfaceCache    :: !(Maybe ModIfaceCache)
   }

-- | Newtype wrapper for compiling Finkel code to Haskell AST.
newtype Fnk a = Fnk {unFnk :: FnkEnvRef -> Ghc a}

-- | Reference to 'FnkEnv'.
newtype FnkEnvRef = FnkEnvRef (IORef FnkEnv)

instance Functor Fnk where
  fmap f (Fnk m) = Fnk (fmap f . m)
  {-# INLINE fmap #-}

instance Applicative Fnk where
  pure x = Fnk (\_ -> pure x)
  {-# INLINE pure #-}
  Fnk f <*> Fnk m = Fnk (\ref -> f ref <*> m ref)
  {-# INLINE (<*>) #-}

instance Monad Fnk where
  Fnk m >>= k = Fnk (\ref -> m ref >>= \v -> unFnk (k v) ref)
  {-# INLINE (>>=) #-}

instance MonadFail Fnk where
  fail = failFnk
  {-# INLINE fail #-}

instance MonadIO Fnk where
  liftIO io = Fnk (\_ -> liftIO io)
  {-# INLINE liftIO #-}

instance MonadCatch Fnk where
  catch m h =
    Fnk (\ref -> unFnk m ref `catch` \e -> unFnk (h e) ref)
  {-# INLINE catch #-}

instance MonadThrow Fnk where
  throwM e = Fnk (\ _ -> throwM e)
  {-# INLINE throwM #-}

instance MonadMask Fnk  where
  mask f =
    Fnk (\ref ->
           mask (\r -> let r' m = Fnk (r . unFnk m)
                       in  unFnk (f r') ref))
  {-# INLINE mask #-}

  uninterruptibleMask f =
    Fnk (\ref ->
           uninterruptibleMask (\r -> let r' m = Fnk (r . unFnk m)
                                      in  unFnk (f r') ref))
  {-# INLINE uninterruptibleMask #-}

#if MIN_VERSION_exceptions(0,10,0)
  generalBracket acquire release use =
    Fnk (\ref ->
           let acquire' = unFnk acquire ref
               release' r err = unFnk (release r err) ref
               use' v = unFnk (use v) ref
           in  generalBracket acquire' release' use')
  {-# INLINE generalBracket #-}
#endif

#if !MIN_VERSION_ghc(9,0,0)

-- Note: [Orphan instances for type classes from Control.Monad.Catch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Manually defining instances instead of "deriving via" approach done in ghc
-- 9.0.1, to support older version of ghc which does not have support of
-- "DerivingVia" language extension.

instance MonadThrow Ghc where
  throwM e = liftIO (throwM e)
  {-# INLINE throwM #-}

instance MonadCatch Ghc where
  catch m h =
    Ghc (\he -> unGhc m he `catch` \e -> unGhc (h e) he)
  {-# INLINE catch #-}

instance MonadMask Ghc where
  mask f =
    Ghc (\he -> mask (\ r -> let r' m = Ghc (r . unGhc m)
                             in  unGhc (f r') he))
  {-# INLINE mask #-}

  uninterruptibleMask f =
    Ghc (\he -> uninterruptibleMask (\r -> let r' m = Ghc (r . unGhc m)
                                           in  unGhc (f r') he))
  {-# INLINE uninterruptibleMask #-}

#if MIN_VERSION_exceptions(0,10,0)
  generalBracket acquire release use =
    Ghc (\he ->
           let acquire' = unGhc acquire he
               release' r err = unGhc (release r err) he
               use' v = unGhc (use v) he
           in  generalBracket acquire' release' use')
  {-# INLINE generalBracket #-}
#endif

instance ExceptionMonad Fnk where
  gcatch m h =
    Fnk (\ref -> unFnk m ref `gcatch` \e -> unFnk (h e) ref)
  {-# INLINE gcatch #-}

  gmask f =
    Fnk (\ref ->
           gmask (\r -> let r' m = Fnk (r . unFnk m)
                        in  unFnk (f r') ref))
  {-# INLINE gmask #-}
#endif

instance MonadUnique Fnk where
  getUniqueSupplyM = do
    fnk_env <- getFnkEnv
    let (us1, us2) = splitUniqSupply (envUniqSupply fnk_env)
    putFnkEnv $ fnk_env { envUniqSupply = us2 }
    return us1
  {-# INLINE getUniqueSupplyM #-}

  getUniqueM = do
    fnk_env <- getFnkEnv
    let (u, us1) = takeUniqFromSupply (envUniqSupply fnk_env)
    putFnkEnv $ fnk_env { envUniqSupply = us1 }
    return u
  {-# INLINE getUniqueM #-}

instance HasDynFlags Fnk where
  getDynFlags = Fnk (const getDynFlags)
  {-# INLINE getDynFlags #-}

#if MIN_VERSION_ghc(9,2,0)
instance HasLogger Fnk where
  getLogger = Fnk (const getLogger)
  {-# INLINE getLogger #-}
#else
instance HasLogger Fnk where
  getLogger = pure (error "getLogger (Fnk): no Logger")
#endif

instance GhcMonad Fnk where
  getSession = Fnk (const getSession)
  {-# INLINE getSession #-}
  setSession hsc_env = Fnk (\_ -> setSession hsc_env)
  {-# INLINE setSession #-}

-- | Run 'Fnk' with given environment.
--
-- Internally calls 'initFnkEnv' and 'runGhc'.
runFnk :: Fnk a -> FnkEnv -> IO a
runFnk m fnk_env0 = do
  fnk_env1 <- initFnkEnv fnk_env0
  ref <- newIORef fnk_env1
  runGhc (envLibDir fnk_env1) (toGhc m (FnkEnvRef ref))

-- | Run 'Fnk' with given 'FnkEnv' and 'HscEnv'.
--
-- This function does /NOT/ call 'initFnkEnv', uses 'unGhc' instead of 'runGhc'.
runFnk' :: Fnk a -> FnkEnv -> HscEnv -> IO a
runFnk' m fnk_env hsc_env = do
  fer <- FnkEnvRef <$> newIORef fnk_env
  session <- Session <$> newIORef hsc_env
  unGhc (toGhc m fer) session
{-# INLINABLE runFnk' #-}

-- | Extract 'Ghc' from 'Fnk'.
toGhc :: Fnk a -> FnkEnvRef -> Ghc a
toGhc = unFnk
{-# INLINABLE toGhc #-}

-- | Lift 'Ghc' to 'Fnk'.
fromGhc :: Ghc a -> Fnk a
fromGhc m = Fnk (const m)
{-# INLINABLE fromGhc #-}

-- | Get current 'FnkEnv'.
getFnkEnv :: Fnk FnkEnv
getFnkEnv = Fnk (\(FnkEnvRef ref) -> liftIO $! readIORef ref)
{-# INLINABLE getFnkEnv #-}

-- | Set current 'FnkEnv' to given argument.
putFnkEnv :: FnkEnv -> Fnk ()
putFnkEnv fnk_env =
  Fnk (\(FnkEnvRef ref) -> liftIO $! atomicWriteIORef ref fnk_env)
{-# INLINABLE putFnkEnv #-}

-- | Update 'FnkEnv' with applying given function to current 'FnkEnv'.
modifyFnkEnv :: (FnkEnv -> FnkEnv) -> Fnk ()
modifyFnkEnv f =
  Fnk (\(FnkEnvRef ref) ->
         liftIO $! atomicModifyIORef' ref (\fnk_env -> (f fnk_env, ())))
{-# INLINABLE modifyFnkEnv #-}

-- | Throw 'FinkelException' with given message.
failFnk :: MonadIO m => String -> m a
failFnk = liftIO . throwIO . FinkelException
{-# INLINABLE failFnk #-}

-- | Throw 'FinkelSrcError' with given 'Code' and message.
finkelSrcError :: (Monad m, MonadIO m) => Code -> String -> m a
finkelSrcError code = liftIO . throwIO . FinkelSrcError code
{-# INLINABLE finkelSrcError #-}

-- | Initialize 'FnkEnv'.
initFnkEnv :: FnkEnv -> IO FnkEnv
initFnkEnv fnk_env = do
  uniqSupply <- mkSplitUniqSupply '_'
  libdir <- maybe getLibDirFromGhc pure (envLibDir fnk_env)
  interpModIfaceCache <- getNewModIfaceCache
  pure fnk_env { envLibDir = Just libdir
               , envUniqSupply = uniqSupply
               , envInterpModIfaceCache = Just interpModIfaceCache }
{-# INLINABLE initFnkEnv #-}

-- ModIfaceCache does not exist in ghc < 9.4.
#if MIN_VERSION_ghc(9,4,0)
getNewModIfaceCache :: MonadIO m => m ModIfaceCache
getNewModIfaceCache = liftIO newIfaceCache
#else
type ModIfaceCache = ()
getNewModIfaceCache :: MonadIO m => m ModIfaceCache
getNewModIfaceCache = pure ()
#endif
{-# INLINABLE getNewModIfaceCache #-}

-- | Empty 'FnkEnv' for performing computation with 'Fnk'.
emptyFnkEnv :: FnkEnv
emptyFnkEnv = FnkEnv
  { envMacros                 = emptyEnvMacros
  , envTmpMacros              = []
  , envDefaultMacros          = emptyEnvMacros
  , envContextModules         = []
  , envDefaultDynFlags        = Nothing
  , envMessager               = batchMsg
  , envRequiredHomeModules    = []
  , envHsOutDir               = Nothing
  , envLibDir                 = Nothing
  , envQualifyQuotePrimitives = False
  , envSessionForExpand       = Nothing
  , envUniqSupply             = uninitializedUniqSupply
  , envVerbosity              = 1
  , envDumpFlags              = zeroBits
  , envInvokedMode            = ExecMode
  , envInterpModIfaceCache    = Nothing
  }
  where
    uninitializedUniqSupply :: UniqSupply
    uninitializedUniqSupply =
      throw (FinkelException "FnkEnv: UniqSupply not initialized")
{-# INLINABLE emptyFnkEnv #-}


-- | Set current 'DynFlags' to given argument. This function also sets the
-- 'DynFlags' in interactive context.
setDynFlags :: GhcMonad m => DynFlags -> m ()
setDynFlags dflags = modifySession (updateDynFlags dflags)
{-# INLINABLE setDynFlags #-}

-- | Update 'DynFlags' to given argument. This function also sets the 'DynFlags'
-- in interactive context.
updateDynFlags :: DynFlags -> HscEnv -> HscEnv
updateDynFlags dflags hsc_env =
  -- From ghc 9.4, HomeUnitEnv data type contains its own homeUnitEnv_dflags
  -- field. HomeUnitEnv data type could be reached from hsc_unit_env field of
  -- HscEnv. Using 'hscSetFlags' function to update hsc_dflags and
  -- homeUnitEnv_dflags at once.
#if MIN_VERSION_ghc(9,4,0)
  hscSetFlags dflags (hsc_env {hsc_IC = (hsc_IC hsc_env) {ic_dflags = dflags}})
#else
  hsc_env { hsc_dflags = dflags
          , hsc_IC = (hsc_IC hsc_env) {ic_dflags = dflags}}
#endif

-- | Run given action with temporary 'DynFlags'.
withTmpDynFlags :: GhcMonad m => DynFlags -> m a -> m a
withTmpDynFlags dflags act = wrap (\_ -> setDynFlags dflags >> act)
  where
#if MIN_VERSION_ghc(9,0,0)
    wrap = bracket getDynFlags setDynFlags
#else
    wrap = gbracket getDynFlags setDynFlags
#endif
{-# INLINABLE withTmpDynFlags #-}

-- | Prepare 'DynFlags' for interactive evaluation.
prepareInterpreter :: GhcMonad m => m ()
prepareInterpreter = do
  -- See: "main''" in "ghc/Main.hs".
  hsc_env <- getSession
  let dflags0 = ic_dflags (hsc_IC hsc_env)
      dflags4 = useInterpreter dflags0
  setDynFlags dflags4
{-# INLINABLE prepareInterpreter #-}

-- | Update given 'DynFlags' to use interpreter.
useInterpreter :: DynFlags -> DynFlags
useInterpreter dflags0 =
  let platform = targetPlatform dflags0
      upd_gopt setter get_flags df =
        foldl setter df (concatMap (get_flags platform) (ways df))
      dflags1 = dflags0 { ghcLink = LinkInMemory
                        , verbosity = 1 }
#if MIN_VERSION_ghc(9,6,0)
      dflags2 = dflags1 { backend = interpreterBackend
                        , targetWays_ = hostFullWays }
#elif MIN_VERSION_ghc(9,2,0)
      dflags2 = dflags1 { backend = Interpreter
                        , targetWays_ = hostFullWays }
#elif MIN_VERSION_ghc(9,0,0)
      dflags2 = dflags1 { hscTarget = HscInterpreted
                        , ways = hostFullWays }
#else
      dflags2 = updateWays (dflags1 { hscTarget = HscInterpreted
                                    , ways = hostFullWays })
      hostFullWays = interpWays
#endif
      dflags3 = upd_gopt gopt_set wayGeneralFlags dflags2
      dflags4 = upd_gopt gopt_unset wayUnsetGeneralFlags dflags3
  in  dflags4
{-# INLINABLE useInterpreter #-}

-- | Insert new macro. This function will override existing macro.
insertMacro :: FastString -> Macro -> Fnk ()
insertMacro k v =
  modifyFnkEnv (\e -> e {envMacros = addMacro k v (envMacros e)})
{-# INLINABLE insertMacro #-}

-- | Lookup macro by name.
--
-- Lookup macro from persistent and temporary macros. When macros with
-- conflicting name exist, the latest temporary macro wins.
lookupMacro :: FastString -> FnkEnv -> Maybe Macro
lookupMacro name fnk_env = go (envTmpMacros fnk_env)
  where
    go []     = Map.lookup (MacroName name) (envMacros fnk_env)
    go (t:ts) = Map.lookup (MacroName name) t `mplus` go ts
{-# INLINABLE lookupMacro #-}

-- | Empty 'EnvMacros'.
emptyEnvMacros :: EnvMacros
emptyEnvMacros = Map.empty

-- | Make 'EnvMacros' from list of macro name and value pairs.
makeEnvMacros :: [(String, Macro)] -> EnvMacros
makeEnvMacros = Map.fromList . map (first (MacroName . fsLit))
{-# INLINABLE makeEnvMacros #-}

-- | Merge macros.
mergeMacros :: EnvMacros -> EnvMacros -> EnvMacros
mergeMacros = Map.union
{-# INLINABLE mergeMacros #-}

-- | Delete macro by macro name.
deleteMacro :: FastString -> EnvMacros -> EnvMacros
deleteMacro fs = Map.delete (MacroName fs)
{-# INLINABLE deleteMacro #-}

addMacro :: FastString -> Macro -> EnvMacros -> EnvMacros
addMacro fs = Map.insert (MacroName fs)
{-# INLINABLE addMacro #-}

-- | All macros in given macro environment, filtering out the special
-- forms.
macroNames :: EnvMacros -> [String]
macroNames = Map.foldrWithKey f []
  where
    f k m acc = case m of
                  Macro _ -> unpackFS (unMacroName k) : acc
                  _       -> acc
{-# INLINABLE macroNames #-}

-- | 'True' when given 'TyThing' is a 'Macro'.
isMacro :: HscEnv -> TyThing -> Bool
isMacro hsc_env thing = do
  let dflags = hsc_dflags hsc_env
#if MIN_VERSION_ghc(9,2,0)
      tystr = showSDocForUser dflags us alwaysQualify . ppr . varType
      us = hsc_units hsc_env
#else
      tystr = showSDocForUser dflags alwaysQualify . ppr . varType
#endif
  case thing of
    AnId var -> tystr var == "Language.Finkel.Fnk.Macro"
    _        -> False
{-# INLINABLE isMacro #-}

-- | Extract function from macro and apply to given code. Uses 'emptyFnkEnv'
-- with 'specialForms' to unwrap the macro from 'Fnk'.
macroFunction :: Macro -> Code -> Fnk Code
macroFunction mac =
  case mac of
    Macro f       -> f
    SpecialForm f -> f
{-# INLINABLE macroFunction #-}


-- ------------------------------------------------------------------------
--
-- Gensym
--
-- ------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,10,0)
type InitialUnique = Word64
#elif MIN_VERSION_ghc(9,2,0)
type InitialUnique = Word
#else
type InitialUnique = Int
#endif

-- | Generate unique symbol with @gensym'@.
gensym :: MonadUnique m => m Code
gensym = gensym' "gensym_var"
{-# INLINABLE gensym #-}

-- | Generate unique symbol with given prefix.
--
-- Note that although this function does not generate same symbol twice,
-- generated symbol has a chance to have a same name from symbols entered from
-- codes written by arbitrary users.
gensym' :: MonadUnique m => String -> m Code
gensym' prefix = do
  u <- getUniqueM
  return (LForm (genSrc (Atom (aSymbol (prefix ++ show u)))))
{-# INLINABLE gensym' #-}

-- Note: [Initialization of UniqSupply]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Test codes in finkel-kernel packages are calling the 'defaultMain' function
-- multiple times. To avoid initialization of UniqSupply multiple times, using
-- top-level IORef to detect whether the initializatio has been done or not.

-- | Variant of 'initUniqSupply' which does initialization only once.
initUniqSupply' :: InitialUnique -> Int -> IO ()
initUniqSupply' ini incr = do
  is_initialized <- readIORef uniqSupplyInitialized
  unless is_initialized
         (do initUniqSupply ini incr
             atomicModifyIORef' uniqSupplyInitialized (const (True, ())))
{-# INLINABLE initUniqSupply' #-}

-- | Top level 'IORef' for book keeping 'UniqSupply' initialization, obtained
-- with 'unsafePerformIO'.
uniqSupplyInitialized :: IORef Bool
uniqSupplyInitialized = unsafePerformIO (newIORef False)
{-# NOINLINE uniqSupplyInitialized #-}


-- ------------------------------------------------------------------------
--
-- GHC lib directory
--
-- ------------------------------------------------------------------------

-- | Read cached ghc libdir from top-level 'IORef'.
--
-- If the libdir is not cached, invoke /ghc/ command to get the libdir.
getLibDirFromGhc :: IO FilePath
getLibDirFromGhc = do
  mb_path <- readIORef globalLibDirRef
  case mb_path of
    Just path -> pure path
    Nothing -> do
      path <- initializeLibDirFromGhc
      atomicModifyIORef' globalLibDirRef $ const (Just path, path)
{-# INLINABLE getLibDirFromGhc #-}

-- | Get ghc lib directory by file layout lookup or invoking @ghc
-- --print-libdir@.
initializeLibDirFromGhc :: IO FilePath
initializeLibDirFromGhc = do
  -- Manually lookup the path of "ghc" executable, then try finding the
  -- "settings" file in installed ghc. Assuming file layouts are:
  -- is located at:
  --
  --   .../bin/ghc        <- symlink to ghc wrapper script
  --   .../lib/ghc-X.Y.Z  <- $topdir
  --
  -- To confirm that the "ghc-X.Y.Z" is indeed the library directory to return,
  -- checking the existence of "settings" file in the directory.  If the
  -- "settings" file was not found, delegating the work by invoking the "ghc"
  -- command with "--print-libdir". This is slower than additional directory and
  -- file lookups, but should be safer and more reliable.
  --
  -- See "GHC.BaseDir.getBaseDir" in "ghc-boot" package, which is doing similar
  -- work but using "getExecutablePath".
  mb_ghc_script <- findExecutable "ghc" >>= mapM canonicalizePath
  case mb_ghc_script of
    Nothing -> exitWithGhcNotFound
    Just ghc_script -> do
      let ghc_top_dir = takeDirectory (takeDirectory ghc_script)
          ghc_lib_dir0 = ghc_top_dir </> "lib" </> "ghc-" ++ cProjectVersion
#if MIN_VERSION_ghc(9,4,0)
          -- Output of "ghc --print-libdir" changed in ghc 9.4.
          ghc_lib_dir1 = ghc_lib_dir0 </> "lib"
#else
          ghc_lib_dir1 = ghc_lib_dir0
#endif
      settings_found <- doesFileExist (ghc_lib_dir1 </> "settings")
      if settings_found
         then return ghc_lib_dir1
         else do
           out <- readProcess "ghc" ["--print-libdir"] ""
           return (reverse (dropWhile isSpace (reverse out)))
{-# INLINABLE initializeLibDirFromGhc #-}

-- | Show ghc not found message and exit with 'exitFailure'.
exitWithGhcNotFound :: IO a
exitWithGhcNotFound = do
  me <- getProgName
  putStrLn $ me ++ ": Cannot find GHC executable in current PATH"
  exitFailure
{-# INLINABLE exitWithGhcNotFound #-}

-- Note: This global ghc libdir is obtained and cached at runtime, not at
-- compile time.
globalLibDirRef :: IORef (Maybe FilePath)
globalLibDirRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE globalLibDirRef #-}


-- ---------------------------------------------------------------------
--
-- Debug related functions
--
-- ---------------------------------------------------------------------

-- | 'True' when the given 'FnkDebugFlag' is turned on.
fopt :: FnkDebugFlag -> FnkEnv -> Bool
fopt flag fnk_env =
  testBit (envDumpFlags fnk_env) (fromEnum flag)
  || envVerbosity fnk_env >= verbosity_to_enable
  where
    verbosity_to_enable =
      case flag of
        -- Dump options
        Fnk_dump_dflags   -> 2
        Fnk_dump_expand   -> 2
        Fnk_dump_hs       -> 2
        Fnk_dump_session  -> 2
        -- Trace options
        Fnk_trace_expand  -> 3
        Fnk_trace_make    -> 3
        Fnk_trace_session -> 3
        Fnk_trace_spf     -> 3
{-# INLINABLE fopt #-}

-- | Turn on the given 'FnkDebugFlag'.
foptSet :: FnkDebugFlag -> FnkEnv -> FnkEnv
foptSet flag fnk_env =
  fnk_env {envDumpFlags = setBit (envDumpFlags fnk_env) (fromEnum flag)}
{-# INLINABLE foptSet #-}

-- | Update the 'envVerbosity' to given value.
setFnkVerbosity :: Int -> FnkEnv -> FnkEnv
setFnkVerbosity v fnk_env = fnk_env {envVerbosity = v}
{-# INLINABLE setFnkVerbosity #-}

-- | Dump 'SDoc's when the given 'FnkDebugFlag' is turned on.
debugWhen
  :: (MonadIO m, HasDynFlags m) => FnkEnv -> FnkDebugFlag -> [SDoc] -> m ()
debugWhen fnk_env flag mdocs =
  getDynFlags >>= \dflags -> debugWhen' dflags fnk_env flag mdocs
{-# INLINABLE debugWhen #-}

debugWhen'
  :: MonadIO m => DynFlags -> FnkEnv -> FnkDebugFlag -> [SDoc] -> m ()
debugWhen' dflags fnk_env flag mdocs =
  when (fopt flag fnk_env) (dumpSDocs dflags mdocs)
{-# INLINABLE debugWhen' #-}

dumpSDocs :: MonadIO m => DynFlags -> [SDoc] -> m ()
dumpSDocs dflags mdocs = liftIO $
  withMVar globalDumpSDocsLock $ const (pr (vcat mdocs))
  where
#if MIN_VERSION_ghc(9,2,0)
    pr = printSDocLn (initSDocContext dflags err_style)
                     (Pretty.PageMode False)
                     stderr
    err_style = defaultErrStyle
#elif MIN_VERSION_ghc(9,0,0)
    pr = printSDocLn (initSDocContext dflags err_style) Pretty.PageMode stderr
    err_style = defaultErrStyle
#else
    pr = printSDocLn Pretty.PageMode dflags stderr err_style
    err_style = defaultErrStyle dflags
#endif
{-# INLINABLE dumpSDocs #-}

-- | Get finkel debug setting from environment variable /FNK_DEBUG/.
getFnkDebug :: MonadIO m => m Bool
getFnkDebug =
  do mb_debug <- liftIO (lookupEnv "FNK_DEBUG")
     case mb_debug of
       Nothing -> return False
       Just _  -> return True
{-# INLINABLE getFnkDebug #-}

-- | Show some fields in 'DynFlags'.
dumpDynFlags :: MonadIO m => FnkEnv -> SDoc -> DynFlags -> m ()
dumpDynFlags fnk_env label dflags =
  debugWhen' dflags fnk_env Fnk_dump_dflags msgs
  where
    msgs =
      [ label
      , "  ghcLink:" <+> text (show (ghcLink dflags))
      , "  ghcMode:" <+> ppr (ghcMode dflags)
#if MIN_VERSION_ghc(9,2,0)
      , "  backend:" <+> text (show (backend dflags))
#else
      , "  hscTarget:" <+> text (show (hscTarget dflags))
#endif
#if MIN_VERSION_ghc(9,2,0)
      , "  ways:" <+> text (show (ways dflags))
#else
      , "  ways:" <+> text (show (ways dflags))
#endif
      , "  forceRecomp:" <+> text (show (gopt Opt_ForceRecomp dflags))
#if MIN_VERSION_ghc(9,0,0)
      , "  hostFullWays:" <+> text (show hostFullWays)
#else
      , "  interpWays:" <+> text (show interpWays)
#endif
      , "  importPaths:" <+> sep (map text (importPaths dflags))
#if MIN_VERSION_ghc(9,4,0)
      , "  workingDirectory:" <+> text (show (workingDirectory dflags))
      , "  num_plugins:" <+> text (show (length (pluginModNames dflags)))
      , "  opt_pp:" <+> text (show (gopt Opt_Pp dflags))
      , "  pgmF:" <+> text (toolSettings_pgm_F (toolSettings dflags))
#endif
#if !MIN_VERSION_ghc(9,4,0)
      , "  optLevel:" <+> text (show (optLevel dflags))
#endif
#if MIN_VERSION_ghc(9,2,0)
      , "  homeUnitId_:" <+> ppr (homeUnitId_ dflags)
#elif MIN_VERSION_ghc(9,0,0)
      , "  homeUnitId:" <+> ppr (homeUnitId dflags)
#else
      , "  thisInstallUnitId:" <+> ppr (thisInstalledUnitId dflags)
#endif
      , "  ldInputs:" <+> sep (map (text . showOpt) (ldInputs dflags))
#if MIN_VERSION_ghc(9,2,0)
      , "  mainModuleNameIs:" <+> ppr (mainModuleNameIs dflags)
#else
      , "  mainModIs:" <+> ppr (mainModIs dflags)
#endif
#if !MIN_VERSION_ghc(9,6,0)
      , "  mainFunIs:" <+> ppr (mainFunIs dflags)
#endif
      , "  safeHaskell:" <+> text (show (safeHaskell dflags))
      , "  lang:" <+> ppr (language dflags)
      , "  extensionFlags:" <+> ppr (EnumSet.toList (extensionFlags dflags))
      , "  includePathsQuote:" <+>
        vcat (map text (includePathsQuote (includePaths dflags)))
      , "  includePathsGlobal:" <+>
        vcat (map text (includePathsGlobal (includePaths dflags)))
      , "  picPOpts:" <+> sep (map text (picPOpts dflags))
#if MIN_VERSION_ghc(9,6,0)
      , "  opt_P_signature:" <+> ppr (snd (opt_P_signature dflags))
#else
      , "  opt_P_signature:" <+> ppr (opt_P_signature dflags)
#endif
      , "  hcSuf:" <+> text (hcSuf dflags)
#if MIN_VERSION_ghc(9,0,0)
      , "  sccProfilingOn:" <+> text (show (sccProfilingEnabled dflags))
#else
      , "  sccProfilingOn:" <+> text (show (gopt Opt_SccProfilingOn dflags))
#endif
      , "  ticky:" <+> ppr (map (`gopt` dflags) [ Opt_Ticky
                                                , Opt_Ticky_Allocd
                                                , Opt_Ticky_LNE
                                                , Opt_Ticky_Dyn_Thunk ])
      , "  debugLevel:" <+> ppr (debugLevel dflags)
      ]

-- | Show 'HomeModInfo' in 'HomePackageTable' (and 'HomeUnitGraph' in ghc >=
-- 9.4).
dumpHscEnv :: MonadIO m => FnkEnv -> SDoc -> HscEnv -> m ()
dumpHscEnv fnk_env label hsc_env =
  debugWhen' (hsc_dflags hsc_env) fnk_env Fnk_dump_session msgs
  where
    msgs =
      label : map (nest 2)
      [ "hsc_targets:" <+> ppr (hsc_targets hsc_env)
      , "hsc_hpt:" <+> pprHPT (hsc_HPT hsc_env)
#if MIN_VERSION_ghc(9,4,0)
      , "home_unit_graph:" <+> ppr (hsc_HUG hsc_env)
      , "hsc_type_env_vars:" <+> ppr (hsc_type_env_vars hsc_env)
      , "hsc_hooks (runPhaseHook):" <+>
        ppr (fmap (const ("<hook>" :: SDoc))
             (runPhaseHook (hsc_hooks hsc_env)))
#endif
      ]

-- XXX: Unsafe global lock to avoid mixing up messages in concurrent settings.
-- When FnkEnv is shared, better to add a MVar field in the shared FnkEnv for
-- such purpose (But FnkEnv is not shared in parsedResultAction).
globalDumpSDocsLock :: MVar ()
globalDumpSDocsLock = unsafePerformIO (newMVar ())
{-# NOINLINE globalDumpSDocsLock #-}
