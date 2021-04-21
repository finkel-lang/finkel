-- | Wrapper for Finkel code compilation monad.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Finkel.Fnk
  ( -- * Finkel compiler monad
    Fnk(..)
  , FnkEnv(..)
  , FnkEnvRef(..)
  , Macro(..)
  , MacroFunction
  , EnvMacros
  , runFnk
  , toGhc
  , fromGhc
  , failFnk
  , finkelSrcError
  , emptyFnkEnv
  , getFnkEnv
  , putFnkEnv
  , modifyFnkEnv
  , setDynFlags
  , withTmpDynFlags
  , setContextModules
  , prepareInterpreter

  -- * Exception
  , FinkelException(..)
  , throwFinkelException
  , throwFinkelExceptionIO
  , handleFinkelException

  -- * Debugging
  , FnkDebugFlag(..)
  , fopt
  , fopt_set
  , setFnkVerbosity
  , debugWhen
  , debugWhen'
  , dumpDynFlags
  , getFnkDebug

  -- * Command line option handlings
  , fnkEnvOptions
  , fnkEnvOptionsWithLib
  , partitionFnkEnvOptions
  , fromFnkEnvOptions
  , fnkEnvOptionsUsage

  -- * Macro related functions
  , emptyEnvMacros
  , insertMacro
  , lookupMacro
  , makeEnvMacros
  , mergeMacros
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
import           Control.Exception       (Exception (..), throw, throwIO)
import           Control.Monad           (mplus, unless, when)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Bifunctor          (first)
import           Data.Bits               (setBit, testBit, zeroBits)
import           Data.Char               (isSpace, toLower)
import           Data.IORef              (IORef, atomicModifyIORef',
                                          atomicWriteIORef, newIORef, readIORef)
import           Data.List               (isPrefixOf, partition)
import           Data.Word               (Word8)
import           System.Console.GetOpt   (ArgDescr (..), OptDescr (..),
                                          usageInfo)
import           System.Environment      (lookupEnv)
import           System.IO               (stderr)
import           System.IO.Unsafe        (unsafePerformIO)

#if !MIN_VERSION_ghc(8,8,0)
import           Control.Monad.Fail      (MonadFail (..))
#endif

-- containers
import qualified Data.Map                as Map

import           System.Directory        (canonicalizePath, doesFileExist,
                                          findExecutable)
import           System.FilePath         (takeDirectory, (</>))

-- exceptions
import           Control.Monad.Catch     (MonadCatch (..), MonadMask (..),
                                          MonadThrow (..))

#if MIN_VERSION_ghc(9,0,0)
import           Control.Monad.Catch     (bracket, handle)
#endif

-- process
import           System.Process          (readProcess)

-- ghc
import           GHC                     (runGhc)
import           GHC_Data_Bag            (unitBag)
import           GHC_Data_FastString     (FastString, fsLit, unpackFS)
import           GHC_Driver_Main         (Messager, batchMsg)
import           GHC_Driver_Monad        (Ghc (..), GhcMonad (..),
                                          getSessionDynFlags, modifySession)
import           GHC_Driver_Session      (DynFlags (..), GeneralFlag (..),
                                          GhcLink (..), HasDynFlags (..),
                                          HscTarget (..), gopt, gopt_set,
                                          gopt_unset, picPOpts)
import           GHC_Driver_Types        (HscEnv (..), InteractiveContext (..),
                                          InteractiveImport (..),
                                          ModSummary (..), TyThing (..),
                                          mkSrcErr)
import           GHC_Hs_ImpExp           (simpleImportDecl)
import           GHC_Runtime_Eval        (setContext)
import           GHC_Settings_Config     (cProjectVersion)
import           GHC_Types_SrcLoc        (GenLocated (..))
import           GHC_Types_Unique_Supply (MonadUnique (..), UniqSupply,
                                          initUniqSupply, mkSplitUniqSupply,
                                          splitUniqSupply, takeUniqFromSupply)
import           GHC_Types_Var           (varType)
import           GHC_Unit_Module         (mkModuleName)
import           GHC_Utils_CliOption     (showOpt)
import           GHC_Utils_Error         (MsgDoc, mkErrMsg)
import           GHC_Utils_Outputable    (alwaysQualify, defaultErrStyle,
                                          neverQualify, ppr, printSDocLn, sep,
                                          showSDocForUser, text, vcat, (<+>))
import qualified GHC_Utils_Ppr           as Pretty

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.Ways         (hostFullWays, wayGeneralFlags,
                                          wayUnsetGeneralFlags)
import           GHC_Driver_Session      (initSDocContext, sccProfilingEnabled)
import           GHC_Utils_Exception     (ExceptionMonad)
#else
import           GHC_Driver_Ways         (interpWays, updateWays,
                                          wayGeneralFlags, wayUnsetGeneralFlags)
import           GHC_Utils_Exception     (ExceptionMonad (..), ghandle)
#endif

#if !MIN_VERSION_ghc(8,10,0)
import           GHC_Driver_Session      (targetPlatform)
#endif

#if MIN_VERSION_ghc(8,6,0)
import           GHC_Driver_Session      (IncludeSpecs (..), opt_P_signature)
#endif

#if MIN_VERSION_ghc(8,4,0)
import qualified GHC_Data_EnumSet        as FlagSet
#else
import qualified Data.IntSet             as FlagSet
#endif

-- Internal
import           Language.Finkel.Form


-- ---------------------------------------------------------------------
--
-- Exception
--
-- ---------------------------------------------------------------------

newtype FinkelException = FinkelException String
  deriving (Eq, Show)

instance Exception FinkelException

throwFinkelException :: FinkelException -> a
throwFinkelException = throw

throwFinkelExceptionIO :: FinkelException -> IO a
throwFinkelExceptionIO = throwIO

handleFinkelException :: ExceptionMonad m
                      => (FinkelException -> m a) -> m a -> m a
#if MIN_VERSION_ghc(9,0,0)
handleFinkelException = handle
#else
handleFinkelException = ghandle
#endif


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
type EnvMacros = Map.Map FastString Macro

-- | Data type for debug information.
data FnkDebugFlag
  = Fnk_dump_dflags
  | Fnk_dump_expand
  | Fnk_dump_hs
  | Fnk_trace_expand
  | Fnk_trace_make
  | Fnk_trace_spf
  deriving (Eq, Show, Enum)

-- | Type synonym for holding on/off of 'FnkDebugFlag'.
type FlagSet = Word8 -- Word8 is enough for now.

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
   , envDefaultDynFlags        :: Maybe DynFlags

     -- | Messager used in make.
   , envMessager               :: Messager
     -- | Required home package modules names in current target.
   , envRequiredHomeModules    :: [ModSummary]

     -- | Directory to save generated Haskell source codes.
   , envHsOutDir               :: Maybe FilePath

     -- | Lib directory passed to 'runGhc'.
   , envLibDir                 :: Maybe FilePath

     -- | Whether to use qualified name for primitive functions used in quoting
     -- codes.
   , envQualifyQuotePrimitives :: Bool

     -- | The 'HscEnv' used by the byte-code interpreter for macro expansion.
   , envSessionForExpand       :: Maybe HscEnv
     -- | The 'UniqSupply' for 'gensym'.
   , envUniqSupply             :: UniqSupply

     -- | Verbosity level for Fnk related messages.
   , envVerbosity              :: {-# UNPACK #-} !Int
     -- | Dump flag settings.
   , envDumpFlags              :: {-# UNPACK #-} !FlagSet
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
  return x = Fnk (\_ -> return x)
  {-# INLINE return #-}
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
  getDynFlags = Fnk (\_ -> getDynFlags)
  {-# INLINE getDynFlags #-}

instance GhcMonad Fnk where
  getSession = Fnk (\_ -> getSession)
  {-# INLINE getSession #-}
  setSession hsc_env = Fnk (\_ -> setSession hsc_env)
  {-# INLINE setSession #-}

-- | Run 'Fnk' with given environment.
runFnk :: Fnk a -> FnkEnv -> IO a
runFnk m fnk_env = do
  us <- mkSplitUniqSupply '_'
  ref <- newIORef $! fnk_env {envUniqSupply=us}
  libdir <- case envLibDir fnk_env of
    Just path -> return path
    Nothing   -> getLibDirFromGhc
  runGhc (Just libdir) (toGhc m (FnkEnvRef ref))

-- | Get ghc lib directory by file layout lookup or invoking @ghc
-- --print-libdir@.
getLibDirFromGhc :: IO FilePath
getLibDirFromGhc = do
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
    Nothing -> throwIO (FinkelException "cannot find ghc in current PATH")
    Just ghc_script -> do
      let ghc_top_dir = takeDirectory (takeDirectory ghc_script)
          ghc_lib_dir = ghc_top_dir </> "lib" </> "ghc-" ++ cProjectVersion
      settings_found <- doesFileExist (ghc_lib_dir </> "settings")
      if settings_found
         then return ghc_lib_dir
         else do
           out  <- readProcess "ghc" ["--print-libdir"] ""
           return (reverse (dropWhile isSpace (reverse out)))
{-# INLINABLE getLibDirFromGhc #-}

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
failFnk msg = liftIO (throwIO (FinkelException msg))

-- | Throw a 'SourceError'.
finkelSrcError :: Code -> String -> Fnk a
finkelSrcError (LForm (L l _)) msg = do
  dflags <- getSessionDynFlags
  let em = mkErrMsg dflags l neverQualify (text msg)
  liftIO (throwIO (mkSrcErr (unitBag em)))

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
  }
  where
    uninitializedUniqSupply :: UniqSupply
    uninitializedUniqSupply =
      throwFinkelException (FinkelException "UniqSupply not initialized")

-- | Set current 'DynFlags' to given argument. This function also sets the
-- 'DynFlags' in interactive context.
setDynFlags :: GhcMonad m => DynFlags -> m ()
setDynFlags dflags =
  modifySession (\h -> h { hsc_dflags = dflags
                         , hsc_IC = (hsc_IC h) {ic_dflags = dflags}})
{-# INLINABLE setDynFlags #-}

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
      platform = targetPlatform dflags0
      dflags1 = dflags0 {ghcLink = LinkInMemory
                        ,hscTarget = HscInterpreted
                        ,verbosity = 1}
#if MIN_VERSION_ghc(9,0,0)
      updateWays = id
#else
      hostFullWays = interpWays
#endif
      dflags2 = updateWays (dflags1 {ways = hostFullWays})
      dflags3 = foldl gopt_set dflags2
                      (concatMap (wayGeneralFlags platform) hostFullWays)
      dflags4 = foldl gopt_unset dflags3
                      (concatMap (wayUnsetGeneralFlags platform) hostFullWays)
  setDynFlags dflags4

-- | Set context modules in current session to given modules.
setContextModules :: [String] -> Fnk ()
setContextModules names =
  setContext (map (IIDecl . simpleImportDecl . mkModuleName) names)

-- | Insert new macro. This function will override existing macro.
insertMacro :: FastString -> Macro -> Fnk ()
insertMacro k v =
  modifyFnkEnv (\e -> e {envMacros = Map.insert k v (envMacros e)})

-- | Lookup macro by name.
--
-- Lookup macro from persistent and temporary macros. When macros with
-- conflicting name exist, the latest temporary macro wins.
lookupMacro :: FastString -> FnkEnv -> Maybe Macro
lookupMacro name fnk_env = go (envTmpMacros fnk_env)
  where
    go []     = Map.lookup name (envMacros fnk_env)
    go (t:ts) = Map.lookup name t `mplus` go ts
{-# INLINABLE lookupMacro #-}

-- | Empty 'EnvMacros'.
emptyEnvMacros :: EnvMacros
emptyEnvMacros = Map.empty

-- | Make 'EnvMacros' from list of pair of macro name and value.
makeEnvMacros :: [(String, Macro)] -> EnvMacros
makeEnvMacros = Map.fromList . map (first fsLit)

-- | Merge macros.
mergeMacros :: EnvMacros -> EnvMacros -> EnvMacros
mergeMacros = Map.union

-- | Delete macro by macro name.
deleteMacro :: FastString -> EnvMacros -> EnvMacros
deleteMacro = Map.delete

-- | All macros in given macro environment, filtering out the special
-- forms.
macroNames :: EnvMacros -> [String]
macroNames = Map.foldrWithKey f []
  where
    f k m acc = case m of
                  Macro _ -> unpackFS k : acc
                  _       -> acc

-- | 'True' when given 'TyThing' is a 'Macro'.
isMacro :: DynFlags -> TyThing -> Bool
isMacro dflags thing =
  case thing of
    AnId var -> showSDocForUser dflags alwaysQualify (ppr (varType var))
                == "Language.Finkel.Fnk.Macro"
    _        -> False

-- | Extract function from macro and apply to given code. Uses 'emptyFnkEnv'
-- with 'specialForms' to unwrap the macro from 'Fnk'.
macroFunction :: Macro -> Code -> Fnk Code
macroFunction mac form =
  let fn = case mac of
             Macro f       -> f
             SpecialForm f -> f
  in  fn form

-- | Generate unique symbol with @gensym'@.
gensym :: Fnk Code
gensym = gensym' "gensym_var"

-- | Generate unique symbol with given prefix.
--
-- Note that although this function does not generate same symbol twice,
-- generated symbol has a chance to have a same name from symbols entered from
-- codes written by arbitrary users.
gensym' :: String -> Fnk Code
gensym' prefix = do
  u <- getUniqueM
  return (LForm (genSrc (Atom (aSymbol (prefix ++ show u)))))

-- Note: [Initialization of UniqSupply]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Test codes in finkel-kernel packages are calling the 'defaultMain' function
-- multiple times. To avoid initialization of UniqSupply multiple times, using
-- top-level IORef to detect whether the initializatio has been done or not.

-- | Variant of 'initUniqSupply' which does initialization only once.
initUniqSupply' :: Int -> Int -> IO ()
initUniqSupply' ini incr = do
  is_initialized <- readIORef uniqSupplyInitialized
  unless is_initialized
         (do initUniqSupply ini incr
             atomicModifyIORef' uniqSupplyInitialized (const (True, ())))

-- | Top level 'IORef' for book keeping 'UniqSupply' initialization, obtained
-- with 'unsafePerformIO'.
uniqSupplyInitialized :: IORef Bool
uniqSupplyInitialized = unsafePerformIO (newIORef False)
{-# NOINLINE uniqSupplyInitialized #-}


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
        Fnk_dump_dflags  -> 2
        Fnk_dump_expand  -> 2
        Fnk_dump_hs      -> 2
        Fnk_trace_expand -> 3
        Fnk_trace_make   -> 3
        Fnk_trace_spf    -> 3
{-# INLINABLE fopt #-}

-- | Turn on the given 'FnkDebugFlag'.
fopt_set :: FnkDebugFlag -> FnkEnv -> FnkEnv
fopt_set flag fnk_env =
  fnk_env {envDumpFlags = setBit (envDumpFlags fnk_env) (fromEnum flag)}
{-# INLINABLE fopt_set #-}

-- | Update the 'envVerbosity' to given value.
setFnkVerbosity :: Int -> FnkEnv -> FnkEnv
setFnkVerbosity v fnk_env = fnk_env {envVerbosity = v}
{-# INLINABLE setFnkVerbosity #-}

-- | Dump 'MsgDoc's when the given 'FnkDebugFlag' is turned on.
debugWhen
  :: (MonadIO m, HasDynFlags m) => FnkEnv -> FnkDebugFlag -> [MsgDoc] -> m ()
debugWhen fnk_env flag mdocs =
  getDynFlags >>= \dflags -> debugWhen' dflags fnk_env flag mdocs
{-# INLINABLE debugWhen #-}

debugWhen'
  :: MonadIO m => DynFlags -> FnkEnv -> FnkDebugFlag -> [MsgDoc] -> m ()
debugWhen' dflags fnk_env flag mdocs =
  when (fopt flag fnk_env) (dumpMsgDocs dflags mdocs)
{-# INLINABLE debugWhen' #-}

dumpMsgDocs :: MonadIO m => DynFlags -> [MsgDoc] -> m ()
dumpMsgDocs dflags mdocs = liftIO (pr (vcat mdocs))
  where
#if MIN_VERSION_ghc(9,0,0)
    pr = printSDocLn (initSDocContext dflags err_style) Pretty.PageMode stderr
    err_style = defaultErrStyle
#else
    pr = printSDocLn Pretty.PageMode dflags stderr err_style
    err_style = defaultErrStyle dflags
#endif
{-# INLINABLE dumpMsgDocs #-}

-- | Get finkel debug setting from environment variable /FNK_DEBUG/.
getFnkDebug :: MonadIO m => m Bool
getFnkDebug =
  do mb_debug <- liftIO (lookupEnv "FNK_DEBUG")
     case mb_debug of
       Nothing -> return False
       Just _  -> return True
{-# INLINABLE getFnkDebug #-}

-- | Show some fields in 'DynFlags'.
dumpDynFlags
  :: (MonadIO m, HasDynFlags m) => FnkEnv -> MsgDoc -> DynFlags -> m ()
dumpDynFlags fnk_env label dflags = debugWhen fnk_env Fnk_dump_dflags msgs
  where
    msgs =
      [ label
      , "DynFlags:"
      , "  ghcLink:" <+> text (show (ghcLink dflags))
      , "  ghcMode:" <+> ppr (ghcMode dflags)
      , "  hscTarget:" <+> text (show (hscTarget dflags))
      , "  ways:" <+> text (show (ways dflags))
      , "  forceRecomp:" <+> text (show (gopt Opt_ForceRecomp dflags))
#if MIN_VERSION_ghc(9,0,0)
      , "  hostFullWays:" <+> text (show hostFullWays)
#else
      , "  interpWays:" <+> text (show interpWays)
#endif
      , "  importPaths:" <+> sep (map text (importPaths dflags))
      , "  optLevel:" <+> text (show (optLevel dflags))
#if MIN_VERSION_ghc(9,0,0)
      , "  homeUnitId:" <+> ppr (homeUnitId dflags)
#else
      , "  thisInstallUnitId:" <+> ppr (thisInstalledUnitId dflags)
#endif
      , "  ldInputs:" <+> sep (map (text . showOpt) (ldInputs dflags))
      , "  mainModIs:" <+> ppr (mainModIs dflags)
      , "  mainFunIs:" <+> ppr (mainFunIs dflags)
      , "  safeHaskell:" <+> text (show (safeHaskell dflags))
      , "  lang:" <+> ppr (language dflags)
      , "  extensionFlags:" <+> ppr (FlagSet.toList (extensionFlags dflags))
#if MIN_VERSION_ghc(8,6,0)
      , "  includePathsQuote:" <+>
        vcat (map text (includePathsQuote (includePaths dflags)))
      , "  includePathsGlobal:" <+>
        vcat (map text (includePathsGlobal (includePaths dflags)))
#else
      , "  includePaths:" <+> vcat (map text (includePaths dflags))
#endif
      , "  picPOpts:" <+> sep (map text (picPOpts dflags))
#if MIN_VERSION_ghc(8,6,0)
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


-- ---------------------------------------------------------------------
--
-- Command line option handling
--
-- ---------------------------------------------------------------------

-- | Separate Finkel debug options from others.
partitionFnkEnvOptions
   :: [String]
   -- ^ Flag inputs, perhaps given as command line arguments.
   -> ([String], [String])
   -- ^ Pair of @(finkel_flags, other_flags)@.
partitionFnkEnvOptions = partition test
  where
    -- The "-B" option is to update the ghc libdir in FnkEnv.
    test arg = "--fnk-" `isPrefixOf` arg || "-B" `isPrefixOf` arg

-- | Command line option handlers to update 'FnkDumpFlag' in 'FnkEnv'.
fnkEnvOptions :: [OptDescr (FnkEnv -> FnkEnv)]
fnkEnvOptions =
  [ opt ["fnk-verbose"]
        (ReqArg (\i o -> o {envVerbosity = parseVerbosity i}) "INT")
        "Set verbosity level to INT."
  , opt ["fnk-hsdir"]
        (ReqArg (\path o -> o {envHsOutDir = Just path}) "DIR")
        "Set Haskell code output directory to DIR."

  -- Dump and trace options
  , debug_opt Fnk_dump_dflags "Dump DynFlags settings."
  , debug_opt Fnk_dump_expand "Dump expanded code."
  , debug_opt Fnk_dump_hs "Dump Haskell source code."
  , debug_opt Fnk_trace_expand "Trace macro expansion."
  , debug_opt Fnk_trace_make "Trace make function."
  , debug_opt Fnk_trace_spf "Trace builtin special forms."
  ]
  where
    opt = Option []
    debug_opt flag descr = opt [to_str flag] (NoArg (fopt_set flag)) descr
    to_str = map replace . show
    replace '_' = '-'
    replace c   = toLower c
    parseVerbosity str =
      case reads str of
        [(n, "")] -> n
        _ -> throwFinkelException
               (FinkelException
                 ("expecting Int value for verbosity but got " ++ show str))

-- | Options for @FnkEnv@ with an option to set ghc @libdir@.
fnkEnvOptionsWithLib :: [OptDescr (FnkEnv -> FnkEnv)]
fnkEnvOptionsWithLib = lib_option : fnkEnvOptions
  where
    lib_option =
      Option ['B'] []
             (ReqArg (\path o -> o {envLibDir = Just path}) "DIR")
             "Set ghc library directory to DIR."

-- | Convert 'fnkEnvOptions' to list of 'OptDescr' taking a function modifying
-- 'FnkEnv'.
fromFnkEnvOptions :: ((FnkEnv -> FnkEnv) -> a) -> [OptDescr a]
fromFnkEnvOptions f = map (fmap f) fnkEnvOptionsWithLib

-- | Usage information for 'fnkEnvOptions', without @-B@ option.
fnkEnvOptionsUsage :: String -> String
fnkEnvOptionsUsage header = usageInfo header fnkEnvOptions
