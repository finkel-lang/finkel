-- | Wrapper for Finkel code compilation monad.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , failS
  , finkelSrcError
  , emptyFnkEnv
  , getFnkEnv
  , putFnkEnv
  , modifyFnkEnv
  , setDynFlags
  , setContextModules

  -- * Exception
  , FinkelException(..)
  , throwFinkelException
  , throwFinkelExceptionIO
  , handleFinkelException

  -- * Debugging
  , debugFnk
  , getFnkDebug
  , dumpDynFlags

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
  ) where

#include "Syntax.h"

-- base
import           Control.Exception      (Exception (..), throw, throwIO)
import           Control.Monad          (mplus, unless, when)

#if !MIN_VERSION_ghc(8,8,0)
import           Control.Monad.Fail     (MonadFail (..))
#endif

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef, writeIORef)
import           System.Environment     (lookupEnv)
import           System.IO              (stderr)
import           System.IO.Unsafe       (unsafePerformIO)

-- containers
import qualified Data.Map               as Map

-- ghc
import           Bag                    (unitBag)
import           DynFlags               (DynFlags (..), GeneralFlag (..),
                                         HasDynFlags (..), gopt, interpWays)
import           ErrUtils               (MsgDoc, mkErrMsg)
import           Exception              (ExceptionMonad (..), ghandle)
import           FastString             (FastString, fsLit, unpackFS)
import           GHC                    (runGhc)
import           GhcMonad               (Ghc (..), GhcMonad (..),
                                         getSessionDynFlags, modifySession)
import           HscMain                (Messager, batchMsg)
import           HscTypes               (HomeModInfo, HscEnv (..),
                                         InteractiveContext (..),
                                         InteractiveImport (..), TyThing (..),
                                         mkSrcErr)
import           InteractiveEval        (setContext)
import           Module                 (ModuleName, mkModuleName)
import           Outputable             (alwaysQualify, defaultErrStyle,
                                         neverQualify, ppr, printSDocLn, sep,
                                         showSDocForUser, text, vcat, (<+>))
import qualified Pretty
import           SrcLoc                 (GenLocated (..), Located)
import           UniqSupply             (initUniqSupply, mkSplitUniqSupply,
                                         uniqFromSupply)
import           Var                    (varType)

import           GHC_Hs_ImpExp          (simpleImportDecl)

-- Import for Option
#if MIN_VERSION_ghc(8,10,0)
import           CliOption              (showOpt)
#else
import           DynFlags               (showOpt)
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
handleFinkelException = ghandle


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

-- | Environment state in 'Fnk'.
data FnkEnv = FnkEnv
   { -- | Macros accessible in current compilation context.
     envMacros                 :: EnvMacros
     -- | Temporary macros in current compilation context.
   , envTmpMacros              :: [EnvMacros]
     -- | Default set of macros, these macros will be used when
     -- resetting 'FnkEnv'.
   , envDefaultMacros          :: EnvMacros
     -- | Flag to hold debug setting.
   , envDebug                  :: Bool
     -- | Modules to import to context.
   , envContextModules         :: [String]
     -- | Flag for controling informative output.
   , envSilent                 :: Bool

     -- | The default 'DynFlags', possibly containing settings from command line.
   , envDefaultDynFlags        :: Maybe DynFlags
     -- | 'DynFlags' used by the 'simpleMake' function.
   , envMakeDynFlags           :: Maybe DynFlags

     -- | Messager used in make.
   , envMessager               :: Messager
     -- | Required modules names in current target.
   , envRequiredModuleNames    :: [Located String]
     -- | Compile home modules during macro-expansion of /require/.
   , envCompiledInRequire      :: [(ModuleName, HomeModInfo)]

     -- | Whether to dump Haskell source code or not.
   , envDumpHs                 :: Bool
     -- | Directory to save generated Haskell source codes.
   , envHsDir                  :: Maybe FilePath

     -- | Lib directory passed to 'runGhc'.
   , envLibDir                 :: Maybe FilePath

     -- | Whether to use qualified name for primitive functions used in quoting
     -- codes.
   , envQualifyQuotePrimitives :: Bool

     -- | The 'HscEnv' used by the byte-code interpreter for macro expansion.
   , envSessionForExpand       :: Maybe HscEnv
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
  fail = failS
  {-# INLINE fail #-}

instance MonadIO Fnk where
  liftIO io = Fnk (\_ -> liftIO io)
  {-# INLINE liftIO #-}

instance ExceptionMonad Fnk where
  gcatch m h =
    Fnk (\ref -> unFnk m ref `gcatch` \e -> unFnk (h e) ref)
  {-# INLINE gcatch #-}
  gmask f =
    Fnk (\ref ->
           gmask (\r -> let r' m = Fnk (\ref' -> r (unFnk m ref'))
                        in  unFnk (f r') ref))
  {-# INLINE gmask #-}

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
runFnk m fnkc_env = do
  ref <- newIORef fnkc_env
  runGhc (envLibDir fnkc_env) (toGhc m (FnkEnvRef ref))

-- | Extract 'Ghc' from 'Fnk'.
toGhc :: Fnk a -> FnkEnvRef -> Ghc a
toGhc = unFnk
{-# INLINE toGhc #-}

-- | Lift 'Ghc' to 'Fnk'.
fromGhc :: Ghc a -> Fnk a
fromGhc m = Fnk (\_ -> m)
{-# INLINE fromGhc #-}

-- | Get current 'FnkEnv'.
getFnkEnv :: Fnk FnkEnv
getFnkEnv = Fnk (\(FnkEnvRef ref) -> liftIO (readIORef ref))
{-# INLINE getFnkEnv #-}

-- | Set current 'FnkEnv' to given argument.
putFnkEnv :: FnkEnv -> Fnk ()
putFnkEnv fnkc_env =
  Fnk (\(FnkEnvRef ref) -> fnkc_env `seq` liftIO (writeIORef ref fnkc_env))
{-# INLINE putFnkEnv #-}

-- | Update 'FnkEnv' with applying given function to current 'FnkEnv'.
modifyFnkEnv :: (FnkEnv -> FnkEnv) -> Fnk ()
modifyFnkEnv f =
  Fnk (\(FnkEnvRef ref) ->
         liftIO (atomicModifyIORef' ref (\fnkc_env -> (f fnkc_env, ()))))
{-# INLINE modifyFnkEnv #-}

-- | Throw 'FinkelException' with given message.
failS :: String -> Fnk a
failS msg = liftIO (throwIO (FinkelException msg))

-- | Throw a 'SourceError'.
finkelSrcError :: Code -> String -> Fnk a
finkelSrcError (LForm (L l _)) msg = do
  dflags <- getSessionDynFlags
  let em = mkErrMsg dflags l neverQualify (text msg)
  liftIO (throwIO (mkSrcErr (unitBag em)))

-- | Print given 'MsgDoc's to 'stderr' when degbug flag is on.
debugFnk :: [MsgDoc] -> Fnk ()
debugFnk mdocs = do
  fnk_env <- getFnkEnv
  when (envDebug fnk_env)
       (do dflags <- getDynFlags
           liftIO (printSDocLn Pretty.PageMode
                               dflags
                               stderr
                               (defaultErrStyle dflags)
                               (vcat mdocs)))
{-# INLINE debugFnk #-}

-- | Get finkel debug setting from environment variable /FNK_DEBUG/.
getFnkDebug :: MonadIO m => m Bool
getFnkDebug =
  do mb_debug <- liftIO (lookupEnv "FNK_DEBUG")
     case mb_debug of
       Nothing -> return False
       Just _  -> return True
{-# INLINE getFnkDebug #-}

-- | Show some fields in 'DynFlags'.
dumpDynFlags :: MsgDoc -> DynFlags -> Fnk ()
dumpDynFlags label dflags =
  debugFnk
    [ label
    , "DynFlags:"
    , "  ghcLink:" <+> text (show (ghcLink dflags))
    , "  ghcMode:" <+> ppr (ghcMode dflags)
    , "  hscTarget:" <+> text (show (hscTarget dflags))
    , "  ways:" <+> text (show (ways dflags))
    , "  forceRecomp:" <+> text (show (gopt Opt_ForceRecomp dflags))
    , "  interpWays:" <+> text (show interpWays)
    , "  importPaths:" <+> sep (map text (importPaths dflags))
    , "  optLevel:" <+> text (show (optLevel dflags))
    , "  thisInstallUnitId:" <+> ppr (thisInstalledUnitId dflags)
    , "  ldInputs: " <+> sep (map (text . showOpt) (ldInputs dflags))]

-- | Empty 'FnkEnv' for performing computation with 'Fnk'.
emptyFnkEnv :: FnkEnv
emptyFnkEnv = FnkEnv
  { envMacros                 = emptyEnvMacros
  , envTmpMacros              = []
  , envDefaultMacros          = emptyEnvMacros
  , envDebug                  = False
  , envContextModules         = []
  , envSilent                 = False
  , envMakeDynFlags           = Nothing
  , envDefaultDynFlags        = Nothing
  , envMessager               = batchMsg
  , envRequiredModuleNames    = []
  , envCompiledInRequire      = []
  , envDumpHs                 = False
  , envHsDir                  = Nothing
  , envLibDir                 = Nothing
  , envQualifyQuotePrimitives = False
  , envSessionForExpand       = Nothing }

-- | Set current 'DynFlags' to given argument. This function also modifies
-- 'DynFlags' in interactive context.
setDynFlags :: DynFlags -> Fnk ()
setDynFlags dflags =
  modifySession (\h -> h { hsc_dflags = dflags
                         , hsc_IC = (hsc_IC h) {ic_dflags = dflags}})
{-# INLINE setDynFlags #-}

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
lookupMacro name fnkc_env = go (envTmpMacros fnkc_env)
  where
    go []     = Map.lookup name (envMacros fnkc_env)
    go (t:ts) = Map.lookup name t `mplus` go ts
{-# INLINE lookupMacro #-}

-- | Empty 'EnvMacros'.
emptyEnvMacros :: EnvMacros
emptyEnvMacros = Map.empty

-- | Make 'EnvMacros' from list of pair of macro name and value.
makeEnvMacros :: [(String, Macro)] -> EnvMacros
makeEnvMacros = Map.fromList . map (\(n,m) -> (fsLit n, m))

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
gensym = gensym' "g"

-- | Generate unique symbol with given prefix.
--
-- Note that although this function does not generate same symbol twice,
-- generated symbol has a chance to have a same name from symbols entered from
-- codes written by arbitrary users.
gensym' :: String -> Fnk Code
gensym' prefix = do
  s <- liftIO (mkSplitUniqSupply '_')
  let u = uniqFromSupply s
  return (LForm (genSrc (Atom (aSymbol (prefix ++ show u)))))

-- Note: Initialization of UniqSupply
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Test codes in finkel-kernel packages uses the 'defaultMain' function multiple
-- times. To avoid initialization of UniqSupply multiple time, using top-level
-- IORef to detect whether the initializatio has been done or not.

-- | Variant of 'initUniqSupply' which does initialization only once.
initUniqSupply' :: Int -> Int -> IO ()
initUniqSupply' ini incr = do
  is_initialized <- readIORef uniqSupplyInitialized
  unless is_initialized
         (do initUniqSupply ini incr
             atomicModifyIORef' uniqSupplyInitialized (\_ -> (True, ())))

-- | Top level 'IORef' for book keeping 'UniqSupply' initialization, obtained
-- with 'unsafePerformIO'.
uniqSupplyInitialized :: IORef Bool
uniqSupplyInitialized = unsafePerformIO (newIORef False)
{-# NOINLINE uniqSupplyInitialized #-}
