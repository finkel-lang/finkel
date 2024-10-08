{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
-- | Simple executable for profiling.
--
-- Simple executable to wrap some simple actions.
module Main where

#include "ghc_modules.h"

-- base
import           Control.Monad.IO.Class       (MonadIO (..))
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.IO                    (Handle, stdout)

-- filepath
import qualified System.FilePath              as FilePath

-- ghc
import qualified GHC                          as GHC
import           GHC_Data_StringBuffer        (hGetStringBuffer)
import           GHC_Driver_Ppr               (printForUser)
import           GHC_Driver_Session           (DynFlags, GeneralFlag (..),
                                               HasDynFlags (..), gopt_set)
import           GHC_Types_Basic              (SuccessFlag (..))
import           GHC_Types_SrcLoc             (mkGeneralLocated)
import           GHC_Utils_Outputable         (Outputable (..), SDoc,
                                               neverQualify)
#if MIN_VERSION_ghc(9,8,0)
import           GHC.Types.Error              (defaultDiagnosticOpts)
#elif MIN_VERSION_ghc(9,6,0)
import           GHC.Types.Error              (Diagnostic (..))
#endif

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Driver.Errors.Types      (GhcMessage)
import           GHC.Utils.Outputable         (NamePprCtx)
#else
import           GHC_Utils_Outputable         (PrintUnqualified)
#endif

#if MIN_VERSION_ghc(9,4,0)
import           GHC.Driver.Config.Diagnostic (initDiagOpts)
import           GHC.Driver.Errors            (printMessages)
#else
import           GHC_Driver_Errors            (printBagOfErrors)
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Utils.Logger             (HasLogger (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC_Utils_Outputable         (Depth (..))
#endif

-- finkel-kernel
import qualified Language.Finkel.Builder      as Builder
import qualified Language.Finkel.Emit         as Emit
import qualified Language.Finkel.Expand       as Expand
import qualified Language.Finkel.Fnk          as Fnk
import qualified Language.Finkel.Lexer        as Lexer
import qualified Language.Finkel.Make         as Make
import qualified Language.Finkel.Reader       as Reader
import qualified Language.Finkel.SpecialForms as SpecialForms
import qualified Language.Finkel.Syntax       as Syntax

main :: IO ()
main =
  do args <- getArgs
     case args of
       -- ["count", file]  -> countTokens file
       ["expand", file] -> printExpandedForms file
       ["parse", file]  -> printForms file
       ["ppr", file]    -> pprFile file
       ["hsrc", file]   -> printHsrc file
       ["lex", file]    -> printTokens file
       "make" : files   -> doMake files
       _                -> usage

usage :: IO ()
usage =
  putStrLn
    (unlines
       ["usage: profile MODE ARGS"
       ,""
       ,"MODE:"
       -- ,"  count  - count number of forms"
       ,"  expand - print expanded forms"
       ,"  parse  - parse input file and print resulting forms"
       ,"  ppr    - pretty print haskell or finkel module with `ppr'"
       ,"  hsrc   - convert Finkel source to Haskell source"
       ,"  lex    - lex input file and print resulting tokens"
       ,"  make   - compile given files to object code"])

printExpandedForms :: FilePath -> IO ()
printExpandedForms path = Fnk.runFnk go SpecialForms.defaultFnkEnv
  where
    go = do
      Make.initSessionForMake
      contents <- liftIO (hGetStringBuffer path)
      (forms, _) <- Reader.parseSexprs (Just path) contents
      forms' <- Make.withExpanderSettings (Expand.expands forms)
      liftIO (mapM_ print forms')

printForms :: FilePath -> IO ()
printForms path =
  do contents <- hGetStringBuffer path
     case Lexer.evalSP Reader.sexprs (Just path) contents of
       Right forms -> mapM_ print forms
       Left err    -> print err

pprFile :: FilePath -> IO ()
pprFile path
  | ext == ".fnk" = pprFnkModule path
  | ext == ".hs"  = pprHsModule path
  | otherwise     = putStrLn "ppr: expeting .fnk or .hs file"
  where
    ext = FilePath.takeExtension path

pprFnkModule :: FilePath -> IO ()
pprFnkModule =
  parseFnkModuleWith
    (\m _ ->
       do dflags <- getDynFlags
          liftIO (prForUser dflags stdout neverQualify (ppr m)))

pprHsModule :: FilePath -> IO ()
pprHsModule path = Fnk.runFnk go SpecialForms.defaultFnkEnv
  where
    go =
      do Make.initSessionForMake
         contents <- liftIO (readFile path)
         dflags0 <- getDynFlags
#if MIN_VERSION_ghc(9,2,0)
         logger <- getLogger
#endif
         let dflags1 = gopt_set dflags0 Opt_Haddock
#if MIN_VERSION_ghc(9,6,0)
             ddopts = defaultDiagnosticOpts @GhcMessage
             pboe = printMessages logger ddopts (initDiagOpts dflags1)
#elif MIN_VERSION_ghc(9,4,0)
             pboe = printMessages logger (initDiagOpts dflags1)
#elif MIN_VERSION_ghc(9,2,0)
             pboe = printBagOfErrors logger dflags1
#else
             pboe = printBagOfErrors dflags1
#endif
             (_warnings, ret) = GHC.parser contents dflags1 path
         liftIO $ case ret of
           Right lmdl -> prForUser dflags1 stdout neverQualify (ppr lmdl)
           Left err   -> putStrLn "pprHsModule: error" >> pboe err

#if MIN_VERSION_ghc(9,6,0)
prForUser :: DynFlags -> Handle -> NamePprCtx -> SDoc -> IO ()
#else
prForUser :: DynFlags -> Handle -> PrintUnqualified -> SDoc -> IO ()
#endif

#if MIN_VERSION_ghc(9,0,0)
prForUser df hdl qual sdoc = printForUser df hdl qual AllTheWay sdoc
#else
prForUser = printForUser
#endif

printHsrc :: FilePath -> IO ()
printHsrc =
  parseFnkModuleWith
    (\mdl sp -> do fnk_str <- Emit.genHsSrc sp (Emit.Hsrc mdl)
                   liftIO (putStrLn fnk_str))

parseFnkModuleWith ::
  (Builder.HModule -> Lexer.SPState -> Fnk.Fnk ()) -> FilePath -> IO ()
parseFnkModuleWith act path = Fnk.runFnk go SpecialForms.defaultFnkEnv
  where
    go =
     do Make.initSessionForMake
        contents <- liftIO (hGetStringBuffer path)
        case Lexer.runSP Reader.sexprs (Just path) contents of
          Right (forms, sp) -> do
            forms' <- Make.withExpanderSettings (Expand.expands forms)
            dflags <- getDynFlags
            case Builder.evalBuilder dflags False Syntax.parseModule forms' of
              Right mdl -> act mdl sp
              Left  err -> liftIO (putStrLn ("error: " ++
                                             Builder.syntaxErrMsg err))
          Left err -> liftIO (print err)

printTokens :: FilePath -> IO ()
printTokens path = do
  contents <- hGetStringBuffer path
  case Lexer.lexTokens (Just path) contents of
    Right toks -> mapM_ (print . GHC.unLoc) toks
    Left err   -> print err

-- countTokens :: FilePath -> IO ()
-- countTokens path = do
--   contents <- hGetStringBuffer path
--   -- contents <- BL.readFile path
--   let f x acc =
--         let n = x `seq` length x
--         in  n `seq` acc `seq` n + acc
--   case Lexer.incrSP Reader.psexpr f 0 (Just path) contents of
--     Right (n, _) -> print n
--     Left err     -> putStrLn err

doMake :: [FilePath] -> IO ()
doMake files =
  do let act = do
           Make.initSessionForMake
           Make.make (zipWith f files (repeat Nothing)) False Nothing
         f file phase = (mkGeneralLocated "commandline" file, phase)
     success_flag <- Fnk.runFnk act SpecialForms.defaultFnkEnv
     case success_flag of
       Failed    -> exitFailure
       Succeeded -> return ()
