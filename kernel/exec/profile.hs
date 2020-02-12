{-# LANGUAGE CPP #-}
-- | Simple executable for profiling.
--
-- Simple executable to wrap some simple actions.
module Main where

-- base
import           Control.Monad.IO.Class  (MonadIO (..))
import           System.Environment      (getArgs)
import           System.IO               (stdout)

-- filepath
import qualified System.FilePath         as FilePath

-- ghc
import           DynFlags                (GeneralFlag (..),
                                          HasDynFlags (..), gopt_set)
import           ErrUtils                (printBagOfErrors)
import qualified GHC                     as GHC
import           Outputable              (Outputable (..), neverQualify,
                                          printForUser)
import           SrcLoc                  (mkGeneralLocated)
import           StringBuffer            (hGetStringBuffer)

-- finkel-kernel
import qualified Language.Finkel.Builder as Builder
import qualified Language.Finkel.Emit    as Emit
import qualified Language.Finkel.Expand  as Expand
import qualified Language.Finkel.Fnk     as Fnk
import qualified Language.Finkel.Lexer   as Lexer
import qualified Language.Finkel.Make    as Make
import qualified Language.Finkel.Reader  as Reader
import qualified Language.Finkel.Syntax  as Syntax

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
printExpandedForms path = Fnk.runFnk go Make.defaultFnkEnv
  where
    go = do
      Make.initSessionForMake
      -- contents <- liftIO (BL.readFile path)
      contents <- liftIO (hGetStringBuffer path)
      (forms, _) <- Reader.parseSexprs (Just path) contents
      forms' <- Expand.withExpanderSettings (Expand.expands forms)
      liftIO (mapM_ print forms')

printForms :: FilePath -> IO ()
printForms path =
  do -- contents <- BL.readFile path
     contents <- hGetStringBuffer path
     case Lexer.evalSP Reader.sexprs (Just path) contents of
       Right forms -> mapM_ print forms
       Left err    -> putStrLn err

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
          liftIO (printForUser dflags stdout neverQualify (ppr m)))

pprHsModule :: FilePath -> IO ()
pprHsModule path = Fnk.runFnk go Make.defaultFnkEnv
  where
    go =
      do Make.initSessionForMake
         contents <- liftIO (readFile path)
         dflags0 <- getDynFlags
         let dflags1 = gopt_set dflags0 Opt_Haddock
#if MIN_VERSION_ghc(8,4,0)
             (_warnings, ret) = GHC.parser contents dflags1 path
#else
             ret = case GHC.parser contents dflags1 path of
                       Right (_, lmdl) -> Right lmdl
                       Left err        -> Left err
#endif
         case ret of
           Right lmdl ->
             liftIO
               (printForUser dflags1 stdout neverQualify (ppr lmdl))
           Left err   -> liftIO (do putStrLn "pprHsModule: error"
                                    printBagOfErrors dflags1 err)

printHsrc :: FilePath -> IO ()
printHsrc =
  parseFnkModuleWith
    (\mdl sp -> do fnk_str <- Emit.genHsSrc sp (Emit.Hsrc mdl)
                   liftIO (putStrLn fnk_str))

parseFnkModuleWith ::
  (Builder.HModule -> Lexer.SPState -> Fnk.Fnk ()) -> FilePath -> IO ()
parseFnkModuleWith act path = Fnk.runFnk go Make.defaultFnkEnv
  where
    go =
     do Make.initSessionForMake
        -- contents <- liftIO (BL.readFile path)
        contents <- liftIO (hGetStringBuffer path)
        case Lexer.runSP Reader.sexprs (Just path) contents of
          Right (forms, sp) -> do
            forms' <- Expand.withExpanderSettings (Expand.expands forms)
            case Builder.evalBuilder Syntax.parseModule forms' of
              Right mdl -> act mdl sp
              Left  err -> liftIO (putStrLn ("error: " ++
                                             Builder.syntaxErrMsg err))
          Left err -> liftIO (putStrLn ("error: " ++ err))

printTokens :: FilePath -> IO ()
printTokens path = do
  contents <- hGetStringBuffer path
  -- contents <- BL.readFile path
  case Lexer.lexTokens (Just path) contents of
    Right toks -> mapM_ (print . GHC.unLoc) toks
    Left err   -> putStrLn err

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
           Make.make (zipWith f files (repeat Nothing))
                     False False Nothing
         f file phase = (mkGeneralLocated "commandline" file, phase)
     Fnk.runFnk act Make.defaultFnkEnv
