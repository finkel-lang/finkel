-- | Sample module to show the usage of frontend plugin in GHC.
--
-- Try:
--
--    > stack ghc -- --frontend SK.Core.FrontendPlugin -x hs foo.sk
--
module SK.Core.FrontendPlugin
  ( frontendPlugin
  ) where

-- From ghc
import GHC
import GhcPlugins

-- Internal
import SK.Core
import SK.Core.Emit
import SK.Core.Macro
import SK.Core.Run
import SK.Core.Typecheck

frontendPlugin :: FrontendPlugin
frontendPlugin =
  defaultFrontendPlugin {
    frontend = skFrontend
  }

skFrontend :: [String] -> [(String, Maybe Phase)] -> Ghc ()
skFrontend flags args = do
  case reverse flags of
    ["-pgmF", orig, input, output] ->
      doWork input (Just output) >>= showRet
    [input] ->
      doWork input Nothing >>= showRet
    _ -> case args of
           [(file,_)] ->
             doWork file Nothing >>= showRet
           _          ->
             liftIO (putStrLn ("Unknown args: " ++ show args))
  where
    doWork input mbout =
      toGhc (work input mbout) specialForms
    showRet ret =
      case ret of
        Left err -> liftIO (putStrLn err)
        Right _  -> return ()

work :: FilePath -> Maybe FilePath -> Skc ()
work file mbout = do
   contents <- liftIO (readFile file)
   setExpanderSettings
   (mdl, sp) <- compile (Just file) contents
   tc <- tcHsModule (Just file) False mdl
   hssrc <- genHsSrc sp mdl
   case mbout of
      Nothing -> liftIO (putStrLn hssrc)
      Just out -> liftIO (writeFile out hssrc)
   -- ds <- desugarModule tc
   -- _ <- loadModule ds
   -- return ()
