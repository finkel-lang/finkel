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
skFrontend flags args =
  case args of
    [(file,_)] ->
      do ret <- toGhc (work file) specialForms
         case ret of
           Left err -> liftIO (putStrLn err)
           Right _  -> return ()
    _ -> liftIO (putStrLn ("Unknown args: " ++ show args))

work :: FilePath -> Skc ()
work file = do
   contents <- liftIO (readFile file)
   setExpanderSettings
   (mdl, sp) <- compile (Just file) contents
   tc <- tcHsModule (Just file) False mdl
   genHsSrc sp mdl >>= liftIO . putStrLn
   -- ds <- desugarModule tc
   -- _ <- loadModule ds
   -- return ()
