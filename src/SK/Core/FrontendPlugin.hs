-- | Sample module to show the usage of frontend plugin in GHC.
--
-- Try:
--
--    > stack ghc -- --frontend SK.Core.FrontendPlugin foo.lisp
--
module SK.Core.FrontendPlugin
  ( frontendPlugin
  ) where

-- From ghc
import GHC
import GhcPlugins

frontendPlugin :: FrontendPlugin
frontendPlugin =
  defaultFrontendPlugin {
    frontend = doNothing
  }

doNothing :: [String] -> [(String, Maybe Phase)] -> Ghc ()
doNothing flags args =
  do hsc_env <- getSession
     targets <- getTargets
     let df = hsc_dflags hsc_env
     liftIO
       (do print flags
           print args
           putStrLn ("Targets: " ++ showSDocDump df (ppr targets)))
