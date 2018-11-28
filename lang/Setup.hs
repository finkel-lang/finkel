module Main where

import System.Environment (lookupEnv)
import Distribution.Simple.SK (defaultMainWithHooks, skcHooksWith)

main :: IO ()
main = decideHooks >>= defaultMainWithHooks
  where
    -- Stack sets "GHC_PACKAGE_PATH" environment variable during
    -- build. Cabal nix-style commands uses "GHC_ENVIRONMENT"
    -- environment variable, and will complain if "GHC_PACKAGE_PATH"
    -- were set.
    --
    -- So, first check whether the GHC_PACKAGE_PATH exist in current
    -- environment variable, if exist, assume as running via
    -- stack. Otherwise assume as cabal nix style build.
    decideHooks = do
      mb_ghc_pkg_path <- lookupEnv "GHC_PACKAGE_PATH"
      case mb_ghc_pkg_path of
        Just _  -> return stackHook
        Nothing -> return cabalHook
    stackHook = skcHooksWith "skkc2" [] False
    cabalHook =
      skcHooksWith "cabal" ["new-run", "-v0", "--", "skkc2"] False
