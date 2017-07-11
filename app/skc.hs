-- | Wrapper executable for sk frontend plugin.
--
-- Prepend argument passed to GHC frontend plugin. Wraps input arguments
-- with "-ffrontend-opt". This file contains codes to intercept some of
-- the conflicting arguments for frontend plugin.
--
module Main where

import qualified GHC.Paths as GhcPaths

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (find, isPrefixOf, isSuffixOf)
import System.Environment (getArgs, lookupEnv)
import System.Process (rawSystem)
import System.Exit (exitWith)

import SK.Core.Plugin ()

main :: IO ()
main = do
  argIns <- getArgs
  let argOuts = [ "--frontend", "SK.Core.Plugin"
                , "-plugin-package", "sk-core" ]
      (srcs, skopts, ghcopts) = groupOptions [] [] [] argIns
      ghcopts' = reverse ghcopts
      ghc = fromMaybe GhcPaths.ghc (findGhc skopts)
  debug <- if isJust (find (== "--sk-debug") skopts)
              then return True
              else do mbDebug <- lookupEnv "SKC_DEBUG"
                      case mbDebug of
                        Nothing -> return False
                        Just _  -> return True
  when (isJust (find (== "--sk-debug") skopts))
       (putStrLn ("ghc: " ++ show ghc))
  let skopts' | isJust (find (== "--sk-debug") skopts) = skopts
              | debug = "-ffrontend-opt":"--sk-debug":skopts
              | otherwise = skopts
      getO =
        let go [] = []
            go (val:_:"--sk-o":_) = [val]
            go (_:rest) = go rest
        in  go skopts
      rawGhcOpts =
        if not (null getO)
          then ghcopts' ++ ("-o":getO)
          else ghcopts'
      runRawGhc = rawSystem ghc rawGhcOpts
      -- Testing whether ghc was invoked for building shared
      -- library. This may happen when building cabal package, to
      -- suppress unwanted warning messages.
      buildingSharedLib =
        null srcs && elem "-shared" ghcopts' && elem "-dynamic" ghcopts
  exitCode <-
    -- When any of conflicting option with frontend plugin was set, OR
    -- building shared library, OR no sk-specific option was specified,
    -- delegate to raw ghc without frontend plugin.
    if any (`elem` conflictingOptions) ghcopts'
       || buildingSharedLib
       || null skopts && null srcs
       then runRawGhc
       else do
         let args = concat [argOuts,skopts',ghcopts',"-x":"hs":srcs]
         rawSystem ghc args
  exitWith exitCode

-- | Categorize command line options.
groupOptions :: [String] -> [String] -> [String] -> [String]
             -> ([String], [String], [String])
groupOptions = go where
  go sksrc skopt ghcopt args =
    case args of
       -- Irregular pattern for "--sk-pgmf" option. Output file comes
       -- before the "--gk-pgmf".
       x:"--sk-pgmf":xs ->
         go sksrc (fopt:"--sk-pgmf":fopt:x: skopt) ghcopt xs

       -- Separate options one by one with predicates. Some are SK
       -- source code, some are conflicting flag with '--frontend'
       -- option, ... etc.
       x:xs
         | isSkSrc x ->
             go (x:sksrc) skopt ghcopt xs
         | isMake x ->
             go sksrc (fopt:"--sk-make":skopt) ghcopt xs
         | isO x ->
             go sksrc (fopt:head xs:fopt:"--sk-o":skopt) ghcopt (tail xs)
         | isC x ->
             go sksrc (fopt:"--sk-c":skopt) ghcopt ("-no-link":xs)
         | isSkOption x ->
             go sksrc (fopt:head xs:fopt:x:skopt) ghcopt (tail xs)
         | isSkFlag x ->
             go sksrc (fopt:x:skopt) ghcopt xs
         | otherwise -> go sksrc skopt (x:ghcopt) xs

       -- Done.
       [] -> (sksrc, skopt, ghcopt)
     where
      fopt = "-ffrontend-opt"

-- | When any of options listed here were found, invoke raw @ghc@
-- without using SK frontend plugin. Otherwise @ghc@ will complain with
-- error message. These options are listed in "ghc/Main.hs" as
-- `mode_flags'.
conflictingOptions :: [String]
conflictingOptions =
  [ "--info"
  , "--show-options"
  , "--supported-languages"
  , "--supported-extensions"
  , "--show-packages"
  , "--show-iface"
  , "--print-libdir"
  , "--abi-hash"
  ]

-- | Test to find the string sequence "--make". Option "--make" could
-- not be used as frontend plugin option.
isMake :: String -> Bool
isMake = (== "--make")

-- | Another option to intercept.
isO :: String -> Bool
isO = (== "-o")

-- | Intercept "-c".
isC :: String -> Bool
isC = (== "-c")

-- | SK source code extension is hard coded as ".sk". Surely it would be
-- better to have alternative choice specified via command line
-- argument.
isSkSrc :: String -> Bool
isSkSrc = isSuffixOf ".sk"

-- | Argument passed to SK plugin with value.
isSkOption :: String -> Bool
isSkOption str = elem str ["--sk-out", "--sk-ghc"]

-- | Argument passed to SK plugin without value.
isSkFlag :: String -> Bool
isSkFlag = isPrefixOf "--sk-"

-- | Find argument passed to "--sk-ghc" option.
findGhc :: [String] -> Maybe String
findGhc xs =
  case xs of
    ghc:_   :"--sk-ghc":_ -> Just ghc
    _  :rest              -> findGhc rest
    []                    -> Nothing
