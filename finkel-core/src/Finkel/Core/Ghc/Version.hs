{-# LANGUAGE CPP #-}
-- | Wrapper module to export version related functions.
module Finkel.Core.Ghc.Version
  ( cProjectVersionInt
  , __glasgow_haskell__
  , getPackageVersion
  ) where

-- base
import Data.Version        (Version)

-- finkel-kernel
import Language.Finkel     (Code, Fnk, finkelSrcError, fromCode)

-- ghc
#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env      (HscEnv (..), hsc_units)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Session  (unitState)
import GHC.Driver.Types    (HscEnv (..))
#else
import HscTypes            (HscEnv (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Data.FastString (fsLit)
import GHC.Settings.Config (cProjectVersionInt)
import GHC.Unit.State      (PackageName (..), lookupPackageName, lookupUnitId,
                            unitPackageVersion)
import GHC.Unit.Types      (indefUnit)
#else
import Config              (cProjectVersionInt)
import FastString          (fsLit)
import Module              (componentIdToInstalledUnitId)
import Packages            (PackageName (..), lookupInstalledPackage,
                            lookupPackageName)

-- ghc-boot
import GHC.PackageDb       (packageVersion)
#endif

-- | Function version of @__GLASGOW_HASKELL__@ C preprocessor macro.
__glasgow_haskell__ :: Int
__glasgow_haskell__ = __GLASGOW_HASKELL__

getPackageVersion :: HscEnv -> Code -> Fnk Version
getPackageVersion hsc_env form =
  let err = finkelSrcError form
  in  case fromCode form of
    Nothing -> err ("want package name `String' value but got: " ++ show form)
    Just name -> case lookupPackageVersion hsc_env name of
      Nothing -> err ("cannot find package: " ++ name)
      Just v  -> pure v

lookupPackageVersion :: HscEnv -> String -> Maybe Version
#if MIN_VERSION_ghc(9,2,0)
lookupPackageVersion hsc_env name =
  do let pname = PackageName (fsLit name)
         us = hsc_units hsc_env
     indef_uid <- lookupPackageName us pname
     uid <- lookupUnitId us (indefUnit indef_uid)
     pure $ unitPackageVersion uid
#elif MIN_VERSION_ghc(9,0,0)
lookupPackageVersion hsc_env name =
  do let pname = PackageName (fsLit name)
         ust = unitState (hsc_dflags hsc_env)
     indef_uid <- lookupPackageName ust pname
     uid <- lookupUnitId ust (indefUnit indef_uid)
     pure $ unitPackageVersion uid
#else
lookupPackageVersion hsc_env name =
  do let pname = PackageName (fsLit name)
     component_id <- lookupPackageName (hsc_dflags hsc_env) pname
     let iuid = componentIdToInstalledUnitId component_id
     conf <- lookupInstalledPackage (hsc_dflags hsc_env) iuid
     pure $ packageVersion conf
#endif
