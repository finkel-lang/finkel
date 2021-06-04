{-# LANGUAGE CPP #-}
-- | Wrapper module to export version related functions.
module Finkel.Core.Ghc.Version
  ( cProjectVersionInt
  , getVersion
  ) where

-- base
import Data.Version        (Version)

-- finkel-kernel
import Language.Finkel     (Code, Fnk, finkelSrcError, fromCode)

#if MIN_VERSION_ghc(9,0,0)
-- ghc
import GHC.Data.FastString (fsLit)
import GHC.Driver.Session  (DynFlags (..))
import GHC.Settings.Config (cProjectVersionInt)
import GHC.Unit.State      (PackageName (..), lookupPackageName, lookupUnitId,
                            unitPackageVersion)
import GHC.Unit.Types      (indefUnit)
#else
-- ghc
import Config              (cProjectVersionInt)
import DynFlags            (DynFlags)
import FastString          (fsLit)
import Module              (componentIdToInstalledUnitId)
import Packages            (PackageName (..), lookupInstalledPackage,
                            lookupPackageName)

-- ghc-boot
import GHC.PackageDb       (packageVersion)
#endif

getVersion :: DynFlags -> Code -> Fnk Version
getVersion dflags form =
  let err = finkelSrcError form
  in  case fromCode form of
    Nothing -> err ("want package name `String' value but got: " ++ show form)
    Just name -> case lookupPackageVersion dflags name of
      Nothing -> err ("cannot find package: " ++ name)
      Just v  -> pure v

lookupPackageVersion :: DynFlags -> String -> Maybe Version
#if MIN_VERSION_ghc(9,0,0)
lookupPackageVersion dflags name =
  do let pname = PackageName (fsLit name)
         ust = unitState dflags
     indef_uid <- lookupPackageName ust pname
     uid <- lookupUnitId ust (indefUnit indef_uid)
     pure $ unitPackageVersion uid
#else
lookupPackageVersion dflags name =
  do let pname = PackageName (fsLit name)
     component_id <- lookupPackageName dflags pname
     let iuid = componentIdToInstalledUnitId component_id
     conf <- lookupInstalledPackage dflags iuid
     pure $ packageVersion conf
#endif
