{-# LANGUAGE CPP #-}

#if 906 <= __GLASGOW_HASKELL__
import Distribution.Simple.Finkel
main = fnkPluginMainForHaddock
#else
import Distribution.Simple (defaultMain)
main = defaultMain
#endif
