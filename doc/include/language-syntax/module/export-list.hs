module M2 -- Haskell
  ( f1           -- Value, field name, or class method
  , T1           -- Type constructor only
  , T2(..)       -- Type constructor and all of its data constructors
  , T3(T3a, T3b) -- Type constructor and specified data constructors
  , T4(t4f1)     -- Type constructor and field label

  , module Data.Char        -- Module reexport
  , Mb.Maybe(Just, Nothing) -- Reexport with a qualified name
  ) where

import Data.Maybe as Mb

-- ... more module contents ...
