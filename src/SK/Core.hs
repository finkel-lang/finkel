-- | Module re-exporting runtime dependency for SK program.
--
-- When using non-standard Haskell values and functions (e.g.: symbols,
-- car, cdr), this module is likely to be required.
--

module SK.Core
  ( module SK.Core.Form
  , module SK.Core.SKC
  ) where

import SK.Core.Form
import SK.Core.SKC
