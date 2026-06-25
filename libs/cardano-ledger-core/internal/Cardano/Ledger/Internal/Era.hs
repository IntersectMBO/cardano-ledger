{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- WARNING! This module contains types that are meant for internal use only.
-- Even when used internally, care must be taken that these types are NOT used
-- prior to their eras.
#if __GLASGOW_HASKELL__ >= 908
module Cardano.Ledger.Internal.Era
{-# WARNING in "x-unsafe-ledger-internal"
  [ "For Ledger internal use only!"
  , "Downstream users must import these era types from"
  , "`cardano-ledger-[era]:lib:Cardano.Ledger.[Era]` modules!"] #-} (
  module Cardano.Ledger.Internal.Definition.Era,
) where
#else
module Cardano.Ledger.Internal.Era (
  module Cardano.Ledger.Internal.Definition.Era,
) where
#endif

-- The Era types are defined in a separate private module, just so they do not get tainted by the
-- WARNING pragma. With a re-export approach only this module is marked with the warning, but not
-- the types that are being exported.
import Cardano.Ledger.Internal.Definition.Era
