{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Binary.Annotator (
  module Test.Cardano.Ledger.Conway.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader
import Test.Cardano.Ledger.Conway.Binary.Annotator

deriving newtype instance Crypto c => DecCBOR (BHeader c)
