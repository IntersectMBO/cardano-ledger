{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance Crypto c => DecCBOR (Annotator (BHeaderRaw c)) where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (BHeaderRaw c)
  instance
    Crypto c => DecCBOR (Annotator (BHeader c))
