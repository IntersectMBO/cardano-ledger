{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides MempoolTx internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.MempoolTx.Internal (
  MempoolTx (..),
  unwrapMempoolTx,
) where

import Cardano.Ledger.Binary (EncCBOR, ToCBOR (..))
import Cardano.Ledger.Core (Era, EraTx (Tx), TxLevel (TopTx), toEraCBOR)
import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)

-- | A top-level transaction as it is received by the mempool.
--
-- A transaction in the mempool and a transaction in a block do not have the same
-- serialization and they have different concerns, therefore they are not interchangeable.
-- New submissions are obtained by decoding their mempool serialization. The constructor
-- is kept internal to prevent accidental wrapping of a transaction taken from a block.
--
-- /Warning/ - The `UnsafeMkMempoolTx` constructor is for codecs, trusted ledger operations,
-- and testing. It must not be used to turn an arbitrary block transaction into a mempool
-- transaction.
newtype MempoolTx era = UnsafeMkMempoolTx {unMempoolTx :: Tx TopTx era}

-- | Get the transaction that was received by the mempool.
unwrapMempoolTx :: MempoolTx era -> Tx TopTx era
unwrapMempoolTx = unMempoolTx

deriving newtype instance EraTx era => Eq (MempoolTx era)

deriving newtype instance EraTx era => Show (MempoolTx era)

deriving newtype instance EraTx era => NFData (MempoolTx era)

deriving newtype instance EraTx era => NoThunks (MempoolTx era)

instance (Era era, EncCBOR (MempoolTx era)) => ToCBOR (MempoolTx era) where
  toCBOR = toEraCBOR @era
