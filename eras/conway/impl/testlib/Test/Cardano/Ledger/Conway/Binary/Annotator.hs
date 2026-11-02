{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.Annotator (
  module Test.Cardano.Ledger.Babbage.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway (ConwayEra, Tx (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (..))
import Test.Cardano.Ledger.Babbage.Binary.Annotator

deriving newtype instance DecCBOR (TxBody TopTx ConwayEra)

instance DecCBOR (MempoolTx ConwayEra) where
  decCBOR = UnsafeMkMempoolTx . MkConwayTx <$> decCBOR

instance DecCBOR h => DecCBOR (Block h ConwayEra) where
  decCBOR =
    decodeRecordNamed "Block" (const 5) $ Block <$> decCBOR <*> decCBOR
