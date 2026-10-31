{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Binary.Annotator (
  module Test.Cardano.Ledger.Alonzo.Binary.Annotator,
) where

import Cardano.Ledger.Babbage (BabbageEra, Tx (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody
import Cardano.Ledger.Binary
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (..))
import Test.Cardano.Ledger.Alonzo.Binary.Annotator

deriving newtype instance DecCBOR (TxBody TopTx BabbageEra)

instance DecCBOR (MempoolTx BabbageEra) where
  decCBOR = UnsafeMkMempoolTx . MkBabbageTx <$> decCBOR

instance DecCBOR h => DecCBOR (Block h BabbageEra) where
  decCBOR =
    decodeRecordNamed "Block" (const 5) $ Block <$> decCBOR <*> decCBOR
