{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Binary.Annotator (
  module Test.Cardano.Ledger.Allegra.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core (TxLevel (..))
import Cardano.Ledger.Mary (MaryEra, Tx (..))
import Cardano.Ledger.Mary.TxBody
import Test.Cardano.Ledger.Allegra.Binary.Annotator

deriving newtype instance DecCBOR (TxBody TopTx MaryEra)

deriving newtype instance DecCBOR (Tx TopTx MaryEra)

instance DecCBOR h => DecCBOR (Block h MaryEra) where
  decCBOR =
    decodeRecordNamed "Block" (const 4) $ Block <$> decCBOR <*> decCBOR
