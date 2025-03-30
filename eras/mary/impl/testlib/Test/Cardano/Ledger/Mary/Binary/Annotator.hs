{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Binary.Annotator (
  module Test.Cardano.Ledger.Allegra.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.TxBody
import Test.Cardano.Ledger.Allegra.Binary.Annotator

deriving via
  Mem (MaryTxBodyRaw era)
  instance
    MaryEraTxBody era => DecCBOR (Annotator (MaryTxBody era))
