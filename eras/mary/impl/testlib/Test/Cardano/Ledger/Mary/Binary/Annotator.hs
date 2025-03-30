{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Binary.Annotator () where

import Cardano.Ledger.Binary
import Cardano.Ledger.Mary.TxBody.Internal
import Test.Cardano.Ledger.Allegra.Binary.Annotator ()
import Test.Cardano.Ledger.Binary.Annotator ()
import Test.Cardano.Ledger.Core.Binary.Annotator

deriving via
  Mem (MaryTxBodyRaw era)
  instance
    MaryEraTxBody era => DecCBOR (Annotator (MaryTxBody era))
