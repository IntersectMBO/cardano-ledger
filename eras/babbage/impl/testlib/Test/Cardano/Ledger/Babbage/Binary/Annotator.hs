{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Binary.Annotator (
  module Test.Cardano.Ledger.Alonzo.Binary.Annotator,
) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody
import Cardano.Ledger.Binary
import Test.Cardano.Ledger.Alonzo.Binary.Annotator

deriving via Mem BabbageTxBodyRaw instance DecCBOR (Annotator (TxBody BabbageEra))

instance DecCBOR (Annotator BabbageTxBodyRaw) where
  decCBOR = pure <$> decCBOR
