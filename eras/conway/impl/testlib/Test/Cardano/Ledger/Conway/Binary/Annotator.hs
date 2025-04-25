{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.Annotator (
  module Test.Cardano.Ledger.Babbage.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.TxBody
import Test.Cardano.Ledger.Babbage.Binary.Annotator

instance DecCBOR (Annotator ConwayTxBodyRaw) where
  decCBOR = pure <$> decCBOR

deriving via Mem ConwayTxBodyRaw instance DecCBOR (Annotator (TxBody ConwayEra))
