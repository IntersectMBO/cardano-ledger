{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.Annotator (
  module Test.Cardano.Ledger.Babbage.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Conway (ConwayEra, Tx (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.TxBody
import Test.Cardano.Ledger.Babbage.Binary.Annotator

deriving newtype instance DecCBOR (TxBody TopTx ConwayEra)

deriving newtype instance DecCBOR (Tx TopTx ConwayEra)
