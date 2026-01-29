{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary () where

import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Conway.CanonicalState.Namespace.UTxO.V0 as UtxoOut.V0
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary (TxOut era) => Arbitrary (UtxoOut.V0.UtxoOut era) where
  arbitrary = UtxoOut.V0.mkUtxoBabbage <$> arbitrary
