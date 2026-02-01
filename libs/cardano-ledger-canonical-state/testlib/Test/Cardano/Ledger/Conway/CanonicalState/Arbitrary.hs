{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary () where

import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Core (Era, EraTxOut)
import qualified Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 as UtxoOut.V0
import Cardano.Ledger.CanonicalState.Conway ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..))

instance (EraTxOut era, Arbitrary (TxOut era), Era era) => Arbitrary (UtxoOut.V0.UtxoOut era) where
  arbitrary = UtxoOut.V0.mkUtxo <$> arbitrary
