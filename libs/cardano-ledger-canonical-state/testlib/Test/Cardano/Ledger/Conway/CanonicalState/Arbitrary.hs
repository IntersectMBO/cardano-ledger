{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary () where

-- import Cardano.Ledger.Core (Era, EraTxOut, TxOut)
-- import qualified Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 as UtxoOut.V0
import Cardano.Ledger.CanonicalState.Conway ()
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 as GovConstitution.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary GovConstitution.V0.CanonicalConstitution where
  arbitrary = GovConstitution.V0.mkCanonicalConstitution <$> arbitrary @(Constitution ConwayEra)

instance Arbitrary GovConstitution.V0.GovConstitutionOut where
  arbitrary = genericArbitraryU
