{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Arbitrary (

) where

import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution (..), GovActionState (..))
import Cardano.Ledger.Conway.SCLS.Namespace.GovProposals
import Cardano.Ledger.Conway.SCLS.Namespace.UTxO
import Cardano.Ledger.Core (PParams (..))
import Cardano.Ledger.SCLS.Namespace.Blocks.V0
import Cardano.Ledger.SCLS.Namespace.GovCommittee.V0
import Cardano.Ledger.SCLS.Namespace.GovConstitution.V0
import Cardano.Ledger.SCLS.Namespace.GovPParams.V0
import Cardano.Ledger.SCLS.Namespace.Snapshots.V0
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.SCLS.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.QuickCheck

deriving newtype instance Arbitrary BlockOut

instance Arbitrary SnapShotOut where
  arbitrary = genericArbitraryU

instance Arbitrary UtxoOut where
  arbitrary =
    oneof
      [ UtxoOutShelley . mkCanonicalShelleyTxOut <$> arbitrary @(Shelley.ShelleyTxOut ConwayEra)
      , UtxoOutBabbage . mkCanonicalBabbageTxOut <$> arbitrary @(Babbage.BabbageTxOut ConwayEra)
      ]

instance Arbitrary GovProposalOut where
  arbitrary = GovProposalOut . mkCanonicalGovActionState <$> arbitrary @(GovActionState ConwayEra)

instance Arbitrary GovPParamsOut where
  arbitrary = GovPParamsOut . mkCanonicalPParams <$> arbitrary @(PParams ConwayEra)

instance Arbitrary GovConstitutionOut where
  arbitrary = GovConstitutionOut . mkCanonicalConstitution <$> arbitrary @(Constitution ConwayEra)

instance Arbitrary GovCommitteeOut where
  arbitrary = genericArbitraryU
