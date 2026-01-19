{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Arbitrary (

) where

import Cardano.Ledger.SCLS.Arbitrary()
import Cardano.Ledger.SCLS.Common
import Cardano.Ledger.SCLS.Namespace.Blocks.V0
import Cardano.Ledger.Conway.SCLS.Namespace.GovCommittee
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
import Cardano.Ledger.Conway.SCLS.Namespace.GovProposals
import Cardano.Ledger.SCLS.Namespace.Nonces.V0
import Cardano.Ledger.SCLS.Namespace.PoolStake.V0
import Cardano.Ledger.SCLS.Namespace.Pots.V0
import Cardano.Ledger.SCLS.Namespace.Snapshots.V0
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
import Cardano.Ledger.Conway.SCLS.Namespace.UTxO
import Cardano.Ledger.Coin
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Core  as Conway
import Test.QuickCheck.Arbitrary
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  GovPurposeId (..),
  GovActionPurpose (..),
  ProposalProcedure (..),
 )

import qualified Cardano.Ledger.Conway.PParams as Conway

deriving newtype instance Arbitrary BlockOut

deriving newtype instance Arbitrary CanonicalCommitteeState

instance Arbitrary (CanonicalCredential kr) where
  arbitrary = fmap mkCanonicalCredential arbitrary

instance Arbitrary CanonicalConstitution where
  arbitrary = genericArbitraryU

instance Arbitrary CanonicalCommitteeAuthorization where
  arbitrary = fmap mkCanonicalCommitteeAuthorization arbitrary

instance Arbitrary CanonicalPParams where
  arbitrary = fmap mkCanonicalPParams (arbitrary @(Conway.PParams ConwayEra))

instance Arbitrary CanonicalGovAction where arbitrary = fmap mkCanonicalGovAction (arbitrary @(GovAction ConwayEra))

instance Arbitrary CanonicalGovActionState where
  arbitrary = fmap mkCanonicalGovActionState (arbitrary @(GovActionState ConwayEra))

instance Arbitrary CanonicalProposalProcedure where
  arbitrary = fmap mkCanonicalProposalProcedure (arbitrary @(ProposalProcedure ConwayEra))

instance Arbitrary CanonicalPParamsUpdate where arbitrary = fmap mkCanonicalPParamsUpdate (arbitrary @(Conway.PParamsUpdate ConwayEra))

instance Arbitrary CanonicalDRepVotingThresholds where
  arbitrary = fmap mkCanonicalDRepVotingThresholds (arbitrary @Conway.DRepVotingThresholds)

instance Arbitrary CanonicalPoolVotingThresholds where
  arbitrary = fmap mkCanonicalPoolVotingThresholds (arbitrary @Conway.PoolVotingThresholds)

instance Arbitrary PoolStakeOut where arbitrary = genericArbitraryU

instance Arbitrary PotsOut where arbitrary = genericArbitraryU

instance Arbitrary SnapShotOut where arbitrary = genericArbitraryU

instance Arbitrary CanonicalStakePoolParams where
  arbitrary = fmap mkCanonicalStakePoolParams arbitrary

instance Arbitrary CanonicalShelleyTxOut where arbitrary = fmap (mkCanonicalShelleyTxOut) (arbitrary @(Shelley.ShelleyTxOut ConwayEra))
instance Arbitrary CanonicalBabbageTxOut where arbitrary = fmap (mkCanonicalBabbageTxOut) (arbitrary @(Babbage.BabbageTxOut ConwayEra))

instance Arbitrary UtxoOut where arbitrary = genericArbitraryU

instance Arbitrary CanonicalExUnits where arbitrary = fmap mkCanonicalExUnits arbitrary

instance Arbitrary CanonicalPurposeId where arbitrary = fmap mkCanonicalPurposeId (arbitrary @(GovPurposeId HardForkPurpose))

instance Arbitrary CanonicalNonce where arbitrary = fmap mkCanonicalNonce arbitrary

instance Arbitrary (CanonicalVRFVerKeyHash k) where
  arbitrary = fmap mkCanonicalVRFVerKeyHash arbitrary

instance Arbitrary CanonicalRewardAccount where
  arbitrary = fmap mkCanonicalRewardAccount arbitrary

instance Arbitrary CanonicalPoolMetadata where
  arbitrary = fmap mkCanonicalPoolMetadata arbitrary

instance Arbitrary CanonicalStakePoolRelay where
  arbitrary = fmap mkCanonicalStakePoolRelay arbitrary

instance Arbitrary CanonicalCoin where
  arbitrary = fmap mkCanonicalCoin (arbitrary @Coin)