{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.SCLS.Arbitrary (

) where

import Cardano.Ledger.SCLS.Common
import qualified Cardano.Ledger.SCLS.Namespace.GovCommittee.V0 as Committee.V0
import qualified Cardano.Ledger.SCLS.Namespace.GovConstitution.V0 as Constitution.V0
import qualified Cardano.Ledger.SCLS.Namespace.GovPParams.V0 as PParams.V0
import qualified Cardano.Ledger.SCLS.Namespace.GovProposals.V0 as Proposals.V0
import qualified Cardano.Ledger.SCLS.Namespace.Nonces.V0 as Nonces.V0
import qualified Cardano.Ledger.SCLS.Namespace.PoolStake.V0 as PoolStake.V0
import qualified Cardano.Ledger.SCLS.Namespace.Pots.V0 as Pots.V0
import qualified Cardano.Ledger.SCLS.Namespace.Snapshots.V0 as Snapshots.V0
import qualified Cardano.Ledger.SCLS.Namespace.UTxO.V0 as UTxO.V0
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck

instance Arbitrary CanonicalCoin where arbitrary = CanonicalCoin . getPositive <$> arbitrary

instance Arbitrary Proposals.V0.CanonicalVote where arbitrary = genericArbitraryU

instance Arbitrary Committee.V0.CanonicalCommitteeAuthorization where
  arbitrary = fmap Committee.V0.mkCanonicalCommitteeAuthorization arbitrary

instance Arbitrary Committee.V0.CanonicalCommitteeState where arbitrary = genericArbitraryU

instance Arbitrary (CanonicalCredential kr) where arbitrary = fmap mkCanonicalCredential arbitrary

instance Arbitrary Constitution.V0.CanonicalConstitution where arbitrary = genericArbitraryU

instance Arbitrary PParams.V0.CanonicalCostModels where arbitrary = genericArbitraryU

instance Arbitrary PParams.V0.CanonicalDRepVotingThresholds where arbitrary = genericArbitraryU

instance Arbitrary PParams.V0.CanonicalPoolVotingThresholds where arbitrary = genericArbitraryU

instance Arbitrary PParams.V0.CanonicalExUnits where arbitrary = genericArbitraryU

instance Arbitrary PParams.V0.CanonicalPrices where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalGovActionState where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalProposalProcedure where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalGovAction where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalGovActionId where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalGovActionIx where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalPurposeId where arbitrary = genericArbitraryU

instance Arbitrary Proposals.V0.CanonicalPParamsUpdate where arbitrary = genericArbitraryU

instance Arbitrary CanonicalRewardAccount where arbitrary = genericArbitraryU

instance Arbitrary Nonces.V0.CanonicalNonce where arbitrary = genericArbitraryU

instance Arbitrary Nonces.V0.NoncesState where arbitrary = genericArbitraryU

instance Arbitrary PoolStake.V0.PoolStakeOut where arbitrary = genericArbitraryU

instance Arbitrary (CanonicalVRFVerKeyHash k) where arbitrary = genericArbitraryU

instance Arbitrary (Nonces.V0.CanonicalWithOrigin SlotNo) where arbitrary = genericArbitraryU

instance Arbitrary Pots.V0.PotsOut where arbitrary = genericArbitraryU

instance Arbitrary Snapshots.V0.CanonicalStakePoolRelay where arbitrary = genericArbitraryU

instance Arbitrary Snapshots.V0.CanonicalPoolMetadata where arbitrary = genericArbitraryU

instance Arbitrary Snapshots.V0.CanonicalStakePoolParams where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.CanonicalValue where
  arbitrary = do
    c <- arbitrary
    m :: Map.Map ScriptHash (Map.Map ShortByteString CanonicalCoin) <- arbitrary
    let m1 =
          Map.map
            ( \innerMap ->
                Map.fromList
                  [ (k', CanonicalCoin (abs x))
                  | (k, CanonicalCoin x) <- Map.toList innerMap
                  , let k' = SBS.take 28 $ k <> (SBS.replicate (28 - (SBS.length k)) 0)
                  ]
            )
            m
        m2 = Map.filter (not . Map.null) m1
    return (UTxO.V0.CanonicalValue c m2)

instance Arbitrary UTxO.V0.CanonicalShelleyTxOut where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.CanonicalBabbageTxOut where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.CanonicalDatum where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.CanonicalNativeScript where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.CanonicalPlutusScript where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.CanonicalScript where arbitrary = genericArbitraryU

instance Arbitrary UTxO.V0.PlutusBinary where arbitrary = genericArbitraryU

instance Arbitrary PParams.V0.CanonicalPParams where
  arbitrary = genericArbitraryU
