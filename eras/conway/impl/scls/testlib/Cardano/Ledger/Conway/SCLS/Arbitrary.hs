{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Arbitrary (

) where

import Cardano.Ledger.Conway.SCLS.Namespace.Blocks
import Cardano.Ledger.Conway.SCLS.Namespace.GovCommittee
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
import Cardano.Ledger.Conway.SCLS.Namespace.GovProposals
import Cardano.Ledger.Conway.SCLS.Namespace.PoolStake
import Cardano.Ledger.Conway.SCLS.Namespace.Pots
import Cardano.Ledger.Conway.SCLS.Namespace.Snapshots
import Cardano.Ledger.Conway.SCLS.Namespace.UTxO
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck.Arbitrary

deriving newtype instance Arbitrary BlockOut

deriving newtype instance Arbitrary GovCommitteeOut

instance Arbitrary CanonicalConstitution where
    arbitrary = genericArbitraryU

deriving newtype instance Arbitrary GovPParamsOut

deriving newtype instance Arbitrary GovProposalOut

instance Arbitrary GovActionState' where arbitrary = genericArbitraryU

instance Arbitrary PoolStakeOut where arbitrary = genericArbitraryU

instance Arbitrary PotsOut where arbitrary = genericArbitraryU

instance Arbitrary SnapShotOut where arbitrary = genericArbitraryU

instance Arbitrary UtxoOut where arbitrary = genericArbitraryU
