{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Cardano.Ledger.SCLS.Arbitrary (

) where

import Cardano.Ledger.SCLS.Namespace.GovProposals.V0
import Generic.Random (genericArbitraryU)
import Test.QuickCheck.Arbitrary

instance Arbitrary CanonicalVote where
    arbitrary = genericArbitraryU