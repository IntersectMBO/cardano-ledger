{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.CanonicalState.Arbitrary () where

import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalCoin (..),
 )
import Cardano.Ledger.CanonicalState.Conway ()
import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 as Committee.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 as UtxoOut.V0
import Cardano.Ledger.Coin (CompactForm (CompactCoin))
import Cardano.Ledger.Core (Era, EraTxOut, TxOut)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..), Positive (..))

instance Arbitrary Blocks.V0.BlockOut where
  -- starting form the QuickCheck-2.17 there is an arbirary instance for
  -- Natural, so it's possible to use genericArbitraryU directly.
  arbitrary = Blocks.V0.BlockOut . fromIntegral . getPositive @Integer <$> arbitrary

instance (EraTxOut era, Arbitrary (TxOut era), Era era) => Arbitrary (UtxoOut.V0.UtxoOut era) where
  arbitrary = UtxoOut.V0.mkUtxo <$> arbitrary

instance Arbitrary CanonicalCoin where
  arbitrary = CanonicalCoin . CompactCoin <$> arbitrary

instance Arbitrary Committee.V0.GovCommitteeOut where
  arbitrary = genericArbitraryU

instance Arbitrary Committee.V0.CanonicalCommitteeAuthorization where
  arbitrary = fmap Committee.V0.mkCanonicalCommitteeAuthorization arbitrary

instance Arbitrary Committee.V0.CanonicalCommitteeState where arbitrary = genericArbitraryU
