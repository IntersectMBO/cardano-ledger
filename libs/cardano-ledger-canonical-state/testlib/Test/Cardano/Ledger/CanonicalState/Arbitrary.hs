{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.CanonicalState.Arbitrary () where

import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import Test.QuickCheck (Arbitrary (..), Positive (..))

instance Arbitrary Blocks.V0.BlockOut where
  -- starting form the QuickCheck-2.17 there is an arbirary instance for
  -- Natural, so it's possible to use genericArbitraryU directly.
  arbitrary = Blocks.V0.BlockOut . fromIntegral . getPositive @Integer <$> arbitrary
