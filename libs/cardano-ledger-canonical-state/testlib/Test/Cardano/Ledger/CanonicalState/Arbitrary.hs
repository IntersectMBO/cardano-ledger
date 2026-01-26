{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.CanonicalState.Arbitrary () where

import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import Test.QuickCheck (Arbitrary (..), Positive (..))

instance Arbitrary Blocks.V0.BlockOut where
  arbitrary = Blocks.V0.BlockOut . fromIntegral . getPositive @Integer <$> arbitrary
