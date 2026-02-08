{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.CanonicalState.Arbitrary () where

import Cardano.Ledger.CanonicalState.BasicTypes (CanonicalCoin (..))
import Cardano.Ledger.CanonicalState.Conway ()
import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.Nonces.V0 as Nonces.V0
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

instance Arbitrary (Nonces.V0.CanonicalNonce) where
  arbitrary = genericArbitraryU

instance Arbitrary a => Arbitrary (Nonces.V0.CanonicalWithOrigin a) where
  arbitrary = genericArbitraryU

instance Arbitrary Nonces.V0.NoncesState where
  arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Nonces.V0.NoncesOut
