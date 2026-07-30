{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Arbitrary' instances for definitions shared across all protocols
-- (TPraos, Praos and Leios).
module Test.Cardano.Protocol.Arbitrary () where

import Cardano.Ledger.Hashes (HashHeader (HashHeader))
import Cardano.Protocol.Crypto (Crypto)
import Cardano.Protocol.TPraos.BlockHeader (PrevHash (BlockHash, GenesisHash))
import Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod), OCert (..))
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Crypto.Instances ()

instance Crypto c => Arbitrary (OCert c) where
  arbitrary =
    OCert
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary KESPeriod

instance Arbitrary PrevHash where
  arbitrary = frequency [(1, pure GenesisHash), (9999, BlockHash . HashHeader <$> arbitrary)]
