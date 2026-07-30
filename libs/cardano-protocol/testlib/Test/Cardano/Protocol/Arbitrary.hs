{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Arbitrary () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Core (BlockBody, EraBlockBody, TopTx, Tx)
import Cardano.Ledger.Hashes (HashHeader (HashHeader))
import Cardano.Protocol.Crypto (Crypto (KES, VRF))
import Cardano.Protocol.TPraos.BlockHeader (
  BHBody (BHBody),
  BHeader (BHeader),
  PrevHash (BlockHash, GenesisHash),
 )
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

instance
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  ) =>
  Arbitrary (BHBody c)
  where
  arbitrary =
    BHBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  Arbitrary (BHeader c)
  where
  arbitrary = do
    bhBody <- arbitrary
    hotKey <- arbitrary
    let sig = KES.unsoundPureSignedKES () 1 bhBody hotKey
    pure $ BHeader bhBody sig

instance
  ( Crypto c
  , EraBlockBody era
  , KES.Signable (KES c) ~ SignableRepresentation
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , Arbitrary (Tx TopTx era)
  , Arbitrary (BlockBody era)
  ) =>
  Arbitrary (Block (BHeader c) era)
  where
  arbitrary =
    Block
      <$> arbitrary
      <*> arbitrary
