{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Praos.Arbitrary () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Hashes (HashHeader (HashHeader))
import Cardano.Protocol.Crypto (Crypto (KES, VRF))
import Cardano.Protocol.Praos.BlockHeader (Header (Header), HeaderBody (HeaderBody))
import Cardano.Protocol.Praos.VRF (InputVRF, mkInputVRF)
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

instance Arbitrary InputVRF where
  arbitrary = mkInputVRF <$> arbitrary <*> arbitrary

instance
  (Crypto c, VRF.Signable (VRF c) ~ SignableRepresentation) =>
  Arbitrary (HeaderBody c)
  where
  arbitrary =
    HeaderBody
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

instance
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  Arbitrary (Header c)
  where
  arbitrary = do
    hBody <- arbitrary
    period <- arbitrary
    sKey <- arbitrary
    let hSig = KES.unsoundPureSignedKES () period hBody sKey
    pure $ Header hBody hSig
