{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Leios.Arbitrary () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Protocol.Crypto (Crypto (KES, VRF))
import Cardano.Protocol.Leios.BlockHeader (
  EbAnnouncement (EbAnnouncement),
  Header (Header),
  HeaderBody (HeaderBody),
 )
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Protocol.Praos.Arbitrary ()
import Test.Crypto.Instances ()

instance Arbitrary EbAnnouncement where
  arbitrary = EbAnnouncement <$> arbitrary <*> arbitrary

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
