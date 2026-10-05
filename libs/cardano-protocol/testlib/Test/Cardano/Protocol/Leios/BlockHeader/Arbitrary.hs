{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary (genHeader, genHeaderBody) where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Version,
  decodeFixedSized,
  decodeRecordNamed,
  natVersion,
 )
import Cardano.Ledger.Block (Block (Block), EbReferencesAnnouncement (EbReferencesAnnouncement))
import Cardano.Ledger.Core (BlockBody, EraBlockBody)
import Cardano.Ledger.MemoBytes (mkMemoized)
import Cardano.Protocol.Crypto (Crypto (KES, VRF))
import Cardano.Protocol.Leios.BlockHeader (
  Header (HeaderConstr),
  HeaderBody (HeaderBodyConstr),
  HeaderBodyRaw (HeaderBodyRaw),
  HeaderRaw (HeaderRaw),
 )
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Protocol.Praos.BlockHeader.Arbitrary ()
import Test.Crypto.Instances ()

instance Arbitrary EbReferencesAnnouncement where
  arbitrary = EbReferencesAnnouncement <$> arbitrary <*> arbitrary

genHeaderBody ::
  (Crypto c, VRF.Signable (VRF c) ~ SignableRepresentation) =>
  Version ->
  Gen (HeaderBody c)
genHeaderBody version =
  fmap (mkMemoized version) $
    HeaderBodyRaw
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
  (Crypto c, VRF.Signable (VRF c) ~ SignableRepresentation) =>
  Arbitrary (HeaderBody c)
  where
  arbitrary = genHeaderBody =<< elements [natVersion @12 .. maxBound]

genHeader ::
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  Version ->
  Gen (Header c)
genHeader version = do
  hBody <- genHeaderBody version
  period <- arbitrary
  sKey <- arbitrary
  let hSig = KES.unsoundPureSignedKES () period hBody sKey
  pure $ mkMemoized version $ HeaderRaw hBody hSig

instance
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  Arbitrary (Header c)
  where
  arbitrary = genHeader =<< elements [natVersion @12 .. maxBound]

deriving newtype instance Crypto c => DecCBOR (HeaderBody c)

instance Crypto c => DecCBOR (HeaderRaw c) where
  decCBOR =
    decodeRecordNamed "HeaderRaw" (const 2) $
      HeaderRaw <$> decCBOR <*> decodeFixedSized

deriving newtype instance Crypto c => DecCBOR (Header c)

instance
  ( Crypto c
  , EraBlockBody era
  , KES.Signable (KES c) ~ SignableRepresentation
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , Arbitrary (BlockBody era)
  ) =>
  Arbitrary (Block (Header c) era)
  where
  arbitrary = Block <$> genHeader (natVersion @12) <*> arbitrary
