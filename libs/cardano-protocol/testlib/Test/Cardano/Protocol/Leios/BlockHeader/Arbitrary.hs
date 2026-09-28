{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary (genHeader) where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Binary (DecCBOR, Version, natVersion)
import Cardano.Ledger.Block (Block (Block), EbReferencesAnnouncement (EbReferencesAnnouncement))
import Cardano.Ledger.Core (BlockBody, EraBlockBody)
import Cardano.Ledger.MemoBytes (mkMemoized)
import Cardano.Protocol.Crypto (Crypto (KES, VRF))
import Cardano.Protocol.Leios.BlockHeader (
  Header (HeaderConstr),
  HeaderBody (HeaderBody),
  HeaderRaw (HeaderRaw),
 )
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Protocol.Praos.BlockHeader.Arbitrary ()
import Test.Crypto.Instances ()

instance Arbitrary EbReferencesAnnouncement where
  arbitrary = EbReferencesAnnouncement <$> arbitrary <*> arbitrary

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

genHeader ::
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  Version ->
  Gen (Header c)
genHeader version = do
  hBody <- arbitrary
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
