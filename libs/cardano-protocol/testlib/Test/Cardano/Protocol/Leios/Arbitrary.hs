{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Leios.Arbitrary () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Binary (DecCBOR)
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Core (BlockBody, EraBlockBody)
import Cardano.Protocol.Crypto (Crypto (KES, VRF))
import Cardano.Protocol.Leios.BlockHeader (
  EbAnnouncement (EbAnnouncement),
  Header (Header, HeaderConstr),
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
  arbitrary = Block <$> arbitrary <*> arbitrary
