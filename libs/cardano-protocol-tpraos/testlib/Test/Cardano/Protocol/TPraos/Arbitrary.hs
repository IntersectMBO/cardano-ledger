{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.TPraos.Arbitrary (
  genBHeader,
  genBlock,
  genCoherentBlock,
  VRFNatVal (..),
) where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (BlockNo (..), Nonce, Seed, SlotNo (..))
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Core
import Cardano.Protocol.Crypto (Crypto (KES, VRF), StandardCrypto)
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Cardano.Protocol.TPraos.BHeader (
  BHBody (BHBody),
  BHeader (BHeader),
  HashHeader (HashHeader),
  PrevHash (BlockHash, GenesisHash),
 )
import Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod), OCert (..))
import Cardano.Protocol.TPraos.Rules.Overlay (OBftSlot)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState)
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState)
import Data.Proxy (Proxy (Proxy))
import Generic.Random (genericArbitraryU)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Protocol.TPraos.Create (AllIssuerKeys, mkBHBody, mkBHeader, mkBlock, mkOCert)

newtype VRFNatVal = VRFNatVal Natural
  deriving (Show)

instance Arbitrary VRFNatVal where
  arbitrary =
    VRFNatVal . fromIntegral
      <$> choose @Integer
        ( 0
        , 2
            ^ ( 8
                  * VRF.sizeOutputVRF
                    (Proxy @(VRF StandardCrypto))
              )
        )
  shrink (VRFNatVal v) = VRFNatVal <$> shrinkIntegral v

instance Arbitrary HashHeader where
  arbitrary = HashHeader <$> arbitrary

instance Arbitrary TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary PrtclState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary OBftSlot where
  arbitrary = genericArbitraryU
  shrink = genericShrink

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

genBHeader ::
  ( VRF.Signable (VRF c) Seed
  , KES.Signable (KES c) (BHBody c)
  , Crypto c
  ) =>
  [AllIssuerKeys c r] ->
  Gen (BHeader c)
genBHeader aiks = do
  prevHash <- arbitrary
  allPoolKeys <- elements aiks
  slotNo <- arbitrary
  blockNo <- arbitrary
  epochNonce <- arbitrary
  bodySize <- arbitrary
  bodyHash <- arbitrary
  protVer <- arbitrary
  let kesPeriod = 1
      keyRegKesPeriod = 1
      oCert = mkOCert allPoolKeys 1 (KESPeriod kesPeriod)
      bhBody =
        mkBHBody protVer prevHash allPoolKeys slotNo blockNo epochNonce oCert bodySize bodyHash
  return $ mkBHeader allPoolKeys kesPeriod keyRegKesPeriod bhBody

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

instance Arbitrary PrevHash where
  arbitrary = do
    hash <- arbitrary
    frequency [(1, pure GenesisHash), (9999, pure (BlockHash hash))]

instance Crypto c => Arbitrary (OCert c) where
  arbitrary =
    OCert
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary KESPeriod

instance
  ( Crypto c
  , EraSegWits era
  , KES.Signable (KES c) ~ SignableRepresentation
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , Arbitrary (Tx era)
  ) =>
  Arbitrary (Block (BHeader c) era)
  where
  arbitrary = Block <$> arbitrary <*> (toTxSeq <$> arbitrary)

-- | Use supplied keys to generate a Block.
genBlock ::
  ( Crypto c
  , VRF.Signable (VRF c) Seed
  , KES.Signable (KES c) (BHBody c)
  , EraSegWits era
  , Arbitrary (Tx era)
  ) =>
  [AllIssuerKeys c r] ->
  Gen (Block (BHeader c) era)
genBlock aiks = Block <$> genBHeader aiks <*> (toTxSeq <$> arbitrary)

-- | For some purposes, a totally random block generator may not be suitable.
-- There are tests in the ouroboros-network repository, for instance, that
-- perform some integrity checks on the generated blocks.
--
-- For other purposes, such as the serialization tests in this repository,
-- 'genBlock' is more appropriate.
--
-- This generator uses 'mkBlock' provide more coherent blocks.
genCoherentBlock ::
  forall era r c.
  ( EraSegWits era
  , Arbitrary (Tx era)
  , KES.Signable (KES c) ~ SignableRepresentation
  , PraosCrypto c
  ) =>
  [AllIssuerKeys c r] ->
  Gen (Block (BHeader c) era)
genCoherentBlock aiks = do
  prevHash <- arbitrary :: Gen HashHeader
  allPoolKeys <- elements aiks
  txs <- arbitrary
  curSlotNo <- SlotNo <$> choose (0, 10)
  curBlockNo <- BlockNo <$> choose (0, 100)
  epochNonce <- arbitrary :: Gen Nonce
  let kesPeriod = 1
      keyRegKesPeriod = 1
      ocert = mkOCert allPoolKeys 1 (KESPeriod kesPeriod)
  return $
    mkBlock
      prevHash
      allPoolKeys
      txs
      curSlotNo
      curBlockNo
      epochNonce
      kesPeriod
      keyRegKesPeriod
      ocert
