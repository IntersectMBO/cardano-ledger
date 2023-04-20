{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.TPraos.Arbitrary (
  genBHeader,
  genBlock,
  genCoherentBlock,
) where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN (Signable)
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (BlockNo (..), Nonce, Seed, SlotNo (..))
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, DSIGN)
import Cardano.Ledger.Keys (signedKES)
import Cardano.Protocol.HeaderCrypto (HeaderCrypto (KES, VRF))
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Cardano.Protocol.TPraos.BHeader (
  BHBody (BHBody),
  BHeader (BHeader),
  HashHeader (HashHeader),
  PrevHash (BlockHash, GenesisHash),
 )
import Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod), OCert (..), OCertSignable (..))
import Cardano.Protocol.TPraos.Rules.Overlay (OBftSlot)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState)
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Protocol.TPraos.Create (AllIssuerKeys, mkBHBody, mkBHeader, mkBlock, mkOCert)

instance Crypto c => Arbitrary (HashHeader c) where
  arbitrary = HashHeader <$> arbitrary

instance Arbitrary TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Crypto c
  , HeaderCrypto hc
  , VRF.Signable (VRF hc) ~ SignableRepresentation
  , KES.Signable (KES hc) ~ SignableRepresentation
  ) =>
  Arbitrary (BHeader c hc)
  where
  arbitrary = do
    bhBody <- arbitrary
    hotKey <- arbitrary
    let sig = signedKES () 1 bhBody hotKey
    pure $ BHeader bhBody sig

genBHeader ::
  ( DSIGN.Signable (DSIGN c) (OCertSignable hc)
  , VRF.Signable (VRF hc) Seed
  , KES.Signable (KES hc) (BHBody c hc)
  , Crypto c
  , HeaderCrypto hc
  ) =>
  [AllIssuerKeys c hc r] ->
  Gen (BHeader c hc)
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
  , HeaderCrypto hc
  , VRF.Signable (VRF hc) ~ SignableRepresentation
  ) =>
  Arbitrary (BHBody c hc)
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

instance Crypto c => Arbitrary (PrevHash c) where
  arbitrary = do
    hash <- arbitrary
    frequency [(1, pure GenesisHash), (9999, pure (BlockHash hash))]

instance (Crypto c, HeaderCrypto hc) => Arbitrary (OCert c hc) where
  arbitrary =
    OCert
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary KESPeriod

instance
  ( Era era
  , c ~ EraCrypto era
  , HeaderCrypto hc
  , EraSegWits era
  , KES.Signable (KES hc) ~ SignableRepresentation
  , VRF.Signable (VRF hc) ~ SignableRepresentation
  , Arbitrary (Tx era)
  ) =>
  Arbitrary (Block (BHeader c hc) era)
  where
  arbitrary = Block <$> arbitrary <*> (toTxSeq <$> arbitrary)

-- | Use supplied keys to generate a Block.
genBlock ::
  ( DSIGN.Signable (DSIGN c) (OCertSignable hc)
  , VRF.Signable (VRF hc) Seed
  , KES.Signable (KES hc) (BHBody c hc)
  , EraSegWits era
  , Arbitrary (Tx era)
  , c ~ EraCrypto era
  , HeaderCrypto hc
  ) =>
  [AllIssuerKeys c hc r] ->
  Gen (Block (BHeader c hc) era)
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
  forall era hc r.
  ( EraSegWits era
  , Arbitrary (Tx era)
  , KES.Signable (KES hc) ~ SignableRepresentation
  , DSIGN.Signable (DSIGN (EraCrypto era)) ~ SignableRepresentation
  , PraosCrypto (EraCrypto era) hc
  ) =>
  [AllIssuerKeys (EraCrypto era) hc r] ->
  Gen (Block (BHeader (EraCrypto era) hc) era)
genCoherentBlock aiks = do
  prevHash <- arbitrary :: Gen (HashHeader (EraCrypto era))
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
