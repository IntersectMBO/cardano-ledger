{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.TPraos.Arbitrary where

import Cardano.Ledger.BaseTypes (BlockNo (BlockNo), Nonce, SlotNo (SlotNo))
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader (HashHeader))
import Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import Cardano.Protocol.TPraos.Rules.Overlay (OBftSlot)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState)
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Protocol.TPraos.ConcreteCryptoTypes (Mock)
import Test.Cardano.Protocol.TPraos.Core (mkBlock, mkBlockHeader, mkOCert)
import Test.Cardano.Protocol.TPraos.Presets (coreNodeKeys)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements, genericShrink, shrink)

instance
  ( Era era
  , EraTxBody era
  , EraSegWits era
  , Mock (EraCrypto era)
  , Arbitrary (Tx era)
  , h ~ BHeader (EraCrypto era)
  , Arbitrary (BHeader (EraCrypto era))
  ) =>
  Arbitrary (Block h era)
  where
  arbitrary = genBlock

genBlock ::
  forall era h.
  ( EraSegWits era
  , Arbitrary (Tx era)
  , h ~ BHeader (EraCrypto era)
  , Arbitrary (BHeader (EraCrypto era))
  ) =>
  Gen (Block h era)
genBlock = Block <$> arbitrary <*> (toTxSeq @era <$> arbitrary)

-- | For some purposes, a totally random block generator may not be suitable.
-- There are tests in the ouroboros-network repository, for instance, that
-- perform some integrity checks on the generated blocks.
--
-- For other purposes, such as the serialization tests in this repository,
-- 'genBlock' is more appropriate.
--
-- This generator uses 'mkBlock' provide more coherent blocks.
genCoherentBlock ::
  forall era h.
  ( Mock (EraCrypto era)
  , EraSegWits era
  , Arbitrary (Tx era)
  , h ~ BHeader (EraCrypto era)
  , Arbitrary (HashHeader (EraCrypto era))
  ) =>
  Gen (Block h era)
genCoherentBlock = do
  let ksCoreNodes = coreNodeKeys defaultConstants
  prevHash <- arbitrary :: Gen (HashHeader (EraCrypto era))
  allPoolKeys <- elements (map snd ksCoreNodes)
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

instance Crypto c => Arbitrary (HashHeader c) where
  arbitrary = HashHeader <$> arbitrary

instance Crypto c => Arbitrary (OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (BHeader c) where
  arbitrary = do
    prevHash <- arbitrary :: Gen (HashHeader c)
    allPoolKeys <- elements (map snd (coreNodeKeys defaultConstants))
    curSlotNo <- arbitrary
    curBlockNo <- arbitrary
    epochNonce <- arbitrary :: Gen Nonce
    bodySize <- arbitrary
    bodyHash <- arbitrary
    let kesPeriod = 1
        keyRegKesPeriod = 1
        ocert = mkOCert allPoolKeys 1 (KESPeriod kesPeriod)
    protVer <- arbitrary
    return $
      mkBlockHeader
        protVer
        prevHash
        allPoolKeys
        curSlotNo
        curBlockNo
        epochNonce
        kesPeriod
        keyRegKesPeriod
        ocert
        bodySize
        bodyHash
