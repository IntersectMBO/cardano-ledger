{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators (
  genCoherentBlock,
  MockGen,
) where

import Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import Cardano.Ledger.BaseTypes (
  BlockNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.Crypto (Crypto, DSIGN)
import Cardano.Ledger.Shelley.API hiding (SignedDSIGN)
import Cardano.Ledger.Shelley.Core
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import qualified Cardano.Protocol.TPraos.BHeader as TP
import qualified Cardano.Protocol.TPraos.OCert as TP
import qualified Cardano.Protocol.TPraos.Rules.Overlay as STS
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS (PrtclState)
import qualified Cardano.Protocol.TPraos.Rules.Tickn as STS
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  mkBlock,
  mkBlockHeader,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.Presets (coreNodeKeys)
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

type MockGen era =
  ( Mock (EraCrypto era)
  , Arbitrary (VerKeyDSIGN (DSIGN (EraCrypto era)))
  )

instance Mock c => Arbitrary (BHeader c) where -- TODO: Move to TPraos
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
        ocert = mkOCert allPoolKeys 1 (TP.KESPeriod kesPeriod)
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

instance Crypto c => Arbitrary (TP.HashHeader c) where -- TODO: Move to TPraos
  arbitrary = TP.HashHeader <$> arbitrary

instance Arbitrary STS.TicknState where -- TODO: Move to TPraos or Shelley
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.PrtclState c) where -- TODO: Move to TPraos or Shelley
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.OBftSlot c) where -- TODO: Move to TPraos or Shelley
  arbitrary = genericArbitraryU
  shrink = genericShrink

genBlock ::
  forall era h.
  ( EraSegWits era
  , Mock (EraCrypto era)
  , Arbitrary (Tx era)
  , h ~ BHeader (EraCrypto era)
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
      ocert = mkOCert allPoolKeys 1 (TP.KESPeriod kesPeriod)
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

instance
  ( EraTxBody era
  , EraSegWits era
  , Mock (EraCrypto era)
  , Arbitrary (Tx era)
  , h ~ BHeader (EraCrypto era)
  ) =>
  Arbitrary (Block h era) -- TODO: Move to TPraos
  where
  arbitrary = genBlock
