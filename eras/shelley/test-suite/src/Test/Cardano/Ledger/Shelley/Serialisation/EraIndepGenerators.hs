{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators (genCoherentBlock) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (
  BlockNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.Crypto (DSIGN, KES, VRF)
import Cardano.Ledger.Shelley.API hiding (SignedDSIGN)
import Cardano.Ledger.Shelley.Core
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import qualified Cardano.Protocol.TPraos.OCert as TP
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  mkBlock,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.Presets (coreNodeKeys)
import Test.Cardano.Protocol.TPraos.Arbitrary ()
import Test.QuickCheck

-- | For some purposes, a totally random block generator may not be suitable.
-- There are tests in the ouroboros-network repository, for instance, that
-- perform some integrity checks on the generated blocks.
--
-- For other purposes, such as the serialization tests in this repository,
-- 'genBlock' is more appropriate.
--
-- This generator uses 'mkBlock' provide more coherent blocks.
genCoherentBlock ::
  forall era.
  ( EraSegWits era
  , Arbitrary (Tx era)
  , VRF.Signable (VRF (EraCrypto era)) ~ SignableRepresentation
  , KES.Signable (KES (EraCrypto era)) ~ SignableRepresentation
  , DSIGN.Signable (DSIGN (EraCrypto era)) ~ SignableRepresentation
  , PraosCrypto (EraCrypto era)
  ) =>
  Gen (Block (BHeader (EraCrypto era)) era)
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
