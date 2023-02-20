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

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (Update era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

deriving newtype instance Arbitrary NominalDiffTimeMicro

maxMetadatumDepth :: Int
maxMetadatumDepth = 2

maxMetadatumListLens :: Int
maxMetadatumListLens = 5

sizedMetadatum :: Int -> Gen Metadatum
sizedMetadatum 0 =
  oneof
    [ I <$> arbitrary
    , B <$> arbitrary
    , S <$> (T.pack <$> arbitrary)
    ]
sizedMetadatum n =
  let xsGen = listOf (sizedMetadatum (n - 1))
   in oneof
        [ Map <$> (zip <$> resize maxMetadatumListLens xsGen <*> xsGen)
        , List <$> resize maxMetadatumListLens xsGen
        , I <$> arbitrary
        , B <$> arbitrary
        , S <$> (T.pack <$> arbitrary)
        ]

instance Arbitrary Metadatum where
  arbitrary = sizedMetadatum maxMetadatumDepth

instance Era era => Arbitrary (ShelleyTxAuxData era) where
  arbitrary = ShelleyTxAuxData <$> arbitrary

maxTxWits :: Int
maxTxWits = 5

instance Arbitrary MIRPot where
  arbitrary = genericArbitraryU

instance Crypto c => Arbitrary (MIRTarget c) where
  arbitrary =
    oneof
      [ StakeAddressesMIR <$> arbitrary
      , SendToOppositePotMIR <$> arbitrary
      ]

instance Arbitrary STS.VotingPeriod where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.OBftSlot c) where -- TODO: Move to TPraos or Shelley
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Arbitrary k, Arbitrary v) => Arbitrary (LM.ListMap k v) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (DelegCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (Delegation c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (PoolCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (ConstitutionalDelegCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (MIRCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (DCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

maxMultiSigDepth :: Int
maxMultiSigDepth = 3

maxMultiSigListLens :: Int
maxMultiSigListLens = 5

sizedMultiSig :: Era era => Int -> Gen (MultiSig era)
sizedMultiSig 0 = RequireSignature <$> arbitrary
sizedMultiSig n =
  oneof
    [ RequireSignature <$> arbitrary
    , RequireAllOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n - 1)))
    , RequireAnyOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n - 1)))
    , RequireMOf <$> arbitrary <*> resize maxMultiSigListLens (listOf (sizedMultiSig (n - 1)))
    ]

instance Era era => Arbitrary (MultiSig era) where
  arbitrary = sizedMultiSig maxMultiSigDepth

instance
  (Crypto c, Arbitrary (PParams (ShelleyEra c))) =>
  Arbitrary (ShelleyGenesis c)
  where
  arbitrary = do
    sgSystemStart <- arbitrary
    sgNetworkMagic <- arbitrary
    sgNetworkId <- arbitrary
    sgActiveSlotsCoeff <- arbitrary
    sgSecurityParam <- arbitrary
    sgEpochLength <- arbitrary
    sgSlotsPerKESPeriod <- arbitrary
    sgMaxKESEvolutions <- arbitrary
    sgSlotLength <- (* 1000000) <$> arbitrary
    sgUpdateQuorum <- arbitrary
    sgMaxLovelaceSupply <- arbitrary
    sgProtocolParams <- arbitrary
    sgGenDelegs <- arbitrary
    sgInitialFunds <- arbitrary
    sgStaking <- arbitrary
    pure ShelleyGenesis {..}

instance Crypto c => Arbitrary (ShelleyGenesisStaking c) where
  arbitrary = ShelleyGenesisStaking <$> arbitrary <*> arbitrary

instance
  ( EraScript era
  , Arbitrary (Script era)
  ) =>
  Arbitrary (ShelleyTxWits era)
  where
  arbitrary =
    ShelleyTxWits
      <$> arbitrary
      <*> (mscriptsToWits <$> arbitrary)
      <*> arbitrary
    where
      mscriptsToWits = Map.fromList . map (\s -> (hashScript @era s, s))

instance Era era => Arbitrary (STS.ShelleyPpupPredFailure era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Era era => Arbitrary (STS.ShelleyPoolPredFailure era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era
  , Arbitrary (STS.PredicateFailure (EraRule "POOL" era))
  , Arbitrary (STS.PredicateFailure (EraRule "DELEG" era))
  ) =>
  Arbitrary (STS.ShelleyDelplPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = recursivelyShrink

instance
  Era era =>
  Arbitrary (STS.ShelleyDelegPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era
  , Arbitrary (STS.PredicateFailure (EraRule "DELPL" era))
  ) =>
  Arbitrary (STS.ShelleyDelegsPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = recursivelyShrink

instance
  ( Era era
  , Arbitrary (STS.PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Arbitrary (STS.ShelleyLedgersPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era
  , Arbitrary (STS.PredicateFailure (EraRule "DELEGS" era))
  , Arbitrary (STS.PredicateFailure (EraRule "UTXOW" era))
  ) =>
  Arbitrary (STS.ShelleyLedgerPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

instance
  ( Era era
  , Arbitrary (STS.PredicateFailure (EraRule "UTXO" era))
  ) =>
  Arbitrary (STS.ShelleyUtxowPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

genTx ::
  ( EraTx era
  , Arbitrary (TxBody era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (TxWits era)
  ) =>
  Gen (ShelleyTx era)
genTx =
  ShelleyTx
    <$> arbitrary
    <*> resize maxTxWits arbitrary
    <*> arbitrary

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
  ( EraSegWits era
  , Mock (EraCrypto era)
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

instance
  ( Era era
  , Arbitrary (STS.PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Arbitrary (ApplyTxError era)
  where
  arbitrary = ApplyTxError <$> arbitrary
  shrink (ApplyTxError xs) = [ApplyTxError xs' | xs' <- shrink xs]
