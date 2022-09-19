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
  maxTxWits,
)
where

import Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import Cardano.Ledger.BaseTypes (
  BlockNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.Coin (CompactForm (..))
import Cardano.Ledger.Crypto (Crypto, DSIGN)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API hiding (SignedDSIGN)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (FutureGenDeleg, StashedAVVMAddresses)
import Cardano.Ledger.Shelley.PoolRank (
  Likelihood (..),
  LogWeight (..),
  PerformanceEstimate (..),
 )
import Cardano.Ledger.Shelley.RewardUpdate (
  FreeVars (..),
  Pulser,
  PulsingRewUpdate (..),
  RewardAns (..),
  RewardPulser (..),
  RewardSnapShot (..),
 )
import Cardano.Ledger.Shelley.Rewards (
  LeaderOnlyReward (..),
  PoolRewardInfo (..),
  StakeShare (..),
 )
import qualified Cardano.Ledger.Shelley.Rules as STS
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.UMapCompact (RDPair (..), Trip (Triple), UMap (UMap))
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import qualified Cardano.Protocol.TPraos.BHeader as TP
import qualified Cardano.Protocol.TPraos.OCert as TP
import qualified Cardano.Protocol.TPraos.Rules.Overlay as STS
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS (PrtclState)
import qualified Cardano.Protocol.TPraos.Rules.Tickn as STS
import Control.State.Transition (STS (State))
import qualified Data.ListMap as LM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.VMap as VMap
import Data.Word (Word64)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  mkBlock,
  mkBlockHeader,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.Presets (coreNodeKeys)
import Test.QuickCheck (
  Arbitrary,
  Gen,
  arbitrary,
  choose,
  chooseBoundedIntegral,
  chooseInt,
  elements,
  frequency,
  genericShrink,
  listOf,
  oneof,
  recursivelyShrink,
  resize,
  shrink,
  vectorOf,
 )

-- =======================================================

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

type MockGen era =
  ( Mock (EraCrypto era)
  , Arbitrary (VerKeyDSIGN (DSIGN (EraCrypto era)))
  )

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

instance Crypto c => Arbitrary (TP.HashHeader c) where
  arbitrary = TP.HashHeader <$> arbitrary

instance Crypto c => Arbitrary (Wdrl c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (EraCrypto era)) => Arbitrary (ProposedPPUpdates era) where
  arbitrary = ProposedPPUpdates <$> pure Map.empty

instance (Era era, Mock (EraCrypto era)) => Arbitrary (Update era) where
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

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (Value era)) =>
  Arbitrary (ShelleyTxOut era)
  where
  arbitrary = ShelleyTxOut <$> arbitrary <*> arbitrary

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

instance Arbitrary STS.TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (PState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (InstantaneousRewards c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (FutureGenDeleg c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Arbitrary k, Arbitrary v) => Arbitrary (LM.ListMap k v) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary RDPair where
  arbitrary = RDPair <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (Trip c) where
  arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (UMap c) where
  arbitrary = UMap <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (DState c) where
  arbitrary =
    DState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

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

instance (Era era, Mock (EraCrypto era)) => Arbitrary (PPUPState era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (DPState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( EraTxOut era
  , Mock (EraCrypto era)
  , Arbitrary (TxOut era)
  , Arbitrary (State (EraRule "PPUP" era))
  ) =>
  Arbitrary (UTxOState era)
  where
  arbitrary = genericArbitraryU
  shrink = recursivelyShrink

instance Crypto c => Arbitrary (IncrementalStake c) where
  arbitrary = IStake <$> arbitrary <*> arbitrary
  shrink = genericShrink

-- The 'genericShrink' function returns first the immediate subterms of a
-- value (in case it is a recursive data-type), and then shrinks the value
-- itself. Since 'UTxOState' is not a recursive data-type, there are no
-- subterms, and we can use `recursivelyShrink` directly. This is particularly
-- important when abstracting away the different fields of the ledger state,
-- since the generic subterms instances will overlap due to GHC not having
-- enough context to infer if 'a' and 'b' are the same types (since in this
-- case this will depend on the definition of 'era').
--
-- > instance OVERLAPPING_ GSubtermsIncl (K1 i a) a where
-- > instance OVERLAPPING_ GSubtermsIncl (K1 i a) b where

instance
  ( EraTxOut era
  , Mock (EraCrypto era)
  , Arbitrary (TxOut era)
  , Arbitrary (State (EraRule "PPUP" era))
  ) =>
  Arbitrary (LedgerState era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( EraTxOut era
  , Mock (EraCrypto era)
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (PParams era)
  , Arbitrary (State (EraRule "PPUP" era))
  , Arbitrary (StashedAVVMAddresses era)
  ) =>
  Arbitrary (NewEpochState era)
  where
  arbitrary = genericArbitraryU

instance
  ( EraTxOut era
  , Mock (EraCrypto era)
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (PParams era)
  , Arbitrary (State (EraRule "PPUP" era))
  ) =>
  Arbitrary (EpochState era)
  where
  arbitrary =
    EpochState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (LeaderOnlyReward c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (RewardUpdate c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Likelihood where
  arbitrary = Likelihood <$> arbitrary

instance Arbitrary LogWeight where
  arbitrary = LogWeight <$> arbitrary

instance Crypto c => Arbitrary (NonMyopic c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (SnapShot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (SnapShots c) where
  arbitrary = do
    mark <- arbitrary
    set <- arbitrary
    go <- arbitrary
    fee <- arbitrary
    pure $ SnapShots mark (calculatePoolDistr mark) set go fee
  shrink = genericShrink

instance Arbitrary PerformanceEstimate where
  arbitrary = PerformanceEstimate <$> arbitrary

-- | In the system, Stake never contains more than the sum of all Ada (which is constant).
-- This makes it safe to store individual Coins (in CompactForm) as Word64. But we must
-- be careful that we never generate Stake where the sum of all the coins exceeds (maxBound :: Word64)
-- There will never be a real Stake in the system with that many Ada, because total Ada is constant.
-- So using a restricted Arbitrary Generator is OK.
instance Crypto c => Arbitrary (Stake c) where
  arbitrary = Stake <$> (VMap.fromMap <$> theMap)
    where
      genWord64 :: Int -> Gen Word64
      genWord64 n =
        frequency
          [ (3, chooseBoundedIntegral (1, 100))
          , (2, chooseBoundedIntegral (101, 10000))
          , (1, chooseBoundedIntegral (1, maxBound `div` (fromIntegral n)))
          ]
      theMap = do
        n <- frequency [(3, chooseInt (1, 20)), (2, chooseInt (21, 150)), (1, chooseInt (151, 1000))]
        let pair = (,) <$> arbitrary <*> (CompactCoin <$> (genWord64 n))
        list <- frequency [(1, pure []), (99, vectorOf n pair)]
        pure (Map.fromList list)

instance Arbitrary AccountState where
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
  (Mock c, Arbitrary (PParams (ShelleyEra c))) =>
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
  ( Mock (EraCrypto era)
  , EraScript era
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
  , Mock (EraCrypto era)
  , Arbitrary (STS.PredicateFailure (EraRule "POOL" era))
  , Arbitrary (STS.PredicateFailure (EraRule "DELEG" era))
  ) =>
  Arbitrary (STS.ShelleyDelplPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = recursivelyShrink

instance
  (Era era, Mock (EraCrypto era)) =>
  Arbitrary (STS.ShelleyDelegPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era
  , Mock (EraCrypto era)
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
  ( EraTx era
  , Arbitrary (TxBody era)
  , Arbitrary (Value era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (Script era)
  , Arbitrary (TxWits era)
  ) =>
  Arbitrary (ShelleyTx era)
  where
  arbitrary = genTx

instance
  ( EraTxBody era
  , EraSegWits era
  , Mock (EraCrypto era)
  , Arbitrary (Tx era)
  , h ~ BHeader (EraCrypto era)
  ) =>
  Arbitrary (Block h era)
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

instance (Mock c) => Arbitrary (PulsingRewUpdate c) where
  arbitrary =
    oneof
      [ Complete <$> arbitrary
      , Pulsing <$> arbitrary <*> arbitrary
      ]

instance
  Mock c =>
  Arbitrary (RewardSnapShot c)
  where
  arbitrary =
    RewardSnapShot
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  Mock c =>
  Arbitrary (PoolRewardInfo c)
  where
  arbitrary =
    PoolRewardInfo
      <$> (StakeShare <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  Mock c =>
  Arbitrary (FreeVars c)
  where
  arbitrary =
    FreeVars
      <$> arbitrary {- addrsRew -}
      <*> arbitrary {- totalStake -}
      <*> arbitrary {- pp_mv -}
      <*> arbitrary {- poolRewardInfo -}
      <*> arbitrary {- delegations -}

instance
  Mock c =>
  Arbitrary (Pulser c)
  where
  arbitrary = RSLP <$> arbitrary <*> arbitrary <*> arbitrary <*> (RewardAns <$> arbitrary <*> arbitrary)
