{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators
  ( genCoherentBlock,
    genHash,
    genShelleyAddress,
    genByronAddress,
    MockGen,
    maxTxWits,
  )
where

import Cardano.Crypto.DSIGN.Class
  ( DSIGNAlgorithm,
    SignedDSIGN (..),
    rawDeserialiseSigDSIGN,
    sizeSigDSIGN,
  )
import Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    BlockNo (..),
    BlocksMade (..),
    DnsName,
    EpochSize (..),
    NonNegativeInterval,
    PositiveInterval,
    PositiveUnitInterval,
    SlotNo (..),
    UnitInterval,
    Url,
    mkActiveSlotCoeff,
    mkCertIxPartial,
    mkNonceFromNumber,
    mkTxIxPartial,
    promoteRatio,
    textToDns,
    textToUrl,
  )
import Cardano.Ledger.Coin (CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Core
  ( Era,
    EraCrypto,
    EraScript (..),
    EraSegWits (..),
    Reward (..),
    RewardType (..),
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
import Cardano.Ledger.Keys.Bootstrap (ChainCode (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.API hiding (SignedDSIGN, TxBody)
import Cardano.Ledger.Shelley.LedgerState (FutureGenDeleg, StashedAVVMAddresses)
import qualified Cardano.Ledger.Shelley.Metadata as MD
import Cardano.Ledger.Shelley.PoolRank
  ( Likelihood (..),
    LogWeight (..),
    PerformanceEstimate (..),
  )
import Cardano.Ledger.Shelley.RewardUpdate
  ( FreeVars (..),
    Pulser,
    PulsingRewUpdate (..),
    RewardAns (..),
    RewardPulser (..),
    RewardSnapShot (..),
  )
import Cardano.Ledger.Shelley.Rewards
  ( LeaderOnlyReward (..),
    PoolRewardInfo (..),
    StakeShare (..),
  )
import qualified Cardano.Ledger.Shelley.Rules as STS
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.UnifiedMap (Trip (Triple), Triple, UMap (UnifiedMap), UnifiedMap)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import qualified Cardano.Protocol.TPraos.BHeader as TP
import qualified Cardano.Protocol.TPraos.OCert as TP
import qualified Cardano.Protocol.TPraos.Rules.Overlay as STS
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS (PrtclState)
import qualified Cardano.Protocol.TPraos.Rules.Tickn as STS
import Control.State.Transition (STS (State))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ListMap as LM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Time
import Data.Typeable (Typeable)
import qualified Data.VMap as VMap
import Data.Word (Word16, Word32, Word64, Word8)
import Generic.Random (genericArbitraryU)
import System.Random.Stateful (uniformByteStringM)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.Random (QC (..), mkDummyHash)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( mkBlock,
    mkBlockHeader,
    mkOCert,
  )
import Test.Cardano.Ledger.Shelley.Generator.Presets (coreNodeKeys)
import Test.Cardano.Ledger.Shelley.Serialisation.Generators.Bootstrap
  ( genBootstrapAddress,
    genSignature,
  )
import Test.Cardano.Ledger.Shelley.Utils (unsafeBoundRational)
import Test.QuickCheck
  ( Arbitrary,
    Gen,
    Positive (..),
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

genHash :: forall a h. HashAlgorithm h => Gen (Hash.Hash h a)
genHash = mkDummyHash <$> (arbitrary :: Gen Int)

instance Hash.HashAlgorithm (CC.HASH c) => Arbitrary (SafeHash c i) where
  arbitrary = unsafeMakeSafeHash <$> arbitrary

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

type MockGen era =
  ( Mock (EraCrypto era),
    Arbitrary (VerKeyDSIGN (DSIGN (EraCrypto era)))
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

instance DSIGNAlgorithm c => Arbitrary (SignedDSIGN c a) where
  arbitrary =
    SignedDSIGN . fromJust . rawDeserialiseSigDSIGN
      <$> (genByteString . fromIntegral $ sizeSigDSIGN (Proxy @c))

instance CC.Crypto c => Arbitrary (BootstrapWitness c) where
  arbitrary = do
    key <- arbitrary
    sig <- genSignature
    chainCode <- ChainCode <$> arbitrary
    attributes <- arbitrary
    pure $ BootstrapWitness key sig chainCode attributes

instance CC.Crypto c => Arbitrary (TP.HashHeader c) where
  arbitrary = TP.HashHeader <$> genHash

instance (Typeable kr, CC.Crypto c) => Arbitrary (WitVKey kr c) where
  arbitrary =
    WitVKey
      <$> arbitrary
      <*> arbitrary

instance CC.Crypto c => Arbitrary (Wdrl c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (EraCrypto era)) => Arbitrary (ProposedPPUpdates era) where
  arbitrary = ProposedPPUpdates <$> pure Map.empty

instance (Era era, Mock (EraCrypto era)) => Arbitrary (Update era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

maxMetadatumDepth :: Int
maxMetadatumDepth = 2

maxMetadatumListLens :: Int
maxMetadatumListLens = 5

sizedMetadatum :: Int -> Gen MD.Metadatum
sizedMetadatum 0 =
  oneof
    [ MD.I <$> arbitrary,
      MD.B <$> arbitrary,
      MD.S <$> (T.pack <$> arbitrary)
    ]
sizedMetadatum n =
  let xsGen = listOf (sizedMetadatum (n - 1))
   in oneof
        [ MD.Map <$> (zip <$> resize maxMetadatumListLens xsGen <*> xsGen),
          MD.List <$> resize maxMetadatumListLens xsGen,
          MD.I <$> arbitrary,
          MD.B <$> arbitrary,
          MD.S <$> (T.pack <$> arbitrary)
        ]

instance Arbitrary MD.Metadatum where
  arbitrary = sizedMetadatum maxMetadatumDepth

instance Era era => Arbitrary (MD.ShelleyTxAuxData era) where
  arbitrary = MD.ShelleyTxAuxData <$> arbitrary

maxTxWits :: Int
maxTxWits = 5

instance CC.Crypto c => Arbitrary (TxId c) where
  arbitrary = TxId <$> arbitrary

instance CC.Crypto c => Arbitrary (TxIn c) where
  arbitrary =
    TxIn
      <$> (TxId <$> arbitrary)
      <*> arbitrary

instance
  (Core.EraTxOut era, Mock (EraCrypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (ShelleyTxOut era)
  where
  arbitrary = ShelleyTxOut <$> arbitrary <*> arbitrary

instance Arbitrary Nonce where
  arbitrary =
    oneof
      [ return NeutralNonce,
        mkNonceFromNumber <$> choose (1, 123 :: Word64)
      ]

instance Arbitrary UnitInterval where
  arbitrary = do
    x :: Word64 <- arbitrary
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (if x > y then y % x else x % y)

instance Arbitrary PositiveUnitInterval where
  arbitrary = do
    Positive (x :: Word64) <- arbitrary
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (if x > y then y % x else x % y)

instance Arbitrary PositiveInterval where
  arbitrary = do
    Positive (x :: Word64) <- arbitrary
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (x % y)

instance Arbitrary NonNegativeInterval where
  arbitrary = do
    x :: Word64 <- arbitrary
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (x % y)

instance CC.Crypto c => Arbitrary (KeyHash a c) where
  arbitrary = KeyHash <$> genHash

instance Arbitrary MIRPot where
  arbitrary = genericArbitraryU

instance CC.Crypto c => Arbitrary (MIRTarget c) where
  arbitrary =
    oneof
      [ StakeAddressesMIR <$> arbitrary,
        SendToOppositePotMIR <$> arbitrary
      ]

instance Arbitrary STS.VotingPeriod where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Coin where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = Coin <$> choose (0, 1000)
  shrink (Coin i) = Coin <$> shrink i

instance Arbitrary DeltaCoin where
  arbitrary = DeltaCoin <$> choose (-1000, 1000)

instance CC.Crypto c => Arbitrary (Addr c) where
  arbitrary = oneof [genShelleyAddress, genByronAddress]

genShelleyAddress :: CC.Crypto c => Gen (Addr c)
genShelleyAddress = Addr <$> arbitrary <*> arbitrary <*> arbitrary

genByronAddress :: Gen (Addr c)
genByronAddress = AddrBootstrap <$> genBootstrapAddress

instance CC.Crypto c => Arbitrary (StakeReference c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (Credential r c) where
  arbitrary =
    oneof
      [ ScriptHashObj . ScriptHash <$> genHash,
        KeyHashObj <$> arbitrary
      ]

instance Arbitrary TxIx where
  arbitrary = mkTxIxPartial . toInteger <$> (arbitrary :: Gen Word16)

instance Arbitrary CertIx where
  arbitrary = mkCertIxPartial . toInteger <$> (arbitrary :: Gen Word16)

instance Arbitrary Ptr where
  arbitrary = Ptr <$> genSlotNo <*> arbitrary <*> arbitrary
    where
      -- We are only allowing 32bit large slot numbers in Ptrs
      genSlotNo = SlotNo . (fromIntegral :: Word32 -> Word64) <$> arbitrary

instance CC.Crypto c => Arbitrary (RewardAcnt c) where
  arbitrary = RewardAcnt <$> arbitrary <*> arbitrary

instance Arbitrary Network where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Arbitrary (VerKeyDSIGN (DSIGN c))) => Arbitrary (VKey kd c) where
  arbitrary = VKey <$> arbitrary

instance Arbitrary ProtVer where
  arbitrary = ProtVer <$> arbitrary <*> arbitrary

instance CC.Crypto c => Arbitrary (ScriptHash c) where
  arbitrary = ScriptHash <$> genHash

instance CC.Crypto c => Arbitrary (AuxiliaryDataHash c) where
  arbitrary = AuxiliaryDataHash <$> arbitrary

instance Arbitrary STS.TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (STS.PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

deriving instance
  ( Core.EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Core.TxOut era)
  ) =>
  Arbitrary (UTxO era)

instance CC.Crypto c => Arbitrary (PState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (InstantaneousRewards c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (FutureGenDeleg c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Arbitrary k, Arbitrary v) => Arbitrary (LM.ListMap k v) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (GenDelegs c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (GenDelegPair c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (Triple c) where
  arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (UnifiedMap c) where
  arbitrary = UnifiedMap <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (DState c) where
  arbitrary =
    DState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance CC.Crypto c => Arbitrary (DelegCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (Delegation c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (PoolCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (GenesisDelegCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (MIRCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (DCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (EraCrypto era)) => Arbitrary (PPUPState era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (DPState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Core.EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Core.TxOut era),
    Arbitrary (State (Core.EraRule "PPUP" era))
  ) =>
  Arbitrary (UTxOState era)
  where
  arbitrary = genericArbitraryU
  shrink = recursivelyShrink

instance CC.Crypto c => Arbitrary (IncrementalStake c) where
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
  ( Core.EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Core.TxOut era),
    Arbitrary (State (Core.EraRule "PPUP" era))
  ) =>
  Arbitrary (LedgerState era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Core.EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Core.TxOut era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.PParams era),
    Arbitrary (State (Core.EraRule "PPUP" era)),
    Arbitrary (StashedAVVMAddresses era)
  ) =>
  Arbitrary (NewEpochState era)
  where
  arbitrary = genericArbitraryU

instance CC.Crypto c => Arbitrary (BlocksMade c) where
  arbitrary = BlocksMade <$> arbitrary

instance CC.Crypto c => Arbitrary (PoolDistr c) where
  arbitrary =
    PoolDistr . Map.fromList
      <$> listOf ((,) <$> arbitrary <*> genVal)
    where
      genVal = IndividualPoolStake <$> arbitrary <*> genHash

instance
  ( Core.EraTxOut era,
    Mock (EraCrypto era),
    Arbitrary (Core.TxOut era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.PParams era),
    Arbitrary (State (Core.EraRule "PPUP" era))
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

instance Arbitrary RewardType where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (Reward c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (LeaderOnlyReward c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (RewardUpdate c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (STS.OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary ActiveSlotCoeff where
  arbitrary = mkActiveSlotCoeff <$> arbitrary

instance Arbitrary Likelihood where
  arbitrary = Likelihood <$> arbitrary

instance Arbitrary LogWeight where
  arbitrary = LogWeight <$> arbitrary

instance CC.Crypto c => Arbitrary (NonMyopic c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (SnapShot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto c => Arbitrary (SnapShots c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary PerformanceEstimate where
  arbitrary = PerformanceEstimate <$> arbitrary

deriving instance Arbitrary (CompactForm Coin)

-- | In the system, Stake never contains more than the sum of all Ada (which is constant).
-- This makes it safe to store individual Coins (in CompactForm) as Word64. But we must
-- be careful that we never generate Stake where the sum of all the coins exceeds (maxBound :: Word64)
-- There will never be a real Stake in the system with that many Ada, because total Ada is constant.
-- So using a restricted Arbitrary Generator is OK.
instance CC.Crypto c => Arbitrary (Stake c) where
  arbitrary = Stake <$> (VMap.fromMap <$> theMap)
    where
      genWord64 :: Int -> Gen Word64
      genWord64 n =
        frequency
          [ (3, chooseBoundedIntegral (1, 100)),
            (2, chooseBoundedIntegral (101, 10000)),
            (1, chooseBoundedIntegral (1, maxBound `div` (fromIntegral n)))
          ]
      theMap = do
        n <- frequency [(3, chooseInt (1, 20)), (2, chooseInt (21, 150)), (1, chooseInt (151, 1000))]
        let pair = (,) <$> arbitrary <*> (CompactCoin <$> (genWord64 n))
        list <- frequency [(1, pure []), (99, vectorOf n pair)]
        pure (Map.fromList list)

instance CC.Crypto c => Arbitrary (PoolParams c) where
  arbitrary =
    PoolParams
      <$> arbitrary
      <*> genHash
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PoolMetadata where
  arbitrary = (`PoolMetadata` BS.pack "bytestring") <$> arbitrary

instance Arbitrary Url where
  arbitrary = return . fromJust $ textToUrl "text"

instance Arbitrary StakePoolRelay where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Port where
  arbitrary = fromIntegral @Word8 @Port <$> arbitrary

instance Arbitrary DnsName where
  arbitrary = pure . fromJust $ textToDns "foo.example.com"

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
    [ RequireSignature <$> arbitrary,
      RequireAllOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n - 1))),
      RequireAnyOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n - 1))),
      RequireMOf <$> arbitrary <*> resize maxMultiSigListLens (listOf (sizedMultiSig (n - 1)))
    ]

instance Era era => Arbitrary (MultiSig era) where
  arbitrary = sizedMultiSig maxMultiSigDepth

-- |
-- Generate a byte string of a given size.
genByteString :: Int -> Gen BS.ByteString
genByteString size = uniformByteStringM size QC

genUTCTime :: Gen Time.UTCTime
genUTCTime = do
  year <- arbitrary
  dayOfYear <- arbitrary
  diff <- arbitrary
  pure $
    Time.UTCTime
      (Time.fromOrdinalDate year dayOfYear)
      (Time.picosecondsToDiffTime diff)

instance
  (Mock (EraCrypto era), Arbitrary (ShelleyPParams era)) =>
  Arbitrary (ShelleyGenesis era)
  where
  arbitrary =
    ShelleyGenesis
      <$> genUTCTime -- sgSystemStart
      <*> arbitrary -- sgNetworkMagic
      <*> arbitrary -- sgNetworkId
      <*> arbitrary -- sgActiveSlotsCoeff
      <*> arbitrary -- sgSecurityParam
      <*> (EpochSize <$> arbitrary) -- sgEpochLength
      <*> arbitrary -- sgSlotsPerKESPeriod
      <*> arbitrary -- sgMaxKESEvolutions
      <*> (fromInteger <$> arbitrary) -- sgSlotLength
      <*> arbitrary -- sgUpdateQuorum
      <*> arbitrary -- sgMaxLovelaceSupply
      <*> arbitrary -- sgProtocolParams
      <*> arbitrary -- sgGenDelegs
      <*> arbitrary -- sgInitialFunds
      <*> (ShelleyGenesisStaking <$> arbitrary <*> arbitrary) -- sgStaking

instance
  ( Mock (EraCrypto era),
    EraScript era,
    Arbitrary (Core.Script era)
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
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (STS.PredicateFailure (Core.EraRule "POOL" era)),
    Arbitrary (STS.PredicateFailure (Core.EraRule "DELEG" era))
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
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (STS.PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  Arbitrary (STS.ShelleyDelegsPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = recursivelyShrink

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Arbitrary (STS.ShelleyLedgersPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (Core.EraRule "DELEGS" era)),
    Arbitrary (STS.PredicateFailure (Core.EraRule "UTXOW" era))
  ) =>
  Arbitrary (STS.ShelleyLedgerPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  Arbitrary (STS.ShelleyUtxowPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

genTx ::
  ( Core.EraTx era,
    Arbitrary (Core.TxBody era),
    Arbitrary (Core.TxAuxData era),
    Arbitrary (Core.TxWits era)
  ) =>
  Gen (ShelleyTx era)
genTx =
  ShelleyTx
    <$> arbitrary
    <*> resize maxTxWits arbitrary
    <*> arbitrary

genBlock ::
  forall era h.
  ( EraSegWits era,
    Mock (EraCrypto era),
    Arbitrary (Core.Tx era),
    h ~ BHeader (EraCrypto era)
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
  ( Mock (EraCrypto era),
    EraSegWits era,
    Arbitrary (Core.Tx era),
    h ~ BHeader (EraCrypto era)
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
  ( Core.EraTx era,
    Arbitrary (Core.TxBody era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.TxAuxData era),
    Arbitrary (Core.Script era),
    Arbitrary (Core.TxWits era)
  ) =>
  Arbitrary (ShelleyTx era)
  where
  arbitrary = genTx

instance
  ( Core.EraTxBody era,
    EraSegWits era,
    Mock (EraCrypto era),
    Arbitrary (Core.Tx era),
    h ~ BHeader (EraCrypto era)
  ) =>
  Arbitrary (Block h era)
  where
  arbitrary = genBlock

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Arbitrary (ApplyTxError era)
  where
  arbitrary = ApplyTxError <$> arbitrary
  shrink (ApplyTxError xs) = [ApplyTxError xs' | xs' <- shrink xs]

instance (Mock c) => Arbitrary (PulsingRewUpdate c) where
  arbitrary =
    oneof
      [ Complete <$> arbitrary,
        Pulsing <$> arbitrary <*> arbitrary
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
