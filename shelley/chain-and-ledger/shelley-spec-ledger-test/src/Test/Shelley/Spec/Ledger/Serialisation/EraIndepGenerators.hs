{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators
  ( genPParams,
    mkDummyHash,
    genHash,
    genShelleyAddress,
    genByronAddress,
    MockGen,
    maxTxWits,
  )
where

import Cardano.Binary
  ( ToCBOR (..),
    toCBOR,
  )
import Cardano.Crypto.DSIGN.Class
  ( DSIGNAlgorithm,
    SignedDSIGN (..),
    rawDeserialiseSigDSIGN,
    rawDeserialiseVerKeyDSIGN,
    sizeSigDSIGN,
    sizeVerKeyDSIGN,
  )
import Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyBased)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Control.SetAlgebra (biMapFromList)
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Data.IP (IPv4, IPv6, toIPv4, toIPv6)
import qualified Data.Map.Strict as Map (empty, fromList)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Time
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Generic.Random (genericArbitraryU)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API hiding (SignedDSIGN, TxBody (..))
import Shelley.Spec.Ledger.Address.Bootstrap
  ( ChainCode (..),
  )
import Shelley.Spec.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    DnsName,
    UnitInterval,
    Url,
    mkActiveSlotCoeff,
    mkNonceFromNumber,
    mkUnitInterval,
    textToDns,
    textToUrl,
  )
import Shelley.Spec.Ledger.Coin (DeltaCoin (..))
import Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.LedgerState
  ( FutureGenDeleg,
    emptyRewardUpdate,
  )
import qualified Shelley.Spec.Ledger.MetaData as MD
import Shelley.Spec.Ledger.Rewards
  ( Likelihood (..),
    LogWeight (..),
    PerformanceEstimate (..),
  )
import qualified Shelley.Spec.Ledger.STS.Deleg as STS
import qualified Shelley.Spec.Ledger.STS.Delegs as STS
import qualified Shelley.Spec.Ledger.STS.Delpl as STS
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Ledgers as STS
import qualified Shelley.Spec.Ledger.STS.Pool as STS
import qualified Shelley.Spec.Ledger.STS.Ppup as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS (PrtclState)
import qualified Shelley.Spec.Ledger.STS.Tickn as STS
import qualified Shelley.Spec.Ledger.STS.Utxow as STS
import Shelley.Spec.Ledger.Tx (ValidateScript, WitnessSetHKD (WitnessSet), hashScript)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    genericShrink,
    listOf,
    oneof,
    resize,
    shrink,
    vectorOf,
  )
import Test.QuickCheck.Gen (chooseAny)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (defaultConstants)
import Test.Shelley.Spec.Ledger.Generator.GenEra(EraGen)
import Test.Shelley.Spec.Ledger.Generator.Core
  ( KeySpace (KeySpace_),
    geConstants,
    geKeySpace,
    ksCoreNodes,
    mkBlock,
    mkBlockHeader,
    mkOCert,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (coreNodeKeys, genEnv)
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Update
import Test.Shelley.Spec.Ledger.Serialisation.Generators.Bootstrap
  ( genBootstrapAddress,
    genSignature,
  )
import Test.Tasty.QuickCheck (Gen, choose, elements)

genHash :: forall a h. HashAlgorithm h => Gen (Hash.Hash h a)
genHash = mkDummyHash <$> arbitrary

mkDummyHash :: forall h a. HashAlgorithm h => Int -> Hash.Hash h a
mkDummyHash = coerce . hashWithSerialiser @h toCBOR

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

type MockGen era =
  ( Mock (Crypto era),
    Arbitrary (VerKeyDSIGN (DSIGN (Crypto era)))
  )

instance Mock crypto => Arbitrary (BHeader crypto) where
  arbitrary = do
    prevHash <- arbitrary :: Gen (HashHeader crypto)
    allPoolKeys <- elements (map snd (coreNodeKeys defaultConstants))
    curSlotNo <- SlotNo <$> choose (0, 10)
    curBlockNo <- BlockNo <$> choose (0, 100)
    epochNonce <- arbitrary :: Gen Nonce
    bodySize <- arbitrary
    bodyHash <- arbitrary
    let kesPeriod = 1
        keyRegKesPeriod = 1
        ocert = mkOCert allPoolKeys 1 (KESPeriod kesPeriod)
    return $
      mkBlockHeader
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

instance DSIGNAlgorithm crypto => Arbitrary (SignedDSIGN crypto a) where
  arbitrary =
    SignedDSIGN . fromJust . rawDeserialiseSigDSIGN
      <$> (genByteString . fromIntegral $ sizeSigDSIGN (Proxy @crypto))

instance DSIGNAlgorithm crypto => Arbitrary (VerKeyDSIGN crypto) where
  arbitrary =
    fromJust . rawDeserialiseVerKeyDSIGN
      <$> (genByteString . fromIntegral $ sizeVerKeyDSIGN (Proxy @crypto))

instance
  (Era era, MockGen era) =>
  Arbitrary (BootstrapWitness era)
  where
  arbitrary = do
    key <- arbitrary
    sig <- genSignature
    chainCode <- ChainCode <$> arbitrary
    attributes <- arbitrary
    pure $ BootstrapWitness key sig chainCode attributes

instance CC.Crypto crypto => Arbitrary (HashHeader crypto) where
  arbitrary = HashHeader <$> genHash

instance CC.Crypto crypto => Arbitrary (HashBBody crypto) where
  arbitrary = UnsafeHashBBody <$> genHash

instance (Typeable kr, Era era, Mock (Crypto era)) => Arbitrary (WitVKey kr era) where
  arbitrary =
    WitVKey
      <$> arbitrary
      <*> arbitrary

instance (Era era, Mock (Crypto era)) => Arbitrary (Wdrl era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (ProposedPPUpdates era) where
  arbitrary = ProposedPPUpdates <$> pure Map.empty

instance (Era era, Mock (Crypto era)) => Arbitrary (Update era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

maxMetaDatumDepth :: Int
maxMetaDatumDepth = 2

maxMetaDatumListLens :: Int
maxMetaDatumListLens = 5

sizedMetaDatum :: Int -> Gen MD.MetaDatum
sizedMetaDatum 0 =
  oneof
    [ MD.I <$> arbitrary,
      MD.B <$> arbitrary,
      MD.S <$> (T.pack <$> arbitrary)
    ]
sizedMetaDatum n =
  oneof
    [ MD.Map
        <$> ( zip
                <$> (resize maxMetaDatumListLens (listOf (sizedMetaDatum (n -1))))
                <*> (listOf (sizedMetaDatum (n -1)))
            ),
      MD.List <$> resize maxMetaDatumListLens (listOf (sizedMetaDatum (n -1))),
      MD.I <$> arbitrary,
      MD.B <$> arbitrary,
      MD.S <$> (T.pack <$> arbitrary)
    ]

instance Arbitrary MD.MetaDatum where
  arbitrary = sizedMetaDatum maxMetaDatumDepth

instance Arbitrary MD.MetaData where
  arbitrary = MD.MetaData <$> arbitrary

maxTxWits :: Int
maxTxWits = 5

instance Era era => Arbitrary (TxId era) where
  arbitrary = TxId <$> genHash

instance Era era => Arbitrary (TxIn era) where
  arbitrary =
    TxIn
      <$> (TxId <$> genHash)
      <*> arbitrary

instance
  (ShelleyBased era, Mock (Crypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (TxOut era)
  where
  arbitrary = TxOut <$> arbitrary <*> arbitrary

instance Arbitrary Nonce where
  arbitrary =
    oneof
      [ return NeutralNonce,
        mkNonceFromNumber <$> choose (1, 123 :: Word64)
      ]

instance Arbitrary UnitInterval where
  arbitrary = fromJust . mkUnitInterval . (% 100) <$> choose (1, 99)

instance
  (CC.Crypto crypto) =>
  Arbitrary (KeyHash a crypto)
  where
  arbitrary = KeyHash <$> genHash

instance Era era => Arbitrary (WitHashes era) where
  arbitrary = genericArbitraryU

instance Arbitrary MIRPot where
  arbitrary = genericArbitraryU

instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (0, 1000)

instance Arbitrary STS.VotingPeriod where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Coin where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = Coin <$> choose (0, 1000)

instance Arbitrary DeltaCoin where
  arbitrary = DeltaCoin <$> choose (-1000, 1000)

instance Arbitrary SlotNo where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = SlotNo <$> choose (1, 100000)

instance Arbitrary EpochNo where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = EpochNo <$> choose (1, 100000)

instance (Era era, Mock (Crypto era)) => Arbitrary (Addr era) where
  arbitrary = oneof [genShelleyAddress, genByronAddress]

genShelleyAddress :: (Era era, Mock (Crypto era)) => Gen (Addr era)
genShelleyAddress = Addr <$> arbitrary <*> arbitrary <*> arbitrary

genByronAddress :: Gen (Addr era)
genByronAddress = AddrBootstrap <$> genBootstrapAddress

instance (Era era, Mock (Crypto era)) => Arbitrary (StakeReference era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Era era => Arbitrary (Credential r era) where
  arbitrary =
    oneof
      [ ScriptHashObj . ScriptHash <$> genHash,
        KeyHashObj <$> arbitrary
      ]

instance Arbitrary Ptr where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (RewardAcnt era) where
  arbitrary = RewardAcnt <$> arbitrary <*> arbitrary

instance Arbitrary Network where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Arbitrary (VerKeyDSIGN (DSIGN crypto))) => Arbitrary (VKey kd crypto) where
  arbitrary = VKey <$> arbitrary

instance Arbitrary ProtVer where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Era era => Arbitrary (ScriptHash era) where
  arbitrary = ScriptHash <$> genHash

instance Era era => Arbitrary (MD.MetaDataHash era) where
  arbitrary = MD.MetaDataHash <$> genHash

instance HashAlgorithm h => Arbitrary (Hash.Hash h a) where
  arbitrary = genHash

instance Arbitrary STS.TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance CC.Crypto crypto => Arbitrary (STS.PrtclState crypto) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (ShelleyBased era, Mock (Crypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (UTxO era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (PState era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (InstantaneousRewards era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Mock crypto) => Arbitrary (FutureGenDeleg crypto) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Mock crypto) => Arbitrary (GenDelegs crypto) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Mock crypto) => Arbitrary (GenDelegPair crypto) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (DState era) where
  arbitrary =
    DState
      <$> arbitrary
      <*> arbitrary
      <*> (biMapFromList const <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Era era, Mock (Crypto era)) => Arbitrary (DelegCert era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (Delegation era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (PoolCert era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (GenesisDelegCert era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (MIRCert era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (DCert era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (PPUPState era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (DPState era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (ShelleyBased era, Mock (Crypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (UTxOState era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (ShelleyBased era, Mock (Crypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (LedgerState era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (EraGen era, Mock (Crypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (NewEpochState era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Era era => Arbitrary (BlocksMade era) where
  arbitrary = BlocksMade <$> arbitrary

instance CC.Crypto crypto => Arbitrary (PoolDistr crypto) where
  arbitrary =
    PoolDistr . Map.fromList
      <$> listOf ((,) <$> arbitrary <*> genVal)
    where
      genVal = IndividualPoolStake <$> arbitrary <*> genHash

instance
  (EraGen era, Mock (Crypto era), Arbitrary (Core.Value era)) =>
  Arbitrary (EpochState era)
  where
  arbitrary =
    EpochState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genPParams (Proxy @era)
      <*> genPParams (Proxy @era)
      <*> arbitrary

instance Arbitrary (RewardUpdate era) where
  arbitrary = return emptyRewardUpdate

instance Arbitrary a => Arbitrary (StrictMaybe a) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

genPParams ::
  (EraGen era) =>
  proxy era ->
  Gen (PParams era)
genPParams p = Update.genPParams (geConstants (genEnv p))

instance CC.Crypto crypto => Arbitrary (OBftSlot crypto) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary ActiveSlotCoeff where
  arbitrary = mkActiveSlotCoeff <$> arbitrary

instance Arbitrary Likelihood where
  arbitrary = Likelihood <$> arbitrary

instance Arbitrary LogWeight where
  arbitrary = LogWeight <$> arbitrary

instance Era era => Arbitrary (NonMyopic era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (SnapShot era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Era era, Mock (Crypto era)) => Arbitrary (SnapShots era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary PerformanceEstimate where
  arbitrary = PerformanceEstimate <$> arbitrary

instance (Era era, Mock (Crypto era)) => Arbitrary (Stake era) where
  arbitrary = Stake <$> arbitrary

instance (Era era, Mock (Crypto era)) => Arbitrary (PoolParams era) where
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

instance Arbitrary PoolMetaData where
  arbitrary = (`PoolMetaData` BS.pack "bytestring") <$> arbitrary

instance Arbitrary Url where
  arbitrary = return . fromJust $ textToUrl "text"

instance Arbitrary a => Arbitrary (StrictSeq a) where
  arbitrary = StrictSeq.toStrict <$> arbitrary
  shrink = map StrictSeq.toStrict . shrink . StrictSeq.getSeq

instance Arbitrary StakePoolRelay where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Port where
  arbitrary = fromIntegral @Word8 @Port <$> arbitrary

instance Arbitrary IPv4 where
  arbitrary = pure $ toIPv4 [192, 0, 2, 1]

instance Arbitrary IPv6 where
  arbitrary = pure $ toIPv6 [0x2001, 0xDB8, 0, 0, 0, 0, 0, 1]

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
      RequireAllOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n -1))),
      RequireAnyOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n -1))),
      RequireMOf <$> arbitrary <*> resize maxMultiSigListLens (listOf (sizedMultiSig (n -1)))
    ]

instance
  (Era era, Mock (Crypto era)) =>
  Arbitrary (MultiSig era)
  where
  arbitrary = sizedMultiSig maxMultiSigDepth

-- |
-- Generate a byte string of a given size.
genByteString :: Int -> Gen BS.ByteString
genByteString size = do
  ws <- vectorOf size (chooseAny @Char)
  return $ BS.pack ws

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
  (EraGen era, Mock (Crypto era)) =>
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
      <*> genPParams (Proxy @era) -- sgProtocolParams
      <*> arbitrary -- sgGenDelegs
      <*> arbitrary -- sgInitialFunds
      <*> (ShelleyGenesisStaking <$> arbitrary <*> arbitrary) -- sgStaking

instance
  ( EraGen era,
    Mock (Crypto era),
    ValidateScript era,
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (WitnessSet era)
  where
  arbitrary =
    WitnessSet
      <$> arbitrary
      <*> (mscriptsToWits <$> arbitrary)
      <*> arbitrary
    where
      mscriptsToWits = Map.fromList . map (\s -> (hashScript s, s))

instance Era era => Arbitrary (STS.PpupPredicateFailure era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Era era => Arbitrary (STS.PoolPredicateFailure era) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (Era era, Mock (Crypto era)) =>
  Arbitrary (STS.DelplPredicateFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (Era era, Mock (Crypto era)) =>
  Arbitrary (STS.DelegPredicateFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  (Era era, Mock (Crypto era)) =>
  Arbitrary (STS.DelegsPredicateFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (LEDGER era))
  ) =>
  Arbitrary (STS.LedgersPredicateFailure era)
  where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (DELEGS era)),
    Arbitrary (STS.PredicateFailure (UTXOW era))
  ) =>
  Arbitrary (STS.LedgerPredicateFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

instance
  ( Era era,
    Arbitrary (STS.PredicateFailure (UTXO era)),
    Arbitrary (WitHashes era)
  ) =>
  Arbitrary (STS.UtxowPredicateFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

genTx ::
  ( ShelleyBased era,
    Arbitrary (WitnessSet era),
    Arbitrary (Core.TxBody era)
  ) =>
  Gen (Tx era)
genTx =
  Tx
    <$> arbitrary
    <*> (resize maxTxWits arbitrary)
    <*> arbitrary

genBlock ::
  forall era.
  ( EraGen era,
    Mock (Crypto era),
    Arbitrary (WitnessSet era),
    Arbitrary (Core.TxBody era)
  ) =>
  Gen (Block era)
genBlock = do
  let KeySpace_ {ksCoreNodes} = geKeySpace (genEnv p)
  prevHash <- arbitrary :: Gen (HashHeader (Crypto era))
  allPoolKeys <- elements (map snd ksCoreNodes)
  txs <- listOf (genTx @era)
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
  where
    p :: Proxy era
    p = Proxy

instance
  ( EraGen era,
    Mock (Crypto era),
    ValidateScript era,
    Arbitrary (Core.TxBody era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (Tx era)
  where
  arbitrary = genTx

instance
  ( EraGen era,
    Mock (Crypto era),
    ValidateScript era,
    Arbitrary (Core.TxBody era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (Block era)
  where
  arbitrary = genBlock
