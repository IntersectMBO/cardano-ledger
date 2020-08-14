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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Serialisation.Generators
  ( genPParams,
    mkDummyHash,
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
import qualified Data.Text as T
import Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Iterate.SetAlgebra (biMapFromList)
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Data.IP (IPv4, IPv6, toIPv4, toIPv6)
import qualified Data.Map.Strict as Map (empty, fromList)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Generic.Random (genericArbitraryU)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API hiding (SignedDSIGN)
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.Address.Bootstrap
  ( BootstrapWitness (..),
    ChainCode (..),
  )
import Shelley.Spec.Ledger.BaseTypes
  ( DnsName,
    Network,
    Nonce (..),
    Port,
    StrictMaybe,
    UnitInterval,
    Url,
    mkNonceFromNumber,
    mkUnitInterval,
    textToDns,
    textToUrl,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHeader,
    Block (..),
    HashHeader (..),
  )
import Shelley.Spec.Ledger.Coin (Coin (Coin))
import Shelley.Spec.Ledger.Credential (Credential (..), Ptr, StakeReference)
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..), PoolDistr (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), Stake (..))
import Shelley.Spec.Ledger.Keys
  ( KeyHash (KeyHash),
    VKey (VKey),
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    FutureGenDeleg,
    InstantaneousRewards,
    LedgerState,
    NewEpochState (..),
    OBftSlot,
    PPUPState,
    RewardUpdate,
    WitHashes (..),
    emptyRewardUpdate,
  )
import Shelley.Spec.Ledger.MetaData
  ( MetaData,
    MetaDataHash (..),
    MetaDatum,
  )
import qualified Shelley.Spec.Ledger.MetaData as MD
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParams, ProtVer)
import Shelley.Spec.Ledger.Rewards
  ( Likelihood (..),
    LogWeight (..),
    PerformanceEstimate (..),
  )
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Ppup as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS (PrtclState)
import qualified Shelley.Spec.Ledger.STS.Tickn as STS
import Shelley.Spec.Ledger.Scripts
  ( MultiSig (..),
    Script (..),
    ScriptHash (ScriptHash),
  )
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (WitnessSet), hashScript)
import Shelley.Spec.Ledger.TxData
  ( MIRPot,
    PoolMetaData (PoolMetaData),
    PoolParams (PoolParams),
    RewardAcnt (RewardAcnt),
    StakePoolRelay,
    TxId (TxId),
    TxIn (TxIn),
    TxOut (TxOut),
  )
import Shelley.Spec.Ledger.UTxO (UTxO)
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
import Test.Shelley.Spec.Ledger.Generator.Core
  ( KeySpace (KeySpace_),
    geConstants,
    geKeySpace,
    ksCoreNodes,
    mkBlock,
    mkOCert,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Update
import Test.Shelley.Spec.Ledger.Serialisation.Generators.Bootstrap (genBootstrapAddress, genSignature)
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

type MockGen c = (Mock c, Arbitrary (VerKeyDSIGN (DSIGN c)))

instance
  Mock c =>
  Arbitrary (Block c)
  where
  arbitrary = do
    let KeySpace_ {ksCoreNodes} = geKeySpace (genEnv p)
    prevHash <- arbitrary :: Gen (HashHeader c)
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
    where
      p :: Proxy c
      p = Proxy

instance Mock c => Arbitrary (BHeader c) where
  arbitrary = do
    res <- arbitrary :: Gen (Block c)
    return $ case res of
      Block header _ -> header
      _ -> error "SerializationProperties::BHeader - failed to deconstruct header from block"

instance DSIGNAlgorithm c => Arbitrary (SignedDSIGN c a) where
  arbitrary =
    SignedDSIGN . fromJust . rawDeserialiseSigDSIGN
      <$> (genByteString . fromIntegral $ sizeSigDSIGN (Proxy @c))

instance DSIGNAlgorithm c => Arbitrary (VerKeyDSIGN c) where
  arbitrary =
    fromJust . rawDeserialiseVerKeyDSIGN
      <$> (genByteString . fromIntegral $ sizeVerKeyDSIGN (Proxy @c))

instance MockGen c => Arbitrary (BootstrapWitness c) where
  arbitrary = do
    key <- arbitrary
    sig <- genSignature
    chainCode <- ChainCode <$> arbitrary
    attributes <- arbitrary
    pure $ BootstrapWitness key sig chainCode attributes

instance Crypto c => Arbitrary (HashHeader c) where
  arbitrary = HashHeader <$> genHash

instance (Typeable kr, Mock c) => Arbitrary (WitVKey c kr) where
  arbitrary =
    WitVKey
      <$> arbitrary
      <*> arbitrary


instance Mock c => Arbitrary (WitnessSet c) where
  arbitrary =
    WitnessSet
      <$> arbitrary
      <*> (mscriptsToWits <$> arbitrary)
      <*> arbitrary
    where
      mscriptsToWits = Map.fromList . map (\s -> (hashScript s, s))

instance Mock c => Arbitrary (Wdrl c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (ProposedPPUpdates c) where
  arbitrary = ProposedPPUpdates <$> pure Map.empty

instance Mock c => Arbitrary (Update c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (TxBody c) where
  -- Our arbitrary instance constructs things using the pattern in order to have
  -- the correct serialised bytes.
  arbitrary =
    TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

maxMetaDatumDepth :: Int
maxMetaDatumDepth = 2

maxMetaDatumListLens :: Int
maxMetaDatumListLens = 5

sizedMetaDatum :: Int -> Gen MetaDatum
sizedMetaDatum 0 =
  oneof
    [ MD.I <$> arbitrary,
      MD.B <$> arbitrary,
      MD.S <$> (T.pack <$> arbitrary)
    ]
sizedMetaDatum n =
    oneof
      [ MD.Map <$>
          (zip
            <$> (resize maxMetaDatumListLens (listOf (sizedMetaDatum (n-1))))
            <*> (listOf (sizedMetaDatum (n-1)))),
        MD.List <$> resize maxMetaDatumListLens (listOf (sizedMetaDatum (n-1))),
        MD.I <$> arbitrary,
        MD.B <$> arbitrary,
        MD.S <$> (T.pack <$> arbitrary)
      ]

instance Arbitrary MetaDatum where
  arbitrary = sizedMetaDatum maxMetaDatumDepth

instance Arbitrary MetaData where
  arbitrary = MD.MetaData <$> arbitrary

maxTxWits :: Int
maxTxWits = 5

instance Mock c => Arbitrary (Tx c) where
  -- Our arbitrary instance constructs things using the pattern in order to have
  -- the correct serialised bytes.
  arbitrary =
    Tx
      <$> arbitrary
      <*> (resize maxTxWits arbitrary)
      <*> arbitrary

instance Crypto c => Arbitrary (TxId c) where
  arbitrary = TxId <$> genHash

instance Crypto c => Arbitrary (TxIn c) where
  arbitrary =
    TxIn
      <$> (TxId <$> genHash)
      <*> arbitrary

instance Mock c => Arbitrary (TxOut c) where
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
  (Crypto c) =>
  Arbitrary (KeyHash a c)
  where
  arbitrary = KeyHash <$> genHash

instance Crypto c => Arbitrary (WitHashes c) where
  arbitrary = genericArbitraryU

instance Arbitrary MIRPot where
  arbitrary = genericArbitraryU

instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (0, 1000)

instance Arbitrary STS.VotingPeriod where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.PredicateFailure (PPUP c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (STS.PredicateFailure (UTXO c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance MockGen c => Arbitrary (STS.PredicateFailure (UTXOW c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.PredicateFailure (POOL c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (STS.PredicateFailure (DELPL c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (STS.PredicateFailure (DELEG c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (STS.PredicateFailure (DELEGS c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance MockGen c => Arbitrary (STS.PredicateFailure (LEDGER c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance MockGen c => Arbitrary (STS.PredicateFailure (LEDGERS c)) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Coin where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = Coin <$> choose (0, 1000)

instance Arbitrary SlotNo where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = SlotNo <$> choose (1, 100000)

instance Arbitrary EpochNo where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = EpochNo <$> choose (1, 100000)

instance Mock c => Arbitrary (Addr c) where
  arbitrary =
    oneof
      [ Addr <$> arbitrary <*> arbitrary <*> arbitrary,
        AddrBootstrap <$> genBootstrapAddress
      ]

instance Mock c => Arbitrary (StakeReference c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Mock c
  ) =>
  Arbitrary (Credential r c)
  where
  arbitrary =
    oneof
      [ ScriptHashObj . ScriptHash <$> genHash,
        KeyHashObj <$> arbitrary
      ]

instance Arbitrary Ptr where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (RewardAcnt c) where
  arbitrary = RewardAcnt <$> arbitrary <*> arbitrary

instance Arbitrary Network where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance (Arbitrary (VerKeyDSIGN (DSIGN c))) => Arbitrary (VKey kd c) where
  arbitrary = VKey <$> arbitrary

instance Arbitrary ProtVer where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (ScriptHash c) where
  arbitrary = ScriptHash <$> genHash

instance Crypto c => Arbitrary (MetaDataHash c) where
  arbitrary = MetaDataHash <$> genHash

instance HashAlgorithm h => Arbitrary (Hash.Hash h a) where
  arbitrary = genHash

instance Arbitrary STS.TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (STS.PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (UTxO c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (PState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (InstantaneousRewards c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (FutureGenDeleg c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (GenDelegs c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (GenDelegPair c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (DState c) where
  arbitrary =
    DState
      <$> arbitrary
      <*> arbitrary
      <*> (biMapFromList const <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (DelegCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (Delegation c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (PoolCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (GenesisDelegCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (MIRCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (DCert c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (PPUPState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (DPState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (UTxOState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (LedgerState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (NewEpochState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (BlocksMade c) where
  arbitrary = BlocksMade <$> arbitrary

instance Crypto c => Arbitrary (PoolDistr c) where
  arbitrary =
    PoolDistr . Map.fromList
      <$> listOf ((,) <$> arbitrary <*> genVal)
    where
      genVal = IndividualPoolStake <$> arbitrary <*> genHash

instance Mock c => Arbitrary (EpochState c) where
  arbitrary =
    EpochState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genPParams (Proxy @c)
      <*> genPParams (Proxy @c)
      <*> arbitrary

instance Arbitrary (RewardUpdate c) where
  arbitrary = return emptyRewardUpdate

instance Arbitrary a => Arbitrary (StrictMaybe a) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

genPParams :: Mock c => proxy c -> Gen PParams
genPParams p = Update.genPParams (geConstants (genEnv p))

instance Arbitrary Likelihood where
  arbitrary = Likelihood <$> arbitrary

instance Arbitrary LogWeight where
  arbitrary = LogWeight <$> arbitrary

instance Mock c => Arbitrary (NonMyopic c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (SnapShot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (SnapShots c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary PerformanceEstimate where
  arbitrary = PerformanceEstimate <$> arbitrary

instance Mock c => Arbitrary (Stake c) where
  arbitrary = Stake <$> arbitrary

instance Mock c => Arbitrary (PoolParams c) where
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

sizedMultiSig :: Mock c => Int -> Gen (MultiSig c)
sizedMultiSig 0 = RequireSignature <$> arbitrary
sizedMultiSig n =
    oneof
      [ RequireSignature <$> arbitrary,
        RequireAllOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n-1))),
        RequireAnyOf <$> resize maxMultiSigListLens (listOf (sizedMultiSig (n-1))),
        RequireMOf <$> arbitrary <*> resize maxMultiSigListLens (listOf (sizedMultiSig (n-1)))
      ]

instance
  Mock c =>
  Arbitrary (MultiSig c)
  where
  arbitrary = sizedMultiSig maxMultiSigDepth

instance
  Mock c =>
  Arbitrary (Script c)
  where
  arbitrary = MultiSigScript <$> arbitrary

-- |
-- Generate a byte string of a given size.
genByteString :: Int -> Gen BS.ByteString
genByteString size = do
  ws <- vectorOf size (chooseAny @Char)
  return $ BS.pack ws
