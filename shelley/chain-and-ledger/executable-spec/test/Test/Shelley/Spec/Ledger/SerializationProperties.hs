{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.SerializationProperties
  ( prop_roundtrip_Block,
    prop_roundtrip_Header,
    prop_roundtrip_BlockHeaderHash,
    prop_roundtrip_Tx,
    prop_roundtrip_TxId,
    prop_roundtrip_LEDGER_PredicateFails,
    prop_roundtrip_PrtclState,
    prop_roundtrip_LedgerState,
    prop_roundtrip_NewEpochState,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (..),
    FullByteString (..),
    ToCBOR (..),
    toCBOR,
  )
import Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Hash.Short (ShortHash)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as Lazy
import Data.Coerce (coerce)
import Data.IP (IPv4, IPv6, toIPv4, toIPv6)
import qualified Data.Map.Strict as Map (fromList)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word8)
import Generic.Random (genericArbitraryU)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (Addr (Addr))
import Shelley.Spec.Ledger.BaseTypes
  ( DnsName,
    Network,
    Nonce (..),
    Port,
    StrictMaybe,
    UnitInterval,
    Url,
    mkNonce,
    mkUnitInterval,
    textToDns,
    textToUrl,
  )
import Shelley.Spec.Ledger.BlockChain (HashHeader (..), pattern Block)
import Shelley.Spec.Ledger.Coin (Coin (Coin))
import Shelley.Spec.Ledger.Credential (Credential (..), Ptr, StakeReference)
import Shelley.Spec.Ledger.Crypto (Crypto, HASH)
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), Stake (..))
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash (KeyHash),
    VKey (VKey),
    hash,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    EpochState (..),
    OBftSlot,
    RewardUpdate,
    emptyRewardUpdate,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash (..))
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (PParams, ProtVer)
import Shelley.Spec.Ledger.Rewards (ApparentPerformance (..))
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS (PrtclState)
import Shelley.Spec.Ledger.Scripts (ScriptHash (ScriptHash))
import Shelley.Spec.Ledger.TxData
  ( MIRPot,
    PoolMetaData (PoolMetaData),
    PoolParams (PoolParams),
    RewardAcnt (RewardAcnt),
    StakePoolRelay,
    TxId (TxId),
    TxIn (TxIn),
  )
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    genericShrink,
    listOf,
    oneof,
    shrink,
  )
import Test.QuickCheck.Hedgehog (hedgehog)
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Mock
import Test.Shelley.Spec.Ledger.Generator.Core
  ( KeySpace (KeySpace_),
    NatNonce (..),
    geConstants,
    geKeySpace,
    ksCoreNodes,
    mkBlock,
    mkOCert,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import Test.Shelley.Spec.Ledger.NonTraceProperties.Generator (genStateTx, genValidStateTx)
import Test.Tasty.QuickCheck ((===), Gen, Property, choose, counterexample, elements)

roundtrip ::
  (Eq a, Show a) =>
  (a -> Encoding) ->
  (forall s. Decoder s a) ->
  a ->
  Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

-- | Roundtrip property for values annotated with their serialized form
--
-- NOTE: Suppose @a@ consists of a pair of the unannotated value @a'@ and some
-- 'Lazy.ByteString'. The roundtrip property will fail if that
-- 'Lazy.ByteString' encoding is not equal to @enc a'@. One way in which this
-- might happen is if the annotation is not canonical CBOR, but @enc@ does
-- produce canonical CBOR.
roundtrip' ::
  (Eq a, Show a) =>
  -- | @enc@
  (a -> Encoding) ->
  (forall s. Decoder s (Lazy.ByteString -> a)) ->
  a ->
  Property
roundtrip' enc dec a = case deserialiseFromBytes dec bs of
  Right (bs', a')
    | Lazy.null bs' ->
      a === a' bs
    | otherwise ->
      counterexample ("left-over bytes: " <> show bs') False
  Left e ->
    counterexample (show e) False
  where
    bs = toLazyByteString (enc a)

genHash :: forall a c. Crypto c => Proxy c -> Gen (Hash c a)
genHash proxy = mkDummyHash proxy <$> arbitrary

mkDummyHash :: forall c a. Crypto c => Proxy c -> Int -> Hash c a
mkDummyHash _ = coerce . hash @(HASH c)

{-------------------------------------------------------------------------------
  Serialization Properties
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: Mock.Block -> Property
prop_roundtrip_Block = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Header :: Mock.BHeader -> Property
prop_roundtrip_Header = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_BlockHeaderHash :: Mock.HashHeader -> Property
prop_roundtrip_BlockHeaderHash = roundtrip toCBOR fromCBOR

prop_roundtrip_Tx :: Mock.Tx -> Property
prop_roundtrip_Tx = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_TxId :: Mock.TxId -> Property
prop_roundtrip_TxId = roundtrip toCBOR fromCBOR

prop_roundtrip_LEDGER_PredicateFails :: [STS.PredicateFailure Mock.LEDGERS] -> Property
prop_roundtrip_LEDGER_PredicateFails = roundtrip toCBOR fromCBOR

prop_roundtrip_PrtclState :: STS.PrtclState Mock.ConcreteCrypto -> Property
prop_roundtrip_PrtclState = roundtrip toCBOR fromCBOR

prop_roundtrip_LedgerState :: Mock.LedgerState -> Property
prop_roundtrip_LedgerState = roundtrip toCBOR fromCBOR

prop_roundtrip_NewEpochState :: Mock.NewEpochState -> Property
prop_roundtrip_NewEpochState = roundtrip toCBOR fromCBOR

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance Arbitrary Mock.Block where
  arbitrary = do
    let KeySpace_ {ksCoreNodes} = geKeySpace genEnv
    prevHash <- arbitrary :: Gen Mock.HashHeader
    allPoolKeys <- elements (map snd ksCoreNodes)
    txs <- arbitrary
    curSlotNo <- SlotNo <$> choose (0, 10)
    curBlockNo <- BlockNo <$> choose (0, 100)
    epochNonce <- arbitrary :: Gen Nonce
    blockNonce <- NatNonce . fromIntegral <$> choose (1, 100 :: Int)
    praosLeaderValue <- arbitrary :: Gen UnitInterval
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
        blockNonce
        praosLeaderValue
        kesPeriod
        keyRegKesPeriod
        ocert

instance Arbitrary Mock.BHeader where
  arbitrary = do
    res <- arbitrary :: Gen Mock.Block
    return $ case res of
      Block header _ -> header
      _ -> error "SerializationProperties::BHeader - failed to deconstruct header from block"

instance Crypto c => Arbitrary (HashHeader c) where
  arbitrary = HashHeader <$> genHash (Proxy @c)

instance Arbitrary Mock.Tx where
  arbitrary = do
    (_ledgerState, _steps, _txfee, tx, _lv) <- hedgehog genStateTx
    return tx

instance Crypto c => Arbitrary (TxId c) where
  arbitrary = TxId <$> genHash (Proxy @c)

instance Crypto c => Arbitrary (TxIn c) where
  arbitrary =
    TxIn
      <$> (TxId <$> genHash (Proxy @c))
      <*> arbitrary

instance Arbitrary Mock.TxOut where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Nonce where
  arbitrary =
    oneof
      [ return NeutralNonce,
        mkNonce . fromIntegral <$> choose (1, 123 :: Int)
      ]

instance Arbitrary UnitInterval where
  arbitrary = fromJust . mkUnitInterval . (% 100) <$> choose (1, 99)

instance Crypto c => Arbitrary (KeyHash a c) where
  arbitrary = KeyHash <$> genHash (Proxy @c)

instance Arbitrary MIRPot where
  arbitrary = genericArbitraryU

instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (0, 1000)

instance Arbitrary (STS.PredicateFailure Mock.PPUP) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.UTXO) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.UTXOW) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.POOL) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.DELPL) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.DELEG) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.DELEGS) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.LEDGER) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (STS.PredicateFailure Mock.LEDGERS) where
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

instance Arbitrary Mock.Addr where
  arbitrary =
    oneof
      [ Addr <$> arbitrary <*> arbitrary <*> arbitrary
      -- TODO generate Byron addresses too
      -- SL.AddrBootstrap
      ]

instance Crypto c => Arbitrary (StakeReference c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (Credential r c) where
  arbitrary =
    oneof
      [ ScriptHashObj . ScriptHash <$> genHash (Proxy @c),
        KeyHashObj <$> arbitrary
      ]

instance Arbitrary Ptr where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (RewardAcnt c) where
  arbitrary = RewardAcnt <$> arbitrary <*> arbitrary

instance Arbitrary Network where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary (VKey kd Mock.ConcreteCrypto) where
  arbitrary = VKey . VerKeyMockDSIGN <$> arbitrary

instance Arbitrary ProtVer where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (ScriptHash c) where
  arbitrary = ScriptHash <$> genHash (Proxy @c)

instance Crypto c => Arbitrary (MetaDataHash c) where
  arbitrary = MetaDataHash <$> genHash (Proxy @c)

instance Arbitrary (Hash.Hash ShortHash a) where
  arbitrary = genHash (Proxy @Mock.ConcreteCrypto)

instance Arbitrary (STS.PrtclState Mock.ConcreteCrypto) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Mock.LedgerState where
  arbitrary = do
    (_ledgerState, _steps, _txfee, _tx, ledgerState) <- hedgehog genValidStateTx
    return ledgerState

instance Arbitrary Mock.NewEpochState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (BlocksMade c) where
  arbitrary = BlocksMade <$> arbitrary

instance Crypto c => Arbitrary (PoolDistr c) where
  arbitrary =
    PoolDistr . Map.fromList
      <$> listOf ((,) <$> arbitrary <*> genVal)
    where
      genVal = (,) <$> arbitrary <*> genHash (Proxy @c)

instance Arbitrary Mock.EpochState where
  arbitrary =
    EpochState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (RewardUpdate c) where
  arbitrary = return emptyRewardUpdate

instance Arbitrary a => Arbitrary (StrictMaybe a) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary PParams where
  arbitrary = genPParams (geConstants genEnv)

instance Arbitrary Mock.NonMyopic where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Mock.SnapShot where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Mock.SnapShots where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary ApparentPerformance where
  arbitrary = ApparentPerformance <$> arbitrary

instance Arbitrary Mock.Stake where
  arbitrary = Stake <$> arbitrary

instance Crypto c => Arbitrary (PoolParams c) where
  arbitrary =
    PoolParams
      <$> arbitrary
      <*> genHash (Proxy @c)
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
