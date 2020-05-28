{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Data.ByteString.Lazy as Lazy
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Generic.Random (genericArbitraryU)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (Addr (Addr))
import Shelley.Spec.Ledger.BaseTypes
  ( Network,
    Nonce (..),
    UnitInterval,
    mkNonce,
    mkUnitInterval,
  )
import Shelley.Spec.Ledger.BlockChain (HashHeader (..), pattern Block)
import Shelley.Spec.Ledger.Coin (Coin (Coin))
import Shelley.Spec.Ledger.Credential (Credential (..), Ptr, StakeReference)
import Shelley.Spec.Ledger.Crypto (Crypto, HASH)
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash (KeyHash),
    VKey (VKey),
    hash,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash (..))
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import Shelley.Spec.Ledger.Scripts (ScriptHash (ScriptHash))
import Shelley.Spec.Ledger.TxData (RewardAcnt (RewardAcnt), TxId (TxId), TxIn (TxIn))
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    genericShrink,
    oneof,
    shrink,
  )
import Test.QuickCheck.Hedgehog (hedgehog)
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Mock
  ( Addr,
    BHeader,
    Block,
    ConcreteCrypto,
    DELEG,
    DELEGS,
    DELPL,
    HashHeader,
    LEDGER,
    LEDGERS,
    POOL,
    PPUP,
    Tx,
    TxId,
    TxOut,
    UTXO,
    UTXOW,
  )
import Test.Shelley.Spec.Ledger.Generator.Core (KeySpace (KeySpace_), NatNonce (..), geKeySpace, ksCoreNodes, mkBlock, mkOCert)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.NonTraceProperties.Generator (genStateTx)
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
