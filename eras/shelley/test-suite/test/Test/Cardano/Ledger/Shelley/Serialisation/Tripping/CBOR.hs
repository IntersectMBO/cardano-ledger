{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR
  ( tests,

    -- * Individual properties
    prop_roundtrip_Addr,
    prop_roundtrip_RewardAcnt,
    prop_roundtrip_BootstrapWitness,
    prop_roundtrip_Block,
    prop_roundtrip_Header,
    prop_roundtrip_BlockHeaderHash,
    prop_roundtrip_Tx,
    prop_roundtrip_TxId,
    prop_roundtrip_TxOut,
    prop_roundtrip_LEDGER_PredicateFails,
    prop_roundtrip_PrtclState,
    prop_roundtrip_LedgerState,
    prop_roundtrip_NewEpochState,
    prop_roundtrip_ShelleyGenesis,

    -- * pusing properties
    prop_roundtrip_RewardUpdate,
    prop_roundtrip_RewardSnapShot,
    prop_roundtrip_FreeVars,
    prop_roundtrip_RewardPulser,
    prop_roundtrip_PulsingRewUpdate,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (..),
    FullByteString (..),
    ToCBOR (..),
    serializeEncoding,
    toCBOR,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis)
import Cardano.Ledger.Shelley.RewardProvenance (RewardProvenance)
import Cardano.Ledger.Shelley.RewardUpdate
  ( FreeVars (..),
    Pulser,
    PulsingRewUpdate (..),
    RewardSnapShot (..),
    RewardUpdate (..),
  )
import qualified Cardano.Ledger.Shelley.Rules.Ledgers as STS
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS (PrtclState)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Maybe (fromJust)
import Data.Sharing (fromNotSharedCBOR)
import qualified Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes as Mock
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, (===))

-- | Check that: deserialize . serialize = id
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

-- | Check that: serialize . deserialize . serialize = serialize
roundtrip2 ::
  (a -> Encoding) ->
  (forall s. Decoder s a) ->
  a ->
  Property
roundtrip2 enc dec = roundtrip2' enc (const <$> dec)

roundtrip2' ::
  -- | @enc@
  (a -> Encoding) ->
  (forall s. Decoder s (Lazy.ByteString -> a)) ->
  a ->
  Property
roundtrip2' enc dec a = case deserialiseFromBytes dec bs of
  Right (bs', a')
    | Lazy.null bs' ->
      serializeEncoding (enc a) === serializeEncoding (enc (a' bs))
    | otherwise ->
      counterexample ("left-over bytes: " <> show bs') False
  Left e ->
    counterexample (show e) False
  where
    bs = toLazyByteString (enc a)

{-------------------------------------------------------------------------------
  Serialization Properties
-------------------------------------------------------------------------------}

prop_roundtrip_Addr :: Ledger.Addr Mock.C_Crypto -> Property
prop_roundtrip_Addr = roundtrip toCBOR fromCBOR

prop_roundtrip_RewardAcnt :: Ledger.RewardAcnt Mock.C_Crypto -> Property
prop_roundtrip_RewardAcnt = roundtrip toCBOR fromCBOR

prop_roundtrip_Block :: Ledger.Block Ledger.BHeader Mock.C -> Property
prop_roundtrip_Block = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Header :: Ledger.BHeader Mock.C_Crypto -> Property
prop_roundtrip_Header = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_BlockHeaderHash :: Ledger.HashHeader Mock.C_Crypto -> Property
prop_roundtrip_BlockHeaderHash = roundtrip toCBOR fromCBOR

prop_roundtrip_TxBody :: Ledger.TxBody Mock.C -> Property
prop_roundtrip_TxBody = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Tx :: Ledger.Tx Mock.C -> Property
prop_roundtrip_Tx = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_TxId :: Ledger.TxId Mock.C_Crypto -> Property
prop_roundtrip_TxId = roundtrip toCBOR fromCBOR

prop_roundtrip_TxOut :: Ledger.TxOut Mock.C -> Property
prop_roundtrip_TxOut = roundtrip toCBOR fromCBOR

prop_roundtrip_BootstrapWitness ::
  Ledger.BootstrapWitness Mock.C_Crypto -> Property
prop_roundtrip_BootstrapWitness =
  roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_LEDGER_PredicateFails ::
  [STS.PredicateFailure (STS.LEDGERS Mock.C)] -> Property
prop_roundtrip_LEDGER_PredicateFails = roundtrip toCBOR fromCBOR

prop_roundtrip_PrtclState :: STS.PrtclState Mock.C_Crypto -> Property
prop_roundtrip_PrtclState = roundtrip toCBOR fromCBOR

prop_roundtrip_LedgerState :: Ledger.LedgerState Mock.C -> Property
prop_roundtrip_LedgerState = roundtrip toCBOR fromCBOR

prop_roundtrip_SnapShots :: Ledger.SnapShots Mock.C_Crypto -> Property
prop_roundtrip_SnapShots = roundtrip2 toCBOR fromNotSharedCBOR

prop_roundtrip_EpochState :: Ledger.EpochState Mock.C -> Property
prop_roundtrip_EpochState = roundtrip2 toCBOR fromCBOR

prop_roundtrip_NewEpochState :: Ledger.NewEpochState Mock.C -> Property
prop_roundtrip_NewEpochState = roundtrip2 toCBOR fromCBOR

prop_roundtrip_MultiSig :: Ledger.MultiSig Mock.C_Crypto -> Property
prop_roundtrip_MultiSig = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_metadata :: Ledger.Metadata Mock.C -> Property
prop_roundtrip_metadata = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_ShelleyGenesis :: ShelleyGenesis Mock.C -> Property
prop_roundtrip_ShelleyGenesis = roundtrip toCBOR fromCBOR

prop_roundtrip_Coin_1 :: Coin -> Property
prop_roundtrip_Coin_1 = roundtrip (toCBOR . fromJust . toCompact) fromCBOR

prop_roundtrip_Coin_2 :: Coin -> Property
prop_roundtrip_Coin_2 = roundtrip toCBOR (fromCompact <$> fromCBOR)

prop_roundtrip_RewardProvenance :: RewardProvenance Mock.C_Crypto -> Property
prop_roundtrip_RewardProvenance = roundtrip toCBOR fromCBOR

prop_roundtrip_RewardUpdate :: RewardUpdate Mock.C_Crypto -> Property
prop_roundtrip_RewardUpdate = roundtrip toCBOR fromCBOR

prop_roundtrip_RewardSnapShot :: RewardSnapShot Mock.C_Crypto -> Property
prop_roundtrip_RewardSnapShot = roundtrip toCBOR fromCBOR

prop_roundtrip_FreeVars :: FreeVars Mock.C_Crypto -> Property
prop_roundtrip_FreeVars x = roundtrip toCBOR fromCBOR x

prop_roundtrip_RewardPulser :: Pulser Mock.C_Crypto -> Property
prop_roundtrip_RewardPulser = roundtrip toCBOR fromCBOR

prop_roundtrip_PulsingRewUpdate :: PulsingRewUpdate Mock.C_Crypto -> Property
prop_roundtrip_PulsingRewUpdate = roundtrip toCBOR fromCBOR

pulsingTest :: TestTree
pulsingTest =
  testGroup
    "Serialisable Pulser tests"
    [ testProperty "roundtrip RewardUpdate" prop_roundtrip_RewardUpdate,
      testProperty "roundtrip RewardSnapShot" prop_roundtrip_RewardSnapShot,
      testProperty "roundtrip RewardFreeVars" prop_roundtrip_FreeVars,
      testProperty "roundtrip RewardPulser" prop_roundtrip_RewardPulser,
      testProperty "roundtrip PulsingRewUpdate" prop_roundtrip_PulsingRewUpdate
    ]

-- TODO

-- roundTripIpv4 :: Property
-- roundTripIpv4 =
--   -- We are using a QC generator which means we need QC test
--   Hedgehog.property $ do
--     ha <- forAll genIPv4
--     Hedgehog.tripping ha ipv4ToBytes ipv4FromBytes

-- roundTripIpv6 :: Property
-- roundTripIpv6 =
--   -- We are using a QC generator which means we need QC test
--   Hedgehog.property $ do
--     ha <- forAll genIPv6
--     Hedgehog.tripping ha ipv6ToBytes ipv6FromBytes

tests :: TestTree
tests =
  testGroup
    "Serialisation roundtrip Property Tests"
    [ testProperty "roundtrip Block" prop_roundtrip_Block,
      testProperty "roundtrip Addr" prop_roundtrip_Addr,
      testProperty "roundtrip RewardAcnt" prop_roundtrip_RewardAcnt,
      testProperty "roundtrip Header" prop_roundtrip_Header,
      testProperty "roundtrip Block Header Hash" prop_roundtrip_BlockHeaderHash,
      testProperty "roundtrip TxBody" prop_roundtrip_TxBody,
      testProperty "roundtrip Tx" prop_roundtrip_Tx,
      testProperty
        "roundtrip Bootstrap Witness"
        prop_roundtrip_BootstrapWitness,
      testProperty "roundtrip TxId" prop_roundtrip_TxId,
      testProperty "roundtrip TxOut" prop_roundtrip_TxOut,
      testProperty
        "roundtrip LEDGER Predicate Failures"
        prop_roundtrip_LEDGER_PredicateFails,
      testProperty "roundtrip Protocol State" prop_roundtrip_PrtclState,
      testProperty "roundtrip Ledger State" prop_roundtrip_LedgerState,
      testProperty "roundtrip SnapShots" prop_roundtrip_SnapShots,
      testProperty "roundtrip Epoch State" prop_roundtrip_EpochState,
      testProperty "roundtrip NewEpoch State" prop_roundtrip_NewEpochState,
      testProperty "roundtrip MultiSig" prop_roundtrip_MultiSig,
      testProperty "roundtrip Metadata" prop_roundtrip_metadata,
      testProperty "roundtrip Shelley Genesis" prop_roundtrip_ShelleyGenesis,
      testProperty "roundtrip coin compactcoin cbor" prop_roundtrip_Coin_1,
      testProperty "roundtrip coin cbor compactcoin" prop_roundtrip_Coin_2,
      testProperty "roundtrip reward provenance" prop_roundtrip_RewardProvenance,
      pulsingTest
    ]

instance FromCBOR (Ledger.LedgerState Mock.C) where
  fromCBOR = fromNotSharedCBOR
