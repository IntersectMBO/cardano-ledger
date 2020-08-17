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

module Test.Shelley.Spec.Ledger.Serialisation.Tripping.CBOR
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
    prop_roundtrip_Script,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (..),
    FullByteString (..),
    ToCBOR (..),
    toCBOR,
  )
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import qualified Shelley.Spec.Ledger.API as Ledger
import Shelley.Spec.Ledger.Coin (Coin)

import qualified Shelley.Spec.Ledger.STS.Ledgers as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS (PrtclState)
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Mock
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, (===))

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

{-------------------------------------------------------------------------------
  Serialization Properties
-------------------------------------------------------------------------------}

-- TODO do these with Val parameter!

prop_roundtrip_Addr :: Ledger.Addr Mock.C -> Property
prop_roundtrip_Addr = roundtrip toCBOR fromCBOR

prop_roundtrip_RewardAcnt :: Ledger.RewardAcnt Mock.C -> Property
prop_roundtrip_RewardAcnt = roundtrip toCBOR fromCBOR

prop_roundtrip_Block :: Ledger.Block Mock.C Coin -> Property
prop_roundtrip_Block = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Header :: Ledger.BHeader Mock.C Coin -> Property
prop_roundtrip_Header = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_BlockHeaderHash :: Ledger.HashHeader Mock.C Coin -> Property
prop_roundtrip_BlockHeaderHash = roundtrip toCBOR fromCBOR

prop_roundtrip_TxBody :: Ledger.TxBody Mock.C Coin -> Property
prop_roundtrip_TxBody = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Tx :: Ledger.Tx Mock.C Coin -> Property
prop_roundtrip_Tx = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_TxId :: Ledger.TxId Mock.C Coin -> Property
prop_roundtrip_TxId = roundtrip toCBOR fromCBOR

prop_roundtrip_TxOut :: Ledger.TxOut Mock.C Coin -> Property
prop_roundtrip_TxOut = roundtrip toCBOR fromCBOR

prop_roundtrip_BootstrapWitness ::
  Ledger.BootstrapWitness Mock.C Coin -> Property
prop_roundtrip_BootstrapWitness = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_LEDGER_PredicateFails :: [STS.PredicateFailure (STS.LEDGERS Mock.C Coin)] -> Property
prop_roundtrip_LEDGER_PredicateFails = roundtrip toCBOR fromCBOR

prop_roundtrip_PrtclState :: STS.PrtclState (Mock.C) -> Property
prop_roundtrip_PrtclState = roundtrip toCBOR fromCBOR

prop_roundtrip_LedgerState :: Ledger.LedgerState Mock.C Coin -> Property
prop_roundtrip_LedgerState = roundtrip toCBOR fromCBOR

prop_roundtrip_NewEpochState :: Ledger.NewEpochState Mock.C Coin -> Property
prop_roundtrip_NewEpochState = roundtrip toCBOR fromCBOR

prop_roundtrip_MultiSig :: Ledger.MultiSig Mock.C -> Property
prop_roundtrip_MultiSig = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Script :: Ledger.Script Mock.C -> Property
prop_roundtrip_Script = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

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
      testProperty "roundtrip Bootstrap Witness" prop_roundtrip_BootstrapWitness,
      testProperty "roundtrip TxId" prop_roundtrip_TxId,
      testProperty "roundtrip TxOut" prop_roundtrip_TxOut,
      testProperty "roundtrip LEDGER Predicate Failures" prop_roundtrip_LEDGER_PredicateFails,
      testProperty "roundtrip Protocol State" prop_roundtrip_PrtclState,
      testProperty "roundtrip Ledger State" prop_roundtrip_LedgerState,
      testProperty "roundtrip NewEpoch State" prop_roundtrip_NewEpochState,
      testProperty "roundtrip MultiSig" prop_roundtrip_MultiSig,
      testProperty "roundtrip Script" prop_roundtrip_Script
    ]
