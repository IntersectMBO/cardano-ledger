{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-orphans #-}

module Test.Cardano.Chain.Block.CBOR
  ( tests
  , exampleBlockSignature
  , exampleBody
  , exampleHeader
  , exampleProof
  , exampleToSign
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Coerce (coerce)
import Data.Maybe (fromJust)

import Hedgehog (Property)
import qualified Hedgehog as H

import Cardano.Binary
  ( Decoder
  , decodeAnnotatedDecoder
  , dropBytes
  , fromCBOREmptyAnnotation
  , serializeEncoding
  , toCBOR
  )
import Cardano.Chain.Block
  ( BlockSignature(..)
  , Block
  , Body (..)
  , BoundaryBlock
  , BoundaryHeader
  , Header
  , HeaderHash
  , Proof(..)
  , ToSign(..)
  , dropBoundaryBody
  , fromCBORBoundaryConsensusData
  , fromCBORBOBBlock
  , fromCBORHeader
  , fromCBORHeaderToHash
  , mkHeaderExplicit
  , toCBORBOBBlock
  , toCBORHeaderToHash
  )

import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting
  ( EpochNumber(..)
  , EpochSlots(EpochSlots)
  , WithEpochSlots(WithEpochSlots)
  , unWithEpochSlots
  )
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto
  ( ProtocolMagicId(..)
  , SignTag(..)
  , abstractHash
  , hash
  , noPassSafeSigner
  , sign
  , toVerification
  )

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode
  , goldenTestCBOR
  , goldenTestCBORAnnotated
  , goldenTestExplicit
  , roundTripsCBORAnnotatedShow
  , roundTripsCBORAnnotatedBuildable
  , roundTripsCBORBuildable
  )
import Test.Cardano.Chain.Block.Gen
import Test.Cardano.Chain.Common.Example (exampleChainDifficulty)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Slotting.Example (exampleEpochAndSlotCount, exampleSlotNumber)
import Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots, genWithEpochSlots)
import Test.Cardano.Chain.UTxO.Example (exampleTxPayload, exampleTxProof)
import qualified Test.Cardano.Chain.Update.Example as Update
import Test.Cardano.Crypto.Example (exampleSigningKeys)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Number of slots-per-epoch to be used throughout the examples in this
-- module.
exampleEs :: EpochSlots
exampleEs = EpochSlots 50

goldenHeader :: Property
goldenHeader = goldenTestExplicit
  (serializeEncoding . toCBOR)
  (decodeAnnotatedDecoder "header" $ fromCBORHeader exampleEs)
  exampleHeader
  "test/golden/cbor/block/Header"

-- | Round-trip test the backwards compatible header encoding/decoding functions
ts_roundTripHeaderCompat :: TSProperty
ts_roundTripHeaderCompat = eachOfTS
  300
  (feedPMEpochSlots $ genWithEpochSlots genHeader)
  roundTripsHeaderCompat
 where
  roundTripsHeaderCompat :: WithEpochSlots Header -> H.PropertyT IO ()
  roundTripsHeaderCompat esh@(WithEpochSlots es _) = trippingBuildable
    esh
    (serializeEncoding . toCBORHeaderToHash . unWithEpochSlots)
    ( fmap (WithEpochSlots es . fromJust)
    . decodeAnnotatedDecoder "Header" (fromCBORHeaderToHash es)
    )

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Round-trip test the backwards compatible block encoding/decoding functions
ts_roundTripBlockCompat :: TSProperty
ts_roundTripBlockCompat = eachOfTS
  300
  (feedPM genBlockWithEpochSlots)
  roundTripsBlockCompat
 where
  roundTripsBlockCompat :: WithEpochSlots Block -> H.PropertyT IO ()
  roundTripsBlockCompat esb@(WithEpochSlots es _) = trippingBuildable
    esb
    (serializeEncoding . toCBORBOBBlock . unWithEpochSlots)
    ( fmap (WithEpochSlots es . fromJust)
    . decodeAnnotatedDecoder "Block" (fromCBORBOBBlock es)
    )


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------
goldenBlockSignature :: Property
goldenBlockSignature =
  goldenTestCBORAnnotated exampleBlockSignature "test/golden/cbor/block/BlockSignature"

ts_roundTripBlockSignatureCBOR :: TSProperty
ts_roundTripBlockSignatureCBOR =
  eachOfTS 300 (feedPMEpochSlots genBlockSignature) roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- BoundaryBlockHeader
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBlockHeader :: Property
goldenDeprecatedBoundaryBlockHeader = deprecatedGoldenDecode
  "BoundaryBlockHeader"
  (void (fromCBOREmptyAnnotation :: forall s. Decoder s BoundaryHeader))
  "test/golden/cbor/block/BoundaryBlockHeader"

ts_roundTripBoundaryBlock :: TSProperty
ts_roundTripBoundaryBlock = eachOfTS
    300
    (feedPM genBVDWithPM)
    (roundTripsCBORAnnotatedBuildable . snd)
  where
    genBVDWithPM :: ProtocolMagicId -> H.Gen (ProtocolMagicId, BoundaryBlock)
    genBVDWithPM pm = (,) <$> pure pm <*> genBoundaryBlock pm


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBody :: Property
goldenDeprecatedBoundaryBody = deprecatedGoldenDecode
  "BoundaryBody"
  dropBoundaryBody
  "test/golden/cbor/block/BoundaryBody"


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryConsensusData :: Property
goldenDeprecatedBoundaryConsensusData = deprecatedGoldenDecode
  "BoundaryConsensusData"
  (void fromCBORBoundaryConsensusData)
  "test/golden/cbor/block/BoundaryConsensusData"


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

goldenHeaderHash :: Property
goldenHeaderHash =
  goldenTestCBOR exampleHeaderHash "test/golden/cbor/block/HeaderHash"

ts_roundTripHeaderHashCBOR :: TSProperty
ts_roundTripHeaderHashCBOR =
  eachOfTS 1000 genHeaderHash roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- BoundaryProof
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryProof :: Property
goldenDeprecatedBoundaryProof = deprecatedGoldenDecode
  "BoundaryProof"
  dropBytes
  "test/golden/cbor/block/BoundaryProof"


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

goldenBody :: Property
goldenBody = goldenTestCBORAnnotated exampleBody "test/golden/cbor/block/Body"

ts_roundTripBodyCBOR :: TSProperty
ts_roundTripBodyCBOR = eachOfTS 20 (feedPM genBody) roundTripsCBORAnnotatedShow


--------------------------------------------------------------------------------
-- Proof
--------------------------------------------------------------------------------

goldenProof :: Property
goldenProof = goldenTestCBORAnnotated exampleProof "test/golden/cbor/block/Proof"

ts_roundTripProofCBOR :: TSProperty
ts_roundTripProofCBOR = eachOfTS 20 (feedPM genProof) roundTripsCBORAnnotatedBuildable


--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

goldenToSign :: Property
goldenToSign = goldenTestCBORAnnotated exampleToSign "test/golden/cbor/block/ToSign"

ts_roundTripToSignCBOR :: TSProperty
ts_roundTripToSignCBOR =
  eachOfTS 20 (feedPMEpochSlots genToSign) roundTripsCBORAnnotatedShow


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleHeader :: Header
exampleHeader = mkHeaderExplicit
  (ProtocolMagicId 7)
  exampleHeaderHash
  exampleChainDifficulty
  exampleEs
  (exampleSlotNumber exampleEs)
  delegateSk
  certificate
  exampleBody
  Update.exampleProtocolVersion
  Update.exampleSoftwareVersion
 where
  pm          = ProtocolMagicId 7
  [delegateSk, issuerSk] = exampleSigningKeys 5 2
  certificate = Delegation.signCertificate
    pm
    (toVerification delegateSk)
    (EpochNumber 5)
    (noPassSafeSigner issuerSk)

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature cert sig
 where
  cert = Delegation.signCertificate
    pm
    (toVerification delegateSK)
    (EpochNumber 5)
    (noPassSafeSigner issuerSK)

  sig = sign pm (SignBlock (toVerification issuerSK)) delegateSK exampleToSign

  [delegateSK, issuerSK] = exampleSigningKeys 5 2
  pm = ProtocolMagicId 7

exampleProof :: Proof
exampleProof = Proof
  exampleTxProof
  SscProof
  (abstractHash dp)
  Update.exampleProof
  where dp = Delegation.UnsafePayload (take 4 exampleCertificates)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleBody :: Body
exampleBody = Body exampleTxPayload SscPayload dp Update.examplePayload
  where dp = Delegation.UnsafePayload (take 4 exampleCertificates)

exampleToSign :: ToSign
exampleToSign = ToSign
  exampleHeaderHash
  exampleProof
  exampleEpochAndSlotCount
  exampleChainDifficulty
  Update.exampleProtocolVersion
  Update.exampleSoftwareVersion


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
