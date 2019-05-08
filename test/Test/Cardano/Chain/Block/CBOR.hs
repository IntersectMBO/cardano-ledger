{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Block.CBOR
  ( tests
  , exampleBlockSignature
  , exampleBody
  , exampleConsensusData
  , exampleHeader
  , exampleProof
  , exampleToSign
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Coerce (coerce)
import Data.Maybe (fromJust)

import Hedgehog (Group, Property, tripping)
import qualified Hedgehog as H

import Cardano.Binary (decodeFullDecoder, dropBytes, serializeEncoding)
import Cardano.Chain.Block
  ( Block
  , BlockSignature(..)
  , Body
  , ConsensusData
  , ExtraBodyData(..)
  , ExtraHeaderData(..)
  , Header
  , HeaderHash
  , Proof(..)
  , ToSign(..)
  , body
  , consensusData
  , fromCBORBlockOrBoundary
  , fromCBORConsensusData
  , fromCBORHeader
  , fromCBORHeader'
  , dropBoundaryBody
  , dropBoundaryConsensusData
  , dropBoundaryHeader
  , toCBORBlock
  , toCBORConsensusData
  , toCBORHeader
  , toCBORHeader'
  , mkHeaderExplicit
  )
import Cardano.Chain.Common (mkAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting
  ( EpochIndex(..)
  , EpochSlots(EpochSlots)
  , WithEpochSlots(WithEpochSlots)
  , unWithEpochSlots
  )
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto
  ( ProtocolMagicId(..)
  , SignTag(..)
  , abstractHash
  , createPsk
  , hash
  , noPassSafeSigner
  , proxySign
  , toPublic
  )

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode
  , goldenTestCBOR
  , goldenTestCBORExplicit
  , roundTripsCBORBuildable
  , roundTripsCBORShow
  )
import Test.Cardano.Chain.Block.Gen
import Test.Cardano.Chain.Common.Example (exampleChainDifficulty)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Slotting.Example (exampleSlotId, exampleFlatSlotId)
import Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots, genWithEpochSlots)
import Test.Cardano.Chain.UTxO.Example (exampleTxPayload, exampleTxProof)
import qualified Test.Cardano.Chain.Update.Example as Update
import Test.Cardano.Crypto.Example (examplePublicKey, exampleSecretKeys)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Number of slots-per-epoch to be used throughout the examples in this
-- module.
exampleEs :: EpochSlots
exampleEs = EpochSlots 50

goldenHeader :: Property
goldenHeader = goldenTestCBORExplicit
  "Header"
  (toCBORHeader' exampleEs)
  (fromCBORHeader' exampleEs)
  exampleHeader
  "test/golden/cbor/block/Header"

-- | Round-trip test the backwards compatible header encoding/decoding functions
ts_roundTripHeaderCompat :: TSProperty
ts_roundTripHeaderCompat = eachOfTS
  10
  (feedPMEpochSlots $ genWithEpochSlots genHeader)
  roundTripsHeaderCompat
 where
  roundTripsHeaderCompat :: WithEpochSlots Header -> H.PropertyT IO ()
  roundTripsHeaderCompat esh@(WithEpochSlots es _) = trippingBuildable
    esh
    (serializeEncoding . toCBORHeader es . unWithEpochSlots)
    ( fmap (WithEpochSlots es . fromJust)
    . decodeFullDecoder "Header" (fromCBORHeader es)
    )

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Round-trip test the backwards compatible block encoding/decoding functions
ts_roundTripBlockCompat :: TSProperty
ts_roundTripBlockCompat = eachOfTS
  10
  (feedPM genBlockWithEpochSlots)
  roundTripsBlockCompat
 where
  roundTripsBlockCompat :: WithEpochSlots Block -> H.PropertyT IO ()
  roundTripsBlockCompat esb@(WithEpochSlots es _) = trippingBuildable
    esb
    (serializeEncoding . toCBORBlock es . unWithEpochSlots)
    ( fmap (WithEpochSlots es . fromJust)
    . decodeFullDecoder "Block" (fromCBORBlockOrBoundary es)
    )


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

goldenBlockSignature :: Property
goldenBlockSignature =
  goldenTestCBOR exampleBlockSignature "test/golden/cbor/block/BlockSignature"

ts_roundTripBlockSignatureCBOR :: TSProperty
ts_roundTripBlockSignatureCBOR =
  eachOfTS 10 (feedPMEpochSlots genBlockSignature) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- BoundaryBlockHeader
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBlockHeader :: Property
goldenDeprecatedBoundaryBlockHeader = deprecatedGoldenDecode
  "BoundaryBlockHeader"
  (void dropBoundaryHeader)
  "test/golden/cbor/block/BoundaryBlockHeader"


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
  dropBoundaryConsensusData
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
goldenBody = goldenTestCBOR exampleBody "test/golden/cbor/block/Body"

ts_roundTripBodyCBOR :: TSProperty
ts_roundTripBodyCBOR = eachOfTS 20 (feedPM genBody) roundTripsCBORShow


--------------------------------------------------------------------------------
-- ConsensusData
--------------------------------------------------------------------------------

goldenConsensusData :: Property
goldenConsensusData = goldenTestCBORExplicit
  "ConcensusData"
  (toCBORConsensusData exampleEs)
  (fromCBORConsensusData exampleEs)
  exampleConsensusData
  "test/golden/cbor/block/ConsensusData"

ts_roundTripConsensusData :: TSProperty
ts_roundTripConsensusData = eachOfTS
  20
  (feedPMEpochSlots $ genWithEpochSlots genConsensusData)
  roundTripConsensusData'
 where
  roundTripConsensusData' :: WithEpochSlots ConsensusData -> H.PropertyT IO ()
  roundTripConsensusData' (WithEpochSlots es cd) = tripping
    cd
    (serializeEncoding . toCBORConsensusData es)
    (decodeFullDecoder "ConsensusData" $ fromCBORConsensusData es)

--------------------------------------------------------------------------------
-- ExtraBodyData
--------------------------------------------------------------------------------

goldenExtraBodyData :: Property
goldenExtraBodyData = goldenTestCBOR
  mebd
  "test/golden/cbor/block/ExtraBodyData"
  where mebd = ExtraBodyData (mkAttributes ())

ts_roundTripExtraBodyDataCBOR :: TSProperty
ts_roundTripExtraBodyDataCBOR =
  eachOfTS 1000 genExtraBodyData roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- ExtraHeaderData
--------------------------------------------------------------------------------

goldenExtraHeaderData :: Property
goldenExtraHeaderData =
  goldenTestCBOR exampleExtraHeaderData "test/golden/cbor/block/ExtraHeaderData"

ts_roundTripExtraHeaderDataCBOR :: TSProperty
ts_roundTripExtraHeaderDataCBOR =
  eachOfTS 1000 genExtraHeaderData roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- Proof
--------------------------------------------------------------------------------

goldenProof :: Property
goldenProof = goldenTestCBOR exampleProof "test/golden/cbor/block/Proof"

ts_roundTripProofCBOR :: TSProperty
ts_roundTripProofCBOR = eachOfTS 20 (feedPM genProof) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

goldenToSign :: Property
goldenToSign = goldenTestCBOR exampleToSign "test/golden/cbor/block/ToSign"

ts_roundTripToSignCBOR :: TSProperty
ts_roundTripToSignCBOR =
  eachOfTS 20 (feedPMEpochSlots genToSign) roundTripsCBORShow


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleHeader :: Header
exampleHeader = mkHeaderExplicit
  (ProtocolMagicId 7)
  exampleHeaderHash
  exampleChainDifficulty
  exampleEs
  (exampleFlatSlotId exampleEs)
  delegateSk
  certificate
  exampleBody
  exampleExtraHeaderData
 where
  pm          = ProtocolMagicId 7
  [delegateSk, issuerSk] = exampleSecretKeys 5 2
  certificate = createPsk
    pm
    (noPassSafeSigner issuerSk)
    (toPublic delegateSk)
    (EpochIndex 5)

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature sig
 where
  sig = proxySign pm SignProxyVK delegateSk psk exampleToSign
  [delegateSk, issuerSk] = exampleSecretKeys 5 2
  psk = createPsk
    pm
    (noPassSafeSigner issuerSk)
    (toPublic delegateSk)
    (EpochIndex 5)
  pm = ProtocolMagicId 7

exampleConsensusData :: ConsensusData
exampleConsensusData = consensusData
  (exampleFlatSlotId exampleEs)
  examplePublicKey
  exampleChainDifficulty
  exampleBlockSignature

exampleExtraHeaderData :: ExtraHeaderData
exampleExtraHeaderData = ExtraHeaderData
  Update.exampleProtocolVersion
  Update.exampleSoftwareVersion
  (mkAttributes ())
  (abstractHash (ExtraBodyData (mkAttributes ())))

exampleProof :: Proof
exampleProof = Proof
  exampleTxProof
  SscProof
  (abstractHash dp)
  Update.exampleProof
  where dp = Delegation.unsafePayload (take 4 exampleCertificates)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleBody :: Body
exampleBody = body exampleTxPayload SscPayload dp Update.examplePayload
  where dp = Delegation.unsafePayload (take 4 exampleCertificates)

exampleToSign :: ToSign
exampleToSign = ToSign
  exampleHeaderHash
  exampleProof
  (exampleSlotId exampleEs)
  exampleChainDifficulty
  exampleExtraHeaderData


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
  [ H.checkSequential $$discoverGolden
  , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
  ]
