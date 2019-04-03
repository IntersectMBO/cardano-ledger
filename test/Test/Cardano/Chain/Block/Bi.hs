{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Block.Bi
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

import Hedgehog (Property, tripping)
import qualified Hedgehog as H

import Cardano.Binary.Class
  ( decodeFullDecoder
  , dropBytes
  , serializeEncoding
  )
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
  , decodeBlockOrBoundary
  , decodeConsensusData
  , decodeHeader
  , decodeHeader'
  , dropBoundaryBody
  , dropBoundaryConsensusData
  , dropBoundaryHeader
  , encodeBlock
  , encodeConsensusData
  , encodeHeader
  , encodeHeader'
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
  , goldenTestBi
  , goldenTestCBOR
  , roundTripsBiBuildable
  , roundTripsBiShow
  )
import Test.Cardano.Chain.Block.Gen
import Test.Cardano.Chain.Common.Example (exampleChainDifficulty)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import Test.Cardano.Chain.Slotting.Example (exampleSlotId, exampleFlatSlotId)
import Test.Cardano.Chain.Slotting.Gen
  ( feedPMEpochSlots
  , genWithEpochSlots
  )
import Test.Cardano.Chain.Txp.Example
  (exampleTxPayload, exampleTxProof)
import qualified Test.Cardano.Chain.Update.Example as Update
import Test.Cardano.Crypto.Example (examplePublicKey, exampleSecretKeys)
import Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Number of slots-per-epoch to be used throughout the examples in this
-- module.
exampleEs :: EpochSlots
exampleEs = EpochSlots 50

goldenHeader :: Property
goldenHeader =
  goldenTestCBOR
    "Header"
    (encodeHeader' exampleEs)
    (decodeHeader' exampleEs)
    exampleHeader
    "test/golden/bi/block/Header"

-- | Round-trip test the backwards compatible header encoding/decoding functions
roundTripHeaderCompat :: Property
roundTripHeaderCompat =
  eachOf 10 (feedPMEpochSlots $ genWithEpochSlots genHeader) roundTripsHeaderCompat
  where
    roundTripsHeaderCompat :: WithEpochSlots Header -> H.PropertyT IO ()
    roundTripsHeaderCompat esh@(WithEpochSlots es _) =
      trippingBuildable
        esh
        (serializeEncoding . encodeHeader es . unWithEpochSlots)
        (fmap (WithEpochSlots es . fromJust) . decodeFullDecoder "Header" (decodeHeader es))

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | Round-trip test the backwards compatible block encoding/decoding functions
roundTripBlockCompat :: Property
roundTripBlockCompat =
  eachOf 10 (feedPM genBlockWithEpochSlots) roundTripsBlockCompat
  where
    roundTripsBlockCompat :: WithEpochSlots Block -> H.PropertyT IO ()
    roundTripsBlockCompat esb@(WithEpochSlots es _) =
      trippingBuildable
        esb
        (serializeEncoding . encodeBlock es . unWithEpochSlots)
        (fmap (WithEpochSlots es . fromJust) . decodeFullDecoder "Block" (decodeBlockOrBoundary es))


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

goldenBlockSignature :: Property
goldenBlockSignature =
  goldenTestBi exampleBlockSignature "test/golden/bi/block/BlockSignature"

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi =
  eachOf 10 (feedPMEpochSlots genBlockSignature) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryBlockHeader
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBlockHeader :: Property
goldenDeprecatedBoundaryBlockHeader = deprecatedGoldenDecode
  "BoundaryBlockHeader"
  (void dropBoundaryHeader)
  "test/golden/bi/block/BoundaryBlockHeader"


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryBody :: Property
goldenDeprecatedBoundaryBody = deprecatedGoldenDecode
  "BoundaryBody"
  dropBoundaryBody
  "test/golden/bi/block/BoundaryBody"


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryConsensusData :: Property
goldenDeprecatedBoundaryConsensusData = deprecatedGoldenDecode
  "BoundaryConsensusData"
  dropBoundaryConsensusData
  "test/golden/bi/block/BoundaryConsensusData"


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

goldenHeaderHash :: Property
goldenHeaderHash =
  goldenTestBi exampleHeaderHash "test/golden/bi/block/HeaderHash"

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryProof
--------------------------------------------------------------------------------

goldenDeprecatedBoundaryProof :: Property
goldenDeprecatedBoundaryProof = deprecatedGoldenDecode
  "BoundaryProof"
  dropBytes
  "test/golden/bi/block/BoundaryProof"


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

goldenBody :: Property
goldenBody = goldenTestBi exampleBody "test/golden/bi/block/Body"

roundTripBodyBi :: Property
roundTripBodyBi = eachOf 20 (feedPM genBody) roundTripsBiShow


--------------------------------------------------------------------------------
-- ConsensusData
--------------------------------------------------------------------------------

goldenConsensusData :: Property
goldenConsensusData =
  goldenTestCBOR
    "ConcensusData"
    (encodeConsensusData exampleEs)
    (decodeConsensusData exampleEs)
    mcd
    "test/golden/bi/block/ConsensusData"
 where
  mcd =
    consensusData
      (exampleFlatSlotId exampleEs)
      examplePublicKey
      exampleChainDifficulty
      exampleBlockSignature

roundTripConsensusData :: Property
roundTripConsensusData =
  eachOf 20 (feedPMEpochSlots $ genWithEpochSlots genConsensusData) roundTripConsensusData'
  where
    roundTripConsensusData' :: WithEpochSlots ConsensusData -> H.PropertyT IO ()
    roundTripConsensusData' (WithEpochSlots es cd) =
      tripping
        cd
        (serializeEncoding . encodeConsensusData es)
        (decodeFullDecoder "ConsensusData" $ decodeConsensusData es)

--------------------------------------------------------------------------------
-- ExtraBodyData
--------------------------------------------------------------------------------

goldenExtraBodyData :: Property
goldenExtraBodyData = goldenTestBi mebd "test/golden/bi/block/ExtraBodyData"
  where mebd = ExtraBodyData (mkAttributes ())

roundTripExtraBodyDataBi :: Property
roundTripExtraBodyDataBi = eachOf 1000 genExtraBodyData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- ExtraHeaderData
--------------------------------------------------------------------------------

goldenExtraHeaderData :: Property
goldenExtraHeaderData =
  goldenTestBi exampleExtraHeaderData "test/golden/bi/block/ExtraHeaderData"

roundTripExtraHeaderDataBi :: Property
roundTripExtraHeaderDataBi =
  eachOf 1000 genExtraHeaderData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- Proof
--------------------------------------------------------------------------------

goldenProof :: Property
goldenProof = goldenTestBi exampleProof "test/golden/bi/block/Proof"

roundTripProofBi :: Property
roundTripProofBi = eachOf 20 (feedPM genProof) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

goldenToSign :: Property
goldenToSign = goldenTestBi exampleToSign "test/golden/bi/block/ToSign"

roundTripToSignBi :: Property
roundTripToSignBi = eachOf 20 (feedPMEpochSlots genToSign) roundTripsBiShow


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
  pm = ProtocolMagicId 7
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

tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
