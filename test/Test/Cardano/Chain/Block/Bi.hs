{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Block.Bi
       ( tests
       , exampleBody
       , exampleConsensusData
       , exampleToSign
       , exampleBlockSignature
       , exampleBlockPSignatureLight
       , exampleBlockPSignatureHeavy
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.Coerce (coerce)
import           Data.List ((!!))
import           Data.Maybe (fromJust)

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Binary.Class (decodeFullDecoder, dropBytes,
                     serializeEncoding)
import           Cardano.Chain.Block (BlockSignature (..), Body (..),
                     ConsensusData (..), ExtraBodyData (..),
                     ExtraHeaderData (..), Header, HeaderHash, Proof (..),
                     SlogUndo (..), ToSign (..), Undo (..), decodeBlock,
                     decodeHeader, dropBoundaryBody, dropBoundaryConsensusData,
                     dropBoundaryHeader, encodeBlock, encodeHeader,
                     mkHeaderExplicit)
import           Cardano.Chain.Common (mkAttributes)
import           Cardano.Chain.Delegation as Delegation (Payload (..))
import           Cardano.Chain.Ssc (SscPayload (..), SscProof (..))
import           Cardano.Crypto (ProtocolMagic (..), SignTag (..), abstractHash,
                     createPsk, hash, proxySign, sign, toPublic)

import           Test.Cardano.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     legacyGoldenDecode, roundTripsBiBuildable,
                     roundTripsBiShow)
import           Test.Cardano.Chain.Block.Gen
import           Test.Cardano.Chain.Common.Example (exampleChainDifficulty)
import           Test.Cardano.Chain.Delegation.Example (exampleLightDlgIndices,
                     staticHeavyDlgIndexes, staticProxySKHeavys)
import qualified Test.Cardano.Chain.Delegation.Example as Delegation
import           Test.Cardano.Chain.Slotting.Example (exampleSlotId)
import           Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots)
import           Test.Cardano.Chain.Txp.Example (exampleTxPayload,
                     exampleTxProof, exampleTxpUndo)
import qualified Test.Cardano.Chain.Update.Example as Update
import           Test.Cardano.Crypto.Example (examplePublicKey,
                     exampleSecretKey, exampleSecretKeys)
import           Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

golden_Header :: Property
golden_Header =
  goldenTestBi exampleHeader "test/golden/block/Header"

roundTripHeaderBi :: Property
roundTripHeaderBi =
  eachOf 10 (feedPMEpochSlots genHeader) roundTripsBiBuildable

-- | Round-trip test the backwards compatible header encoding/decoding functions
roundTripHeaderCompat :: Property
roundTripHeaderCompat = eachOf
  10
  (feedPMEpochSlots genHeader)
  roundTripsHeaderCompat
 where
  roundTripsHeaderCompat a = trippingBuildable
    a
    (serializeEncoding . encodeHeader)
    (fmap fromJust . decodeFullDecoder "Header" decodeHeader)


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

roundTripBlock :: Property
roundTripBlock = eachOf 10 (feedPMEpochSlots genBlock) roundTripsBiBuildable

-- | Round-trip test the backwards compatible block encoding/decoding functions
roundTripBlockCompat :: Property
roundTripBlockCompat = eachOf
  10
  (feedPMEpochSlots genBlock)
  roundTripsBlockCompat
 where
  roundTripsBlockCompat a = trippingBuildable
    a
    (serializeEncoding . encodeBlock)
    (fmap fromJust . decodeFullDecoder "Block" decodeBlock)


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

golden_BlockSignature :: Property
golden_BlockSignature =
  goldenTestBi exampleBlockSignature "test/golden/BlockSignature"

golden_BlockSignature_Light :: Property
golden_BlockSignature_Light =
  goldenTestBi exampleBlockPSignatureLight "test/golden/BlockSignature_Light"

golden_BlockSignature_Heavy :: Property
golden_BlockSignature_Heavy =
  goldenTestBi exampleBlockPSignatureHeavy "test/golden/BlockSignature_Heavy"

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi =
  eachOf 10 (feedPMEpochSlots genBlockSignature) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryBlockHeader
--------------------------------------------------------------------------------

golden_legacy_BoundaryBlockHeader :: Property
golden_legacy_BoundaryBlockHeader = legacyGoldenDecode
  "BoundaryBlockHeader"
  dropBoundaryHeader
  "test/golden/BoundaryBlockHeader"


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

golden_legacy_BoundaryBody :: Property
golden_legacy_BoundaryBody = legacyGoldenDecode
  "BoundaryBody"
  dropBoundaryBody
  "test/golden/BoundaryBody"


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

golden_legacy_BoundaryConsensusData :: Property
golden_legacy_BoundaryConsensusData = legacyGoldenDecode
  "BoundaryConsensusData"
  dropBoundaryConsensusData
  "test/golden/BoundaryConsensusData"


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

golden_HeaderHash :: Property
golden_HeaderHash = goldenTestBi exampleHeaderHash "test/golden/HeaderHash"

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryProof
--------------------------------------------------------------------------------

golden_legacy_BoundaryProof :: Property
golden_legacy_BoundaryProof = legacyGoldenDecode
  "BoundaryProof"
  dropBytes
  "test/golden/BoundaryProof"


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

golden_Body :: Property
golden_Body = goldenTestBi exampleBody "test/golden/block/Body"

roundTripBodyBi :: Property
roundTripBodyBi = eachOf 20 (feedPM genBody) roundTripsBiShow


--------------------------------------------------------------------------------
-- ConsensusData
--------------------------------------------------------------------------------

golden_ConsensusData :: Property
golden_ConsensusData = goldenTestBi mcd "test/golden/block/ConsensusData"
 where
  mcd = ConsensusData
    exampleSlotId
    examplePublicKey
    exampleChainDifficulty
    exampleBlockSignature

roundTripConsensusData :: Property
roundTripConsensusData =
  eachOf 20 (feedPMEpochSlots genConsensusData) roundTripsBiShow


--------------------------------------------------------------------------------
-- ExtraBodyData
--------------------------------------------------------------------------------

golden_ExtraBodyData :: Property
golden_ExtraBodyData = goldenTestBi mebd "test/golden/block/ExtraBodyData"
  where mebd = ExtraBodyData (mkAttributes ())

roundTripExtraBodyDataBi :: Property
roundTripExtraBodyDataBi =
  eachOf 1000 genExtraBodyData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- ExtraHeaderData
--------------------------------------------------------------------------------

golden_ExtraHeaderData :: Property
golden_ExtraHeaderData =
  goldenTestBi exampleExtraHeaderData "test/golden/block/ExtraHeaderData"

roundTripExtraHeaderDataBi :: Property
roundTripExtraHeaderDataBi =
  eachOf 1000 genExtraHeaderData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- Proof
--------------------------------------------------------------------------------

golden_Proof :: Property
golden_Proof = goldenTestBi exampleProof "test/golden/block/Proof"

roundTripProofBi :: Property
roundTripProofBi = eachOf 20 (feedPM genProof) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- ToSign
--------------------------------------------------------------------------------

golden_ToSign :: Property
golden_ToSign = goldenTestBi exampleToSign "test/golden/block/ToSign"

roundTripToSignBi :: Property
roundTripToSignBi =
  eachOf 20 (feedPMEpochSlots genToSign) roundTripsBiShow


--------------------------------------------------------------------------------
-- Undo
--------------------------------------------------------------------------------

golden_Undo :: Property
golden_Undo = goldenTestBi exampleUndo "test/golden/block/Undo"

roundTripUndo :: Property
roundTripUndo = eachOf 20 (feedPMEpochSlots genUndo) roundTripsBiShow


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleHeader :: Header
exampleHeader = mkHeaderExplicit
  (ProtocolMagic 7)
  exampleHeaderHash
  exampleChainDifficulty
  exampleSlotId
  exampleSecretKey
  Nothing
  exampleBody
  exampleExtraHeaderData

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature
  (sign (ProtocolMagic 7) SignMainBlock exampleSecretKey exampleToSign)

exampleBlockPSignatureLight :: BlockSignature
exampleBlockPSignatureLight = BlockPSignatureLight sig
 where
  sig = proxySign pm SignProxySK delegateSk psk exampleToSign
  [delegateSk, issuerSk] = exampleSecretKeys 5 2
  psk = createPsk pm issuerSk (toPublic delegateSk) exampleLightDlgIndices
  pm = ProtocolMagic 2

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
 where
  sig = proxySign pm SignProxySK delegateSk psk exampleToSign
  [delegateSk, issuerSk] = exampleSecretKeys 5 2
  psk =
    createPsk pm issuerSk (toPublic delegateSk) (staticHeavyDlgIndexes !! 0)
  pm = ProtocolMagic 2

exampleConsensusData :: ConsensusData
exampleConsensusData = ConsensusData
  exampleSlotId
  examplePublicKey
  exampleChainDifficulty
  exampleBlockSignature

exampleExtraHeaderData :: ExtraHeaderData
exampleExtraHeaderData = ExtraHeaderData
  Update.exampleBlockVersion
  Update.exampleSoftwareVersion
  (mkAttributes ())
  (abstractHash (ExtraBodyData (mkAttributes ())))

exampleProof :: Proof
exampleProof = Proof
  exampleTxProof
  SscProof
  (abstractHash dp)
  Update.exampleProof
  where dp = Delegation.UnsafePayload (take 4 staticProxySKHeavys)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleBody :: Body
exampleBody = Body exampleTxPayload SscPayload dp Update.examplePayload
  where dp = Delegation.UnsafePayload (take 4 staticProxySKHeavys)

exampleToSign :: ToSign
exampleToSign = ToSign
  exampleHeaderHash
  exampleProof
  exampleSlotId
  exampleChainDifficulty
  exampleExtraHeaderData

exampleSlogUndo :: SlogUndo
exampleSlogUndo = SlogUndo $ Just 999

exampleUndo :: Undo
exampleUndo = Undo
  { undoTx   = exampleTxpUndo
  , undoDlg  = Delegation.exampleUndo
  , undoUS   = Update.exampleUndo
  , undoSlog = exampleSlogUndo
  }


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
