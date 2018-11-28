{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Block.Bi
  ( tests
  , exampleBlockSignature
  , exampleBlockPSignatureHeavy
  , exampleBody
  , exampleConsensusData
  , exampleHeader
  , exampleProof
  , exampleToSign
  , exampleUndo
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Coerce (coerce)
import Data.Maybe (fromJust)

import Hedgehog (Property)
import qualified Hedgehog as H

import Cardano.Binary.Class (decodeFullDecoder, dropBytes, serializeEncoding)
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
  , SlogUndo(..)
  , ToSign(..)
  , Undo(..)
  , body
  , consensusData
  , decodeBlockOrBoundary
  , decodeHeader
  , dropBoundaryBody
  , dropBoundaryConsensusData
  , dropBoundaryHeader
  , encodeBlock
  , encodeHeader
  , mkHeaderExplicit
  )
import Cardano.Chain.Common (mkAttributes)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (EpochIndex(..))
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto
  ( ProtocolMagic(..)
  , SignTag(..)
  , abstractHash
  , createPsk
  , hash
  , noPassSafeSigner
  , proxySign
  , sign
  , toPublic
  )

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode
  , goldenTestBi
  , roundTripsBiBuildable
  , roundTripsBiShow
  )
import Test.Cardano.Chain.Block.Gen
import Test.Cardano.Chain.Common.Example (exampleChainDifficulty)
import Test.Cardano.Chain.Delegation.Example (exampleCertificates)
import qualified Test.Cardano.Chain.Delegation.Example as Delegation
import Test.Cardano.Chain.Slotting.Example (exampleSlotId)
import Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots)
import Test.Cardano.Chain.Txp.Example
  (exampleTxPayload, exampleTxProof, exampleTxpUndo)
import qualified Test.Cardano.Chain.Update.Example as Update
import Test.Cardano.Crypto.Example
  (examplePublicKey, exampleSecretKey, exampleSecretKeys)
import Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

goldenHeader :: Property
goldenHeader = goldenTestBi exampleHeader "test/golden/bi/block/Header"

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
  roundTripsHeaderCompat :: Header -> H.PropertyT IO ()
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
  roundTripsBlockCompat :: Block -> H.PropertyT IO ()
  roundTripsBlockCompat a = trippingBuildable
    a
    (serializeEncoding . encodeBlock)
    (fmap fromJust . decodeFullDecoder "Block" decodeBlockOrBoundary)


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

goldenBlockSignature :: Property
goldenBlockSignature =
  goldenTestBi exampleBlockSignature "test/golden/bi/block/BlockSignature"

goldenBlockSignature_Heavy :: Property
goldenBlockSignature_Heavy = goldenTestBi
  exampleBlockPSignatureHeavy
  "test/golden/bi/block/BlockSignature_Heavy"

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
goldenConsensusData = goldenTestBi mcd "test/golden/bi/block/ConsensusData"
 where
  mcd = consensusData
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
-- Undo
--------------------------------------------------------------------------------

goldenUndo :: Property
goldenUndo = goldenTestBi exampleUndo "test/golden/bi/block/Undo"

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

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
 where
  sig                    = proxySign pm SignProxySK delegateSk psk exampleToSign
  [delegateSk, issuerSk] = exampleSecretKeys 5 2
  psk                    = createPsk
    pm
    (noPassSafeSigner issuerSk)
    (toPublic delegateSk)
    (EpochIndex 5)
  pm = ProtocolMagic 2

exampleConsensusData :: ConsensusData
exampleConsensusData = consensusData
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
