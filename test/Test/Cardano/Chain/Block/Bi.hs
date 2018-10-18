{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Block.Bi
       ( tests
       , exampleMainBody
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.Coerce (coerce)
import           Data.List ((!!))
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Chain.Block (BlockHeader (..), BlockHeaderAttributes,
                     BlockSignature (..), BoundaryBlockHeader,
                     BoundaryBody (..), BoundaryConsensusData (..),
                     BoundaryProof (..), HeaderHash, MainBlockHeader,
                     MainBody (..), MainConsensusData (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), MainToSign (..), SlogUndo (..), Undo (..),
                     mkBoundaryHeader, mkMainHeaderExplicit)
import           Cardano.Chain.Common (mkAttributes)
import           Cardano.Chain.Delegation as Delegation (Payload (..))
import           Cardano.Chain.Genesis (GenesisHash (..))
import           Cardano.Chain.Slotting (EpochIndex (..))
import           Cardano.Chain.Ssc (SscPayload (..), SscProof (..))
import           Cardano.Crypto (Hash, ProtocolMagic (..), SignTag (..),
                     abstractHash, createPsk, hash, proxySign, sign, toPublic)

import           Test.Cardano.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Cardano.Chain.Block.Gen
import           Test.Cardano.Chain.Common.Example (exampleChainDifficulty,
                     exampleSlotLeaders)
import           Test.Cardano.Chain.Delegation.Example (exampleLightDlgIndices,
                     staticHeavyDlgIndexes, staticProxySKHeavys)
import qualified Test.Cardano.Chain.Delegation.Example as Delegation
import           Test.Cardano.Chain.Slotting.Example (exampleEpochIndex,
                     exampleSlotId)
import           Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots)
import           Test.Cardano.Chain.Txp.Example (exampleTxPayload,
                     exampleTxProof, exampleTxpUndo)
import qualified Test.Cardano.Chain.Update.Example as Update
import           Test.Cardano.Crypto.Example (examplePublicKey,
                     exampleSecretKey, exampleSecretKeys)
import           Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- BlockBodyAttributes
--------------------------------------------------------------------------------

golden_BlockBodyAttributes :: Property
golden_BlockBodyAttributes = goldenTestBi bba "test/golden/BlockBodyAttributes"
    where bba = mkAttributes ()

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi =
    eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BlockHeader
--------------------------------------------------------------------------------

golden_BlockHeader_Boundary :: Property
golden_BlockHeader_Boundary =
    goldenTestBi exampleBlockHeaderBoundary "test/golden/BlockHeader_Boundary"

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
golden_BlockHeaderMain :: Property
golden_BlockHeaderMain =
    goldenTestBi exampleBlockHeaderMain "test/golden/BlockHeaderMain"

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi =
    eachOf 10 (feedPMEpochSlots genBlockHeader) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BlockHeaderAttributes
--------------------------------------------------------------------------------

golden_BlockHeaderAttributes :: Property
golden_BlockHeaderAttributes = goldenTestBi
    (mkAttributes () :: BlockHeaderAttributes)
    "test/golden/BlockHeaderAttributes"

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi =
    eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable


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

golden_BoundaryBlockHeader :: Property
golden_BoundaryBlockHeader =
    goldenTestBi exampleBoundaryBlockHeader "test/golden/BoundaryBlockHeader"

roundTripBoundaryBlockHeaderBi :: Property
roundTripBoundaryBlockHeaderBi =
    eachOf 20 (feedPMEpochSlots genBoundaryBlockHeader) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

golden_BoundaryBody :: Property
golden_BoundaryBody =
    goldenTestBi exampleBoundaryBody "test/golden/BoundaryBody"

roundTripBoundaryBodyBi :: Property
roundTripBoundaryBodyBi = eachOf 1000 genBoundaryBody roundTripsBiShow


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

golden_BoundaryConsensusData :: Property
golden_BoundaryConsensusData = goldenTestBi
    cd
    "test/golden/BoundaryConsensusData"
    where cd = BoundaryConsensusData exampleEpochIndex exampleChainDifficulty

roundTripBoundaryConsensusDataBi :: Property
roundTripBoundaryConsensusDataBi =
    eachOf 1000 genBoundaryConsensusData roundTripsBiShow


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

golden_BoundaryProof :: Property
golden_BoundaryProof = goldenTestBi gp "test/golden/BoundaryProof"
    where gp = BoundaryProof (abstractHash exampleSlotLeaders)

roundTripBoundaryProofBi :: Property
roundTripBoundaryProofBi = eachOf 1000 genBoundaryProof roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainBlockHeader
--------------------------------------------------------------------------------

golden_MainBlockHeader :: Property
golden_MainBlockHeader =
    goldenTestBi exampleMainBlockHeader "test/golden/MainBlockHeader"

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi =
    eachOf 20 (feedPMEpochSlots genMainBlockHeader) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainBody
--------------------------------------------------------------------------------

golden_MainBody :: Property
golden_MainBody = goldenTestBi exampleMainBody "test/golden/MainBody"

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 20 (feedPM genMainBody) roundTripsBiShow


--------------------------------------------------------------------------------
-- MainConsensusData
--------------------------------------------------------------------------------

golden_MainConsensusData :: Property
golden_MainConsensusData = goldenTestBi mcd "test/golden/MainConsensusData"
  where
    mcd = MainConsensusData
        exampleSlotId
        examplePublicKey
        exampleChainDifficulty
        exampleBlockSignature

roundTripMainConsensusData :: Property
roundTripMainConsensusData =
    eachOf 20 (feedPMEpochSlots genMainConsensusData) roundTripsBiShow


--------------------------------------------------------------------------------
-- MainExtraBodyData
--------------------------------------------------------------------------------

golden_MainExtraBodyData :: Property
golden_MainExtraBodyData = goldenTestBi mebd "test/golden/MainExtraBodyData"
    where mebd = MainExtraBodyData (mkAttributes ())

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi =
    eachOf 1000 genMainExtraBodyData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainExtraHeaderData
--------------------------------------------------------------------------------

golden_MainExtraHeaderData :: Property
golden_MainExtraHeaderData =
    goldenTestBi exampleMainExtraHeaderData "test/golden/MainExtraHeaderData"

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi =
    eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainProof
--------------------------------------------------------------------------------

golden_MainProof :: Property
golden_MainProof = goldenTestBi exampleMainProof "test/golden/MainProof"

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 20 (feedPM genMainProof) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainToSign
--------------------------------------------------------------------------------

golden_MainToSign :: Property
golden_MainToSign = goldenTestBi exampleMainToSign "test/golden/MainToSign"

roundTripMainToSignBi :: Property
roundTripMainToSignBi =
    eachOf 20 (feedPMEpochSlots genMainToSign) roundTripsBiShow


--------------------------------------------------------------------------------
-- Undo
--------------------------------------------------------------------------------

golden_Undo :: Property
golden_Undo = goldenTestBi exampleUndo "test/golden/Undo"

roundTripUndo :: Property
roundTripUndo = eachOf 20 (feedPMEpochSlots genUndo) roundTripsBiShow


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------


exampleBlockHeaderBoundary :: BlockHeader
exampleBlockHeaderBoundary = BlockHeaderBoundary exampleBoundaryBlockHeader

exampleBlockHeaderMain :: MainBlockHeader
exampleBlockHeaderMain = mkMainHeaderExplicit
    (ProtocolMagic 0)
    exampleHeaderHash
    exampleChainDifficulty
    exampleSlotId
    exampleSecretKey
    Nothing
    exampleMainBody
    exampleMainExtraHeaderData

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature
    (sign (ProtocolMagic 7) SignMainBlock exampleSecretKey exampleMainToSign)

exampleBlockPSignatureLight :: BlockSignature
exampleBlockPSignatureLight = BlockPSignatureLight sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk =
        createPsk pm issuerSk (toPublic delegateSk) exampleLightDlgIndices
    pm = ProtocolMagic 2

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk
        pm
        issuerSk
        (toPublic delegateSk)
        (staticHeavyDlgIndexes !! 0)
    pm = ProtocolMagic 2

exampleMainConsensusData :: MainConsensusData
exampleMainConsensusData = MainConsensusData
    exampleSlotId
    examplePublicKey
    exampleChainDifficulty
    exampleBlockSignature

exampleMainExtraHeaderData :: MainExtraHeaderData
exampleMainExtraHeaderData = MainExtraHeaderData
    Update.exampleBlockVersion
    Update.exampleSoftwareVersion
    (mkAttributes ())
    (abstractHash (MainExtraBodyData (mkAttributes ())))

exampleBoundaryBlockHeader :: BoundaryBlockHeader
exampleBoundaryBlockHeader = mkBoundaryHeader
    (ProtocolMagic 0)
    (Left (GenesisHash prevHash))
    (EpochIndex 11)
    exampleBoundaryBody
    where prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
exampleMainBlockHeader :: MainBlockHeader
exampleMainBlockHeader = mkMainHeaderExplicit
    (ProtocolMagic 7)
    exampleHeaderHash
    exampleChainDifficulty
    exampleSlotId
    exampleSecretKey
    Nothing
    exampleMainBody
    exampleMainExtraHeaderData

exampleMainProof :: MainProof
exampleMainProof = MainProof
    exampleTxProof
    SscProof
    (abstractHash dp)
    Update.exampleProof
    where dp = Delegation.UnsafePayload (take 4 staticProxySKHeavys)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleBoundaryBody :: BoundaryBody
exampleBoundaryBody = BoundaryBody exampleSlotLeaders

exampleMainBody :: MainBody
exampleMainBody = MainBody
    exampleTxPayload
    SscPayload
    dp
    Update.examplePayload
    where dp = Delegation.UnsafePayload (take 4 staticProxySKHeavys)

exampleMainToSign :: MainToSign
exampleMainToSign = MainToSign
    (abstractHash (BlockHeaderBoundary exampleBoundaryBlockHeader))
    exampleMainProof
    exampleSlotId
    exampleChainDifficulty
    exampleMainExtraHeaderData

exampleSlogUndo :: SlogUndo
exampleSlogUndo = SlogUndo $ Just 999

exampleUndo :: Undo
exampleUndo = Undo
  { undoTx = exampleTxpUndo
  , undoDlg = Delegation.exampleUndo
  , undoUS = Update.exampleUndo
  , undoSlog = exampleSlogUndo
  }


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
