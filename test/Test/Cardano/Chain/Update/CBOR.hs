{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Update.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Group, Property)
import qualified Hedgehog as H

import Cardano.Binary (Raw(..))
import Cardano.Chain.Common (LovelacePortion(..))
import Cardano.Chain.Update (ApplicationName(..), SoftforkRule(..))
import Cardano.Crypto (Hash, abstractHash)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBOR, roundTripsCBORBuildable, roundTripsCBORShow)
import Test.Cardano.Chain.Update.Example
  ( exampleProtocolParametersUpdate
  , examplePayload
  , exampleProof
  , exampleProposal
  , exampleProposalBody
  , exampleProtocolParameters
  , exampleProtocolVersion
  , exampleSoftwareVersion
  , exampleSystemTag
  , exampleUpId
  , exampleUpdateData
  , exampleVote
  , exampleVoteId
  )
import Test.Cardano.Chain.Update.Gen
  ( genApplicationName
  , genProtocolParametersUpdate
  , genPayload
  , genProof
  , genProposal
  , genProposalBody
  , genProtocolParameters
  , genProtocolVersion
  , genSoftforkRule
  , genSoftwareVersion
  , genSystemTag
  , genUpId
  , genUpdateData
  , genUpsData
  , genVote
  , genVoteId
  )
import Test.Cardano.Crypto.Gen (feedPM, genHashRaw)
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

goldenApplicationName :: Property
goldenApplicationName = goldenTestCBOR
  aN
  "test/golden/cbor/update/ApplicationName"
  where aN = ApplicationName "Golden"

ts_roundTripApplicationName :: TSProperty
ts_roundTripApplicationName =
  eachOfTS 50 genApplicationName roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- ProtocolVersion
--------------------------------------------------------------------------------

goldenProtocolVersion :: Property
goldenProtocolVersion = goldenTestCBOR
  exampleProtocolVersion
  "test/golden/cbor/update/ProtocolVersion"

ts_roundTripProtocolVersion :: TSProperty
ts_roundTripProtocolVersion =
  eachOfTS 50 genProtocolVersion roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- ProtocolParameters
--------------------------------------------------------------------------------

goldenProtocolParameters :: Property
goldenProtocolParameters = goldenTestCBOR
  bVerDat
  "test/golden/cbor/update/ProtocolParameters"
  where bVerDat = exampleProtocolParameters

ts_roundTripProtocolParameters :: TSProperty
ts_roundTripProtocolParameters =
  eachOfTS 50 genProtocolParameters roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- ProtocolParametersUpdate
--------------------------------------------------------------------------------

goldenProtocolParametersUpdate :: Property
goldenProtocolParametersUpdate = goldenTestCBOR
  ppu
  "test/golden/cbor/update/ProtocolParametersUpdate"
  where ppu = exampleProtocolParametersUpdate

ts_roundTripProtocolParametersUpdate :: TSProperty
ts_roundTripProtocolParametersUpdate =
  eachOfTS 50 genProtocolParametersUpdate roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

goldenBlockHashRaw :: Property
goldenBlockHashRaw = goldenTestCBOR hRaw "test/golden/cbor/update/HashRaw"
  where hRaw = (abstractHash $ Raw ("9") :: Hash Raw)

ts_roundTripHashRaw :: TSProperty
ts_roundTripHashRaw = eachOfTS 50 genHashRaw roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

goldenSoftforkRule :: Property
goldenSoftforkRule = goldenTestCBOR sfR "test/golden/cbor/update/SoftforkRule"
 where
  sfR = SoftforkRule
    (LovelacePortion 99)
    (LovelacePortion 99)
    (LovelacePortion 99)

ts_roundTripSoftforkRule :: TSProperty
ts_roundTripSoftforkRule = eachOfTS 10 genSoftforkRule roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

goldenSoftwareVersion :: Property
goldenSoftwareVersion = goldenTestCBOR
  exampleSoftwareVersion
  "test/golden/cbor/update/SoftwareVersion"

ts_roundTripSoftwareVersion :: TSProperty
ts_roundTripSoftwareVersion =
  eachOfTS 10 genSoftwareVersion roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

goldenSystemTag :: Property
goldenSystemTag =
  goldenTestCBOR exampleSystemTag "test/golden/cbor/update/SystemTag"

ts_roundTripSystemTag :: TSProperty
ts_roundTripSystemTag = eachOfTS 10 genSystemTag roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

goldenUpdateData :: Property
goldenUpdateData =
  goldenTestCBOR exampleUpdateData "test/golden/cbor/update/UpdateData"

ts_roundTripUpdateData :: TSProperty
ts_roundTripUpdateData = eachOfTS 20 genUpdateData roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

goldenUpdatePayload :: Property
goldenUpdatePayload =
  goldenTestCBOR examplePayload "test/golden/cbor/update/Payload"

ts_roundTripUpdatePayload :: TSProperty
ts_roundTripUpdatePayload =
  eachOfTS 20 (feedPM genPayload) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

goldenUpdateProof :: Property
goldenUpdateProof = goldenTestCBOR exampleProof "test/golden/cbor/update/Proof"

ts_roundTripUpdateProof :: TSProperty
ts_roundTripUpdateProof = eachOfTS 20 (feedPM genProof) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

goldenUpdateProposal :: Property
goldenUpdateProposal =
  goldenTestCBOR exampleProposal "test/golden/cbor/update/Proposal"

ts_roundTripUpdateProposal :: TSProperty
ts_roundTripUpdateProposal =
  eachOfTS 20 (feedPM genProposal) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

goldenProposalBody :: Property
goldenProposalBody =
  goldenTestCBOR exampleProposalBody "test/golden/cbor/update/ProposalBody"

ts_roundTripProposalBody :: TSProperty
ts_roundTripProposalBody = eachOfTS 20 genProposalBody roundTripsCBORShow


--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

goldenUpdateVote :: Property
goldenUpdateVote = goldenTestCBOR exampleVote "test/golden/cbor/update/Vote"

ts_roundTripUpdateVote :: TSProperty
ts_roundTripUpdateVote = eachOfTS 20 (feedPM genVote) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

goldenUpId :: Property
goldenUpId = goldenTestCBOR exampleUpId "test/golden/cbor/update/UpId"

ts_roundTripUpId :: TSProperty
ts_roundTripUpId = eachOfTS 20 (feedPM genUpId) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- UpsData NB: UpsData is not a type it is a record accessor of `ProposalBody`
--------------------------------------------------------------------------------

ts_roundTripUpsData :: TSProperty
ts_roundTripUpsData = eachOfTS 20 genUpsData roundTripsCBORShow


--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

goldenVoteId :: Property
goldenVoteId = goldenTestCBOR exampleVoteId "test/golden/cbor/update/VoteId"

ts_roundTripVoteId :: TSProperty
ts_roundTripVoteId = eachOfTS 20 (feedPM genVoteId) roundTripsCBORBuildable


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
  [ H.checkSequential $$discoverGolden
  , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
  ]
