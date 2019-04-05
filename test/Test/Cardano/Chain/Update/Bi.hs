{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Update.Bi
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Map.Strict as Map

import Hedgehog (Group, Property)
import qualified Hedgehog as H

import Cardano.Binary.Class (Raw(..))
import Cardano.Chain.Common (LovelacePortion(..))
import Cardano.Chain.Update (ApplicationName(..), SoftforkRule(..))
import Cardano.Crypto (Hash, abstractHash)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestBi, roundTripsBiBuildable, roundTripsBiShow)
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
  , genProposals
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
goldenApplicationName = goldenTestBi aN "test/golden/bi/update/ApplicationName"
  where aN = ApplicationName "Golden"

ts_roundTripApplicationName :: TSProperty
ts_roundTripApplicationName = eachOfTS 50 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProtocolVersion
--------------------------------------------------------------------------------

goldenProtocolVersion :: Property
goldenProtocolVersion =
  goldenTestBi exampleProtocolVersion "test/golden/bi/update/ProtocolVersion"

ts_roundTripProtocolVersion :: TSProperty
ts_roundTripProtocolVersion = eachOfTS 50 genProtocolVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProtocolParameters
--------------------------------------------------------------------------------

goldenProtocolParameters :: Property
goldenProtocolParameters = goldenTestBi
  bVerDat
  "test/golden/bi/update/ProtocolParameters"
  where bVerDat = exampleProtocolParameters

ts_roundTripProtocolParameters :: TSProperty
ts_roundTripProtocolParameters =
  eachOfTS 50 genProtocolParameters roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProtocolParametersUpdate
--------------------------------------------------------------------------------

goldenProtocolParametersUpdate :: Property
goldenProtocolParametersUpdate = goldenTestBi
  ppu
  "test/golden/bi/update/ProtocolParametersUpdate"
  where ppu = exampleProtocolParametersUpdate

ts_roundTripProtocolParametersUpdate :: TSProperty
ts_roundTripProtocolParametersUpdate =
  eachOfTS 50 genProtocolParametersUpdate roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

goldenBlockHashRaw :: Property
goldenBlockHashRaw = goldenTestBi hRaw "test/golden/bi/update/HashRaw"
  where hRaw = (abstractHash $ Raw ("9") :: Hash Raw)

ts_roundTripHashRaw :: TSProperty
ts_roundTripHashRaw = eachOfTS 50 genHashRaw roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

goldenSoftforkRule :: Property
goldenSoftforkRule = goldenTestBi sfR "test/golden/bi/update/SoftforkRule"
 where
  sfR = SoftforkRule
    (LovelacePortion 99)
    (LovelacePortion 99)
    (LovelacePortion 99)

ts_roundTripSoftforkRule :: TSProperty
ts_roundTripSoftforkRule = eachOfTS 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

goldenSoftwareVersion :: Property
goldenSoftwareVersion =
  goldenTestBi exampleSoftwareVersion "test/golden/bi/update/SoftwareVersion"

ts_roundTripSoftwareVersion :: TSProperty
ts_roundTripSoftwareVersion = eachOfTS 10 genSoftwareVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

goldenSystemTag :: Property
goldenSystemTag =
  goldenTestBi exampleSystemTag "test/golden/bi/update/SystemTag"

ts_roundTripSystemTag :: TSProperty
ts_roundTripSystemTag = eachOfTS 10 genSystemTag roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

goldenUpdateData :: Property
goldenUpdateData =
  goldenTestBi exampleUpdateData "test/golden/bi/update/UpdateData"

ts_roundTripUpdateData :: TSProperty
ts_roundTripUpdateData = eachOfTS 20 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

goldenUpdatePayload :: Property
goldenUpdatePayload =
  goldenTestBi examplePayload "test/golden/bi/update/Payload"

ts_roundTripUpdatePayload :: TSProperty
ts_roundTripUpdatePayload = eachOfTS 20 (feedPM genPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

goldenUpdateProof :: Property
goldenUpdateProof = goldenTestBi exampleProof "test/golden/bi/update/Proof"

ts_roundTripUpdateProof :: TSProperty
ts_roundTripUpdateProof = eachOfTS 20 (feedPM genProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

goldenUpdateProposal :: Property
goldenUpdateProposal =
  goldenTestBi exampleProposal "test/golden/bi/update/Proposal"

ts_roundTripUpdateProposal :: TSProperty
ts_roundTripUpdateProposal = eachOfTS 20 (feedPM genProposal) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

goldenUpdateProposals :: Property
goldenUpdateProposals = goldenTestBi ups "test/golden/bi/update/Proposals"
  where
    -- Need to revisit this.
        ups = Map.fromList [(exampleUpId, exampleProposal)]

ts_roundTripUpdateProposals :: TSProperty
ts_roundTripUpdateProposals = eachOfTS 20 (feedPM genProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

goldenProposalBody :: Property
goldenProposalBody =
  goldenTestBi exampleProposalBody "test/golden/bi/update/ProposalBody"

ts_roundTripProposalBody :: TSProperty
ts_roundTripProposalBody = eachOfTS 20 genProposalBody roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

goldenUpdateVote :: Property
goldenUpdateVote = goldenTestBi exampleVote "test/golden/bi/update/Vote"

ts_roundTripUpdateVote :: TSProperty
ts_roundTripUpdateVote = eachOfTS 20 (feedPM genVote) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

goldenUpId :: Property
goldenUpId = goldenTestBi exampleUpId "test/golden/bi/update/UpId"

ts_roundTripUpId :: TSProperty
ts_roundTripUpId = eachOfTS 20 (feedPM genUpId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData NB: UpsData is not a type it is a record accessor of `ProposalBody`
--------------------------------------------------------------------------------

ts_roundTripUpsData :: TSProperty
ts_roundTripUpsData = eachOfTS 20 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

goldenVoteId :: Property
goldenVoteId = goldenTestBi exampleVoteId "test/golden/bi/update/VoteId"

ts_roundTripVoteId :: TSProperty
ts_roundTripVoteId = eachOfTS 20 (feedPM genVoteId) roundTripsBiBuildable

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
  [ H.checkSequential $$discoverGolden
  , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
  ]
