{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Update.Bi
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Map.Strict as Map

import Hedgehog (Property)
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


--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

goldenApplicationName :: Property
goldenApplicationName = goldenTestBi aN "test/golden/bi/update/ApplicationName"
  where aN = ApplicationName "Golden"

roundTripApplicationName :: Property
roundTripApplicationName = eachOf 50 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProtocolVersion
--------------------------------------------------------------------------------

goldenProtocolVersion :: Property
goldenProtocolVersion =
  goldenTestBi exampleProtocolVersion "test/golden/bi/update/ProtocolVersion"

roundTripProtocolVersion :: Property
roundTripProtocolVersion = eachOf 50 genProtocolVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProtocolParameters
--------------------------------------------------------------------------------

goldenProtocolParameters :: Property
goldenProtocolParameters = goldenTestBi
  bVerDat
  "test/golden/bi/update/ProtocolParameters"
  where bVerDat = exampleProtocolParameters

roundTripProtocolParameters :: Property
roundTripProtocolParameters =
  eachOf 50 genProtocolParameters roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProtocolParametersUpdate
--------------------------------------------------------------------------------

goldenProtocolParametersUpdate :: Property
goldenProtocolParametersUpdate = goldenTestBi
  ppu
  "test/golden/bi/update/ProtocolParametersUpdate"
  where ppu = exampleProtocolParametersUpdate

roundTripProtocolParametersUpdate :: Property
roundTripProtocolParametersUpdate =
  eachOf 50 genProtocolParametersUpdate roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

goldenBlockHashRaw :: Property
goldenBlockHashRaw = goldenTestBi hRaw "test/golden/bi/update/HashRaw"
  where hRaw = (abstractHash $ Raw ("9") :: Hash Raw)

roundTripHashRaw :: Property
roundTripHashRaw = eachOf 50 genHashRaw roundTripsBiBuildable

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

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

goldenSoftwareVersion :: Property
goldenSoftwareVersion =
  goldenTestBi exampleSoftwareVersion "test/golden/bi/update/SoftwareVersion"

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion = eachOf 10 genSoftwareVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

goldenSystemTag :: Property
goldenSystemTag =
  goldenTestBi exampleSystemTag "test/golden/bi/update/SystemTag"

roundTripSystemTag :: Property
roundTripSystemTag = eachOf 10 genSystemTag roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

goldenUpdateData :: Property
goldenUpdateData =
  goldenTestBi exampleUpdateData "test/golden/bi/update/UpdateData"

roundTripUpdateData :: Property
roundTripUpdateData = eachOf 20 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

goldenUpdatePayload :: Property
goldenUpdatePayload =
  goldenTestBi examplePayload "test/golden/bi/update/Payload"

roundTripUpdatePayload :: Property
roundTripUpdatePayload = eachOf 20 (feedPM genPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

goldenUpdateProof :: Property
goldenUpdateProof = goldenTestBi exampleProof "test/golden/bi/update/Proof"

roundTripUpdateProof :: Property
roundTripUpdateProof = eachOf 20 (feedPM genProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

goldenUpdateProposal :: Property
goldenUpdateProposal =
  goldenTestBi exampleProposal "test/golden/bi/update/Proposal"

roundTripUpdateProposal :: Property
roundTripUpdateProposal = eachOf 20 (feedPM genProposal) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

goldenUpdateProposals :: Property
goldenUpdateProposals = goldenTestBi ups "test/golden/bi/update/Proposals"
  where
    -- Need to revisit this.
        ups = Map.fromList [(exampleUpId, exampleProposal)]

roundTripUpdateProposals :: Property
roundTripUpdateProposals = eachOf 20 (feedPM genProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

goldenProposalBody :: Property
goldenProposalBody =
  goldenTestBi exampleProposalBody "test/golden/bi/update/ProposalBody"

roundTripProposalBody :: Property
roundTripProposalBody = eachOf 20 genProposalBody roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

goldenUpdateVote :: Property
goldenUpdateVote = goldenTestBi exampleVote "test/golden/bi/update/Vote"

roundTripUpdateVote :: Property
roundTripUpdateVote = eachOf 20 (feedPM genVote) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

goldenUpId :: Property
goldenUpId = goldenTestBi exampleUpId "test/golden/bi/update/UpId"

roundTripUpId :: Property
roundTripUpId = eachOf 20 (feedPM genUpId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData NB: UpsData is not a type it is a record accessor of `ProposalBody`
--------------------------------------------------------------------------------

roundTripUpsData :: Property
roundTripUpsData = eachOf 20 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

goldenVoteId :: Property
goldenVoteId = goldenTestBi exampleVoteId "test/golden/bi/update/VoteId"

roundTripVoteId :: Property
roundTripVoteId = eachOf 20 (feedPM genVoteId) roundTripsBiBuildable

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
