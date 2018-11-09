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
import Cardano.Chain.Common (CoinPortion(..))
import Cardano.Chain.Update (ApplicationName(..), SoftforkRule(..))
import Cardano.Crypto (Hash, abstractHash)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestBi, roundTripsBiBuildable, roundTripsBiShow)
import Test.Cardano.Chain.Update.Example
  ( exampleBlockVersion
  , exampleBlockVersionData
  , exampleBlockVersionModifier
  , examplePayload
  , exampleProof
  , exampleProposal
  , exampleProposalBody
  , exampleSoftwareVersion
  , exampleSystemTag
  , exampleUpId
  , exampleUpdateData
  , exampleVote
  , exampleVoteId
  )
import Test.Cardano.Chain.Update.Gen
  ( genApplicationName
  , genBlockVersion
  , genBlockVersionData
  , genBlockVersionModifier
  , genPayload
  , genProof
  , genProposal
  , genProposalBody
  , genProposals
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
-- BlockVersion
--------------------------------------------------------------------------------

goldenBlockVersion :: Property
goldenBlockVersion =
  goldenTestBi exampleBlockVersion "test/golden/bi/update/BlockVersion"

roundTripBlockVersion :: Property
roundTripBlockVersion = eachOf 50 genBlockVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

goldenBlockVersionData :: Property
goldenBlockVersionData = goldenTestBi
  bVerDat
  "test/golden/bi/update/BlockVersionData"
  where bVerDat = exampleBlockVersionData

roundTripBlockVersionData :: Property
roundTripBlockVersionData = eachOf 50 genBlockVersionData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionModifier
--------------------------------------------------------------------------------

goldenBlockVersionModifier :: Property
goldenBlockVersionModifier = goldenTestBi
  bVerMod
  "test/golden/bi/update/BlockVersionModifier"
  where bVerMod = exampleBlockVersionModifier

roundTripBlockVersionModifier :: Property
roundTripBlockVersionModifier =
  eachOf 50 genBlockVersionModifier roundTripsBiBuildable

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
  where sfR = SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99)

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
