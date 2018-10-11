{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Update.Bi
       ( tests
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import qualified Data.Map.Strict as Map

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Binary.Class (Raw (..))
import           Cardano.Chain.Common (CoinPortion (..))
import           Cardano.Chain.Update (ApplicationName (..), SoftforkRule (..))
import           Cardano.Crypto (Hash, abstractHash)

import           Test.Cardano.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Cardano.Chain.Update.Example (exampleBlockVersion,
                     exampleBlockVersionData, exampleBlockVersionModifier,
                     examplePayload, exampleProof, exampleProposal,
                     exampleProposalBody, exampleSoftwareVersion,
                     exampleSystemTag, exampleUpId, exampleUpdateData,
                     exampleVote, exampleVoteId)
import           Test.Cardano.Chain.Update.Gen (genApplicationName,
                     genBlockVersion, genBlockVersionData,
                     genBlockVersionModifier, genPayload, genProof,
                     genProposal, genProposalBody, genProposals,
                     genSoftforkRule, genSoftwareVersion, genSystemTag,
                     genUpId, genUpdateData, genUpsData, genVote, genVoteId)
import           Test.Cardano.Crypto.Gen (feedPM, genHashRaw)


--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

golden_ApplicationName :: Property
golden_ApplicationName = goldenTestBi aN "test/golden/ApplicationName"
    where aN = ApplicationName "Golden"

roundTripApplicationName :: Property
roundTripApplicationName = eachOf 50 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersion
--------------------------------------------------------------------------------

golden_BlockVersion :: Property
golden_BlockVersion =
    goldenTestBi exampleBlockVersion "test/golden/BlockVersion"

roundTripBlockVersion :: Property
roundTripBlockVersion = eachOf 50 genBlockVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

golden_BlockVersionData :: Property
golden_BlockVersionData = goldenTestBi bVerDat "test/golden/BlockVersionData"
    where bVerDat = exampleBlockVersionData

roundTripBlockVersionData :: Property
roundTripBlockVersionData = eachOf 50 genBlockVersionData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionModifier
--------------------------------------------------------------------------------

golden_BlockVersionModifier :: Property
golden_BlockVersionModifier = goldenTestBi
    bVerMod
    "test/golden/BlockVersionModifier"
    where bVerMod = exampleBlockVersionModifier

roundTripBlockVersionModifier :: Property
roundTripBlockVersionModifier =
    eachOf 50 genBlockVersionModifier roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

golden_BlockHashRaw :: Property
golden_BlockHashRaw = goldenTestBi hRaw "test/golden/HashRaw"
    where hRaw = (abstractHash $ Raw ("9") :: Hash Raw)

roundTripHashRaw :: Property
roundTripHashRaw = eachOf 50 genHashRaw roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

golden_SoftforkRule :: Property
golden_SoftforkRule = goldenTestBi sfR "test/golden/SoftforkRule"
    where sfR = SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99)

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

golden_SoftwareVersion :: Property
golden_SoftwareVersion =
    goldenTestBi exampleSoftwareVersion "test/golden/SoftwareVersion"

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion = eachOf 10 genSoftwareVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

golden_SystemTag :: Property
golden_SystemTag = goldenTestBi exampleSystemTag "test/golden/SystemTag"

roundTripSystemTag :: Property
roundTripSystemTag = eachOf 10 genSystemTag roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

golden_UpdateData :: Property
golden_UpdateData = goldenTestBi exampleUpdateData "test/golden/update/UpdateData"

roundTripUpdateData :: Property
roundTripUpdateData = eachOf 20 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

golden_UpdatePayload :: Property
golden_UpdatePayload =
    goldenTestBi examplePayload "test/golden/update/Payload"

roundTripUpdatePayload :: Property
roundTripUpdatePayload =
    eachOf 20 (feedPM genPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

golden_UpdateProof :: Property
golden_UpdateProof = goldenTestBi exampleProof "test/golden/update/Proof"

roundTripUpdateProof :: Property
roundTripUpdateProof = eachOf 20 (feedPM genProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

golden_UpdateProposal :: Property
golden_UpdateProposal =
    goldenTestBi exampleProposal "test/golden/update/Proposal"

roundTripUpdateProposal :: Property
roundTripUpdateProposal =
    eachOf 20 (feedPM genProposal) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

golden_UpdateProposals :: Property
golden_UpdateProposals = goldenTestBi ups "test/golden/update/Proposals"
    where
    -- Need to revisit this.
          ups = Map.fromList [(exampleUpId, exampleProposal)]

roundTripUpdateProposals :: Property
roundTripUpdateProposals =
    eachOf 20 (feedPM genProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- ProposalBody
--------------------------------------------------------------------------------

golden_ProposalBody :: Property
golden_ProposalBody =
    goldenTestBi exampleProposalBody "test/golden/update/ProposalBody"

roundTripProposalBody :: Property
roundTripProposalBody =
    eachOf 20 genProposalBody roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

golden_UpdateVote :: Property
golden_UpdateVote = goldenTestBi exampleVote "test/golden/update/Vote"

roundTripUpdateVote :: Property
roundTripUpdateVote = eachOf 20 (feedPM genVote) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

golden_UpId :: Property
golden_UpId = goldenTestBi exampleUpId "test/golden/update/UpId"

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

golden_VoteId :: Property
golden_VoteId = goldenTestBi exampleVoteId "test/golden/VoteId"

roundTripVoteId :: Property
roundTripVoteId = eachOf 20 (feedPM genVoteId) roundTripsBiBuildable

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
