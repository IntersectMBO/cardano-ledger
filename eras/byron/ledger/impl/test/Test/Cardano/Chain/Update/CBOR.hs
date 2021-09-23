{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Update.CBOR
  ( tests,
  )
where

import Cardano.Binary (Raw (..))
import Cardano.Chain.Common (rationalToLovelacePortion)
import Cardano.Chain.Update (ApplicationName (..), SoftforkRule (..))
import Cardano.Crypto (Hash, abstractHash)
import Cardano.Prelude
import Hedgehog (Property)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    roundTripsCBORBuildable,
    roundTripsCBORShow,
  )
import Test.Cardano.Chain.Update.Example
  ( exampleInstallerHash,
    examplePayload,
    exampleProof,
    exampleProposal,
    exampleProposalBody,
    exampleProtocolParameters,
    exampleProtocolParametersUpdate,
    exampleProtocolVersion,
    exampleSoftwareVersion,
    exampleSystemTag,
    exampleUpId,
    exampleVote,
  )
import Test.Cardano.Chain.Update.Gen
  ( genApplicationName,
    genInstallerHash,
    genPayload,
    genProof,
    genProposal,
    genProposalBody,
    genProtocolParameters,
    genProtocolParametersUpdate,
    genProtocolVersion,
    genSoftforkRule,
    genSoftwareVersion,
    genSystemTag,
    genUpId,
    genUpsData,
    genVote,
  )
import Test.Cardano.Crypto.Gen (feedPM, genHashRaw)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

goldenApplicationName :: Property
goldenApplicationName =
  goldenTestCBOR
    aN
    "test/golden/cbor/update/ApplicationName"
  where
    aN = ApplicationName "Golden"

ts_roundTripApplicationName :: TSProperty
ts_roundTripApplicationName =
  eachOfTS 50 genApplicationName roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- ProtocolVersion
--------------------------------------------------------------------------------

goldenProtocolVersion :: Property
goldenProtocolVersion =
  goldenTestCBOR
    exampleProtocolVersion
    "test/golden/cbor/update/ProtocolVersion"

ts_roundTripProtocolVersion :: TSProperty
ts_roundTripProtocolVersion =
  eachOfTS 50 genProtocolVersion roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- ProtocolParameters
--------------------------------------------------------------------------------

goldenProtocolParameters :: Property
goldenProtocolParameters =
  goldenTestCBOR
    bVerDat
    "test/golden/cbor/update/ProtocolParameters"
  where
    bVerDat = exampleProtocolParameters

ts_roundTripProtocolParameters :: TSProperty
ts_roundTripProtocolParameters =
  eachOfTS 50 genProtocolParameters roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- ProtocolParametersUpdate
--------------------------------------------------------------------------------

goldenProtocolParametersUpdate :: Property
goldenProtocolParametersUpdate =
  goldenTestCBOR
    ppu
    "test/golden/cbor/update/ProtocolParametersUpdate"
  where
    ppu = exampleProtocolParametersUpdate

ts_roundTripProtocolParametersUpdate :: TSProperty
ts_roundTripProtocolParametersUpdate =
  eachOfTS 50 genProtocolParametersUpdate roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

goldenBlockHashRaw :: Property
goldenBlockHashRaw = goldenTestCBOR hRaw "test/golden/cbor/update/HashRaw"
  where
    hRaw = (abstractHash $ Raw ("9") :: Hash Raw)

ts_roundTripHashRaw :: TSProperty
ts_roundTripHashRaw = eachOfTS 50 genHashRaw roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

goldenSoftforkRule :: Property
goldenSoftforkRule = goldenTestCBOR sfR "test/golden/cbor/update/SoftforkRule"
  where
    sfR =
      SoftforkRule
        (rationalToLovelacePortion 99e-15)
        (rationalToLovelacePortion 99e-15)
        (rationalToLovelacePortion 99e-15)

ts_roundTripSoftforkRule :: TSProperty
ts_roundTripSoftforkRule = eachOfTS 10 genSoftforkRule roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

goldenSoftwareVersion :: Property
goldenSoftwareVersion =
  goldenTestCBOR
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
-- InstallerHash
--------------------------------------------------------------------------------

goldenInstallerHash :: Property
goldenInstallerHash =
  goldenTestCBOR exampleInstallerHash "test/golden/cbor/update/InstallerHash"

ts_roundTripInstallerHash :: TSProperty
ts_roundTripInstallerHash = eachOfTS 20 genInstallerHash roundTripsCBORBuildable

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
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
