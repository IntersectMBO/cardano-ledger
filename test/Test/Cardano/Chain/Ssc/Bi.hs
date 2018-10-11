{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Ssc.Bi
       ( tests
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import qualified Data.Map.Strict as Map
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Binary.Class (dropBytes)
import           Cardano.Chain.Ssc (SscPayload (..), SscProof (..),
                     dropCommitment, dropCommitmentsMap, dropInnerSharesMap,
                     dropOpeningsMap, dropSharesMap, dropSignedCommitment,
                     dropSscPayload, dropSscProof, dropVssCertificate,
                     dropVssCertificatesMap)
import           Cardano.Crypto (hash)

import           Test.Cardano.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     legacyGoldenDecode, roundTripsBiBuildable,
                     roundTripsBiShow)
import           Test.Cardano.Chain.Common.Example (exampleStakeholderId)
import           Test.Cardano.Chain.Ssc.Example (exampleCommitment,
                     exampleCommitmentSignature, exampleCommitmentsMap,
                     exampleInnerSharesMap, exampleOpening, exampleOpeningsMap,
                     exampleSignedCommitment, exampleSscPayload,
                     exampleSscProof, exampleVssCertificate,
                     exampleVssCertificatesHash, exampleVssCertificatesMap)
import           Test.Cardano.Chain.Ssc.Gen (genCommitment,
                     genCommitmentSignature, genCommitmentsMap,
                     genInnerSharesMap, genOpening, genOpeningsMap,
                     genSharesMap, genSignedCommitment, genSscPayload,
                     genSscProof, genVssCertificate, genVssCertificatesHash,
                     genVssCertificatesMap)
import           Test.Cardano.Crypto.Gen (feedPM)


--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

golden_Commitment :: Property
golden_Commitment = goldenTestBi exampleCommitment "test/golden/Commitment"

golden_legacy_Commitment :: Property
golden_legacy_Commitment =
  legacyGoldenDecode "Commitment" dropCommitment "test/golden/Commitment"

roundTripCommitment :: Property
roundTripCommitment = eachOf 10 genCommitment roundTripsBiShow


--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

golden_CommitmentsMap :: Property
golden_CommitmentsMap =
  goldenTestBi exampleCommitmentsMap "test/golden/CommitmentsMap"

golden_legacy_CommitmentsMap :: Property
golden_legacy_CommitmentsMap = legacyGoldenDecode
  "CommitmentsMap"
  dropCommitmentsMap
  "test/golden/CommitmentsMap"

roundTripCommitmentsMap :: Property
roundTripCommitmentsMap = eachOf 10 (feedPM genCommitmentsMap) roundTripsBiShow


--------------------------------------------------------------------------------
-- CommitmentsSignature
--------------------------------------------------------------------------------

golden_CommitmentSignature :: Property
golden_CommitmentSignature =
  goldenTestBi exampleCommitmentSignature "test/golden/CommitmentSignature"

roundTripCommitmentSignature :: Property
roundTripCommitmentSignature =
  eachOf 10 (feedPM genCommitmentSignature) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

golden_InnerSharesMap :: Property
golden_InnerSharesMap = goldenTestBi iSm "test/golden/InnerSharesMap"
  where iSm = exampleInnerSharesMap 3 1

golden_legacy_InnerSharesMap :: Property
golden_legacy_InnerSharesMap = legacyGoldenDecode
  "InnerSharesMap"
  dropInnerSharesMap
  "test/golden/InnerSharesMap"

roundTripInnerSharesMap :: Property
roundTripInnerSharesMap = eachOf 50 genInnerSharesMap roundTripsBiShow


--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

golden_Opening :: Property
golden_Opening = goldenTestBi exampleOpening "test/golden/Opening"

golden_legacy_Opening :: Property
golden_legacy_Opening =
  legacyGoldenDecode "Opening" dropBytes "test/golden/Opening"

roundTripOpening :: Property
roundTripOpening = eachOf 10 genOpening roundTripsBiBuildable


--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

golden_OpeningsMap :: Property
golden_OpeningsMap = goldenTestBi exampleOpeningsMap "test/golden/OpeningsMap"

golden_legacy_OpeningsMap :: Property
golden_legacy_OpeningsMap = legacyGoldenDecode
  "OpeningsMap"
  dropOpeningsMap
  "test/golden/OpeningsMap"

roundTripOpeningsMap :: Property
roundTripOpeningsMap = eachOf 10 genOpeningsMap roundTripsBiShow


--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

golden_SignedCommitment :: Property
golden_SignedCommitment =
  goldenTestBi exampleSignedCommitment "test/golden/SignedCommitment"

golden_legacy_SignedCommitment :: Property
golden_legacy_SignedCommitment = legacyGoldenDecode
  "SignedCommitment"
  dropSignedCommitment
  "test/golden/SignedCommitment"

roundTripSignedCommitment :: Property
roundTripSignedCommitment =
  eachOf 10 (feedPM genSignedCommitment) roundTripsBiShow


--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

golden_SharesMap :: Property
golden_SharesMap = goldenTestBi sM "test/golden/SharesMap"
  where sM = Map.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

golden_legacy_SharesMap :: Property
golden_legacy_SharesMap = legacyGoldenDecode
  "SharesMap"
  dropSharesMap
  "test/golden/SharesMap"

roundTripSharesMap :: Property
roundTripSharesMap = eachOf 10 genSharesMap roundTripsBiShow


--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

golden_SscPayload_CommitmentsPayload :: Property
golden_SscPayload_CommitmentsPayload = goldenTestBi
  cP
  "test/golden/SscPayload_CommitmentsPayload"
 where
  cP =
    CommitmentsPayload exampleCommitmentsMap (exampleVssCertificatesMap 10 4)

golden_legacy_SscPayload_CommitmentsPayload :: Property
golden_legacy_SscPayload_CommitmentsPayload = legacyGoldenDecode
  "SscPayload_CommitmentsPayload"
  dropSscPayload
  "test/golden/SscPayload_CommitmentsPayload"

golden_SscPayload_OpeningsPayload :: Property
golden_SscPayload_OpeningsPayload = goldenTestBi
  oP
  "test/golden/SscPayload_OpeningsPayload"
 where
  oP = OpeningsPayload exampleOpeningsMap (exampleVssCertificatesMap 10 4)

golden_legacy_SscPayload_OpeningsPayload :: Property
golden_legacy_SscPayload_OpeningsPayload = legacyGoldenDecode
  "SscPayload_OpeningsPayload"
  dropSscPayload
  "test/golden/SscPayload_OpeningsPayload"

golden_SscPayload_SharesPayload :: Property
golden_SscPayload_SharesPayload =
  goldenTestBi exampleSscPayload "test/golden/SscPayload_SharesPayload"

golden_legacy_SscPayload_SharesPayload :: Property
golden_legacy_SscPayload_SharesPayload = legacyGoldenDecode
  "SscPayload_SharesPayload"
  dropSscPayload
  "test/golden/SscPayload_SharesPayload"

golden_SscPayload_CertificatesPayload :: Property
golden_SscPayload_CertificatesPayload = goldenTestBi
  shP
  "test/golden/SscPayload_CertificatesPayload"
  where shP = CertificatesPayload (exampleVssCertificatesMap 10 4)

golden_legacy_SscPayload_CertificatesPayload :: Property
golden_legacy_SscPayload_CertificatesPayload = legacyGoldenDecode
  "SscPayload_CertificatesPayload"
  dropSscPayload
  "test/golden/SscPayload_CertificatesPayload"

roundTripSscPayload :: Property
roundTripSscPayload = eachOf 10 (feedPM genSscPayload) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

golden_SscProof_CommitmentsProof :: Property
golden_SscProof_CommitmentsProof =
  goldenTestBi exampleSscProof "test/golden/SscProof_CommitmentsProof"

golden_legacy_SscProof_CommitmentsProof :: Property
golden_legacy_SscProof_CommitmentsProof = legacyGoldenDecode
  "SscProof_CommitmentsProof"
  dropSscProof
  "test/golden/SscProof_CommitmentsProof"

golden_SscProof_OpeningsProof :: Property
golden_SscProof_OpeningsProof = goldenTestBi
  oP
  "test/golden/SscProof_OpeningsProof"
 where
  oP =
    OpeningsProof (hash exampleOpeningsMap) (exampleVssCertificatesHash 10 4)

golden_legacy_SscProof_OpeningsProof :: Property
golden_legacy_SscProof_OpeningsProof = legacyGoldenDecode
  "SscProof_OpeningsProof"
  dropSscProof
  "test/golden/SscProof_OpeningsProof"

golden_SscProof_SharesProof :: Property
golden_SscProof_SharesProof = goldenTestBi
  sP
  "test/golden/SscProof_SharesProof"
 where
  sP = SharesProof (hash exampleSharesMap) (exampleVssCertificatesHash 10 4)
  exampleSharesMap =
    Map.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

golden_legacy_SscProof_SharesProof :: Property
golden_legacy_SscProof_SharesProof = legacyGoldenDecode
  "SscProof_SharesProof"
  dropSscProof
  "test/golden/SscProof_SharesProof"

golden_SscProof_CertificatesProof :: Property
golden_SscProof_CertificatesProof = goldenTestBi
  shP
  "test/golden/SscProof_CertificatesProof"
  where shP = CertificatesProof (exampleVssCertificatesHash 10 4)

golden_legacy_SscProof_CertificatesProof :: Property
golden_legacy_SscProof_CertificatesProof = legacyGoldenDecode
  "SscProof_CertificatesProof"
  dropSscProof
  "test/golden/SscProof_CertificatesProof"

roundTripSscProof :: Property
roundTripSscProof = eachOf 10 (feedPM genSscProof) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

golden_VssCertificate :: Property
golden_VssCertificate =
  goldenTestBi exampleVssCertificate "test/golden/VssCertificate"

golden_legacy_VssCertificate :: Property
golden_legacy_VssCertificate = legacyGoldenDecode
  "VssCertificate"
  dropVssCertificate
  "test/golden/VssCertificate"

roundTripVssCertificate :: Property
roundTripVssCertificate =
  eachOf 10 (feedPM genVssCertificate) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

golden_VssCertificatesHash :: Property
golden_VssCertificatesHash = goldenTestBi
  (exampleVssCertificatesHash 10 4)
  "test/golden/VssCertificatesHash"

roundTripVssCertificatesHash :: Property
roundTripVssCertificatesHash =
  eachOf 10 (feedPM genVssCertificatesHash) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

golden_VssCertificatesMap :: Property
golden_VssCertificatesMap =
  goldenTestBi (exampleVssCertificatesMap 10 4) "test/golden/VssCertificatesMap"

golden_legacy_VssCertificatesMap :: Property
golden_legacy_VssCertificatesMap = legacyGoldenDecode
  "VssCertificatesMap"
  dropVssCertificatesMap
  "test/golden/VssCertificatesMap"

roundTripVssCertificatesMap :: Property
roundTripVssCertificatesMap =
  eachOf 10 (feedPM genVssCertificatesMap) roundTripsBiShow


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
