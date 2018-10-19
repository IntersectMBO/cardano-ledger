{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Ssc.Bi
       ( tests
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Binary.Class (dropBytes)
import           Cardano.Chain.Ssc (SscPayload (..), SscProof (..),
                     dropCommitment, dropCommitmentsMap, dropInnerSharesMap,
                     dropOpeningsMap, dropSharesMap, dropSignedCommitment,
                     dropSscPayload, dropSscProof, dropVssCertificate,
                     dropVssCertificatesMap)

import           Test.Cardano.Binary.Helpers.GoldenRoundTrip
                     (legacyGoldenDecode, roundTripsBiShow)


--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

golden_legacy_Commitment :: Property
golden_legacy_Commitment =
  legacyGoldenDecode "Commitment" dropCommitment "test/golden/Commitment"


--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

golden_legacy_CommitmentsMap :: Property
golden_legacy_CommitmentsMap = legacyGoldenDecode
  "CommitmentsMap"
  dropCommitmentsMap
  "test/golden/CommitmentsMap"


--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

golden_legacy_InnerSharesMap :: Property
golden_legacy_InnerSharesMap = legacyGoldenDecode
  "InnerSharesMap"
  dropInnerSharesMap
  "test/golden/InnerSharesMap"


--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

golden_legacy_Opening :: Property
golden_legacy_Opening =
  legacyGoldenDecode "Opening" dropBytes "test/golden/Opening"


--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

golden_legacy_OpeningsMap :: Property
golden_legacy_OpeningsMap = legacyGoldenDecode
  "OpeningsMap"
  dropOpeningsMap
  "test/golden/OpeningsMap"


--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

golden_legacy_SignedCommitment :: Property
golden_legacy_SignedCommitment = legacyGoldenDecode
  "SignedCommitment"
  dropSignedCommitment
  "test/golden/SignedCommitment"


--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

golden_legacy_SharesMap :: Property
golden_legacy_SharesMap = legacyGoldenDecode
  "SharesMap"
  dropSharesMap
  "test/golden/SharesMap"


--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

golden_legacy_SscPayload_CommitmentsPayload :: Property
golden_legacy_SscPayload_CommitmentsPayload = legacyGoldenDecode
  "SscPayload_CommitmentsPayload"
  dropSscPayload
  "test/golden/SscPayload_CommitmentsPayload"

golden_legacy_SscPayload_OpeningsPayload :: Property
golden_legacy_SscPayload_OpeningsPayload = legacyGoldenDecode
  "SscPayload_OpeningsPayload"
  dropSscPayload
  "test/golden/SscPayload_OpeningsPayload"

golden_legacy_SscPayload_SharesPayload :: Property
golden_legacy_SscPayload_SharesPayload = legacyGoldenDecode
  "SscPayload_SharesPayload"
  dropSscPayload
  "test/golden/SscPayload_SharesPayload"

golden_legacy_SscPayload_CertificatesPayload :: Property
golden_legacy_SscPayload_CertificatesPayload = legacyGoldenDecode
  "SscPayload_CertificatesPayload"
  dropSscPayload
  "test/golden/SscPayload_CertificatesPayload"

roundTripSscPayload :: Property
roundTripSscPayload = eachOf 1 (pure SscPayload) roundTripsBiShow


--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

golden_legacy_SscProof_CommitmentsProof :: Property
golden_legacy_SscProof_CommitmentsProof = legacyGoldenDecode
  "SscProof_CommitmentsProof"
  dropSscProof
  "test/golden/SscProof_CommitmentsProof"

golden_legacy_SscProof_OpeningsProof :: Property
golden_legacy_SscProof_OpeningsProof = legacyGoldenDecode
  "SscProof_OpeningsProof"
  dropSscProof
  "test/golden/SscProof_OpeningsProof"

golden_legacy_SscProof_SharesProof :: Property
golden_legacy_SscProof_SharesProof = legacyGoldenDecode
  "SscProof_SharesProof"
  dropSscProof
  "test/golden/SscProof_SharesProof"

golden_legacy_SscProof_CertificatesProof :: Property
golden_legacy_SscProof_CertificatesProof = legacyGoldenDecode
  "SscProof_CertificatesProof"
  dropSscProof
  "test/golden/SscProof_CertificatesProof"

roundTripSscProof :: Property
roundTripSscProof = eachOf 1 (pure SscProof) roundTripsBiShow


--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

golden_legacy_VssCertificate :: Property
golden_legacy_VssCertificate = legacyGoldenDecode
  "VssCertificate"
  dropVssCertificate
  "test/golden/VssCertificate"


--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

golden_legacy_VssCertificatesHash :: Property
golden_legacy_VssCertificatesHash = legacyGoldenDecode
  "VssCertiificatesHash"
  dropBytes
  "test/golden/VssCertificatesHash"


--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

golden_legacy_VssCertificatesMap :: Property
golden_legacy_VssCertificatesMap = legacyGoldenDecode
  "VssCertificatesMap"
  dropVssCertificatesMap
  "test/golden/VssCertificatesMap"


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
  [H.checkSequential $$discoverGolden, H.checkParallel $$discoverRoundTrip]
