{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Ssc.CBOR
  ( tests,
  )
where

import Cardano.Binary (dropBytes)
import Cardano.Chain.Ssc
  ( SscPayload (..),
    SscProof (..),
    dropCommitment,
    dropCommitmentsMap,
    dropInnerSharesMap,
    dropOpeningsMap,
    dropSharesMap,
    dropSignedCommitment,
    dropSscPayload,
    dropSscProof,
    dropVssCertificate,
    dropVssCertificatesMap,
  )
import Cardano.Prelude
import Hedgehog (Group (..), Property)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode,
    roundTripsCBORShow,
  )
import Test.Cardano.Prelude
import Test.Options (concatGroups)

--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

goldenDeprecatedCommitment :: Property
goldenDeprecatedCommitment =
  deprecatedGoldenDecode
    "Commitment"
    dropCommitment
    "test/golden/cbor/ssc/Commitment"

--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

goldenDeprecatedCommitmentsMap :: Property
goldenDeprecatedCommitmentsMap =
  deprecatedGoldenDecode
    "CommitmentsMap"
    dropCommitmentsMap
    "test/golden/cbor/ssc/CommitmentsMap"

--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

goldenDeprecatedInnerSharesMap :: Property
goldenDeprecatedInnerSharesMap =
  deprecatedGoldenDecode
    "InnerSharesMap"
    dropInnerSharesMap
    "test/golden/cbor/ssc/InnerSharesMap"

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

goldenDeprecatedOpening :: Property
goldenDeprecatedOpening =
  deprecatedGoldenDecode "Opening" dropBytes "test/golden/cbor/ssc/Opening"

--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

goldenDeprecatedOpeningsMap :: Property
goldenDeprecatedOpeningsMap =
  deprecatedGoldenDecode
    "OpeningsMap"
    dropOpeningsMap
    "test/golden/cbor/ssc/OpeningsMap"

--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

goldenDeprecatedSignedCommitment :: Property
goldenDeprecatedSignedCommitment =
  deprecatedGoldenDecode
    "SignedCommitment"
    dropSignedCommitment
    "test/golden/cbor/ssc/SignedCommitment"

--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

goldenDeprecatedSharesMap :: Property
goldenDeprecatedSharesMap =
  deprecatedGoldenDecode
    "SharesMap"
    dropSharesMap
    "test/golden/cbor/ssc/SharesMap"

--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

goldenDeprecatedSscPayload_CommitmentsPayload :: Property
goldenDeprecatedSscPayload_CommitmentsPayload =
  deprecatedGoldenDecode
    "SscPayload_CommitmentsPayload"
    dropSscPayload
    "test/golden/cbor/ssc/SscPayload_CommitmentsPayload"

goldenDeprecatedSscPayload_OpeningsPayload :: Property
goldenDeprecatedSscPayload_OpeningsPayload =
  deprecatedGoldenDecode
    "SscPayload_OpeningsPayload"
    dropSscPayload
    "test/golden/cbor/ssc/SscPayload_OpeningsPayload"

goldenDeprecatedSscPayload_SharesPayload :: Property
goldenDeprecatedSscPayload_SharesPayload =
  deprecatedGoldenDecode
    "SscPayload_SharesPayload"
    dropSscPayload
    "test/golden/cbor/ssc/SscPayload_SharesPayload"

goldenDeprecatedSscPayload_CertificatesPayload :: Property
goldenDeprecatedSscPayload_CertificatesPayload =
  deprecatedGoldenDecode
    "SscPayload_CertificatesPayload"
    dropSscPayload
    "test/golden/cbor/ssc/SscPayload_CertificatesPayload"

roundTripSscPayload :: Property
roundTripSscPayload = eachOf 1 (pure SscPayload) roundTripsCBORShow

--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

goldenDeprecatedSscProof_CommitmentsProof :: Property
goldenDeprecatedSscProof_CommitmentsProof =
  deprecatedGoldenDecode
    "SscProof_CommitmentsProof"
    dropSscProof
    "test/golden/cbor/ssc/SscProof_CommitmentsProof"

goldenDeprecatedSscProof_OpeningsProof :: Property
goldenDeprecatedSscProof_OpeningsProof =
  deprecatedGoldenDecode
    "SscProof_OpeningsProof"
    dropSscProof
    "test/golden/cbor/ssc/SscProof_OpeningsProof"

goldenDeprecatedSscProof_SharesProof :: Property
goldenDeprecatedSscProof_SharesProof =
  deprecatedGoldenDecode
    "SscProof_SharesProof"
    dropSscProof
    "test/golden/cbor/ssc/SscProof_SharesProof"

goldenDeprecatedSscProof_CertificatesProof :: Property
goldenDeprecatedSscProof_CertificatesProof =
  deprecatedGoldenDecode
    "SscProof_CertificatesProof"
    dropSscProof
    "test/golden/cbor/ssc/SscProof_CertificatesProof"

roundTripSscProof :: Property
roundTripSscProof = eachOf 1 (pure SscProof) roundTripsCBORShow

--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

goldenDeprecatedVssCertificate :: Property
goldenDeprecatedVssCertificate =
  deprecatedGoldenDecode
    "VssCertificate"
    dropVssCertificate
    "test/golden/cbor/ssc/VssCertificate"

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

goldenDeprecatedVssCertificatesHash :: Property
goldenDeprecatedVssCertificatesHash =
  deprecatedGoldenDecode
    "VssCertiificatesHash"
    dropBytes
    "test/golden/cbor/ssc/VssCertificatesHash"

--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

goldenDeprecatedVssCertificatesMap :: Property
goldenDeprecatedVssCertificatesMap =
  deprecatedGoldenDecode
    "VssCertificatesMap"
    dropVssCertificatesMap
    "test/golden/cbor/ssc/VssCertificatesMap"

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: Group
tests = concatGroups [$$discoverGolden, $$discoverRoundTrip]
