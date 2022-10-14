{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Ssc.CBOR
  ( tests,
  )
where

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
import Cardano.Ledger.Binary (dropBytes)
import Cardano.Prelude
import GetDataFileName ((<:<))
import Hedgehog (Group (..), Property)
import Test.Cardano.Ledger.Binary.Vintage.Helpers.GoldenRoundTrip
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
    <:< "golden/cbor/ssc/Commitment"

--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

goldenDeprecatedCommitmentsMap :: Property
goldenDeprecatedCommitmentsMap =
  deprecatedGoldenDecode
    "CommitmentsMap"
    dropCommitmentsMap
    <:< "golden/cbor/ssc/CommitmentsMap"

--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

goldenDeprecatedInnerSharesMap :: Property
goldenDeprecatedInnerSharesMap =
  deprecatedGoldenDecode
    "InnerSharesMap"
    dropInnerSharesMap
    <:< "golden/cbor/ssc/InnerSharesMap"

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

goldenDeprecatedOpening :: Property
goldenDeprecatedOpening =
  deprecatedGoldenDecode "Opening" dropBytes <:< "golden/cbor/ssc/Opening"

--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

goldenDeprecatedOpeningsMap :: Property
goldenDeprecatedOpeningsMap =
  deprecatedGoldenDecode
    "OpeningsMap"
    dropOpeningsMap
    <:< "golden/cbor/ssc/OpeningsMap"

--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

goldenDeprecatedSignedCommitment :: Property
goldenDeprecatedSignedCommitment =
  deprecatedGoldenDecode
    "SignedCommitment"
    dropSignedCommitment
    <:< "golden/cbor/ssc/SignedCommitment"

--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

goldenDeprecatedSharesMap :: Property
goldenDeprecatedSharesMap =
  deprecatedGoldenDecode
    "SharesMap"
    dropSharesMap
    <:< "golden/cbor/ssc/SharesMap"

--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

goldenDeprecatedSscPayload_CommitmentsPayload :: Property
goldenDeprecatedSscPayload_CommitmentsPayload =
  deprecatedGoldenDecode
    "SscPayload_CommitmentsPayload"
    dropSscPayload
    <:< "golden/cbor/ssc/SscPayload_CommitmentsPayload"

goldenDeprecatedSscPayload_OpeningsPayload :: Property
goldenDeprecatedSscPayload_OpeningsPayload =
  deprecatedGoldenDecode
    "SscPayload_OpeningsPayload"
    dropSscPayload
    <:< "golden/cbor/ssc/SscPayload_OpeningsPayload"

goldenDeprecatedSscPayload_SharesPayload :: Property
goldenDeprecatedSscPayload_SharesPayload =
  deprecatedGoldenDecode
    "SscPayload_SharesPayload"
    dropSscPayload
    <:< "golden/cbor/ssc/SscPayload_SharesPayload"

goldenDeprecatedSscPayload_CertificatesPayload :: Property
goldenDeprecatedSscPayload_CertificatesPayload =
  deprecatedGoldenDecode
    "SscPayload_CertificatesPayload"
    dropSscPayload
    <:< "golden/cbor/ssc/SscPayload_CertificatesPayload"

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
    <:< "golden/cbor/ssc/SscProof_CommitmentsProof"

goldenDeprecatedSscProof_OpeningsProof :: Property
goldenDeprecatedSscProof_OpeningsProof =
  deprecatedGoldenDecode
    "SscProof_OpeningsProof"
    dropSscProof
    <:< "golden/cbor/ssc/SscProof_OpeningsProof"

goldenDeprecatedSscProof_SharesProof :: Property
goldenDeprecatedSscProof_SharesProof =
  deprecatedGoldenDecode
    "SscProof_SharesProof"
    dropSscProof
    <:< "golden/cbor/ssc/SscProof_SharesProof"

goldenDeprecatedSscProof_CertificatesProof :: Property
goldenDeprecatedSscProof_CertificatesProof =
  deprecatedGoldenDecode
    "SscProof_CertificatesProof"
    dropSscProof
    <:< "golden/cbor/ssc/SscProof_CertificatesProof"

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
    <:< "golden/cbor/ssc/VssCertificate"

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

goldenDeprecatedVssCertificatesHash :: Property
goldenDeprecatedVssCertificatesHash =
  deprecatedGoldenDecode
    "VssCertiificatesHash"
    dropBytes
    <:< "golden/cbor/ssc/VssCertificatesHash"

--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

goldenDeprecatedVssCertificatesMap :: Property
goldenDeprecatedVssCertificatesMap =
  deprecatedGoldenDecode
    "VssCertificatesMap"
    dropVssCertificatesMap
    <:< "golden/cbor/ssc/VssCertificatesMap"

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: Group
tests = concatGroups [$$discoverGolden, $$discoverRoundTrip]
