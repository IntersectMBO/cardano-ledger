{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Ssc.Gen
       ( genCommitment
       , genCommitmentsMap
       , genCommitmentSignature
       , genInnerSharesMap
       , genSharesMap
       , genOpening
       , genOpeningsMap
       , genSignedCommitment
       , genVssCertificate
       , genVssCertificatesMap
       ) where

import           Cardano.Prelude

import           Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Binary.Class (asBinary)
import           Cardano.Chain.Ssc (Commitment, CommitmentSignature,
                     CommitmentsMap, InnerSharesMap, Opening, OpeningsMap,
                     SharesMap, SignedCommitment, VssCertificate (..),
                     VssCertificatesMap (..), mkCommitmentsMap,
                     mkVssCertificate, mkVssCertificatesMap)
import           Cardano.Crypto (ProtocolMagic, deterministic)

import           Test.Cardano.Chain.Common.Gen (genStakeholderId)
import           Test.Cardano.Chain.Slotting.Gen (genEpochIndex)
import           Test.Cardano.Chain.Ssc.Example (randCommitmentAndOpening)
import           Test.Cardano.Crypto.Gen (genDecShare, genPublicKey,
                     genSecretKey, genSignature, genVssPublicKey)


genCommitment :: Gen Commitment
genCommitment = fst <$> genCommitmentOpening

genCommitmentOpening :: Gen (Commitment, Opening)
genCommitmentOpening = do
    let numKeys = 128 :: Int
    parties <-
        Gen.integral (Range.constant 4 (fromIntegral numKeys)) :: Gen Integer
    threshold <- Gen.integral (Range.constant 2 (parties - 2)) :: Gen Integer
    vssKeys   <- replicateM numKeys genVssPublicKey
    pure $ deterministic "commitmentOpening" $ randCommitmentAndOpening
        threshold
        (fromList vssKeys)

genCommitmentSignature :: ProtocolMagic -> Gen CommitmentSignature
genCommitmentSignature pm =
    genSignature pm $ (,) <$> genEpochIndex <*> genCommitment

genCommitmentsMap :: ProtocolMagic -> Gen CommitmentsMap
genCommitmentsMap pm = mkCommitmentsMap
    <$> Gen.list range (genSignedCommitment pm)
    where range = Range.linear 0 10

genInnerSharesMap :: Gen InnerSharesMap
genInnerSharesMap = do
    hMS <- Gen.int (Range.linear 0 10)
    stakeholderId <- Gen.list (Range.singleton hMS) genStakeholderId
    nonEmptyDS <- Gen.nonEmpty (Range.singleton hMS) (asBinary <$> genDecShare)
    pure $ Map.fromList $ zip stakeholderId [nonEmptyDS]

genOpening :: Gen Opening
genOpening = snd <$> genCommitmentOpening

genOpeningsMap :: Gen OpeningsMap
genOpeningsMap = do
    hMapSize      <- Gen.int (Range.linear 0 10)
    stakeholderId <- Gen.list (Range.singleton hMapSize) genStakeholderId
    opening       <- Gen.list (Range.singleton hMapSize) genOpening
    pure $ Map.fromList $ zip stakeholderId opening

genSharesMap :: Gen SharesMap
genSharesMap = do
    hMapSize       <- Gen.int (Range.linear 0 10)
    stakeholderId  <- Gen.list (Range.singleton hMapSize) genStakeholderId
    innerSharesMap <- Gen.list (Range.singleton hMapSize) genInnerSharesMap
    pure $ Map.fromList $ zip stakeholderId innerSharesMap

genSignedCommitment :: ProtocolMagic -> Gen SignedCommitment
genSignedCommitment pm =
    (,,) <$> genPublicKey <*> genCommitment <*> genCommitmentSignature pm

genVssCertificate :: ProtocolMagic -> Gen VssCertificate
genVssCertificate pm =
    mkVssCertificate pm
        <$> genSecretKey
        <*> (asBinary <$> genVssPublicKey)
        <*> genEpochIndex

genVssCertificatesMap :: ProtocolMagic -> Gen VssCertificatesMap
genVssCertificatesMap pm =
    mkVssCertificatesMap <$> Gen.list (Range.linear 0 5) (genVssCertificate pm)
