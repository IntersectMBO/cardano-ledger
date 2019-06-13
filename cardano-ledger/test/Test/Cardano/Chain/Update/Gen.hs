module Test.Cardano.Chain.Update.Gen
  ( genCanonicalProtocolParameters
  , genApplicationName
  , genProtocolVersion
  , genProtocolParameters
  , genProtocolParametersUpdate
  , genSoftforkRule
  , genSoftwareVersion
  , genSystemTag
  , genInstallerHash
  , genPayload
  , genProof
  , genProposal
  , genProposalBody
  , genUpId
  , genUpsData
  , genVote
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Update
  ( ApplicationName(..)
  , Payload
  , Proof
  , Proposal
  , ProposalBody(..)
  , ProtocolParametersUpdate(..)
  , ProtocolParameters(..)
  , ProtocolVersion(..)
  , SoftforkRule(..)
  , SoftwareVersion(..)
  , SystemTag(..)
  , UpId
  , InstallerHash(..)
  , Vote
  , applicationNameMaxLength
  , mkProposal
  , mkVote
  , payload
  , systemTagMaxLength
  )
import Cardano.Crypto (ProtocolMagicId)

import Test.Cardano.Chain.Common.Gen
  ( genCanonicalTxFeePolicy
  , genLovelacePortion
  , genScriptVersion
  , genTxFeePolicy
  )
import Test.Cardano.Chain.Slotting.Gen
  (genEpochNumber, genSlotNumber)
import Test.Cardano.Crypto.Gen
  ( genAbstractHash
  , genHashRaw
  , genVerificationKey
  , genSigningKey
  , genSignature
  )


genApplicationName :: Gen ApplicationName
genApplicationName =
  ApplicationName
    <$> Gen.text (Range.constant 0 applicationNameMaxLength) Gen.alphaNum

genCanonicalProtocolParameters :: Gen ProtocolParameters
genCanonicalProtocolParameters =
  ProtocolParameters
    <$> genScriptVersion
    <*> genNominalDiffTime
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genSlotNumber
    <*> genSoftforkRule
    <*> genCanonicalTxFeePolicy
    <*> genEpochNumber

genProtocolVersion :: Gen ProtocolVersion
genProtocolVersion =
  ProtocolVersion
    <$> Gen.word16 Range.constantBounded
    <*> Gen.word16 Range.constantBounded
    <*> Gen.word8 Range.constantBounded

genProtocolParameters :: Gen ProtocolParameters
genProtocolParameters =
  ProtocolParameters
    <$> genScriptVersion
    <*> genNominalDiffTime
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genSlotNumber
    <*> genSoftforkRule
    <*> genTxFeePolicy
    <*> genEpochNumber

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate =
  ProtocolParametersUpdate
    <$> Gen.maybe genScriptVersion
    <*> Gen.maybe genNominalDiffTime
    <*> Gen.maybe genNatural
    <*> Gen.maybe genNatural
    <*> Gen.maybe genNatural
    <*> Gen.maybe genNatural
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genSlotNumber
    <*> Gen.maybe genSoftforkRule
    <*> Gen.maybe genTxFeePolicy
    <*> Gen.maybe genEpochNumber

genSoftforkRule :: Gen SoftforkRule
genSoftforkRule =
  SoftforkRule
    <$> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion

genSoftwareVersion :: Gen SoftwareVersion
genSoftwareVersion =
  SoftwareVersion <$> genApplicationName <*> Gen.word32 Range.constantBounded

genSystemTag :: Gen SystemTag
genSystemTag =
  SystemTag <$> Gen.text (Range.constant 0 systemTagMaxLength) Gen.alphaNum

genInstallerHash :: Gen InstallerHash
genInstallerHash = InstallerHash <$> genHashRaw

genPayload :: ProtocolMagicId -> Gen Payload
genPayload pm = payload <$> Gen.maybe (genProposal pm) <*> Gen.list
  (Range.linear 0 10)
  (genVote pm)

genProof :: ProtocolMagicId -> Gen Proof
genProof pm = genAbstractHash (genPayload pm)

genProposal :: ProtocolMagicId -> Gen Proposal
genProposal pm =
  mkProposal
    <$> genProposalBody
    <*> genVerificationKey
    <*> genSignature pm genProposalBody

genProposalBody :: Gen ProposalBody
genProposalBody =
  ProposalBody
    <$> genProtocolVersion
    <*> genProtocolParametersUpdate
    <*> genSoftwareVersion
    <*> genUpsData

genUpId :: ProtocolMagicId -> Gen UpId
genUpId pm = genAbstractHash (genProposal pm)

genUpsData :: Gen (Map SystemTag InstallerHash)
genUpsData =
  Gen.map (Range.linear 0 20) ((,) <$> genSystemTag <*> genInstallerHash)

genVote :: ProtocolMagicId -> Gen Vote
genVote pm = mkVote pm <$> genSigningKey <*> genUpId pm <*> Gen.bool
