module Test.Cardano.Chain.Update.Gen
  ( genCanonicalProtocolParameters,
    genApplicationName,
    genError,
    genProtocolVersion,
    genProtocolParameters,
    genProtocolParametersUpdate,
    genSoftforkRule,
    genSoftwareVersion,
    genSystemTag,
    genInstallerHash,
    genPayload,
    genProof,
    genProposal,
    genProposalBody,
    genUpId,
    genUpsData,
    genVote,
  )
where

import Cardano.Chain.Slotting (SlotNumber (..))
import Cardano.Chain.Update
  ( ApplicationName (..),
    ApplicationNameError (..),
    InstallerHash (..),
    Payload,
    Proof,
    Proposal,
    ProposalBody (..),
    ProtocolParameters (..),
    ProtocolParametersUpdate (..),
    ProtocolVersion (..),
    SoftforkRule (..),
    SoftwareVersion (..),
    SoftwareVersionError (..),
    SystemTag (..),
    SystemTagError (..),
    UpId,
    Vote,
    applicationNameMaxLength,
    mkVote,
    payload,
    systemTagMaxLength,
    unsafeProposal,
  )
import qualified Cardano.Chain.Update.Validation.Endorsement as Endorsement
import Cardano.Chain.Update.Validation.Interface (Error (..))
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import qualified Cardano.Chain.Update.Validation.Voting as Voting
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Prelude
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Chain.Common.Gen
  ( genCanonicalTxFeePolicy,
    genKeyHash,
    genLovelacePortion,
    genScriptVersion,
    genTxFeePolicy,
  )
import Test.Cardano.Chain.Slotting.Gen
  ( genEpochNumber,
    genSlotNumber,
  )
import Test.Cardano.Crypto.Gen
  ( genAbstractHash,
    genHashRaw,
    genSignature,
    genSigningKey,
    genVerificationKey,
  )
import Test.Cardano.Prelude

genApplicationName :: Gen ApplicationName
genApplicationName =
  ApplicationName
    <$> Gen.text (Range.constant 0 applicationNameMaxLength) Gen.alphaNum

genCanonicalProtocolParameters :: Gen ProtocolParameters
genCanonicalProtocolParameters =
  ProtocolParameters
    <$> genScriptVersion
    <*> genNatural
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
    <*> genNatural
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
    <*> Gen.maybe genNatural
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
genPayload pm =
  payload <$> Gen.maybe (genProposal pm)
    <*> Gen.list
      (Range.linear 0 10)
      (genVote pm)

genProof :: ProtocolMagicId -> Gen Proof
genProof pm = genAbstractHash (genPayload pm)

genProposal :: ProtocolMagicId -> Gen Proposal
genProposal pm =
  unsafeProposal
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

genError :: ProtocolMagicId -> Gen Error
genError pm =
  Gen.choice
    [ Registration <$> genRegistrationError,
      Voting <$> genVotingError pm,
      Endorsement <$> genEndorsementError,
      NumberOfGenesisKeysTooLarge <$> genRegistrationTooLarge
    ]

genRegistrationError :: Gen Registration.Error
genRegistrationError =
  Gen.choice
    [ Registration.DuplicateProtocolVersion <$> genProtocolVersion,
      Registration.DuplicateSoftwareVersion <$> genSoftwareVersion,
      Registration.InvalidProposer <$> genKeyHash,
      Registration.InvalidProtocolVersion
        <$> genProtocolVersion
        <*> (Registration.Adopted <$> genProtocolVersion),
      Registration.InvalidScriptVersion <$> genWord16 <*> genWord16,
      pure Registration.InvalidSignature,
      Registration.InvalidSoftwareVersion
        <$> ( Gen.map (Range.linear 1 20) $ do
                name <- genApplicationName
                version <- genWord32
                slotNo <- SlotNumber <$> Gen.word64 Range.constantBounded
                meta <-
                  Gen.map (Range.linear 1 10) $
                    (,) <$> genSystemTag <*> genInstallerHash
                pure (name, (Registration.ApplicationVersion version slotNo meta))
            )
          <*> genSoftwareVersion,
      Registration.MaxBlockSizeTooLarge <$> (Registration.TooLarge <$> genNatural <*> genNatural),
      Registration.MaxTxSizeTooLarge <$> (Registration.TooLarge <$> genNatural <*> genNatural),
      pure Registration.ProposalAttributesUnknown,
      Registration.ProposalTooLarge <$> (Registration.TooLarge <$> genNatural <*> genNatural),
      (Registration.SoftwareVersionError . SoftwareVersionApplicationNameError)
        <$> Gen.choice
          [ ApplicationNameTooLong <$> Gen.text (Range.linear 0 20) Gen.alphaNum,
            ApplicationNameNotAscii <$> Gen.text (Range.linear 0 20) Gen.alphaNum
          ],
      Registration.SystemTagError
        <$> Gen.choice
          [ SystemTagNotAscii <$> Gen.text (Range.linear 0 20) Gen.alphaNum,
            SystemTagTooLong <$> Gen.text (Range.linear 0 20) Gen.alphaNum
          ]
    ]

genVotingError :: ProtocolMagicId -> Gen Voting.Error
genVotingError pm =
  Gen.choice
    [ pure Voting.VotingInvalidSignature,
      Voting.VotingProposalNotRegistered <$> genUpId pm,
      Voting.VotingVoterNotDelegate <$> genKeyHash
    ]

genEndorsementError :: Gen Endorsement.Error
genEndorsementError =
  Endorsement.MultipleProposalsForProtocolVersion
    <$> genProtocolVersion

genRegistrationTooLarge :: Gen (Registration.TooLarge Int)
genRegistrationTooLarge =
  Registration.TooLarge
    <$> Gen.int Range.constantBounded
    <*> Gen.int Range.constantBounded
