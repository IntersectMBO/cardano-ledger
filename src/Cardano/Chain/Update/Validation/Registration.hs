{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Validation rules for registering updates
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Registration
  ( Error
  , Environment (..)
  , State (..)
  , ProtocolUpdateProposals
  , registerProposal
  , TooLarge (..)
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

import Cardano.Binary (Annotated(..))
import Cardano.Chain.Common
  (Attributes(..), StakeholderId, UnparsedFields(..), mkStakeholderId)
import Cardano.Chain.Slotting (FlatSlotId)
import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters
  , ppMaxBlockSize
  , ppMaxProposalSize
  , ppScriptVersion
  , ppMaxTxSize
  )
import qualified Cardano.Chain.Update.ProtocolParametersUpdate as PPU
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion(ProtocolVersion))
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion(SoftwareVersion)
  )
import Cardano.Chain.Update.Vote
  ( AProposal(AProposal)
  , ProposalBody(..)
  , UpId
  , pbProtocolParametersUpdate
  , pbSoftwareVersion
  , proposalAnnotation
  , proposalAnnotation
  , proposalBody
  , recoverProposalSignedBytes
  , recoverUpId
  , pbProtocolVersion
  )
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignUSProposal)
  , verifySignatureDecoded
  )


data Environment = Environment
  { protocolMagic             :: ProtocolMagicId
  , adoptedProtocolVersion    :: !ProtocolVersion
  , adoptedProtocolParameters :: !ProtocolParameters
  , appVersions               :: !(Map ApplicationName (NumSoftwareVersion, FlatSlotId))
  , delegationMap             :: !(Map StakeholderId StakeholderId)
  }

-- | State keeps track of registered protocol and software update
--   proposals
data State = State
  { rsProtocolUpdateProposals :: !ProtocolUpdateProposals
  , rsSoftwareUpdateProposals :: !SoftwareUpdateProposals
  }

type ProtocolUpdateProposals = Map UpId (ProtocolVersion, ProtocolParameters)
type SoftwareUpdateProposals = Map UpId SoftwareVersion

type ApplicationVersions = Map ApplicationName (NumSoftwareVersion, FlatSlotId)

-- | Error captures the ways in which registration could fail
data Error
  = RegistrationDuplicateProtocolVersion ProtocolVersion
  | RegistrationDuplicateSoftwareVersion SoftwareVersion
  | RegistrationInvalidProposer StakeholderId
  | RegistrationInvalidProtocolVersion ProtocolVersion
  | RegistrationInvalidScriptVersion Word16 Word16
  | RegistrationInvalidSignature
  | RegistrationInvalidSoftwareVersion SoftwareVersion
  | RegistrationMaxBlockSizeTooLarge (TooLarge Natural)
  | RegistrationMaxTxSizeTooLarge (TooLarge Natural)
  | RegistrationProposalAttributesTooLarge
  | RegistrationProposalEmpty
  | RegistrationProposalTooLarge (TooLarge Natural)
  deriving (Eq, Show)

data TooLarge n = TooLarge
  { tlActual   :: n
  , tlMaxBound :: n
  } deriving (Eq, Show)


-- | Register an update proposal after verifying its signature and validating
--   its contents. This corresponds to the @UPREG@ rules in the spec.
registerProposal
  :: MonadError Error m
  => Environment
  -> State
  -> AProposal ByteString
  -> m State
registerProposal env rs proposal = do
  -- Check that the proposal attributes are empty
  length attributes == 0 `orThrowError` RegistrationProposalAttributesTooLarge

  -- Check that the proposer is delegated to by a genesis key
  not (null $ M.filter (== proposer) delegationMap)
    `orThrowError` RegistrationInvalidProposer proposer

  -- Verify the proposal signature
  verifySignatureDecoded
      protocolMagic
      SignUSProposal
      proposerPK
      (recoverProposalSignedBytes body)
      sig
    `orThrowError` RegistrationInvalidSignature

  -- Check that the proposal is valid
  registerProposalComponents
    adoptedProtocolVersion
    adoptedProtocolParameters
    appVersions
    rs
    proposal
 where
  AProposal body proposerPK sig _ = proposal

  Attributes _ (UnparsedFields attributes) = pbAttributes $ unAnnotated body

  proposer = mkStakeholderId proposerPK

  Environment
    { protocolMagic
    , adoptedProtocolVersion
    , adoptedProtocolParameters
    , appVersions
    , delegationMap
    } = env


-- | Register the individual components of an update proposal
--
--   The proposal may contain a protocol update, a software update, or both.
--   This corresponds to the `UPV` rules in the spec.
registerProposalComponents
  :: MonadError Error m
  => ProtocolVersion
  -> ProtocolParameters
  -> ApplicationVersions
  -> State
  -> AProposal ByteString
  -> m State
registerProposalComponents adoptedPV adoptedPP appVersions rs proposal = do

  (protocolVersionChanged || softwareVersionChanged)
    `orThrowError` RegistrationProposalEmpty

  -- Register protocol update if we have one
  registeredPUPs' <- if protocolVersionChanged
    then registerProtocolUpdate adoptedPV adoptedPP registeredPUPs proposal
    else pure registeredPUPs

  -- Register software update if we have one
  registeredSUPs' <- if softwareVersionChanged
    then registerSoftwareUpdate appVersions registeredSUPs proposal
    else pure registeredSUPs

  pure $ State registeredPUPs' registeredSUPs'
 where
  ProposalBody protocolVersion ppu softwareVersion _ _ =
    proposalBody proposal

  SoftwareVersion appName appVersion = softwareVersion

  softwareVersionChanged =
    maybe True ((/= appVersion) . fst) $ M.lookup appName appVersions

  protocolVersionChanged =
    not $ protocolVersion == adoptedPV && PPU.apply ppu adoptedPP == adoptedPP

  State registeredPUPs registeredSUPs = rs


-- | Validate a protocol update
--
--   We check that:
--
--   1) The protocol update hasn't already been registered
--   2) The protocol version is a valid next version
--   3) The new 'ProtocolParameters' represent a valid update
--
--   This corresponds to the `UPPVV` rule in the spec.
registerProtocolUpdate
  :: MonadError Error m
  => ProtocolVersion
  -> ProtocolParameters
  -> ProtocolUpdateProposals
  -> AProposal ByteString
  -> m ProtocolUpdateProposals
registerProtocolUpdate adoptedPV adoptedPP registeredPUPs proposal = do

  -- Check that this protocol version isn't already registered
  null (M.filter ((== newPV) . fst) registeredPUPs)
    `orThrowError` RegistrationDuplicateProtocolVersion newPV

  -- Check that this protocol version is a valid next version
  pvCanFollow newPV adoptedPV
    `orThrowError` RegistrationInvalidProtocolVersion newPV

  canUpdate adoptedPP newPP proposal

  pure $ M.insert (recoverUpId proposal) (newPV, newPP) registeredPUPs
 where
  body  = proposalBody proposal
  newPV = pbProtocolVersion body
  ppu   = pbProtocolParametersUpdate body
  newPP = PPU.apply ppu adoptedPP

-- | Check that the new 'ProtocolVersion' is a valid next version
--
pvCanFollow :: ProtocolVersion -> ProtocolVersion -> Bool
pvCanFollow newPV adoptedPV = adoptedPV < newPV && isNextVersion
 where
  ProtocolVersion newMajor     newMinor     _ = newPV
  ProtocolVersion adoptedMajor adoptedMinor _ = adoptedPV
  isNextVersion = case newMajor - adoptedMajor of
    0 -> newMinor == adoptedMinor + 1
    1 -> newMinor == 0
    _ -> False


-- | Check that the new 'ProtocolParameters' represent a valid update
--
--   This is where we enforce constraints on how the 'ProtocolParameters'
--   change.
--
canUpdate
  :: MonadError Error m
  => ProtocolParameters
  -> ProtocolParameters
  -> AProposal ByteString
  -> m ()
canUpdate adoptedPP proposedPP proposal = do

  -- Check that the proposal size is less than the maximum
  (proposalSize <= maxProposalSize) `orThrowError` RegistrationProposalTooLarge
    (TooLarge maxProposalSize proposalSize)

  -- Check that the new maximum block size is no more than twice the current one
  (newMaxBlockSize <= 2 * adoptedMaxBlockSize)
    `orThrowError` RegistrationMaxBlockSizeTooLarge
                     (TooLarge adoptedMaxBlockSize newMaxBlockSize)

  -- Check that the new max transaction size is less than the new max block size
  (newMaxTxSize <=  newMaxBlockSize)
    `orThrowError` RegistrationMaxTxSizeTooLarge
                     (TooLarge newMaxBlockSize newMaxTxSize)

  -- Check that the new script version is either the same or incremented
  (0 <= scriptVersionDiff && scriptVersionDiff <= 1)
    `orThrowError` RegistrationInvalidScriptVersion
                     adoptedScriptVersion newScriptVersion
 where
  proposalSize :: Natural
  proposalSize         = fromIntegral . BS.length $ proposalAnnotation proposal
  maxProposalSize      = ppMaxProposalSize adoptedPP

  adoptedMaxBlockSize  = ppMaxBlockSize adoptedPP
  newMaxBlockSize      = ppMaxBlockSize proposedPP

  newMaxTxSize         = ppMaxTxSize proposedPP

  adoptedScriptVersion = ppScriptVersion adoptedPP
  newScriptVersion     = ppScriptVersion proposedPP
  scriptVersionDiff    = newScriptVersion - adoptedScriptVersion

-- | Check that a new 'SoftwareVersion' is valid
--
--   We check that:
--
--   1) The 'SoftwareVersion' hasn't already been registered
--   2) The new 'SoftwareVersion' is a valid next version
--
--   This corresponds to the `UPSVV` rule in the spec.
registerSoftwareUpdate
  :: MonadError Error m
  => ApplicationVersions
  -> SoftwareUpdateProposals
  -> AProposal ByteString
  -> m SoftwareUpdateProposals
registerSoftwareUpdate appVersions registeredSUPs proposal = do

  -- Check that this software version isn't already registered
  null (M.filter (== softwareVersion) registeredSUPs)
    `orThrowError` RegistrationDuplicateSoftwareVersion softwareVersion

  -- Check that this software version is a valid next version
  svCanFollow appVersions softwareVersion
    `orThrowError` RegistrationInvalidSoftwareVersion softwareVersion

  -- Add to the list of registered software update proposals
  pure $ M.insert (recoverUpId proposal) softwareVersion registeredSUPs
  where softwareVersion = pbSoftwareVersion $ proposalBody proposal


-- | Check that a new 'SoftwareVersion' is a valid next version
--
--   The new version is valid for a given application if it is the same or one
--   more than the current version
svCanFollow :: ApplicationVersions -> SoftwareVersion -> Bool
svCanFollow avs softwareVersion = case M.lookup appName avs of
  Nothing -> appVersion == 1
  Just (currentAppVersion, _) -> appVersion == currentAppVersion + 1
  where SoftwareVersion appName appVersion = softwareVersion
