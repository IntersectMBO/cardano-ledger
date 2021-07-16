{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Validation rules for registering updates
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Registration
  ( Error (..),
    Environment (..),
    State (..),
    ApplicationVersion (..),
    ApplicationVersions,
    Metadata,
    ProtocolUpdateProposal (..),
    ProtocolUpdateProposals,
    SoftwareUpdateProposal (..),
    SoftwareUpdateProposals,
    registerProposal,
    TooLarge (..),
    Adopted (..),
  )
where

import Cardano.Binary
  ( Annotated (unAnnotated),
    Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    enforceSize,
    matchSize,
  )
import Cardano.Chain.Common (KeyHash, hashKey)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (SlotNumber (SlotNumber))
import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.InstallerHash (InstallerHash)
import Cardano.Chain.Update.Proposal
  ( AProposal (..),
    ProposalBody (..),
    UpId,
    protocolParametersUpdate,
    protocolVersion,
    recoverProposalSignedBytes,
    recoverUpId,
    softwareVersion,
  )
import qualified Cardano.Chain.Update.Proposal as Proposal
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters,
    ppMaxBlockSize,
    ppMaxProposalSize,
    ppMaxTxSize,
    ppScriptVersion,
  )
import qualified Cardano.Chain.Update.ProtocolParametersUpdate as PPU
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion (ProtocolVersion))
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion,
    SoftwareVersion (SoftwareVersion),
    SoftwareVersionError,
    checkSoftwareVersion,
    svAppName,
  )
import Cardano.Chain.Update.SystemTag (SystemTag, SystemTagError, checkSystemTag)
import Cardano.Crypto
  ( ProtocolMagicId (..),
    SignTag (SignUSProposal),
    verifySignatureDecoded,
  )
import Cardano.Prelude hiding (State)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import NoThunks.Class (NoThunks (..))

data Environment = Environment
  { protocolMagic :: !(Annotated ProtocolMagicId ByteString),
    currentSlot :: !SlotNumber,
    adoptedProtocolVersion :: !ProtocolVersion,
    adoptedProtocolParameters :: !ProtocolParameters,
    appVersions :: !ApplicationVersions,
    delegationMap :: !Delegation.Map
  }

data ApplicationVersion = ApplicationVersion
  { avNumSoftwareVersion :: !NumSoftwareVersion,
    avSlotNumber :: !SlotNumber,
    avMetadata :: !Metadata
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance FromCBOR ApplicationVersion where
  fromCBOR = do
    enforceSize "ApplicationVersion" 3
    ApplicationVersion <$> fromCBOR <*> fromCBOR <*> fromCBOR

instance ToCBOR ApplicationVersion where
  toCBOR av =
    encodeListLen 3
      <> toCBOR (avNumSoftwareVersion av)
      <> toCBOR (avSlotNumber av)
      <> toCBOR (avMetadata av)

type ApplicationVersions = Map ApplicationName ApplicationVersion

type Metadata = Map SystemTag InstallerHash

-- | State keeps track of registered protocol and software update
--   proposals
data State = State
  { rsProtocolUpdateProposals :: !ProtocolUpdateProposals,
    rsSoftwareUpdateProposals :: !SoftwareUpdateProposals
  }

data ProtocolUpdateProposal = ProtocolUpdateProposal
  { pupProtocolVersion :: !ProtocolVersion,
    pupProtocolParameters :: !ProtocolParameters
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance FromCBOR ProtocolUpdateProposal where
  fromCBOR = do
    enforceSize "ProtocolUpdateProposal" 2
    ProtocolUpdateProposal <$> fromCBOR <*> fromCBOR

instance ToCBOR ProtocolUpdateProposal where
  toCBOR pup =
    encodeListLen 2
      <> toCBOR (pupProtocolVersion pup)
      <> toCBOR (pupProtocolParameters pup)

type ProtocolUpdateProposals = Map UpId ProtocolUpdateProposal

data SoftwareUpdateProposal = SoftwareUpdateProposal
  { supSoftwareVersion :: !SoftwareVersion,
    supSoftwareMetadata :: !Metadata
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance FromCBOR SoftwareUpdateProposal where
  fromCBOR = do
    enforceSize "SoftwareUpdateProposal" 2
    SoftwareUpdateProposal <$> fromCBOR <*> fromCBOR

instance ToCBOR SoftwareUpdateProposal where
  toCBOR sup =
    encodeListLen 2
      <> toCBOR (supSoftwareVersion sup)
      <> toCBOR (supSoftwareMetadata sup)

type SoftwareUpdateProposals = Map UpId SoftwareUpdateProposal

-- | Error captures the ways in which registration could fail
data Error
  = DuplicateProtocolVersion ProtocolVersion
  | DuplicateSoftwareVersion SoftwareVersion
  | InvalidProposer KeyHash
  | InvalidProtocolVersion ProtocolVersion Adopted
  | InvalidScriptVersion Word16 Word16
  | InvalidSignature
  | InvalidSoftwareVersion ApplicationVersions SoftwareVersion
  | MaxBlockSizeTooLarge (TooLarge Natural)
  | MaxTxSizeTooLarge (TooLarge Natural)
  | ProposalAttributesUnknown
  | ProposalTooLarge (TooLarge Natural)
  | SoftwareVersionError SoftwareVersionError
  | SystemTagError SystemTagError
  | -- | The update proposal proposes neither a bump in the protocol or
    -- application versions.
    NullUpdateProposal
  deriving (Eq, Show)

instance ToCBOR Error where
  toCBOR err = case err of
    DuplicateProtocolVersion protocolVersion ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR protocolVersion
    DuplicateSoftwareVersion softwareVersion ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR softwareVersion
    InvalidProposer keyHash ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR keyHash
    InvalidProtocolVersion protocolVersion adopted ->
      encodeListLen 3
        <> toCBOR (3 :: Word8)
        <> toCBOR protocolVersion
        <> toCBOR adopted
    InvalidScriptVersion adoptedScriptVersion newScriptVersion ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR adoptedScriptVersion
        <> toCBOR newScriptVersion
    InvalidSignature ->
      encodeListLen 1
        <> toCBOR (5 :: Word8)
    InvalidSoftwareVersion applicationVersions softwareVersion ->
      encodeListLen 3
        <> toCBOR (6 :: Word8)
        <> toCBOR applicationVersions
        <> toCBOR softwareVersion
    MaxBlockSizeTooLarge tooLarge ->
      encodeListLen 2
        <> toCBOR (7 :: Word8)
        <> toCBOR tooLarge
    MaxTxSizeTooLarge tooLarge ->
      encodeListLen 2
        <> toCBOR (8 :: Word8)
        <> toCBOR tooLarge
    ProposalAttributesUnknown ->
      encodeListLen 1
        <> toCBOR (9 :: Word8)
    ProposalTooLarge tooLarge ->
      encodeListLen 2
        <> toCBOR (10 :: Word8)
        <> toCBOR tooLarge
    SoftwareVersionError softwareVersionError ->
      encodeListLen 2
        <> toCBOR (11 :: Word8)
        <> toCBOR softwareVersionError
    SystemTagError systemTagError ->
      encodeListLen 2
        <> toCBOR (12 :: Word8)
        <> toCBOR systemTagError
    NullUpdateProposal ->
      encodeListLen 1
        <> toCBOR (13 :: Word8)

instance FromCBOR Error where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "Registration.Error" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> DuplicateProtocolVersion <$> fromCBOR
      1 -> checkSize 2 >> DuplicateSoftwareVersion <$> fromCBOR
      2 -> checkSize 2 >> InvalidProposer <$> fromCBOR
      3 -> checkSize 3 >> InvalidProtocolVersion <$> fromCBOR <*> fromCBOR
      4 -> checkSize 3 >> InvalidScriptVersion <$> fromCBOR <*> fromCBOR
      5 -> checkSize 1 >> pure InvalidSignature
      6 -> checkSize 3 >> InvalidSoftwareVersion <$> fromCBOR <*> fromCBOR
      7 -> checkSize 2 >> MaxBlockSizeTooLarge <$> fromCBOR
      8 -> checkSize 2 >> MaxTxSizeTooLarge <$> fromCBOR
      9 -> checkSize 1 >> pure ProposalAttributesUnknown
      10 -> checkSize 2 >> ProposalTooLarge <$> fromCBOR
      11 -> checkSize 2 >> SoftwareVersionError <$> fromCBOR
      12 -> checkSize 2 >> SystemTagError <$> fromCBOR
      13 -> checkSize 1 >> pure NullUpdateProposal
      _ -> cborError $ DecoderErrorUnknownTag "Registration.Error" tag

data TooLarge n = TooLarge
  { tlActual :: n,
    tlMaxBound :: n
  }
  deriving (Eq, Show)

instance (ToCBOR n) => ToCBOR (TooLarge n) where
  toCBOR TooLarge {tlActual, tlMaxBound} =
    encodeListLen 2
      <> toCBOR tlActual
      <> toCBOR tlMaxBound

instance (FromCBOR n) => FromCBOR (TooLarge n) where
  fromCBOR = do
    enforceSize "TooLarge" 2
    TooLarge <$> fromCBOR <*> fromCBOR

newtype Adopted = Adopted ProtocolVersion
  deriving (Eq, Show)
  deriving newtype (ToCBOR, FromCBOR)

-- | Register an update proposal after verifying its signature and validating
--   its contents. This corresponds to the @UPREG@ rules in the spec.
registerProposal ::
  MonadError Error m =>
  Environment ->
  State ->
  AProposal ByteString ->
  m State
registerProposal env rs proposal = do
  -- Check that the proposer is delegated to by a genesis key
  Delegation.memberR proposerId delegationMap
    `orThrowError` InvalidProposer proposerId

  -- Verify the proposal signature
  verifySignatureDecoded
    protocolMagic
    SignUSProposal
    issuer
    (recoverProposalSignedBytes aBody)
    signature
    `orThrowError` InvalidSignature

  -- Check that the proposal is valid
  registerProposalComponents
    env
    rs
    proposal
  where
    AProposal {aBody, issuer, signature} = proposal

    proposerId = hashKey issuer

    Environment
      { protocolMagic,
        delegationMap
      } = env

-- | Register the individual components of an update proposal
--
--   The proposal may contain a protocol update, a software update, or both.
--   This corresponds to the `UPV` rules in the spec.
registerProposalComponents ::
  MonadError Error m =>
  Environment ->
  State ->
  AProposal ByteString ->
  m State
registerProposalComponents env rs proposal = do
  (protocolVersionChanged || softwareVersionChanged || nullUpdateExemptions)
    `orThrowError` NullUpdateProposal

  -- Register protocol update if we have one
  registeredPUPs' <-
    if protocolVersionChanged
      then registerProtocolUpdate adoptedPV adoptedPP registeredPUPs proposal
      else pure registeredPUPs

  -- Register software update if we have one
  registeredSUPs' <-
    if softwareVersionChanged
      then registerSoftwareUpdate appVersions registeredSUPs proposal
      else pure registeredSUPs

  pure $ State registeredPUPs' registeredSUPs'
  where
    ProposalBody
      { protocolVersion,
        protocolParametersUpdate = ppu,
        softwareVersion
      } = Proposal.body proposal

    SoftwareVersion appName appVersion = softwareVersion

    softwareVersionChanged =
      maybe True ((/= appVersion) . avNumSoftwareVersion) $
        M.lookup appName appVersions

    protocolVersionChanged =
      not $ protocolVersion == adoptedPV && PPU.apply ppu adoptedPP == adoptedPP

    Environment
      { protocolMagic,
        currentSlot,
        adoptedProtocolVersion = adoptedPV,
        adoptedProtocolParameters = adoptedPP,
        appVersions
      } = env

    State registeredPUPs registeredSUPs = rs

    -- A "null" update proposal is one that neither increase the protocol
    -- version nor the software version. Such update proposals are invalid
    -- according to the Byron specification. However in the cardano-sl code
    -- they are accepted onto the chain but without any state change.
    --
    -- We cannot follow the legacy cardano-sl interpretation of accepting null
    -- update onto the chain with no effect because it opens the door to DoS
    -- attacks by replaying null update proposals.
    --
    -- For further details see:
    --
    -- https://github.com/input-output-hk/cardano-ledger/issues/759
    -- https://github.com/input-output-hk/cardano-ledger/pull/766
    --
    -- The existing staging network (protocol magic 633343913) does have existing
    -- null update proposals however: one in epoch 44 (slot number 969188) and
    -- one in epoch 88 (slot number 1915231). We could delete the staging network
    -- blockchain and start from scratch, however it is extremely useful for
    -- testing to have a realistic chain that is as long as the mainnet chain,
    -- and indeed that has a large prefix that was created by the legacy
    -- cardano-sl codebase. Therefore we allow for these specific excemptions on
    -- this non-public testing network.
    --
    nullUpdateExemptions =
      unAnnotated protocolMagic == ProtocolMagicId 633343913 -- staging
        && ( currentSlot == SlotNumber 969188 -- in epoch 44
               || currentSlot == SlotNumber 1915231 -- in epoch 88
           )

-- | Validate a protocol update
--
--   We check that:
--
--   1) The protocol update hasn't already been registered
--   2) The protocol version is a valid next version
--   3) The new 'ProtocolParameters' represent a valid update
--
--   This corresponds to the `UPPVV` rule in the spec.
registerProtocolUpdate ::
  MonadError Error m =>
  ProtocolVersion ->
  ProtocolParameters ->
  ProtocolUpdateProposals ->
  AProposal ByteString ->
  m ProtocolUpdateProposals
registerProtocolUpdate adoptedPV adoptedPP registeredPUPs proposal = do
  -- Check that this protocol version isn't already registered
  null (M.filter ((== newPV) . pupProtocolVersion) registeredPUPs)
    `orThrowError` DuplicateProtocolVersion newPV

  -- Check that this protocol version is a valid next version
  pvCanFollow newPV adoptedPV
    `orThrowError` InvalidProtocolVersion newPV (Adopted adoptedPV)

  canUpdate adoptedPP newPP proposal

  pure $
    M.insert
      (recoverUpId proposal)
      (ProtocolUpdateProposal newPV newPP)
      registeredPUPs
  where
    ProposalBody {protocolVersion = newPV, protocolParametersUpdate} =
      Proposal.body proposal
    newPP = PPU.apply protocolParametersUpdate adoptedPP

-- | Check that the new 'ProtocolVersion' is a valid next version
pvCanFollow :: ProtocolVersion -> ProtocolVersion -> Bool
pvCanFollow newPV adoptedPV = adoptedPV < newPV && isNextVersion
  where
    ProtocolVersion newMajor newMinor _ = newPV
    ProtocolVersion adoptedMajor adoptedMinor _ = adoptedPV
    isNextVersion = case newMajor - adoptedMajor of
      0 -> newMinor == adoptedMinor + 1
      1 -> newMinor == 0
      _ -> False

-- | Check that the new 'ProtocolParameters' represent a valid update
--
--   This is where we enforce constraints on how the 'ProtocolParameters'
--   change.
canUpdate ::
  MonadError Error m =>
  ProtocolParameters ->
  ProtocolParameters ->
  AProposal ByteString ->
  m ()
canUpdate adoptedPP proposedPP proposal = do
  -- Check that the proposal size is less than the maximum
  (proposalSize <= maxProposalSize)
    `orThrowError` ProposalTooLarge
      (TooLarge maxProposalSize proposalSize)

  -- Check that the new maximum block size is no more than twice the current one
  (newMaxBlockSize <= 2 * adoptedMaxBlockSize)
    `orThrowError` MaxBlockSizeTooLarge
      (TooLarge adoptedMaxBlockSize newMaxBlockSize)

  -- Check that the new max transaction size is less than the new max block size
  (newMaxTxSize < newMaxBlockSize)
    `orThrowError` MaxTxSizeTooLarge
      (TooLarge newMaxBlockSize newMaxTxSize)

  -- Check that the new script version is either the same or incremented
  (0 <= scriptVersionDiff && scriptVersionDiff <= 1)
    `orThrowError` InvalidScriptVersion
      adoptedScriptVersion
      newScriptVersion
  where
    proposalSize :: Natural
    proposalSize = fromIntegral . BS.length $ Proposal.annotation proposal
    maxProposalSize = ppMaxProposalSize adoptedPP

    adoptedMaxBlockSize = ppMaxBlockSize adoptedPP
    newMaxBlockSize = ppMaxBlockSize proposedPP

    newMaxTxSize = ppMaxTxSize proposedPP

    adoptedScriptVersion = ppScriptVersion adoptedPP
    newScriptVersion = ppScriptVersion proposedPP
    scriptVersionDiff = newScriptVersion - adoptedScriptVersion

-- | Check that a new 'SoftwareVersion' is valid
--
--   We check that:
--
--   1) The 'SoftwareVersion' hasn't already been registered
--   2) The 'SoftwareVersion' is valid according to static checks
--   3) The new 'SoftwareVersion' is a valid next version
--
--   This corresponds to the `UPSVV` rule in the spec.
registerSoftwareUpdate ::
  MonadError Error m =>
  ApplicationVersions ->
  SoftwareUpdateProposals ->
  AProposal ByteString ->
  m SoftwareUpdateProposals
registerSoftwareUpdate appVersions registeredSUPs proposal = do
  -- Check that the 'SystemTag's in the metadata are valid
  mapM_ checkSystemTag (M.keys metadata) `wrapError` SystemTagError

  -- Check that this software version isn't already registered
  null
    ( M.filter
        ((== svAppName softwareVersion) . svAppName . supSoftwareVersion)
        registeredSUPs
    )
    `orThrowError` DuplicateSoftwareVersion softwareVersion

  -- Check that the software version is valid
  checkSoftwareVersion softwareVersion `wrapError` SoftwareVersionError

  -- Check that this software version is a valid next version
  svCanFollow appVersions softwareVersion
    `orThrowError` InvalidSoftwareVersion appVersions softwareVersion

  -- Add to the list of registered software update proposals
  pure $
    M.insert
      (recoverUpId proposal)
      (SoftwareUpdateProposal softwareVersion metadata)
      registeredSUPs
  where
    ProposalBody {softwareVersion, metadata} = Proposal.body proposal

-- | Check that a new 'SoftwareVersion' is a valid next version
--
--   The new version is valid for a given application if it is exactly one
--   more than the current version
svCanFollow :: ApplicationVersions -> SoftwareVersion -> Bool
svCanFollow avs (SoftwareVersion appName appVersion) =
  case M.lookup appName avs of
    -- For new apps, the version must start at 0 or 1.
    Nothing -> appVersion == 0 || appVersion == 1
    -- For existing apps, it must be exactly one more than the current version
    Just (ApplicationVersion currentAppVersion _ _) -> appVersion == currentAppVersion + 1
