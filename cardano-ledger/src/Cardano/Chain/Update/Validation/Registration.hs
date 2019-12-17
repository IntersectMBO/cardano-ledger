{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Validation rules for registering updates
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Registration
  ( Error
  , Environment (..)
  , State (..)
  , ApplicationVersions
  , Metadata
  , ProtocolUpdateProposals
  , SoftwareUpdateProposals
  , registerProposal
  , TooLarge (..)
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M

import Cardano.Binary
  ( Annotated
  , Decoder
  , DecoderError(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeListLen
  , decodeWord8
  , encodeListLen
  , enforceSize
  , matchSize
  )
import Cardano.Chain.Common (KeyHash, hashKey)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (SlotNumber)
import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.InstallerHash (InstallerHash)
import qualified Cardano.Chain.Update.Proposal as Proposal
import Cardano.Chain.Update.Proposal
  ( AProposal(..)
  , ProposalBody(..)
  , UpId
  , protocolParametersUpdate
  , protocolVersion
  , recoverProposalSignedBytes
  , recoverUpId
  , softwareVersion
  )
import Cardano.Chain.Update.ProtocolParameters
  ( ProtocolParameters
  , ppMaxBlockSize
  , ppMaxProposalSize
  , ppMaxTxSize
  , ppScriptVersion
  )
import qualified Cardano.Chain.Update.ProtocolParametersUpdate as PPU
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion(ProtocolVersion))
import Cardano.Chain.Update.SoftwareVersion
  ( NumSoftwareVersion
  , SoftwareVersion(SoftwareVersion)
  , SoftwareVersionError
  , checkSoftwareVersion
  )
import Cardano.Chain.Update.SystemTag (SystemTagError, checkSystemTag, SystemTag)
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignUSProposal)
  , verifySignatureDecoded
  )


data Environment = Environment
  { protocolMagic             :: !(Annotated ProtocolMagicId ByteString)
  , adoptedProtocolVersion    :: !ProtocolVersion
  , adoptedProtocolParameters :: !ProtocolParameters
  , appVersions               :: !ApplicationVersions
  , delegationMap             :: !Delegation.Map
  }

type ApplicationVersions = Map ApplicationName (NumSoftwareVersion, SlotNumber, Metadata)

type Metadata = Map SystemTag InstallerHash

-- | State keeps track of registered protocol and software update
--   proposals
data State = State
  { rsProtocolUpdateProposals :: !ProtocolUpdateProposals
  , rsSoftwareUpdateProposals :: !SoftwareUpdateProposals
  }

type ProtocolUpdateProposals = Map UpId (ProtocolVersion, ProtocolParameters)
type SoftwareUpdateProposals = Map UpId (SoftwareVersion, Metadata)

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
    InvalidProtocolVersion protocolVersion adopted  ->
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
    SoftwareVersionError softwareVersionError  ->
      encodeListLen 2
        <> toCBOR (11 :: Word8)
        <> toCBOR softwareVersionError
    SystemTagError systemTagError ->
      encodeListLen 2
        <> toCBOR (12 :: Word8)
        <> toCBOR systemTagError

instance FromCBOR Error where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "Registration.Error" size len
    tag <- decodeWord8
    case tag of
      0  -> checkSize 2 >> DuplicateProtocolVersion <$> fromCBOR
      1  -> checkSize 2 >> DuplicateSoftwareVersion <$> fromCBOR
      2  -> checkSize 2 >> InvalidProposer <$> fromCBOR
      3  -> checkSize 3 >> InvalidProtocolVersion <$> fromCBOR <*> fromCBOR
      4  -> checkSize 3 >> InvalidScriptVersion <$> fromCBOR <*> fromCBOR
      5  -> checkSize 1 >> pure InvalidSignature
      6  -> checkSize 3 >> InvalidSoftwareVersion <$> fromCBOR <*> fromCBOR
      7  -> checkSize 2 >> MaxBlockSizeTooLarge <$> fromCBOR
      8  -> checkSize 2 >> MaxTxSizeTooLarge <$> fromCBOR
      9  -> checkSize 1 >> pure ProposalAttributesUnknown
      10 -> checkSize 2 >> ProposalTooLarge <$> fromCBOR
      11 -> checkSize 2 >> SoftwareVersionError <$> fromCBOR
      12 -> checkSize 2 >> SystemTagError <$> fromCBOR
      _  -> cborError   $  DecoderErrorUnknownTag "Registration.Error" tag

data TooLarge n = TooLarge
  { tlActual   :: n
  , tlMaxBound :: n
  } deriving (Eq, Show)

instance (ToCBOR n) => ToCBOR (TooLarge n) where
  toCBOR TooLarge{ tlActual, tlMaxBound } =
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
registerProposal
  :: MonadError Error m
  => Environment
  -> State
  -> AProposal ByteString
  -> m State
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
    adoptedProtocolVersion
    adoptedProtocolParameters
    appVersions
    rs
    proposal
 where
  AProposal { aBody, issuer, signature } = proposal

  proposerId = hashKey issuer

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
  ProposalBody
    { protocolVersion
    , protocolParametersUpdate = ppu
    , softwareVersion
    } = Proposal.body proposal

  SoftwareVersion appName appVersion = softwareVersion

  softwareVersionChanged =
    maybe True ((/= appVersion) . fst3) $ M.lookup appName appVersions
    where
      fst3 :: (a, b, c) -> a
      fst3 (x, _, _) = x

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
    `orThrowError` DuplicateProtocolVersion newPV

  -- Check that this protocol version is a valid next version
  pvCanFollow newPV adoptedPV
    `orThrowError` InvalidProtocolVersion newPV (Adopted adoptedPV)

  canUpdate adoptedPP newPP proposal

  pure $ M.insert (recoverUpId proposal) (newPV, newPP) registeredPUPs
 where
  ProposalBody { protocolVersion = newPV, protocolParametersUpdate } =
    Proposal.body proposal
  newPP = PPU.apply protocolParametersUpdate adoptedPP


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
  (proposalSize <= maxProposalSize) `orThrowError` ProposalTooLarge
    (TooLarge maxProposalSize proposalSize)

  -- Check that the new maximum block size is no more than twice the current one
  (newMaxBlockSize <= 2 * adoptedMaxBlockSize)
    `orThrowError` MaxBlockSizeTooLarge
                     (TooLarge adoptedMaxBlockSize newMaxBlockSize)

  -- Check that the new max transaction size is less than the new max block size
  (newMaxTxSize <=  newMaxBlockSize)
    `orThrowError` MaxTxSizeTooLarge
                     (TooLarge newMaxBlockSize newMaxTxSize)

  -- Check that the new script version is either the same or incremented
  (0 <= scriptVersionDiff && scriptVersionDiff <= 1)
    `orThrowError` InvalidScriptVersion
                     adoptedScriptVersion newScriptVersion
 where
  proposalSize :: Natural
  proposalSize         = fromIntegral . BS.length $ Proposal.annotation proposal
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
--   2) The 'SoftwareVersion' is valid according to static checks
--   3) The new 'SoftwareVersion' is a valid next version
--
--   This corresponds to the `UPSVV` rule in the spec.
registerSoftwareUpdate
  :: MonadError Error m
  => ApplicationVersions
  -> SoftwareUpdateProposals
  -> AProposal ByteString
  -> m SoftwareUpdateProposals
registerSoftwareUpdate appVersions registeredSUPs proposal = do

  -- Check that the 'SystemTag's in the metadata are valid
  mapM_ checkSystemTag (M.keys metadata) `wrapError` SystemTagError

  -- Check that this software version isn't already registered
  null (M.filter ((== softwareVersion) . fst) registeredSUPs)
    `orThrowError` DuplicateSoftwareVersion softwareVersion

  -- Check that the software version is valid
  checkSoftwareVersion softwareVersion `wrapError` SoftwareVersionError

  -- Check that this software version is a valid next version
  svCanFollow appVersions softwareVersion
    `orThrowError` InvalidSoftwareVersion appVersions softwareVersion

  -- Add to the list of registered software update proposals
  pure $ M.insert (recoverUpId proposal) (softwareVersion, metadata) registeredSUPs
  where ProposalBody { softwareVersion, metadata } = Proposal.body proposal


-- | Check that a new 'SoftwareVersion' is a valid next version
--
--   The new version is valid for a given application if it is the same or one
--   more than the current version
svCanFollow :: ApplicationVersions -> SoftwareVersion -> Bool
svCanFollow avs softwareVersion = case M.lookup appName avs of
  Nothing -> appVersion == 1
  Just (currentAppVersion, _, _) -> appVersion == currentAppVersion + 1
  where SoftwareVersion appName appVersion = softwareVersion
