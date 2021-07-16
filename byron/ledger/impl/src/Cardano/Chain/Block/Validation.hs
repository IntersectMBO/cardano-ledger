{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Validation
  ( UPI.adoptedProtocolParameters,
    updateBody,
    updateChainBlockOrBoundary,
    updateChainBoundary,
    epochTransition,
    headerIsValid,
    validateHeaderMatchesBody,
    updateBlock,
    BodyState (..),
    BodyEnvironment (..),
    EpochEnvironment (..),
    ChainValidationState (..),
    initialChainValidationState,
    ChainValidationError (..),

    -- * UTxO
    HeapSize (..),
    UTxOSize (..),
    calcUTxOSize,
    foldUTxO,
    foldUTxOBlock,
  )
where

import Cardano.Binary
  ( Annotated (..),
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
    serialize',
  )
import Cardano.Chain.Block.Block
  ( ABlock (..),
    ABlockOrBoundary (..),
    ABoundaryBlock (..),
    blockAProtocolMagicId,
    blockDlgPayload,
    blockHashAnnotated,
    blockHeader,
    blockIssuer,
    blockLength,
    blockProtocolMagicId,
    blockProtocolVersion,
    blockSlot,
    blockTxPayload,
    blockUpdatePayload,
  )
import Cardano.Chain.Block.Body (ABody (..))
import Cardano.Chain.Block.Header
  ( ABoundaryHeader (..),
    AHeader (..),
    BlockSignature,
    HeaderHash,
    headerLength,
    headerProof,
    wrapBoundaryBytes,
  )
import Cardano.Chain.Block.Proof (Proof (..), ProofValidationError (..))
import Cardano.Chain.Common
  ( BlockCount (..),
    KeyHash,
    hashKey,
  )
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as DI
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import Cardano.Chain.Epoch.File (ParseError, mainnetEpochSlots)
import Cardano.Chain.Genesis as Genesis
  ( Config (..),
    GenesisHash,
    GenesisKeyHashes (..),
    configEpochSlots,
    configGenesisKeyHashes,
    configHeavyDelegation,
    configK,
    configProtocolMagicId,
  )
import Cardano.Chain.ProtocolConstants (kEpochSlots)
import Cardano.Chain.Slotting
  ( EpochAndSlotCount (..),
    EpochNumber (..),
    SlotNumber (..),
    fromSlotNumber,
    slotNumberEpoch,
  )
import Cardano.Chain.UTxO (ATxPayload (..), UTxO (..), genesisUtxo, recoverTxProof)
import Cardano.Chain.UTxO.UTxOConfiguration (UTxOConfiguration)
import qualified Cardano.Chain.UTxO.Validation as UTxO
import qualified Cardano.Chain.Update as Update
import Cardano.Chain.Update.Validation.Endorsement (Endorsement (..))
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import Cardano.Chain.ValidationMode
  ( ValidationMode,
    orThrowErrorInBlockValidationMode,
    whenBlockValidation,
    wrapErrorWithValidationMode,
  )
import Cardano.Crypto
  ( AProtocolMagic (..),
    ProtocolMagicId,
    VerificationKey,
    hashDecoded,
    hashRaw,
  )
import Cardano.Prelude
import Control.Monad.Trans.Resource (ResIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))
import Streaming (Of (..), Stream, hoist)
import qualified Streaming.Prelude as S

--------------------------------------------------------------------------------
-- ChainValidationState
--------------------------------------------------------------------------------

data ChainValidationState = ChainValidationState
  { cvsLastSlot :: !SlotNumber,
    -- | GenesisHash for the previous hash of the zeroth boundary block and
    --   HeaderHash for all others.
    cvsPreviousHash :: !(Either GenesisHash HeaderHash),
    cvsUtxo :: !UTxO,
    cvsUpdateState :: !UPI.State,
    cvsDelegationState :: !DI.State
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance FromCBOR ChainValidationState where
  fromCBOR = do
    enforceSize "ChainValidationState" 5
    ChainValidationState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance ToCBOR ChainValidationState where
  toCBOR c =
    encodeListLen 5
      <> toCBOR (cvsLastSlot c)
      <> toCBOR (cvsPreviousHash c)
      <> toCBOR (cvsUtxo c)
      <> toCBOR (cvsUpdateState c)
      <> toCBOR (cvsDelegationState c)

-- | Create the state needed to validate the zeroth epoch of the chain. The
--   zeroth epoch starts with a boundary block where the previous hash is the
--   genesis hash.
initialChainValidationState ::
  MonadError Scheduling.Error m =>
  Genesis.Config ->
  m ChainValidationState
initialChainValidationState config = do
  delegationState <- DI.initialState delegationEnv genesisDelegation
  pure $
    ChainValidationState
      { cvsLastSlot = 0,
        -- Ensure that we don't allow the internal value of this 'Left' to be
        -- lazy as we want to ensure that the 'ChainValidationState' is always
        -- in normal form.
        cvsPreviousHash = Left $! configGenesisHash config,
        cvsUtxo = genesisUtxo config,
        cvsUpdateState = UPI.initialState config,
        cvsDelegationState = delegationState
      }
  where
    delegationEnv =
      DI.Environment
        { DI.protocolMagic = Annotated pm (serialize' pm),
          DI.allowedDelegators = unGenesisKeyHashes $ configGenesisKeyHashes config,
          DI.k = configK config,
          DI.currentEpoch = EpochNumber 0,
          DI.currentSlot = SlotNumber 0
        }

    pm = configProtocolMagicId config

    genesisDelegation = configHeavyDelegation config

--------------------------------------------------------------------------------
-- ChainValidationError
--------------------------------------------------------------------------------

data ChainValidationError
  = -- | The size of an epoch boundary block exceeds the limit
    ChainValidationBoundaryTooLarge
  | -- | The size of a block's attributes is non-zero
    ChainValidationBlockAttributesTooLarge
  | -- | The size of a regular block exceeds the limit
    ChainValidationBlockTooLarge Natural Natural
  | -- | The size of a block header's attributes is non-zero
    ChainValidationHeaderAttributesTooLarge
  | -- | The size of a block header exceeds the limit
    ChainValidationHeaderTooLarge Natural Natural
  | -- | There is a problem with the delegation payload signature
    ChainValidationDelegationPayloadError Text
  | -- | The delegation used in the signature is not valid according to the ledger
    ChainValidationInvalidDelegation VerificationKey VerificationKey
  | -- | Genesis hash mismatch
    ChainValidationGenesisHashMismatch GenesisHash GenesisHash
  | -- | Expected GenesisHash but got HeaderHash
    ChainValidationExpectedGenesisHash GenesisHash HeaderHash
  | -- | Expected HeaderHash but GenesisHash
    ChainValidationExpectedHeaderHash HeaderHash GenesisHash
  | -- | The hash of the previous block does not match the value in the header
    ChainValidationInvalidHash HeaderHash HeaderHash
  | -- | The hash of the previous block is missing and should be given hash.
    ChainValidationMissingHash HeaderHash
  | -- | There should not be a hash of the previous but there is.
    ChainValidationUnexpectedGenesisHash HeaderHash
  | -- | The signature of the block is invalid
    ChainValidationInvalidSignature BlockSignature
  | -- | A delegation certificate failed validation in the ledger layer
    ChainValidationDelegationSchedulingError Scheduling.Error
  | -- | The 'ProtocolMagic' in the block doesn't match the configured one
    ChainValidationProtocolMagicMismatch ProtocolMagicId ProtocolMagicId
  | -- | A block is using unsupported lightweight delegation
    ChainValidationSignatureLight
  | -- | The delegator for this block has delegated in too many recent blocks
    ChainValidationTooManyDelegations VerificationKey
  | -- | Something failed to register in the update interface
    ChainValidationUpdateError SlotNumber UPI.Error
  | -- | A transaction failed validation in the ledger layer
    ChainValidationUTxOValidationError UTxO.UTxOValidationError
  | -- | A payload proof did not match.
    ChainValidationProofValidationError ProofValidationError
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Validation Functions
--------------------------------------------------------------------------------

updateChainBlockOrBoundary ::
  (MonadError ChainValidationError m, MonadReader ValidationMode m) =>
  Genesis.Config ->
  ChainValidationState ->
  ABlockOrBoundary ByteString ->
  m ChainValidationState
updateChainBlockOrBoundary config c b = case b of
  ABOBBoundary bvd -> updateChainBoundary c bvd
  ABOBBlock block -> updateBlock config c block

updateChainBoundary ::
  MonadError ChainValidationError m =>
  ChainValidationState ->
  ABoundaryBlock ByteString ->
  m ChainValidationState
updateChainBoundary cvs bvd = do
  case (cvsPreviousHash cvs, boundaryPrevHash (boundaryHeader bvd)) of
    (Left expected, Left actual) ->
      (expected == actual)
        `orThrowError` ChainValidationGenesisHashMismatch expected actual
    (Right expected, Right actual) ->
      (expected == actual)
        `orThrowError` ChainValidationInvalidHash expected actual
    (Left gh, Right hh) ->
      throwError $ ChainValidationExpectedGenesisHash gh hh
    (Right hh, Left gh) ->
      throwError $ ChainValidationExpectedHeaderHash hh gh

  -- Validate that the block is within the size bounds
  (boundaryBlockLength bvd <= 2e6)
    `orThrowError` ChainValidationBoundaryTooLarge

  -- Update the previous hash
  pure $
    cvs
      { cvsPreviousHash = Right $! previousHash
      }
  where
    previousHash :: HeaderHash
    previousHash =
      coerce
        . hashRaw
        . BSL.fromStrict
        . wrapBoundaryBytes
        $ boundaryHeaderAnnotation (boundaryHeader bvd)

validateHeaderMatchesBody ::
  MonadError ProofValidationError m =>
  AHeader ByteString ->
  ABody ByteString ->
  m ()
validateHeaderMatchesBody hdr body = do
  let hdrProof = headerProof hdr

  -- Validate the delegation payload signature
  proofDelegation hdrProof == hashDecoded (bodyDlgPayload body)
    `orThrowError` DelegationProofValidationError

  -- Validate the transaction payload proof
  proofUTxO hdrProof == recoverTxProof (bodyTxPayload body)
    `orThrowError` UTxOProofValidationError

  -- Validate the update payload proof
  proofUpdate hdrProof == hashDecoded (bodyUpdatePayload body)
    `orThrowError` UpdateProofValidationError

validateBlockProofs ::
  MonadError ProofValidationError m =>
  ABlock ByteString ->
  m ()
validateBlockProofs b =
  validateHeaderMatchesBody blockHeader blockBody
  where
    ABlock
      { blockHeader,
        blockBody
      } = b

data BodyEnvironment = BodyEnvironment
  { protocolMagic :: !(AProtocolMagic ByteString),
    utxoConfiguration :: !UTxOConfiguration,
    k :: !BlockCount,
    allowedDelegators :: !(Set KeyHash),
    protocolParameters :: !Update.ProtocolParameters,
    currentEpoch :: !EpochNumber
  }

data BodyState = BodyState
  { utxo :: !UTxO,
    updateState :: !UPI.State,
    delegationState :: !DI.State
  }

-- | This is an implementation of the BBODY rule as per the chain specification.
--
--   Compared to `updateChain`, this does not validate any header level checks,
--   nor does it carry out anything which might be considered part of the
--   protocol.
updateBody ::
  (MonadError ChainValidationError m, MonadReader ValidationMode m) =>
  BodyEnvironment ->
  BodyState ->
  ABlock ByteString ->
  m BodyState
updateBody env bs b = do
  -- Validate the block size
  blockLength b <= maxBlockSize
    `orThrowErrorInBlockValidationMode` ChainValidationBlockTooLarge maxBlockSize (blockLength b)

  -- Validate the delegation, transaction, and update payload proofs.
  whenBlockValidation (validateBlockProofs b)
    `wrapErrorWithValidationMode` ChainValidationProofValidationError

  -- Update the delegation state
  delegationState' <-
    DI.updateDelegation delegationEnv delegationState certificates
      `wrapError` ChainValidationDelegationSchedulingError

  -- Update the UTxO
  utxo' <-
    UTxO.updateUTxO utxoEnv utxo txs
      `wrapErrorWithValidationMode` ChainValidationUTxOValidationError

  -- Update the update state
  updateState' <-
    UPI.registerUpdate updateEnv updateState updateSignal
      `wrapError` ChainValidationUpdateError currentSlot

  pure $
    BodyState
      { utxo = utxo',
        updateState = updateState',
        delegationState = delegationState'
      }
  where
    BodyEnvironment
      { protocolMagic,
        k,
        allowedDelegators,
        utxoConfiguration,
        currentEpoch
      } = env

    BodyState {utxo, updateState, delegationState} = bs

    maxBlockSize =
      Update.ppMaxBlockSize $ UPI.adoptedProtocolParameters updateState

    currentSlot = blockSlot b

    certificates = Delegation.getPayload $ blockDlgPayload b

    txs = aUnTxPayload $ blockTxPayload b

    delegationEnv =
      DI.Environment
        { DI.protocolMagic = getAProtocolMagicId protocolMagic,
          DI.allowedDelegators = allowedDelegators,
          DI.k = k,
          DI.currentEpoch = currentEpoch,
          DI.currentSlot = currentSlot
        }

    utxoEnv =
      UTxO.Environment
        { UTxO.protocolMagic = protocolMagic,
          UTxO.protocolParameters = UPI.adoptedProtocolParameters updateState,
          UTxO.utxoConfiguration = utxoConfiguration
        }

    updateEnv =
      UPI.Environment
        { UPI.protocolMagic = getAProtocolMagicId protocolMagic,
          UPI.k = k,
          UPI.currentSlot = currentSlot,
          UPI.numGenKeys = toNumGenKeys $ Set.size allowedDelegators,
          UPI.delegationMap = DI.delegationMap delegationState
        }
    updateSignal = UPI.Signal updateProposal updateVotes updateEndorsement

    updateProposal = Update.payloadProposal $ blockUpdatePayload b
    updateVotes = Update.payloadVotes $ blockUpdatePayload b
    updateEndorsement =
      Endorsement (blockProtocolVersion b) (hashKey $ blockIssuer b)

toNumGenKeys :: Integral n => n -> Word8
toNumGenKeys n
  | n > fromIntegral (maxBound :: Word8) = panic "updateBody: Too many genesis keys"
  | otherwise = fromIntegral n

-- | This is an implementation of the headerIsValid function from the Byron
--   chain specification
headerIsValid ::
  (MonadError ChainValidationError m, MonadReader ValidationMode m) =>
  UPI.State ->
  AHeader ByteString ->
  m ()
headerIsValid updateState h =
  -- Validate the header size
  headerLength h <= maxHeaderSize
    `orThrowErrorInBlockValidationMode` ChainValidationHeaderTooLarge maxHeaderSize (headerLength h)
  where
    maxHeaderSize = Update.ppMaxHeaderSize $ UPI.adoptedProtocolParameters updateState

data EpochEnvironment = EpochEnvironment
  { protocolMagic :: !(Annotated ProtocolMagicId ByteString),
    k :: !BlockCount,
    allowedDelegators :: !(Set KeyHash),
    delegationMap :: !Delegation.Map,
    currentEpoch :: !EpochNumber
  }

-- | Perform epoch transition if we have moved across the epoch boundary
--
--   We pass through to the update interface UPIEC rule, which adopts any
--   confirmed proposals and cleans up the state. This corresponds to the EPOCH
--   rules from the Byron chain specification.
epochTransition ::
  EpochEnvironment ->
  UPI.State ->
  SlotNumber ->
  UPI.State
epochTransition env st slot =
  if nextEpoch > currentEpoch
    then UPI.registerEpoch updateEnv st nextEpoch
    else st
  where
    EpochEnvironment {protocolMagic, k, allowedDelegators, delegationMap, currentEpoch} =
      env

    nextEpoch = slotNumberEpoch (kEpochSlots k) slot

    updateEnv =
      UPI.Environment
        { UPI.protocolMagic = protocolMagic,
          UPI.k = k,
          UPI.currentSlot = slot,
          UPI.numGenKeys = toNumGenKeys $ Set.size allowedDelegators,
          UPI.delegationMap = delegationMap
        }

-- | This represents the CHAIN rule. It is intended more for use in tests than
--   in a real implementation, which will want to invoke its constituent rules
--   directly.
--
--   Note that this also updates the previous block hash, which would usually be
--   done as part of the PBFT rule.
updateBlock ::
  (MonadError ChainValidationError m, MonadReader ValidationMode m) =>
  Genesis.Config ->
  ChainValidationState ->
  ABlock ByteString ->
  m ChainValidationState
updateBlock config cvs b = do
  -- Compare the block's 'ProtocolMagic' to the configured value
  blockProtocolMagicId b == configProtocolMagicId config
    `orThrowErrorInBlockValidationMode` ChainValidationProtocolMagicMismatch
      (blockProtocolMagicId b)
      (configProtocolMagicId config)

  -- Process a potential epoch transition
  let updateState' = epochTransition epochEnv (cvsUpdateState cvs) (blockSlot b)

  -- Process header by checking its validity
  headerIsValid updateState' (blockHeader b)

  let bodyEnv =
        BodyEnvironment
          { protocolMagic =
              AProtocolMagic
                (blockAProtocolMagicId b)
                (configReqNetMagic config),
            k = configK config,
            allowedDelegators,
            protocolParameters = UPI.adoptedProtocolParameters updateState',
            utxoConfiguration = Genesis.configUTxOConfiguration config,
            currentEpoch = slotNumberEpoch (configEpochSlots config) (blockSlot b)
          }

      bs =
        BodyState
          { utxo = cvsUtxo cvs,
            updateState = updateState',
            delegationState = cvsDelegationState cvs
          }

  BodyState {utxo, updateState, delegationState} <- updateBody bodyEnv bs b

  pure $
    cvs
      { cvsLastSlot = blockSlot b,
        cvsPreviousHash = Right $! blockHashAnnotated b,
        cvsUtxo = utxo,
        cvsUpdateState = updateState,
        cvsDelegationState = delegationState
      }
  where
    epochEnv =
      EpochEnvironment
        { protocolMagic = blockAProtocolMagicId b,
          k = configK config,
          allowedDelegators,
          delegationMap,
          currentEpoch = slotNumberEpoch (configEpochSlots config) (cvsLastSlot cvs)
        }

    allowedDelegators :: Set KeyHash
    allowedDelegators = unGenesisKeyHashes $ configGenesisKeyHashes config

    delegationMap = DI.delegationMap $ cvsDelegationState cvs

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

data Error
  = ErrorParseError ParseError
  | ErrorUTxOValidationError EpochAndSlotCount UTxO.UTxOValidationError
  deriving (Eq, Show)

-- | Fold transaction validation over a 'Stream' of 'Block's
foldUTxO ::
  UTxO.Environment ->
  UTxO ->
  Stream (Of (ABlock ByteString)) (ExceptT ParseError ResIO) () ->
  ExceptT Error (ReaderT ValidationMode ResIO) UTxO
foldUTxO env utxo blocks =
  S.foldM_
    (foldUTxOBlock env)
    (pure utxo)
    pure
    (pure (hoist (withExceptT ErrorParseError) blocks))

-- | Fold 'updateUTxO' over the transactions in a single 'Block'
foldUTxOBlock ::
  UTxO.Environment ->
  UTxO ->
  ABlock ByteString ->
  ExceptT Error (ReaderT ValidationMode ResIO) UTxO
foldUTxOBlock env utxo block =
  withExceptT
    ( ErrorUTxOValidationError . fromSlotNumber mainnetEpochSlots $
        blockSlot
          block
    )
    $ UTxO.updateUTxO env utxo (aUnTxPayload $ blockTxPayload block)

-- | Size of a heap value, in words
newtype HeapSize a = HeapSize {unHeapSize :: Int}
  deriving (Show)
  deriving newtype (Buildable)

-- | Number of entries in the UTxO
newtype UTxOSize = UTxOSize {unUTxOSize :: Int}
  deriving (Show)
  deriving newtype (Buildable)

calcUTxOSize :: UTxO -> (HeapSize UTxO, UTxOSize)
calcUTxOSize utxo =
  ( HeapSize . heapWords $ unUTxO utxo,
    UTxOSize . M.size $ unUTxO utxo
  )
