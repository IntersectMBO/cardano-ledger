{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Block.Validation
  ( updateBody
  , updateChainBlockOrBoundary
  , updateChainBoundary
  , updateHeader
  , updateBlock
  , BodyState(..)
  , BodyEnvironment(..)
  , ChainValidationState(..)
  , initialChainValidationState
  , ChainValidationError
  , HeaderEnvironment(..)

  -- * SigningHistory
  , SigningHistory(..)
  , updateSigningHistory

  -- * UTxO
  , HeapSize(..)
  , UTxOSize(..)
  , calcUTxOSize
  , foldUTxO
  , foldUTxOBlock
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Formatting.Buildable (Buildable)
import Streaming (Of(..), Stream, hoist)
import qualified Streaming.Prelude as S

import Cardano.Binary
  ( Annotated(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  , serialize'
  )
import Cardano.Chain.Block.Body (ABody (..))
import Cardano.Chain.Block.Block
  ( ABlock(..)
  , ABlockOrBoundary(..)
  , BoundaryValidationData(..)
  , blockAProtocolMagicId
  , blockDlgPayload
  , blockHashAnnotated
  , blockHeader
  , blockIssuer
  , blockLength
  , blockProtocolMagicId
  , blockProtocolVersion
  , blockSlot
  , blockTxPayload
  , blockUpdatePayload
  )
import Cardano.Chain.Block.Header
  ( AHeader (..)
  , BlockSignature
  , HeaderHash
  , headerLength
  , headerProof
  , headerSlot
  , wrapBoundaryBytes
  )
import Cardano.Chain.Block.Proof (Proof(..), ProofValidationError (..))
import Cardano.Chain.Common
  ( BlockCount(..)
  , KeyHash
  , hashKey
  )
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as DI
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import Cardano.Chain.Epoch.File (ParseError, mainnetEpochSlots)
import Cardano.Chain.Genesis as Genesis
  ( Config(..)
  , GenesisKeyHashes(..)
  , GenesisHash
  , configGenesisKeyHashes
  , configEpochSlots
  , configHeavyDelegation
  , configK
  , configProtocolMagicId
  )
import Cardano.Chain.ProtocolConstants (kEpochSlots)
import Cardano.Chain.Slotting
  (EpochNumber(..), SlotNumber(..), EpochAndSlotCount(..), slotNumberEpoch, fromSlotNumber)
import Cardano.Chain.UTxO (ATxPayload(..), UTxO(..), genesisUtxo, recoverTxProof)
import qualified Cardano.Chain.UTxO.Validation as UTxO
import qualified Cardano.Chain.Update as Update
import Cardano.Chain.Update.Validation.Endorsement (Endorsement(..))
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import Cardano.Crypto
  ( AProtocolMagic(..)
  , ProtocolMagicId
  , VerificationKey
  , hashRaw
  , hashDecoded
  )
import Cardano.Chain.ValidationMode
  ( ValidationMode
  , orThrowErrorInBlockValidationMode
  , whenBlockValidation
  , wrapErrorWithValidationMode
  )

--------------------------------------------------------------------------------
-- SigningHistory
--------------------------------------------------------------------------------

-- | The history of signers in the last @K@ blocks
--
--   We maintain a map of the number of blocks signed for each keyHash to
--   improve performance. The sum of the `BlockCount`s in the map should be
--   equal to the length of the sequence.
data SigningHistory = SigningHistory
  { shK                 :: !BlockCount
  , shSigningQueue      :: !(Seq KeyHash)
  , shKeyHashCounts     :: !(Map KeyHash BlockCount)
  } deriving (Eq, Show, Generic, NFData)

instance FromCBOR SigningHistory where
  fromCBOR = do
    enforceSize "SigningHistory" 3
    SigningHistory
      <$> fromCBOR
      <*> (Seq.fromList <$> fromCBOR)
      <*> fromCBOR

instance ToCBOR SigningHistory where
  toCBOR sh =
    encodeListLen 3
      <> toCBOR (shK sh)
      <> toCBOR (toList (shSigningQueue sh))
      <> toCBOR (shKeyHashCounts sh)

-- | Update the `SigningHistory` with a new signer, removing the oldest value if
--   the sequence is @K@ blocks long
updateSigningHistory :: VerificationKey -> SigningHistory -> SigningHistory
updateSigningHistory vk sh
  | length (shSigningQueue sh) < fromIntegral (unBlockCount $ shK sh) = sh & addKeyHashIn
  | otherwise = sh & addKeyHashIn & removeKeyHashOut
 where
  keyHashIn = hashKey vk

  addKeyHashIn :: SigningHistory -> SigningHistory
  addKeyHashIn sh' = sh'
    { shSigningQueue      = keyHashIn <| shSigningQueue sh'
    , shKeyHashCounts = M.adjust
      succ
      keyHashIn
      (shKeyHashCounts sh')
    }

  removeKeyHashOut :: SigningHistory -> SigningHistory
  removeKeyHashOut sh' = case shSigningQueue sh' of
    Empty -> sh'
    rest :|> keyHashOut -> sh'
      { shSigningQueue      = rest
      , shKeyHashCounts = M.adjust
        pred
        keyHashOut
        (shKeyHashCounts sh')
      }


--------------------------------------------------------------------------------
-- ChainValidationState
--------------------------------------------------------------------------------

data ChainValidationState = ChainValidationState
  { cvsLastSlot        :: !SlotNumber
  , cvsSigningHistory  :: !SigningHistory
  , cvsPreviousHash    :: !(Either GenesisHash HeaderHash)
  -- ^ GenesisHash for the previous hash of the zeroth boundary block and
  --   HeaderHash for all others.
  , cvsUtxo            :: !UTxO
  , cvsUpdateState     :: !UPI.State
  , cvsDelegationState :: !DI.State
  } deriving (Eq, Show, Generic, NFData)

instance FromCBOR ChainValidationState where
  fromCBOR = do
    enforceSize "ChainValidationState" 6
    ChainValidationState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance ToCBOR ChainValidationState where
  toCBOR c =
    encodeListLen 6
      <> toCBOR (cvsLastSlot c)
      <> toCBOR (cvsSigningHistory c)
      <> toCBOR (cvsPreviousHash c)
      <> toCBOR (cvsUtxo c)
      <> toCBOR (cvsUpdateState c)
      <> toCBOR (cvsDelegationState c)

-- | Create the state needed to validate the zeroth epoch of the chain. The
--   zeroth epoch starts with a boundary block where the previous hash is the
--   genesis hash.
initialChainValidationState
  :: MonadError Scheduling.Error m
  => Genesis.Config
  -> m ChainValidationState
initialChainValidationState config = do
  delegationState <- DI.initialState delegationEnv genesisDelegation
  pure $ ChainValidationState
    { cvsLastSlot       = 0
    , cvsSigningHistory = SigningHistory
      { shK = configK config
      , shKeyHashCounts = M.fromSet (const $ BlockCount 0)
        . unGenesisKeyHashes
        $ configGenesisKeyHashes config
      , shSigningQueue = Empty
      }
    , cvsPreviousHash   = Left $ configGenesisHash config
    , cvsUtxo           = genesisUtxo config
    , cvsUpdateState    = UPI.initialState config
    , cvsDelegationState = delegationState
    }
 where
  delegationEnv = DI.Environment
    { DI.protocolMagic = Annotated pm (serialize' pm)
    , DI.allowedDelegators = unGenesisKeyHashes $ configGenesisKeyHashes config
    , DI.k           = configK config
    , DI.currentEpoch = EpochNumber 0
    , DI.currentSlot = SlotNumber 0
    }

  pm = configProtocolMagicId config

  genesisDelegation = configHeavyDelegation config


--------------------------------------------------------------------------------
-- ChainValidationError
--------------------------------------------------------------------------------

data ChainValidationError

  = ChainValidationBoundaryTooLarge
  -- ^ The size of an epoch boundary block exceeds the limit

  | ChainValidationBlockAttributesTooLarge
  -- ^ The size of a block's attributes is non-zero

  | ChainValidationBlockTooLarge Natural Natural
  -- ^ The size of a regular block exceeds the limit

  | ChainValidationHeaderAttributesTooLarge
  -- ^ The size of a block header's attributes is non-zero

  | ChainValidationHeaderTooLarge Natural Natural
  -- ^ The size of a block header exceeds the limit

  | ChainValidationDelegationPayloadError Text
  -- ^ There is a problem with the delegation payload signature

  | ChainValidationInvalidDelegation VerificationKey VerificationKey
  -- ^ The delegation used in the signature is not valid according to the ledger

  | ChainValidationGenesisHashMismatch GenesisHash GenesisHash
  -- ^ Genesis hash mismatch

  | ChainValidationExpectedGenesisHash GenesisHash HeaderHash
  -- ^ Expected GenesisHash but got HeaderHash

  | ChainValidationExpectedHeaderHash HeaderHash GenesisHash
  -- ^ Expected HeaderHash but GenesisHash

  | ChainValidationInvalidHash HeaderHash HeaderHash
  -- ^ The hash of the previous block does not match the value in the header

  | ChainValidationMissingHash HeaderHash
  -- ^ The hash of the previous block is missing and should be given hash.

  | ChainValidationUnexpectedGenesisHash HeaderHash
  -- ^ There should not be a hash of the previous but there is.

  | ChainValidationInvalidSignature BlockSignature
  -- ^ The signature of the block is invalid

  | ChainValidationDelegationSchedulingError Scheduling.Error
  -- ^ A delegation certificate failed validation in the ledger layer

  | ChainValidationProtocolMagicMismatch ProtocolMagicId ProtocolMagicId
  -- ^ The 'ProtocolMagic' in the block doesn't match the configured one

  | ChainValidationSignatureLight
  -- ^ A block is using unsupported lightweight delegation

  | ChainValidationTooManyDelegations VerificationKey
  -- ^ The delegator for this block has delegated in too many recent blocks

  | ChainValidationUpdateError UPI.Error
  -- ^ Something failed to register in the update interface

  | ChainValidationUTxOValidationError UTxO.UTxOValidationError
  -- ^ A transaction failed validation in the ledger layer

  | ChainValidationProofValidationError ProofValidationError
  -- ^ A payload proof did not match.

  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Validation Functions
--------------------------------------------------------------------------------

updateChainBlockOrBoundary
  :: (MonadError ChainValidationError m, MonadReader ValidationMode m)
  => Genesis.Config
  -> ChainValidationState
  -> ABlockOrBoundary ByteString
  -> m ChainValidationState
updateChainBlockOrBoundary config c b = case b of
  ABOBBoundary bvd   -> updateChainBoundary c bvd
  ABOBBlock    block -> updateBlock config c block


updateChainBoundary
  :: MonadError ChainValidationError m
  => ChainValidationState
  -> BoundaryValidationData ByteString
  -> m ChainValidationState
updateChainBoundary cvs bvd = do
  case (cvsPreviousHash cvs, boundaryPrevHash bvd) of
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
  pure $ cvs
    { cvsPreviousHash =
      Right
      . coerce
      . hashRaw
      . BSL.fromStrict
      . wrapBoundaryBytes
      $ boundaryHeaderBytes bvd
    }


validateHeaderMatchesBody
  :: MonadError ProofValidationError m
  => AHeader ByteString
  -> ABody ByteString
  -> m ()
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

validateBlockProofs
  :: MonadError ProofValidationError m
  => ABlock ByteString
  -> m ()
validateBlockProofs b =
  validateHeaderMatchesBody blockHeader blockBody
 where
  ABlock
    { blockHeader
    , blockBody
    } = b


data BodyEnvironment = BodyEnvironment
  { protocolMagic      :: !(AProtocolMagic ByteString)
  , k                  :: !BlockCount
  , allowedDelegators  :: !(Set KeyHash)
  , protocolParameters :: !Update.ProtocolParameters
  , currentEpoch       :: !EpochNumber
  }

data BodyState = BodyState
  { utxo            :: !UTxO
  , updateState     :: !UPI.State
  , delegationState :: !DI.State
  }

-- | This is an implementation of the BBODY rule as per the chain specification.
--
--   Compared to `updateChain`, this does not validate any header level checks,
--   nor does it carry out anything which might be considered part of the
--   protocol.
updateBody
  :: (MonadError ChainValidationError m, MonadReader ValidationMode m)
  => BodyEnvironment
  -> BodyState
  -> ABlock ByteString
  -> m BodyState
updateBody env bs b = do
  -- Validate the block size
  blockLength b <= maxBlockSize
    `orThrowErrorInBlockValidationMode`
      ChainValidationBlockTooLarge maxBlockSize (blockLength b)

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
      `wrapError` ChainValidationUpdateError

  pure $ BodyState
    { utxo        = utxo'
    , updateState = updateState'
    , delegationState = delegationState'
    }
 where
  BodyEnvironment { protocolMagic, k, allowedDelegators, currentEpoch } = env

  BodyState { utxo, updateState, delegationState } = bs

  maxBlockSize =
    Update.ppMaxBlockSize $ UPI.adoptedProtocolParameters updateState

  currentSlot   = blockSlot b

  certificates  = Delegation.getPayload $ blockDlgPayload b

  txs           = aUnTxPayload $ blockTxPayload b

  delegationEnv = DI.Environment
    { DI.protocolMagic = getAProtocolMagicId protocolMagic
    , DI.allowedDelegators = allowedDelegators
    , DI.k = k
    , DI.currentEpoch = currentEpoch
    , DI.currentSlot = currentSlot
    }

  utxoEnv = UTxO.Environment
    { UTxO.protocolMagic = protocolMagic
    , UTxO.protocolParameters = UPI.adoptedProtocolParameters updateState
    }

  updateEnv = UPI.Environment
    { UPI.protocolMagic = getAProtocolMagicId protocolMagic
    , UPI.k = k
    , UPI.currentSlot = currentSlot
    , UPI.numGenKeys = toNumGenKeys $ Set.size allowedDelegators
    , UPI.delegationMap = DI.delegationMap delegationState
    }
  updateSignal   = UPI.Signal updateProposal updateVotes updateEndorsement

  updateProposal = Update.payloadProposal $ blockUpdatePayload b
  updateVotes    = Update.payloadVotes $ blockUpdatePayload b
  updateEndorsement =
    Endorsement (blockProtocolVersion b) (hashKey $ blockIssuer b)


toNumGenKeys :: Integral n => n -> Word8
toNumGenKeys n
  | n > fromIntegral (maxBound :: Word8) = panic  "updateBody: Too many genesis keys"
  | otherwise = fromIntegral n

data HeaderEnvironment = HeaderEnvironment
  { protocolMagic     :: !(Annotated ProtocolMagicId ByteString)
  , k                 :: !BlockCount
  , allowedDelegators :: !(Set KeyHash)
  , delegationMap     :: !Delegation.Map
  , lastSlot          :: !SlotNumber
  }


-- | This is an implementation of the the BHEAD rule.
updateHeader
  :: (MonadError ChainValidationError m, MonadReader ValidationMode m)
  => HeaderEnvironment
  -> UPI.State
  -> AHeader ByteString
  -> m UPI.State
updateHeader env st h = do
  -- Validate the header size
  headerLength h <= maxHeaderSize
    `orThrowErrorInBlockValidationMode`
      ChainValidationHeaderTooLarge maxHeaderSize (headerLength h)

  -- Perform epoch transition
  pure $! epochTransition epochEnv st (headerSlot h)
 where
  maxHeaderSize = Update.ppMaxHeaderSize $ UPI.adoptedProtocolParameters st

  HeaderEnvironment { protocolMagic, k, allowedDelegators, delegationMap, lastSlot }
    = env

  epochEnv = EpochEnvironment
    { protocolMagic
    , k
    , allowedDelegators
    , delegationMap
    , currentEpoch
    }

  currentEpoch = slotNumberEpoch (kEpochSlots k) lastSlot


data EpochEnvironment = EpochEnvironment
  { protocolMagic     :: !(Annotated ProtocolMagicId ByteString)
  , k                 :: !BlockCount
  , allowedDelegators :: !(Set KeyHash)
  , delegationMap     :: !Delegation.Map
  , currentEpoch      :: !EpochNumber
  }


-- | Perform epoch transition if we have moved across the epoch boundary
--
--   We pass through to the update interface UPIEC rule, which adopts any
--   confirmed proposals and cleans up the state. This corresponds to the EPOCH
--   rules from the Byron chain specification.
epochTransition
  :: EpochEnvironment
  -> UPI.State
  -> SlotNumber
  -> UPI.State
epochTransition env st slot = if nextEpoch > currentEpoch
  then UPI.registerEpoch updateEnv st nextEpoch
  else st
 where
  EpochEnvironment { protocolMagic, k, allowedDelegators, delegationMap, currentEpoch }
    = env

  nextEpoch = slotNumberEpoch (kEpochSlots k) slot

  updateEnv = UPI.Environment
    { UPI.protocolMagic = protocolMagic
    , UPI.k           = k
    , UPI.currentSlot = slot
    , UPI.numGenKeys  = toNumGenKeys $ Set.size allowedDelegators
    , UPI.delegationMap = delegationMap
    }


-- | This represents the CHAIN rule. It is intended more for use in tests than
--   in a real implementation, which will want to invoke its constituent rules
--   directly.
--
--   Note that this also updates the previous block hash, which would usually be
--   done as part of the PBFT rule.
updateBlock
  :: (MonadError ChainValidationError m, MonadReader ValidationMode m)
  => Genesis.Config
  -> ChainValidationState
  -> ABlock ByteString
  -> m ChainValidationState
updateBlock config cvs b = do

  -- Compare the block's 'ProtocolMagic' to the configured value
  blockProtocolMagicId b == configProtocolMagicId config
    `orThrowErrorInBlockValidationMode`
      ChainValidationProtocolMagicMismatch
        (blockProtocolMagicId b)
        (configProtocolMagicId config)

  -- Update the header
  updateState' <- updateHeader headerEnv (cvsUpdateState cvs) (blockHeader b)

  let
    bodyEnv = BodyEnvironment
      { protocolMagic = AProtocolMagic
        (blockAProtocolMagicId b)
        (configReqNetMagic config)
      , k          = configK config
      , allowedDelegators
      , protocolParameters = UPI.adoptedProtocolParameters updateState'
      , currentEpoch = slotNumberEpoch (configEpochSlots config) (blockSlot b)
      }

    bs = BodyState
      { utxo        = cvsUtxo cvs
      , updateState = updateState'
      , delegationState = cvsDelegationState cvs
      }

  BodyState { utxo, updateState, delegationState } <- updateBody bodyEnv bs b

  pure $ cvs
    { cvsLastSlot     = blockSlot b
    , cvsPreviousHash = Right $! blockHashAnnotated b
    , cvsUtxo         = utxo
    , cvsUpdateState  = updateState
    , cvsDelegationState = delegationState
    }
 where
  headerEnv = HeaderEnvironment
    { protocolMagic = blockAProtocolMagicId b
    , k          = configK config
    , allowedDelegators
    , delegationMap
    , lastSlot   = cvsLastSlot cvs
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
foldUTxO
  :: UTxO.Environment
  -> UTxO
  -> Stream (Of (ABlock ByteString)) (ExceptT ParseError ResIO) ()
  -> ExceptT Error (ReaderT ValidationMode ResIO) UTxO
foldUTxO env utxo blocks = S.foldM_
  (foldUTxOBlock env)
  (pure utxo)
  pure
  (pure (hoist (withExceptT ErrorParseError) blocks))

-- | Fold 'updateUTxO' over the transactions in a single 'Block'
foldUTxOBlock
  :: UTxO.Environment
  -> UTxO
  -> ABlock ByteString
  -> ExceptT Error (ReaderT ValidationMode ResIO) UTxO
foldUTxOBlock env utxo block =
  withExceptT
    (ErrorUTxOValidationError . fromSlotNumber mainnetEpochSlots $ blockSlot
      block
    )
  $ UTxO.updateUTxO env utxo (aUnTxPayload $ blockTxPayload block)

-- | Size of a heap value, in words
newtype HeapSize a =
  HeapSize { unHeapSize :: Int}
  deriving Show
  deriving newtype Buildable

-- | Number of entries in the UTxO
newtype UTxOSize =
  UTxOSize { unUTxOSize :: Int}
  deriving Show
  deriving newtype Buildable

calcUTxOSize :: UTxO -> (HeapSize UTxO, UTxOSize)
calcUTxOSize utxo =
  ( HeapSize . heapWords $ unUTxO utxo
  , UTxOSize . M.size $ unUTxO utxo
  )
