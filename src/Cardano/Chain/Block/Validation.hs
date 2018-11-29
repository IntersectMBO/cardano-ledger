{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumDecimals      #-}
{-# LANGUAGE TupleSections    #-}

module Cardano.Chain.Block.Validation
  ( updateChain
  , updateChainBoundary
  , ChainValidationState
  , initialChainValidationState
  , ChainValidationError

  -- * SigningHistory
  , SigningHistory(..)
  , updateSigningHistory
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (<|))

import Cardano.Chain.Block.Block
  ( ABlock(..)
  , BoundaryValidationData(..)
  , blockDlgPayload
  , blockHashAnnotated
  , blockLeaderKey
  , blockPrevHash
  , blockSignature
  , blockSlot
  )
import Cardano.Chain.Block.Header
  (BlockSignature(..), HeaderHash, recoverSignedBytes, wrapBoundaryBytes)
import Cardano.Chain.Common (BlockCount(..), StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Payload (APayload(..))
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Delegation.Validation
  (delegates, initialInterfaceState, updateDelegation)
import Cardano.Chain.Genesis as Genesis
  ( Config(..)
  , GenesisHash(..)
  , GenesisWStakeholders(..)
  , configBootStakeholders
  , configEpochSlots
  , configK
  , configProtocolMagic
  , configSlotSecurityParam
  )
import Cardano.Chain.Slotting (flattenSlotId)
import Cardano.Crypto
  ( AProxySecretKey(..)
  , AProxySignature(..)
  , PublicKey
  , SignTag(SignMainBlock, SignMainBlockHeavy)
  , hashRaw
  , proxyVerifyDecoded
  , verifySignatureDecoded
  )


--------------------------------------------------------------------------------
-- SigningHistory
--------------------------------------------------------------------------------

-- | The history of signers in the last @K@ blocks
--
--   We maintain a map of the number of blocks signed for each stakeholder to
--   improve performance. The sum of the `BlockCount`s in the map should be
--   equal to the length of the sequence.
data SigningHistory = SigningHistory
  { shK                 :: BlockCount
  , shSigningQueue      :: Seq StakeholderId
  , shStakeholderCounts :: Map StakeholderId BlockCount
  } deriving (Eq, Show)

checkDelegator :: BlockCount -> PublicKey -> SigningHistory -> Bool
checkDelegator byzantineNodes s sh =
  delegatorSlots % 1 < shK sh % byzantineNodes
 where
  delegatorSlots =
    fromMaybe 0 $ M.lookup (mkStakeholderId s) (shStakeholderCounts sh)

-- | Update the `SigningHistory` with a new signer, removing the oldest value if
--   the sequence is @K@ blocks long
updateSigningHistory :: PublicKey -> SigningHistory -> SigningHistory
updateSigningHistory pk sh
  | length (shSigningQueue sh) < fromIntegral (shK sh) = sh & addStakeholderIn
  | otherwise = sh & addStakeholderIn & removeStakeholderOut
 where
  stakeholderIn = mkStakeholderId pk

  addStakeholderIn :: SigningHistory -> SigningHistory
  addStakeholderIn sh' = sh'
    { shSigningQueue      = stakeholderIn <| shSigningQueue sh'
    , shStakeholderCounts = M.adjust
      (+ 1)
      stakeholderIn
      (shStakeholderCounts sh')
    }

  removeStakeholderOut :: SigningHistory -> SigningHistory
  removeStakeholderOut sh' = case shSigningQueue sh' of
    Empty                   -> sh'
    rest :|> stakeholderOut -> sh'
      { shSigningQueue      = rest
      , shStakeholderCounts = M.adjust
        (subtract 1)
        stakeholderOut
        (shStakeholderCounts sh')
      }


--------------------------------------------------------------------------------
-- ChainValidationState
--------------------------------------------------------------------------------

data ChainValidationState = ChainValidationState
  { cvsSigningHistory  :: SigningHistory
  , cvsPreviousHash    :: Maybe HeaderHash
  , cvsDelegationState :: Delegation.InterfaceState
  } deriving (Eq, Show)

initialChainValidationState
  :: MonadError Delegation.SchedulingError m
  => Genesis.Config
  -> m ChainValidationState
initialChainValidationState config = do
  delegationState <- initialInterfaceState config
  pure $ ChainValidationState
    { cvsSigningHistory  = SigningHistory
      { shK                 = configK config
      , shStakeholderCounts = M.fromList
        . map (, 0)
        . M.keys
        . getGenesisWStakeholders
        $ configBootStakeholders config
      , shSigningQueue      = Empty
      }
    , cvsPreviousHash    = Nothing
    , cvsDelegationState = delegationState
    }


--------------------------------------------------------------------------------
-- ChainValidationError
--------------------------------------------------------------------------------

data ChainValidationError

  = ChainValidationBoundaryTooLarge
  -- ^ The size of an epoch boundary block exceeds the limit

  | ChainValidationInvalidDelegation PublicKey PublicKey
  -- ^ The delegation used in the signature is not valid according to the ledger

  | ChainValidationInvalidHash HeaderHash HeaderHash
  -- ^ The hash of the previous block does not match the value in the header

  | ChainValidationInvalidSignature BlockSignature
  -- ^ The signature of the block is invalid

  | ChainValidationDelegationSchedulingError Delegation.SchedulingError
  -- ^ A delegation certificate failed validation in the ledger layer

  | ChainValidationSignatureLight
  -- ^ A block is using unsupported lightweight delegation

  | ChainValidationTooManyDelegations PublicKey
  -- ^ The delegator for this block has delegated in too many recent blocks

  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Validation Functions
--------------------------------------------------------------------------------

updateChainBoundary
  :: MonadError ChainValidationError m
  => Genesis.Config
  -> ChainValidationState
  -> BoundaryValidationData ByteString
  -> m ChainValidationState
updateChainBoundary config cvs bvd = do
  let
    prevHash = fromMaybe
      (getGenesisHash $ configGenesisHash config)
      (cvsPreviousHash cvs)

  -- Validate the previous block hash of 'b'
  (boundaryPrevHash bvd == prevHash)
    `orThrowError` ChainValidationInvalidHash prevHash (boundaryPrevHash bvd)

  -- Validate that the block is within the size bounds
  (boundaryBlockLength bvd <= 2e6)
    `orThrowError` ChainValidationBoundaryTooLarge

  -- Update the previous hash
  pure $ cvs
    { cvsPreviousHash =
      Just
      . coerce
      . hashRaw
      . BSL.fromStrict
      . wrapBoundaryBytes
      $ boundaryHeaderBytes bvd
    }


-- | This is an implementation of the blockchain extension rule from the chain
--   specification. It validates a new block and passes parts of the body
--   through to the ledger for validation.
updateChain
  :: MonadError ChainValidationError m
  => Genesis.Config
  -> ChainValidationState
  -> ABlock ByteString
  -> m ChainValidationState
updateChain config cvs b = do
  let
    prevHash = fromMaybe
      (getGenesisHash $ configGenesisHash config)
      (cvsPreviousHash cvs)

  -- Validate the previous block hash of 'b'
  (blockPrevHash b == prevHash)
    `orThrowError` ChainValidationInvalidHash prevHash (blockPrevHash b)

  -- Validate Signature
  (delegator, signer) <- case blockSignature b of

    BlockSignature signature -> do
      let signer = blockLeaderKey b
      verifySignatureDecoded
          pm
          SignMainBlock
          signer
          (recoverSignedBytes $ blockHeader b)
          signature
        `orThrowError` ChainValidationInvalidSignature (blockSignature b)
      pure (signer, signer)

    BlockPSignatureHeavy signature -> do
      proxyVerifyDecoded
          pm
          SignMainBlockHeavy
          signature
          (const True)
          (recoverSignedBytes $ blockHeader b)
        `orThrowError` ChainValidationInvalidSignature (blockSignature b)
      let psk = psigPsk signature
      pure (pskIssuerPk psk, pskDelegatePk psk)

  -- Check that the delegation is valid according to the ledger
  delegates (cvsDelegationState cvs) delegator signer
    `orThrowError` ChainValidationInvalidDelegation delegator signer

  let signingHistory = cvsSigningHistory cvs

  -- Check that 'delegator' hasn't delegated too many previous blocks
  let
    byzantineNodes :: BlockCount
    byzantineNodes = 5
  checkDelegator byzantineNodes delegator signingHistory
    `orThrowError` ChainValidationTooManyDelegations delegator

  -- Update the signing history
  let signingHistory' = updateSigningHistory delegator signingHistory

  -- Update the delegation state
  delegationState' <-
    updateDelegation config slot d delegationState certificates
      `wrapError` ChainValidationDelegationSchedulingError

  pure $ ChainValidationState
    { cvsSigningHistory  = signingHistory'
    , cvsPreviousHash    = Just $ blockHashAnnotated b
    , cvsDelegationState = delegationState'
    }
 where
  pm              = configProtocolMagic config
  slot            = flattenSlotId (configEpochSlots config) $ blockSlot b
  d               = configSlotSecurityParam config

  delegationState = cvsDelegationState cvs

  certificates    = getPayload $ blockDlgPayload b
