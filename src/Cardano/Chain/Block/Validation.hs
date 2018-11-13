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
  , DelegationState

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
  , blockHashAnnotated
  , blockLeaderKey
  , blockPrevHash
  , blockSignature
  )
import Cardano.Chain.Block.Header
  (BlockSignature(..), HeaderHash, recoverSignedBytes, wrapBoundaryBytes)
import Cardano.Chain.Common (BlockCount(..), StakeholderId, mkStakeholderId)
import Cardano.Chain.Genesis as Genesis
  ( Config(..)
  , GenesisHash(..)
  , GenesisWStakeholders(..)
  , configK
  , configProtocolMagic
  , configBootStakeholders
  )
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
  }

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
  , cvsDelegationState :: DelegationState
  }

initialChainValidationState :: Genesis.Config -> ChainValidationState
initialChainValidationState config = ChainValidationState
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
  , cvsDelegationState = DelegationState
  }

data DelegationState = DelegationState


--------------------------------------------------------------------------------
-- ChainValidationError
--------------------------------------------------------------------------------

data ChainValidationError
  = ChainValidationBoundaryTooLarge
  | ChainValidationInvalidDelegation PublicKey PublicKey
  | ChainValidationInvalidHash HeaderHash HeaderHash
  | ChainValidationInvalidSignature BlockSignature
  | ChainValidationMissingDelegator PublicKey
  | ChainValidationSignatureLight
  | ChainValidationTooManyDelegations PublicKey
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
  unless
    (boundaryPrevHash bvd == prevHash)
    (throwError $ ChainValidationInvalidHash prevHash (boundaryPrevHash bvd))

  -- Validate that the block is within the size bounds
  unless
    (boundaryBlockLength bvd <= 2e6)
    (throwError ChainValidationBoundaryTooLarge)

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


updateChain
  :: MonadError ChainValidationError m
  => Genesis.Config
  -> (DelegationState -> PublicKey -> PublicKey -> Bool)
  -> ChainValidationState
  -> ABlock ByteString
  -> m ChainValidationState
updateChain config delegates cvs b = do
  let
    prevHash = fromMaybe
      (getGenesisHash $ configGenesisHash config)
      (cvsPreviousHash cvs)

  -- Validate the previous block hash of 'b'
  unless
    (blockPrevHash b == prevHash)
    (throwError $ ChainValidationInvalidHash prevHash (blockPrevHash b))

  -- Validate Signature
  (delegator, signer) <- case blockSignature b of

    BlockSignature signature -> do
      let signer = blockLeaderKey b
      unless
        (verifySignatureDecoded
          pm
          SignMainBlock
          signer
          (recoverSignedBytes $ blockHeader b)
          signature
        )
        (throwError $ ChainValidationInvalidSignature (blockSignature b))
      pure (signer, signer)

    BlockPSignatureHeavy signature -> do
      unless
        (proxyVerifyDecoded
          pm
          SignMainBlockHeavy
          signature
          (const True)
          (recoverSignedBytes $ blockHeader b)
        )
        (throwError $ ChainValidationInvalidSignature (blockSignature b))
      let psk = psigPsk signature
      pure (pskIssuerPk psk, pskDelegatePk psk)

  -- Check that the delegation is valid according to the ledger
  unless
    (delegates (cvsDelegationState cvs) delegator signer)
    (throwError $ ChainValidationInvalidDelegation delegator signer)

  let signingHistory = cvsSigningHistory cvs

  -- Check that 'delegator' hasn't delegated too many previous blocks
  let
    byzantineNodes :: BlockCount
    byzantineNodes = 5
  unless
    (checkDelegator byzantineNodes delegator signingHistory)
    (throwError $ ChainValidationTooManyDelegations delegator)

  -- Update the signing history
  let signingHistory' = updateSigningHistory delegator signingHistory

  pure $ ChainValidationState
    { cvsSigningHistory  = signingHistory'
    , cvsPreviousHash    = Just $ blockHashAnnotated b
    , cvsDelegationState = DelegationState
    }
  where pm = configProtocolMagic config
