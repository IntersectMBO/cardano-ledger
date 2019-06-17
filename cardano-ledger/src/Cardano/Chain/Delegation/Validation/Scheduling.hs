{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Cardano.Chain.Delegation.Validation.Scheduling
  (
  -- * Scheduling
    Environment(..)
  , State(..)
  , Error(..)
  , ScheduledDelegation(..)
  , scheduleCertificate
  )
where

import Cardano.Prelude hiding (State)

import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Cardano.Binary (Annotated)
import Cardano.Chain.Common (BlockCount, KeyHash, hashKey)
import Cardano.Chain.Delegation.Certificate (ACertificate)
import qualified Cardano.Chain.Delegation.Certificate as Certificate
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting
  ( EpochNumber
  , SlotNumber(..)
  , addSlotCount
  )
import Cardano.Crypto (ProtocolMagicId)


--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic     :: !(Annotated ProtocolMagicId ByteString)
  , allowedDelegators :: !(Set KeyHash)
  , currentEpoch      :: !EpochNumber
  , currentSlot       :: !SlotNumber
  , k                 :: !BlockCount
  } deriving (Eq, Show, Generic, NFData)

data State = State
  { scheduledDelegations :: !(Seq ScheduledDelegation)
  , keyEpochDelegations  :: !(Set (EpochNumber, KeyHash))
  } deriving (Eq, Show, Generic, NFData)

data ScheduledDelegation = ScheduledDelegation
  { sdSlot      :: !SlotNumber
  , sdDelegator :: !KeyHash
  , sdDelegate  :: !KeyHash
  } deriving (Eq, Show, Generic, NFData)

data Error

  = InvalidCertificate
  -- ^ The delegation certificate has an invalid signature

  | MultipleDelegationsForEpoch EpochNumber KeyHash
  -- ^ This delegator has already delegated for the given epoch

  | MultipleDelegationsForSlot SlotNumber KeyHash
  -- ^ This delegator has already delgated in this slot

  | NonGenesisDelegator KeyHash
  -- ^ This delegator is not one of the allowed genesis keys

  | PastEpoch EpochNumber EpochNumber
  -- ^ This delegation is for a past epoch

  deriving (Eq, Show)


-- | Update the delegation 'State' with a 'Certificate' if it passes
--   all the validation rules. This is an implementation of the delegation
--   scheduling inference rule from the ledger specification.
scheduleCertificate
  :: MonadError Error m
  => Environment
  -> State
  -> ACertificate ByteString
  -> m State
scheduleCertificate env st cert = do
  -- Check that the delegator is a genesis key
  delegator `Set.member` allowedDelegators
    `orThrowError` NonGenesisDelegator delegator

  -- Check that the delegation epoch is greater than or equal to the current one
  currentEpoch <= delegationEpoch
    `orThrowError` PastEpoch currentEpoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  (delegationEpoch, delegator) `Set.notMember` keyEpochDelegations
    `orThrowError` MultipleDelegationsForEpoch delegationEpoch delegator

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot scheduledDelegations)
    `orThrowError` MultipleDelegationsForSlot currentSlot delegator

  -- Check that the delegation certificate is valid
  Certificate.isValid protocolMagic cert `orThrowError` InvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $ State
    { scheduledDelegations = delegation <| scheduledDelegations
    , keyEpochDelegations  = Set.insert
      (delegationEpoch, delegator)
      keyEpochDelegations
    }
 where
  Environment { protocolMagic, allowedDelegators, currentEpoch, currentSlot, k }
    = env

  State { scheduledDelegations, keyEpochDelegations } = st

  delegator       = hashKey $ Certificate.issuerVK cert
  delegate        = hashKey $ Certificate.delegateVK cert

  delegationEpoch = Certificate.epoch cert

  activationSlot  = addSlotCount (kSlotSecurityParam k) currentSlot

  delegatesThisSlot sd =
    sdSlot sd == activationSlot && sdDelegator sd == delegator

  delegation = ScheduledDelegation activationSlot delegator delegate
