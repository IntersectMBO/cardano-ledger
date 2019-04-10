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

import Cardano.Chain.Common (BlockCount, StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (ACertificate)
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting
  ( EpochIndex
  , FlatSlotId(..)
  , addSlotNumber
  )
import Cardano.Crypto
  ( AProxyVerificationKey(..)
  , ProtocolMagicId
  , pskOmega
  , validateProxyVerificationKey
  )


--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic     :: !ProtocolMagicId
  , allowedDelegators :: !(Set StakeholderId)
  , currentEpoch      :: !EpochIndex
  , currentSlot       :: !FlatSlotId
  , k                 :: !BlockCount
  } deriving (Eq, Show, Generic, NFData)

data State = State
  { ssScheduledDelegations :: !(Seq ScheduledDelegation)
  , ssKeyEpochDelegations  :: !(Set (EpochIndex, StakeholderId))
  } deriving (Eq, Show, Generic, NFData)

data ScheduledDelegation = ScheduledDelegation
  { sdSlot      :: !FlatSlotId
  , sdDelegator :: !StakeholderId
  , sdDelegate  :: !StakeholderId
  } deriving (Eq, Show, Generic, NFData)

data Error

  = InvalidCertificate Text
  -- ^ The delegation certificate has an invalid signature

  | MultipleDelegationsForEpoch EpochIndex StakeholderId
  -- ^ This delegator has already delegated for the given epoch

  | MultipleDelegationsForSlot FlatSlotId StakeholderId
  -- ^ This delegator has already delgated in this slot

  | NonGenesisDelegator StakeholderId
  -- ^ This delegator is not one of the allowed genesis keys

  | PastEpoch EpochIndex EpochIndex
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
scheduleCertificate env ss cert = do

  -- Check that the delegator is a genesis key
  (delegator `Set.member` allowedDelegators)
    `orThrowError` NonGenesisDelegator delegator

  -- Check that the delegation epoch is greater than or equal to the current one
  (currentEpoch <= delegationEpoch)
    `orThrowError` PastEpoch currentEpoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  ((delegationEpoch, delegator) `Set.notMember` ssKeyEpochDelegations ss)
    `orThrowError` MultipleDelegationsForEpoch delegationEpoch delegator

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot (ssScheduledDelegations ss))
    `orThrowError` MultipleDelegationsForSlot currentSlot delegator

  -- Check that the delegation certificate is valid
  validateProxyVerificationKey protocolMagic cert `wrapError` InvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $ State
    { ssScheduledDelegations = delegation <| ssScheduledDelegations ss
    , ssKeyEpochDelegations  = Set.insert
      (delegationEpoch, delegator)
      (ssKeyEpochDelegations ss)
    }
 where
  Environment { protocolMagic, allowedDelegators, currentEpoch, currentSlot, k }
    = env

  delegator       = mkStakeholderId $ pskIssuerPk cert
  delegate        = mkStakeholderId $ pskDelegatePk cert

  delegationEpoch = pskOmega cert

  activationSlot  = addSlotNumber (kSlotSecurityParam k) currentSlot

  delegatesThisSlot sd =
    sdSlot sd == activationSlot && sdDelegator sd == delegator

  delegation = ScheduledDelegation activationSlot delegator delegate
