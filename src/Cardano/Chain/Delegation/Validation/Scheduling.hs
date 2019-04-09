{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Chain.Delegation.Validation.Scheduling
  (
  -- * Scheduling
    State(..)
  , Error(..)
  , ScheduledDelegation(..)
  , scheduleCertificate
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (ACertificate)
import Cardano.Chain.Genesis as Genesis
  ( Config
  , GenesisWStakeholders(..)
  , configBootStakeholders
  , configEpochSlots
  , configProtocolMagicId
  )
import Cardano.Chain.Slotting
  ( EpochIndex
  , FlatSlotId(..)
  , EpochSlots(..)
  , addSlotNumber
  , slotNumberEpoch
  )
import Cardano.Crypto
  (AProxyVerificationKey(..), pskOmega, validateProxyVerificationKey)


--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

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
  => Genesis.Config
  -> FlatSlotId
  -> EpochSlots
  -> State
  -> ACertificate ByteString
  -> m State
scheduleCertificate config slot d ss cert = do

  -- Check that the delegator is a genesis key
  (delegator `M.member` genesisStakeholders)
    `orThrowError` NonGenesisDelegator delegator

  -- Check that the delegation epoch is greater than or equal to the current one
  (epoch <= delegationEpoch)
    `orThrowError` PastEpoch epoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  ((delegationEpoch, delegator) `Set.notMember` ssKeyEpochDelegations ss)
    `orThrowError` MultipleDelegationsForEpoch
                    delegationEpoch
                    delegator

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot (ssScheduledDelegations ss))
    `orThrowError` MultipleDelegationsForSlot slot delegator

  -- Check that the delegation certificate is valid
  validateProxyVerificationKey (configProtocolMagicId config) cert
    `wrapError` InvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $ State
    { ssScheduledDelegations = delegation <| ssScheduledDelegations ss
    , ssKeyEpochDelegations  = Set.insert
      (delegationEpoch, delegator)
      (ssKeyEpochDelegations ss)
    }
 where
  delegator = mkStakeholderId $ pskIssuerPk cert
  delegate  = mkStakeholderId $ pskDelegatePk cert

  genesisStakeholders =
    unGenesisWStakeholders $ configBootStakeholders config

  epoch           = slotNumberEpoch (configEpochSlots config) slot
  delegationEpoch = pskOmega cert

  activationSlot  = addSlotNumber d slot

  delegatesThisSlot sd =
    sdSlot sd == activationSlot && sdDelegator sd == delegator

  delegation = ScheduledDelegation activationSlot delegator delegate
