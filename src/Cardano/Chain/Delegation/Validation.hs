{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Validation of delegation certificates and updating of delegation state
--
--   Delegation is split into two phases, Scheduling and Activation. During the
--   Scheduling phase new delegation certificates are validated and added to a
--   queue of 'ScheduledDelegation's. During the Activation phase
--   'ScheduledDelgation's are added to the active delegation state.
module Cardano.Chain.Delegation.Validation
  (
  -- * Scheduling
    SchedulingState(..)
  , SchedulingError(..)
  , ScheduledDelegation(..)
  , scheduleCertificate

  -- * Activation
  , ActivationState(..)
  , activateDelegation

  -- * Blockchain Interface
  , InterfaceState(..)
  , initialInterfaceState
  , delegates
  , updateDelegation
  )
where

import Cardano.Prelude

import qualified Data.Map as M
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Cardano.Binary.Class (Annotated(..), serialize')
import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (ACertificate, Certificate)
import Cardano.Chain.Genesis as Genesis
  ( Config
  , GenesisDelegation(..)
  , GenesisWStakeholders(..)
  , configBootStakeholders
  , configEpochSlots
  , configHeavyDelegation
  , configProtocolMagic
  )
import Cardano.Chain.Slotting
  ( EpochIndex
  , FlatSlotId
  , SlotCount
  , addSlotNumber
  , slotNumberEpoch
  , subSlotNumber
  )
import Cardano.Crypto
  (AProxySecretKey(..), PublicKey, pskOmega, validateProxySecretKey)


--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data SchedulingState = SchedulingState
  { ssScheduledDelegations :: Seq ScheduledDelegation
  , ssKeyEpochDelegations  :: Set (EpochIndex, StakeholderId)
  } deriving (Eq, Show)

data ScheduledDelegation = ScheduledDelegation
  { sdSlot      :: FlatSlotId
  , sdDelegator :: StakeholderId
  , sdDelegate  :: StakeholderId
  } deriving (Eq, Show)

data SchedulingError

  = SchedulingInvalidCertificate Text
  -- ^ The delegation certificate has an invalid signature

  | SchedulingMultipleDelegationsForEpoch EpochIndex StakeholderId
  -- ^ This delegator has already delegated for the given epoch

  | SchedulingMultipleDelegationsForSlot FlatSlotId StakeholderId
  -- ^ This delegator has already delgated in this slot

  | SchedulingNonGenesisDelegator StakeholderId
  -- ^ This delegator is not one of the allowed genesis keys

  | SchedulingPastEpoch EpochIndex EpochIndex
  -- ^ This delegation is for a past epoch

  deriving (Eq, Show)


-- | Update the delegation 'SchedulingState' with a 'Certificate' if it passes
--   all the validation rules. This is an implementation of the delegation
--   scheduling inference rule from the ledger specification.
scheduleCertificate
  :: MonadError SchedulingError m
  => Genesis.Config
  -> FlatSlotId
  -> SlotCount
  -> SchedulingState
  -> ACertificate ByteString
  -> m SchedulingState
scheduleCertificate config slot d ss cert = do

  -- Check that the delegator is a genesis key
  (delegator `M.member` genesisStakeholders)
    `orThrowError` SchedulingNonGenesisDelegator delegator

  -- Check that the delegation epoch is greater than or equal to the current one
  (epoch <= delegationEpoch)
    `orThrowError` SchedulingPastEpoch epoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  ((delegationEpoch, delegator) `Set.notMember` ssKeyEpochDelegations ss)
    `orThrowError` SchedulingMultipleDelegationsForEpoch
                    delegationEpoch
                    delegator

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot (ssScheduledDelegations ss))
    `orThrowError` SchedulingMultipleDelegationsForSlot slot delegator

  -- Check that the delegation certificate is valid
  validateProxySecretKey (configProtocolMagic config) cert
    `wrapError` SchedulingInvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $ SchedulingState
    { ssScheduledDelegations = delegation <| ssScheduledDelegations ss
    , ssKeyEpochDelegations  = Set.insert
      (delegationEpoch, delegator)
      (ssKeyEpochDelegations ss)
    }
 where
  delegator = mkStakeholderId $ pskIssuerPk cert
  delegate  = mkStakeholderId $ pskDelegatePk cert

  genesisStakeholders =
    getGenesisWStakeholders $ configBootStakeholders config

  epoch           = slotNumberEpoch (configEpochSlots config) slot
  delegationEpoch = pskOmega cert

  activationSlot  = addSlotNumber d slot

  delegatesThisSlot sd =
    sdSlot sd == activationSlot && sdDelegator sd == delegator

  delegation = ScheduledDelegation activationSlot delegator delegate


--------------------------------------------------------------------------------
-- Activation
--------------------------------------------------------------------------------

-- | Maps containing, for each delegator, the active delegation and the slot it
--   became active in.
data ActivationState = ActivationState
  { asDelegationMap   :: Map StakeholderId StakeholderId
  , asDelegationSlots :: Map StakeholderId FlatSlotId
  } deriving (Eq, Show)


-- | Activate a 'ScheduledDelegation' if its activation slot is less than the
--   previous delegation slot for this delegate, otherwise discard it. This is
--   an implementation of the delegation activation rule in the ledger
--   specification.
activateDelegation :: ActivationState -> ScheduledDelegation -> ActivationState
activateDelegation as delegation
  | prevDelegationSlot < slot || slot == 0 = ActivationState
    { asDelegationMap   = M.insert delegator delegate delegationMap
    , asDelegationSlots = M.insert delegator slot delegationSlots
    }
  | otherwise = as
 where
  ActivationState delegationMap delegationSlots = as
  ScheduledDelegation slot delegator delegate   = delegation

  prevDelegationSlot =
    fromMaybe 0 $ M.lookup delegator (asDelegationSlots as)


--------------------------------------------------------------------------------
-- Blockchain Interface
--------------------------------------------------------------------------------

-- | State shared between the blockchain and the ledger
data InterfaceState = InterfaceState
  { isSchedulingState :: SchedulingState
  , isActivationState :: ActivationState
  } deriving (Eq, Show)


-- | The initial state maps each genesis key to itself and overrides this using
--   certificates from the genesis block.
initialInterfaceState
  :: MonadError SchedulingError m => Genesis.Config -> m InterfaceState
initialInterfaceState config = updateDelegation config 0 0 is certificates
 where
  is = InterfaceState
    { isSchedulingState = SchedulingState
      { ssScheduledDelegations = mempty
      , ssKeyEpochDelegations  = mempty
      }
    , isActivationState = ActivationState
      { asDelegationMap   = M.fromList $ zip genesisKeys genesisKeys
      , asDelegationSlots = M.fromList $ (, 0) <$> genesisKeys
      }
    }

  genesisKeys =
    M.keys . getGenesisWStakeholders $ configBootStakeholders config

  certificates =
    fmap annotateCertificate
      . M.elems
      . unGenesisDelegation
      $ configHeavyDelegation config

  annotateCertificate :: Certificate -> ACertificate ByteString
  annotateCertificate c = UnsafeAProxySecretKey
    { aPskOmega     = Annotated (pskOmega c) (serialize' $ pskOmega c)
    , pskIssuerPk   = pskIssuerPk c
    , pskDelegatePk = pskDelegatePk c
    , pskCert       = pskCert c
    }


-- | Check whether a delegation is valid in the 'InterfaceState'
delegates :: InterfaceState -> PublicKey -> PublicKey -> Bool
delegates is delegator delgate =
  case M.lookup (mkStakeholderId delegator) delegationMap of
    Nothing -> False
    Just pk -> pk == mkStakeholderId delgate
  where delegationMap = asDelegationMap $ isActivationState is


-- | Update the 'InterfaceState' with a list of new 'Certificate's. This is an
--   implementation of the delegation interface rule from the ledger
--   specification.
updateDelegation
  :: MonadError SchedulingError m
  => Genesis.Config
  -> FlatSlotId
  -> SlotCount
  -> InterfaceState
  -> [ACertificate ByteString]
  -> m InterfaceState
updateDelegation config slot d is certificates = do

  -- Schedule new certificates
  SchedulingState delegations keyEpochs <- foldM
    (scheduleCertificate config slot d)
    (isSchedulingState is)
    certificates

  -- Activate certificates up to this slot
  let
    as = foldl
      activateDelegation
      (isActivationState is)
      (Seq.filter ((<= slot) . sdSlot) delegations)

  -- Remove stale values from 'SchedulingState'
  let
    inWindow s = subSlotNumber d slot <= s && s <= addSlotNumber d slot
    epoch = slotNumberEpoch (configEpochSlots config) slot

    ss'   = SchedulingState
      { ssScheduledDelegations = Seq.filter (inWindow . sdSlot) delegations
      , ssKeyEpochDelegations  = Set.filter ((>= epoch) . fst) keyEpochs
      }

  pure $ InterfaceState {isSchedulingState = ss', isActivationState = as}
