{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

module Cardano.Chain.Delegation.Validation.Interface
  (
  -- * Blockchain Interface
    Environment(..)
  , State(..)
  , delegationMap
  , initialState
  , delegates
  , updateDelegation
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Cardano.Binary (Annotated(..), serialize')
import Cardano.Chain.Common (BlockCount, StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (ACertificate, Certificate)
import qualified Cardano.Chain.Delegation.Validation.Activation as Activation
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import Cardano.Chain.Genesis (GenesisDelegation(..))
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting
  ( EpochIndex
  , FlatSlotId(..)
  , addSlotNumber
  , subSlotNumber
  )
import Cardano.Crypto
  (AProxyVerificationKey(..), ProtocolMagicId, VerificationKey, pskOmega)


--------------------------------------------------------------------------------
-- Blockchain Interface
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic      :: !(Annotated ProtocolMagicId ByteString)
  , allowedDelegators  :: !(Set StakeholderId)
  , k                  :: !BlockCount
  , currentEpoch       :: !EpochIndex
  , currentSlot        :: !FlatSlotId
  } deriving (Eq, Show, Generic, NFData)


-- | State shared between the blockchain and the ledger
data State = State
  { isSchedulingState :: !Scheduling.State
  , isActivationState :: !Activation.State
  } deriving (Eq, Show, Generic, NFData)


delegationMap :: State -> Map StakeholderId StakeholderId
delegationMap = Activation.asDelegationMap . isActivationState


-- | The initial state maps each genesis key to itself and overrides this using
--   certificates from the genesis block.
initialState
  :: MonadError Scheduling.Error m
  => Environment
  -> GenesisDelegation
  -> m State
initialState env genesisDelegation = updateDelegation env is certificates
 where
  Environment { allowedDelegators } = env

  is = State
    { isSchedulingState = Scheduling.State
      { Scheduling.ssScheduledDelegations = mempty
      , Scheduling.ssKeyEpochDelegations  = mempty
      }
    , isActivationState = Activation.State
      { Activation.asDelegationMap   = M.fromList
        $ zip (toList allowedDelegators) (toList allowedDelegators)
      , Activation.asDelegationSlots = M.fromList
        $   (, FlatSlotId 0)
        <$> toList allowedDelegators
      }
    }

  certificates =
    fmap annotateCertificate . M.elems $ unGenesisDelegation genesisDelegation

  annotateCertificate :: Certificate -> ACertificate ByteString
  annotateCertificate c = UnsafeAProxyVerificationKey
    { aPskOmega     = Annotated (pskOmega c) (serialize' $ pskOmega c)
    , pskIssuerVK   = pskIssuerVK c
    , pskDelegateVK = pskDelegateVK c
    , pskCert       = pskCert c
    }


-- | Check whether a delegation is valid in the 'State'
delegates :: State -> VerificationKey -> VerificationKey -> Bool
delegates is delegator delegate =
  case M.lookup (mkStakeholderId delegator) (delegationMap is) of
    Nothing -> False
    Just vk -> vk == mkStakeholderId delegate


-- | Update the 'State' with a list of new 'Certificate's
--
--   This corresponds to the `DELEG` rule from the Byron ledger specification
updateDelegation
  :: MonadError Scheduling.Error m
  => Environment
  -> State
  -> [ACertificate ByteString]
  -> m State
updateDelegation env is certificates = do

  -- Schedule new certificates
  Scheduling.State delegations keyEpochs <- foldM
    (Scheduling.scheduleCertificate schedulingEnv)
    (isSchedulingState is)
    certificates

  -- Activate certificates up to this slot
  let
    as = foldl
      Activation.activateDelegation
      (isActivationState is)
      (Seq.filter ((<= currentSlot) . Scheduling.sdSlot) delegations)

  -- Remove stale values from 'Scheduling.State'
  let
    ss' = Scheduling.State
      { Scheduling.ssScheduledDelegations = Seq.filter
        (inWindow . Scheduling.sdSlot)
        delegations
      , Scheduling.ssKeyEpochDelegations  = Set.filter
        ((>= currentEpoch) . fst)
        keyEpochs
      }

  pure $ State {isSchedulingState = ss', isActivationState = as}
 where
  Environment { protocolMagic, allowedDelegators, k, currentEpoch, currentSlot }
    = env

  inWindow s =
    subSlotNumber d currentSlot <= s && s <= addSlotNumber d currentSlot

  d = kSlotSecurityParam k

  schedulingEnv = Scheduling.Environment
    { Scheduling.protocolMagic = protocolMagic
    , Scheduling.allowedDelegators = allowedDelegators
    , Scheduling.currentEpoch = currentEpoch
    , Scheduling.currentSlot = currentSlot
    , Scheduling.k           = k
    }
