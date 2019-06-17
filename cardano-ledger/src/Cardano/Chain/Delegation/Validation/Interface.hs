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
import Cardano.Chain.Common (BlockCount(..), KeyHash, hashKey)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Delegation.Certificate (ACertificate, Certificate)
import qualified Cardano.Chain.Delegation.Validation.Activation as Activation
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import Cardano.Chain.Genesis (GenesisDelegation(..))
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting
  ( EpochNumber
  , SlotNumber(..)
  , addSlotCount
  , subSlotCount
  )
import Cardano.Crypto (ProtocolMagicId, VerificationKey)


--------------------------------------------------------------------------------
-- Blockchain Interface
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic      :: !(Annotated ProtocolMagicId ByteString)
  , allowedDelegators  :: !(Set KeyHash)
  , k                  :: !BlockCount
  , currentEpoch       :: !EpochNumber
  , currentSlot        :: !SlotNumber
  } deriving (Eq, Show, Generic, NFData)


-- | State shared between the blockchain and the ledger
data State = State
  { schedulingState :: !Scheduling.State
  , activationState :: !Activation.State
  } deriving (Eq, Show, Generic, NFData)


delegationMap :: State -> Delegation.Map
delegationMap = Activation.delegationMap . activationState


-- | The initial state maps each genesis key to itself and overrides this using
--   certificates from the genesis block.
initialState
  :: MonadError Scheduling.Error m
  => Environment
  -> GenesisDelegation
  -> m State
initialState env genesisDelegation = updateDelegation env' is certificates
 where
  Environment { allowedDelegators } = env
  -- We modify the environment here to allow the delegation certificates to
  -- be applied immediately. Since the environment is not propagated, this
  -- should be harmless.
  env' = env { k = BlockCount 0 }

  is = State
    { schedulingState = Scheduling.State
      { Scheduling.scheduledDelegations = mempty
      , Scheduling.keyEpochDelegations  = mempty
      }
    , activationState = Activation.State
      { Activation.delegationMap   = Delegation.fromList
        $ zip (toList allowedDelegators) (toList allowedDelegators)
      , Activation.delegationSlots = M.fromList
        $   (, SlotNumber 0)
        <$> toList allowedDelegators
      }
    }

  certificates =
    fmap annotateCertificate . M.elems $ unGenesisDelegation genesisDelegation

  annotateCertificate :: Certificate -> ACertificate ByteString
  annotateCertificate c = c
    { Delegation.aEpoch = Annotated
      (Delegation.epoch c)
      (serialize' $ Delegation.epoch c)
    }


-- | Check whether a delegation is valid in the 'State'
delegates :: State -> VerificationKey -> VerificationKey -> Bool
delegates is delegator delegate =
  (hashKey delegator, hashKey delegate)
    `Delegation.pairMember` delegationMap is


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
    (schedulingState is)
    certificates

  -- Activate certificates up to this slot
  let
    as = foldl
      Activation.activateDelegation
      (activationState is)
      (Seq.filter ((<= currentSlot) . Scheduling.sdSlot) delegations)

  -- Remove stale values from 'Scheduling.State'
  let
    ss' = Scheduling.State
      { Scheduling.scheduledDelegations = Seq.filter
        (inWindow . Scheduling.sdSlot)
        delegations
      , Scheduling.keyEpochDelegations  = Set.filter
        ((>= currentEpoch) . fst)
        keyEpochs
      }

  pure $ State {schedulingState = ss', activationState = as}
 where
  Environment { protocolMagic, allowedDelegators, k, currentEpoch, currentSlot }
    = env

  inWindow s = subSlotCount d currentSlot <= s && s <= addSlotCount d currentSlot

  d = kSlotSecurityParam k

  schedulingEnv = Scheduling.Environment
    { Scheduling.protocolMagic = protocolMagic
    , Scheduling.allowedDelegators = allowedDelegators
    , Scheduling.currentEpoch = currentEpoch
    , Scheduling.currentSlot = currentSlot
    , Scheduling.k           = k
    }
