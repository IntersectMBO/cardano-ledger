{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Cardano.Chain.Delegation.Validation.Interface
  (
  -- * Blockchain Interface
    State(..)
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
import Cardano.Chain.Common (mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (ACertificate, Certificate)
import qualified Cardano.Chain.Delegation.Validation.Activation as Activation
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import Cardano.Chain.Genesis as Genesis
  ( Config
  , GenesisDelegation(..)
  , GenesisWStakeholders(..)
  , configBootStakeholders
  , configEpochSlots
  , configHeavyDelegation
  )
import Cardano.Chain.Slotting
  ( FlatSlotId(..)
  , EpochSlots(..)
  , addSlotNumber
  , slotNumberEpoch
  , subSlotNumber
  )
import Cardano.Crypto
  (AProxyVerificationKey(..), PublicKey, pskOmega)


--------------------------------------------------------------------------------
-- Blockchain Interface
--------------------------------------------------------------------------------

-- | State shared between the blockchain and the ledger
data State = State
  { isSchedulingState :: !Scheduling.State
  , isActivationState :: !Activation.State
  } deriving (Eq, Show, Generic, NFData)


-- | The initial state maps each genesis key to itself and overrides this using
--   certificates from the genesis block.
initialState
  :: MonadError Scheduling.Error m => Genesis.Config -> m State
initialState config = updateDelegation
  config
  (FlatSlotId 0)
  (EpochSlots 0)
  is
  certificates
 where
  is = State
    { isSchedulingState = Scheduling.State
      { Scheduling.ssScheduledDelegations = mempty
      , Scheduling.ssKeyEpochDelegations  = mempty
      }
    , isActivationState = Activation.State
      { Activation.asDelegationMap   = M.fromList $ zip genesisKeys genesisKeys
      , Activation.asDelegationSlots = M.fromList
        $   (, FlatSlotId 0)
        <$> genesisKeys
      }
    }

  genesisKeys =
    M.keys . unGenesisWStakeholders $ configBootStakeholders config

  certificates =
    fmap annotateCertificate
      . M.elems
      . unGenesisDelegation
      $ configHeavyDelegation config

  annotateCertificate :: Certificate -> ACertificate ByteString
  annotateCertificate c = UnsafeAProxyVerificationKey
    { aPskOmega     = Annotated (pskOmega c) (serialize' $ pskOmega c)
    , pskIssuerPk   = pskIssuerPk c
    , pskDelegatePk = pskDelegatePk c
    , pskCert       = pskCert c
    }


-- | Check whether a delegation is valid in the 'State'
delegates :: State -> PublicKey -> PublicKey -> Bool
delegates is delegator delegate =
  case M.lookup (mkStakeholderId delegator) delegationMap of
    Nothing -> False
    Just pk -> pk == mkStakeholderId delegate
  where delegationMap = Activation.asDelegationMap $ isActivationState is


-- | Update the 'State' with a list of new 'Certificate's. This is an
--   implementation of the delegation interface rule from the ledger
--   specification.
updateDelegation
  :: MonadError Scheduling.Error m
  => Genesis.Config
  -> FlatSlotId
  -> EpochSlots
  -> State
  -> [ACertificate ByteString]
  -> m State
updateDelegation config slot d is certificates = do

  -- Schedule new certificates
  Scheduling.State delegations keyEpochs <- foldM
    (Scheduling.scheduleCertificate config slot d)
    (isSchedulingState is)
    certificates

  -- Activate certificates up to this slot
  let
    as = foldl
      Activation.activateDelegation
      (isActivationState is)
      (Seq.filter ((<= slot) . Scheduling.sdSlot) delegations)

  -- Remove stale values from 'Scheduling.State'
  let
    inWindow s = subSlotNumber d slot <= s && s <= addSlotNumber d slot
    epoch = slotNumberEpoch (configEpochSlots config) slot

    ss'   = Scheduling.State
      { Scheduling.ssScheduledDelegations = Seq.filter
        (inWindow . Scheduling.sdSlot)
        delegations
      , Scheduling.ssKeyEpochDelegations  = Set.filter
        ((>= epoch) . fst)
        keyEpochs
      }

  pure $ State {isSchedulingState = ss', isActivationState = as}
