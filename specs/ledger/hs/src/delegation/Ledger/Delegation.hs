{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ledger.Delegation where

import Control.Lens
import Control.State.Transition
import Ledger.Signatures
import Ledger.Core
import Numeric.Natural (Natural)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Abstract types
--------------------------------------------------------------------------------

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGen = VKeyGen VKey
  deriving (Eq, Ord, Show)


data DCert = DCert
  { -- | Body of the delegation certificate
    _dbody :: (VKey, Epoch)
    -- | Witness for the delegation cerfiticate
  , _dwit :: Sig VKeyGen
    -- | Who delegates to whom
  , _dwho :: (VKeyGen, VKey)
    -- | Certificate epoch
  , _depoch :: Epoch
  }

makeLenses ''DCert

--------------------------------------------------------------------------------
-- Derived types
--------------------------------------------------------------------------------

-- | Delegation scheduling environment
data DSEnv = DSEnv
  { _dSEnvAllowedDelegators :: Set VKeyGen
  , _dSEnvEpoch :: Epoch
  , _dSEnvSlot :: Slot
  , _dSEnvLiveness :: SlotCount
  }

makeFields ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dSStateScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dSStateKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeFields ''DSState

-- | Delegation state
data DState = DState
  { _dStateDelegationMap :: Map VKeyGen VKey
    -- | When was the last time each genesis key delegated.
  , _dStateLastDelegation :: Map VKeyGen Slot
  }

makeFields ''DState

-- | Interface environment is the same as scheduling environment.
type DIEnv = DSEnv

data DIState = DIState
  { _dIStateDelegationMap :: Map VKeyGen VKey
  , _dIStateLastDelegation :: Map VKeyGen Slot
  , _dIStateScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dIStateKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeFields ''DIState

dIStateDSState :: Lens' DIState DSState
dIStateDSState = lens
  (\dis -> DSState (dis ^. scheduledDelegations) (dis ^. keyEpochDelegations))
  (\dis dss ->
    dis
      &  scheduledDelegations
      .~ dss
      ^. scheduledDelegations
      &  keyEpochDelegations
      .~ dss
      ^. keyEpochDelegations
  )

dIStateDState :: Lens' DIState DState
dIStateDState = lens
  (\dis -> DState (dis ^. delegationMap) (dis ^. lastDelegation))
  (\dis dss ->
    dis
      &  delegationMap
      .~ dss
      ^. delegationMap
      &  lastDelegation
      .~ dss
      ^. lastDelegation
  )

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------

-- | Delegation scheduling rules
data SDELEG

instance STS SDELEG where
  type State SDELEG = DSState
  type Signal SDELEG = DCert
  type Environment SDELEG = DSEnv

  data PredicateFailure SDELEG
    = IsNotGenesisKey
    | IsPastEpoch
    | HasAlreadyDelegated
    | IsAlreadyScheduled
    | DoesNotVerify
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (env, st, cert) <- judgmentContext
        verify cert ?! DoesNotVerify
        notAlreadyDelegated st cert ?! HasAlreadyDelegated
        notAlreadyScheduled env st cert ?! IsAlreadyScheduled
        Set.member (cert ^. dwho . _1) (env ^. allowedDelegators) ?! IsNotGenesisKey
        env ^. epoch <= cert ^. depoch ?! IsPastEpoch
        return $ st
          & scheduledDelegations <>~ [( ((env ^. slot) `addSlot` (env ^. liveness))
                                         , cert ^. dwho
                                        )]
          & keyEpochDelegations %~ (Set.insert (env ^. epoch, cert ^. dwho . _1))
    ]
    where
      verify :: DCert -> Bool
      verify = const True
      -- Check that this delegator hasn't already delegated this epoch
      notAlreadyDelegated :: DSState -> DCert -> Bool
      notAlreadyDelegated st cert = not $ Set.member (cert ^. depoch, cert ^. dwho . _1) (st ^. keyEpochDelegations)
      -- Check that there is not already a scheduled delegation from this key
      notAlreadyScheduled :: DSEnv -> DSState -> DCert -> Bool
      notAlreadyScheduled env st cert =
        List.elem
          (((env ^. slot) `addSlot` (env ^. liveness)), cert ^. dwho . _1)
          (st ^. scheduledDelegations . to (fmap $ fmap fst))

-- | Delegation rules
data ADELEG

instance STS ADELEG where
  type State ADELEG = DState
  type Signal ADELEG = (Slot, (VKeyGen, VKey))
  type Environment ADELEG = ()

  data PredicateFailure ADELEG
    = BeforeExistingDelegation
      -- | Not actually a failure; this should just trigger the other rule.
    | NoLastDelegation
      -- | Not a failure; this should just pass the other rule
    | AfterExistingDelegation
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (env, st, (slot, (vks, vkd))) <- judgmentContext
        case Map.lookup vks (st ^. lastDelegation) of
          Nothing -> True ?! BeforeExistingDelegation
          Just sp -> sp < slot ?! BeforeExistingDelegation
        return $ st
          & delegationMap %~ (\sdm -> sdm ⨃ Map.singleton vks vkd)
          & lastDelegation %~ (\ldm -> ldm ⨃ Map.singleton vks slot)
    , do
        jc@(TRC (env, st, (slot, (vks, vkd)))) <- judgmentContext
        case Map.lookup vks (st ^. lastDelegation) of
          Just sp -> sp >= slot ?! AfterExistingDelegation
          Nothing -> False ?! NoLastDelegation
        return st
    ]

-- | Delegation scheduling sequencing
data SDELEGS

instance STS SDELEGS where
  type State SDELEGS = DSState
  type Signal SDELEGS = [DCert]
  type Environment SDELEGS = DSEnv

  data PredicateFailure SDELEGS
    = SDelegFailure (PredicateFailure SDELEG)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x:xs) -> do
            dss' <- trans @SDELEGS $ TRC (env, st, xs)
            dss'' <- trans @SDELEG $ TRC (env, dss', x)
            return dss'
    ]

instance Embed SDELEG SDELEGS where
  wrapFailed = SDelegFailure

-- | Delegation rules sequencing
data ADELEGS

instance STS ADELEGS where
  type State ADELEGS = DState
  type Signal ADELEGS = [(Slot, (VKeyGen, VKey))]
  type Environment ADELEGS = ()

  data PredicateFailure ADELEGS
    = ADelegFailure (PredicateFailure ADELEG)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x:xs) -> do
            ds' <- trans @ADELEGS $ TRC (env, st, xs)
            ds'' <- trans @ADELEG $ TRC (env, ds', x)
            return ds''
    ]

instance Embed ADELEG ADELEGS where
  wrapFailed = ADelegFailure

-- | Delegation interface
data DELEG

instance STS DELEG where
  type State DELEG = DIState
  type Signal DELEG = [DCert]
  type Environment DELEG = DIEnv

  data PredicateFailure DELEG
    = SDelegSFailure (PredicateFailure SDELEGS)
    | ADelegSFailure (PredicateFailure ADELEGS)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        sds <- trans @SDELEGS $ TRC (env, st ^. dIStateDSState, sig)
        let slots = filter ((< (env ^. slot)) . fst) $ sds ^. scheduledDelegations
        dms <- trans @ADELEGS $ TRC ((), st ^. dIStateDState, slots)
        return $ DIState
          (dms ^. delegationMap)
          (dms ^. lastDelegation)
          (filter (aboutSlot (env ^. slot) (env ^. liveness) . fst)
            $ sds ^. scheduledDelegations)
          (Set.filter ((< (env ^. epoch)) . fst)
            $ sds ^. keyEpochDelegations)
    ]
    where
      aboutSlot :: Slot -> SlotCount -> (Slot -> Bool)
      aboutSlot a b c = c >= (a `minusSlot` b) && c <= (a `addSlot` b)

instance Embed SDELEGS DELEG where
  wrapFailed = SDelegSFailure

instance Embed ADELEGS DELEG where
  wrapFailed = ADelegSFailure
