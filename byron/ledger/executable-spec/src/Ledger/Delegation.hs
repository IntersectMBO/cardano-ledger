{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ledger.Delegation
  ( -- * Delegation scheduling
    SDELEG
  , SDELEGS
  , DSState(DSState)
  , _dSStateScheduledDelegations
  , _dSStateKeyEpochDelegations
  , DCert(DCert)
  , _dbody
  , _dwit
  , _dwho
  , _depoch
  , mkDCert
  , delegator
  , delegate
  , dbody
  , dwit
  , dwho
  , depoch
    -- * Delegation activation
  , ADELEG
  , ADELEGS
  , DSEnv
    ( DSEnv
    , _dSEnvAllowedDelegators
    , _dSEnvEpoch
    , _dSEnvSlot
    , _dSEnvStableAfter
    )
  , allowedDelegators
  , DState
    ( DState
    , _dStateDelegationMap
    , _dStateLastDelegation
    )
  -- * Delegation interface
  , DELEG
  , DIEnv
  , DIState(DIState)
  , _dIStateDelegationMap
  , _dIStateLastDelegation
  , _dIStateScheduledDelegations
  , _dIStateKeyEpochDelegations
  , PredicateFailure(SDelegSFailure, SDelegFailure, IsAlreadyScheduled)
  , liveAfter
  -- * State lens fields
  , slot
  , stableAfter
  , delegationMap
  -- * State lens type classes
  , HasScheduledDelegations
  , scheduledDelegations
  , dms
  -- * Generators
  , dcertGen
  , dcertsGen
  -- * Functions on delegation state
  , delegatorOf
  )
where

import Data.AbstractSize
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (constant, linear)
import Control.Lens
  ( Lens'
  , (%~)
  , (&)
  , (.~)
  , (<>~)
  , (^.)
  , _1
  , _2
  , lens
  , makeFields
  , makeLenses
  , to
  )


import Control.State.Transition
  ( Embed
  , Environment
  , PredicateFailure
  , STS
  , Signal
  , State
  , IRC(IRC)
  , TRC(TRC)
  , (?!)
  , initialRules
  , judgmentContext
  , trans
  , transitionRules
  , wrapFailed
  )
import Control.State.Transition.Generator
  ( HasTrace
  , initEnvGen
  , sigGen
  )
import Ledger.Core
  ( Epoch(Epoch)
  , Sig(Sig)
  , Slot(Slot)
  , SlotCount(SlotCount)
  , BlockCount(BlockCount)
  , unBlockCount
  , VKey
  , VKeyGenesis(VKeyGenesis)
  , (⨃)
  , addSlot
  , minusSlot
  , owner
  , owner
  )
import Ledger.Core.Generator (vkGen, vkgenesisGen)

--------------------------------------------------------------------------------
-- Abstract types
--------------------------------------------------------------------------------

-- | A delegation certificate.
data DCert = DCert
  { -- | Body of the delegation certificate
    _dbody :: (VKey, Epoch)
    -- | Witness for the delegation cerfiticate
  , _dwit :: Sig VKeyGenesis
    -- | Who delegates to whom
  , _dwho :: (VKeyGenesis, VKey)
    -- | Certificate epoch
  , _depoch :: Epoch
  } deriving (Show, Eq, Generic)

instance HasTypeReps DCert

makeLenses ''DCert

mkDCert
  :: VKeyGenesis
  -> Sig VKeyGenesis
  -> VKey
  -> Epoch
  -> DCert
mkDCert vkg sigVkg vkd e
  = DCert
  { _dbody = (vkd, e)
  , _dwit = sigVkg
  , _dwho = (vkg, vkd)
  , _depoch = e
  }

-- | Key that is delegating.
delegator :: DCert -> VKeyGenesis
delegator c = c ^. dwho . _1

-- | Key being delegated to.
delegate :: DCert -> VKey
delegate c = c ^. dwho . _2

--------------------------------------------------------------------------------
-- Derived types
--------------------------------------------------------------------------------

-- | Delegation scheduling environment
data DSEnv = DSEnv
  { _dSEnvAllowedDelegators :: Set VKeyGenesis
  , _dSEnvEpoch :: Epoch
  , _dSEnvSlot :: Slot
  , _dSEnvStableAfter :: BlockCount
  } deriving (Show, Eq)

makeFields ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dSStateScheduledDelegations :: [(Slot, (VKeyGenesis, VKey))]
  , _dSStateKeyEpochDelegations :: Set (Epoch, VKeyGenesis)
  } deriving (Show, Eq)

makeFields ''DSState

-- | Delegation state
data DState = DState
  { _dStateDelegationMap :: Map VKeyGenesis VKey
    -- | When was the last time each genesis key delegated.
  , _dStateLastDelegation :: Map VKeyGenesis Slot
  } deriving (Eq, Show)

makeFields ''DState

delegatorOf :: Map VKeyGenesis VKey -> VKey -> Maybe VKeyGenesis
delegatorOf dm vk =
  case Map.keys $ Map.filter (== vk) dm of
    res:_ -> Just res
    []    -> Nothing

-- | Interface environment is the same as scheduling environment.
type DIEnv = DSEnv

data DIState = DIState
  { _dIStateDelegationMap :: Map VKeyGenesis VKey
  , _dIStateLastDelegation :: Map VKeyGenesis Slot
  , _dIStateScheduledDelegations :: [(Slot, (VKeyGenesis, VKey))]
  , _dIStateKeyEpochDelegations :: Set (Epoch, VKeyGenesis)
  } deriving (Show, Eq)

makeFields ''DIState

dms :: HasDelegationMap a (Map VKeyGenesis VKey)
    => Lens' a (Map VKeyGenesis VKey)
dms = delegationMap

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

  initialRules = [ return DSState
                   { _dSStateScheduledDelegations = []
                   , _dSStateKeyEpochDelegations  = Set.empty
                   }
                 ]
  transitionRules =
    [ do
        TRC (env, st, cert) <- judgmentContext
        verify cert ?! DoesNotVerify
        notAlreadyDelegated st cert ?! HasAlreadyDelegated
        let d = liveAfter (env ^. stableAfter)
        notAlreadyScheduled d env st cert ?! IsAlreadyScheduled
        Set.member (cert ^. dwho . _1) (env ^. allowedDelegators) ?! IsNotGenesisKey
        env ^. epoch <= cert ^. depoch ?! IsPastEpoch
        return $ st
          & scheduledDelegations <>~ [((env ^. slot) `addSlot` d
                                      , cert ^. dwho
                                      )]
          & keyEpochDelegations %~ Set.insert (epochDelegator cert)
    ]
    where
      verify :: DCert -> Bool
      verify = const True

      -- Check that this delegator hasn't already delegated this epoch
      notAlreadyDelegated :: DSState -> DCert -> Bool
      notAlreadyDelegated st cert =
        Set.notMember (epochDelegator cert) (st ^. keyEpochDelegations)

      epochDelegator :: DCert -> (Epoch, VKeyGenesis)
      epochDelegator cert = (cert ^. depoch, cert ^. dwho . _1)

      -- Check whether there is a scheduled delegation from this key
      notAlreadyScheduled :: SlotCount -> DSEnv -> DSState -> DCert -> Bool
      notAlreadyScheduled d env st cert =
        List.notElem
          ((env ^. slot) `addSlot` d, cert ^. dwho . _1)
          (st ^. scheduledDelegations . to (fmap $ fmap fst))

-- | Compute after which slot the delegation certificate will become live,
-- using the chain stability parameter.
liveAfter :: BlockCount -> SlotCount
liveAfter bc = SlotCount $ 2 * unBlockCount bc

-- | Delegation rules
data ADELEG

instance STS ADELEG where
  type State ADELEG = DState
  type Signal ADELEG = (Slot, (VKeyGenesis, VKey))
  type Environment ADELEG = Set VKeyGenesis

  data PredicateFailure ADELEG
    = BeforeExistingDelegation
      -- | Not actually a failure; this should just trigger the other rule.
    | NoLastDelegation
      -- | Not a failure; this should just pass the other rule
    | AfterExistingDelegation
    deriving (Eq, Show)

  initialRules = [ do
                     IRC env <- judgmentContext
                     return DState
                       { _dStateDelegationMap  = Map.fromSet (\(VKeyGenesis k) -> k) env
                       , _dStateLastDelegation = Map.fromSet (const (Slot 0)) env
                       }
                 ]
  transitionRules =
    [ do
        TRC (_env, st, (slt, (vks, vkd))) <- judgmentContext
        case Map.lookup vks (st ^. lastDelegation) of
          Nothing -> True ?! BeforeExistingDelegation
          Just sp -> sp < slt ?! BeforeExistingDelegation
        return $ st
          & delegationMap %~ (\sdm -> sdm ⨃ Map.singleton vks vkd)
          & lastDelegation %~ (\ldm -> ldm ⨃ Map.singleton vks slt)
    , do
        (TRC (_env, st, (slt, (vks, _vkd)))) <- judgmentContext
        case Map.lookup vks (st ^. lastDelegation) of
          Just sp -> sp >= slt ?! AfterExistingDelegation
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

  initialRules = [ do
                     IRC env <- judgmentContext
                     trans @SDELEG $ IRC env
                 ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x:xs) -> do
            dss' <- trans @SDELEGS $ TRC (env, st, xs)
            dss'' <- trans @SDELEG $ TRC (env, dss', x)
            return dss''
    ]

instance Embed SDELEG SDELEGS where
  wrapFailed = SDelegFailure

-- | Delegation rules sequencing
data ADELEGS

instance STS ADELEGS where
  type State ADELEGS = DState
  type Signal ADELEGS = [(Slot, (VKeyGenesis, VKey))]
  type Environment ADELEGS = Set VKeyGenesis

  data PredicateFailure ADELEGS
    = ADelegFailure (PredicateFailure ADELEG)
    deriving (Eq, Show)

  initialRules = [ do
                     IRC env <- judgmentContext
                     trans @ADELEG $ IRC env
                 ]
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

  initialRules = [ do
                     IRC env <- judgmentContext
                     initADelegsState <- trans @ADELEGS $ IRC (env ^. allowedDelegators)
                     initSDelegsState <- trans @SDELEGS $ IRC env
                     return DIState
                       { _dIStateDelegationMap  = initADelegsState ^. delegationMap
                       , _dIStateLastDelegation = initADelegsState ^. lastDelegation
                       , _dIStateScheduledDelegations = initSDelegsState ^. scheduledDelegations
                       , _dIStateKeyEpochDelegations  = initSDelegsState ^. keyEpochDelegations
                       }
                 ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        sds <- trans @SDELEGS $ TRC (env, st ^. dIStateDSState, sig)
        let slots = filter ((<= (env ^. slot)) . fst) $ sds ^. scheduledDelegations
        as <- trans @ADELEGS $ TRC (env ^. allowedDelegators, st ^. dIStateDState, slots)
        let d = liveAfter (env ^. stableAfter)
        return $ DIState
          (as ^. delegationMap)
          (as ^. lastDelegation)
          (filter (aboutSlot (env ^. slot) d . fst)
            $ sds ^. scheduledDelegations)
          (Set.filter ((env ^. epoch <=) . fst)
            $ sds ^. keyEpochDelegations)
    ]
    where
      aboutSlot :: Slot -> SlotCount -> (Slot -> Bool)
      aboutSlot a b c = c >= (a `minusSlot` b) && c <= (a `addSlot` b)

instance Embed SDELEGS DELEG where
  wrapFailed = SDelegSFailure

instance Embed ADELEGS DELEG where
  wrapFailed = ADelegSFailure

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate delegation certificates, using the allowed delegators in the
-- environment passed as parameter.
dcertGen :: DSEnv -> Gen DCert
dcertGen env = do
  -- The generated delegator must be one of the genesis keys in the
  -- environment.
  vkS <- Gen.element $ Set.toList (env ^. allowedDelegators)
  vkD <- vkGen
  let Epoch n = env ^. epoch
  m   <- Gen.integral (linear 0 100)
  let epo = Epoch (n + m)
  return DCert { _dbody = (vkD, epo)
               , _dwit = Sig vkS (owner vkS)
               , _dwho = (vkS, vkD)
               , _depoch = epo
               }

-- | Generate a list of delegation certificates.
--
-- At the moment the size of the generated list is severely constrained since
-- the longer the list the higher the probability that it will contain
-- conflicting delegation certificates (that will be rejected by the
-- transition-system-rules).
dcertsGen :: DSEnv -> Gen [DCert]
-- NOTE: at the moment we cannot use a linear range that depends on the
-- generator size: the problem is that the more delegation certificates in the
-- resulting list, the higher the probability that this list will be rejected
-- and the generator will have to retry.
--
dcertsGen env = Gen.list (constant 0 n) (dcertGen env)
  where n = env ^. allowedDelegators . to length

instance HasTrace DELEG where

  initEnvGen
    = DSEnv
    -- We need at least one delegator in the environment to be able to generate
    -- delegation certificates.
    --
    -- The use of a linear generator and its bound is rather arbitrary. The
    -- sizes passed to the `Gen` monad would be around 100~300, so we rather
    -- arbitrarily decided that this size times 100 is a reasonable upper
    -- bounds for epochs.
    --
    -- A similar remark applies to the ranges chosen for slot and slot count
    -- generators.
    <$> Gen.set (linear 1 7) vkgenesisGen
    <*> (Epoch <$> Gen.integral (linear 0 100))
    <*> (Slot <$> Gen.integral (linear 0 10000))
    <*> (BlockCount <$> Gen.integral (linear 0 10))

  sigGen e _st = dcertsGen e
