{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Properties of the delegation traces induced by the transition systems
-- associated with this aspect of the ledger.
module Ledger.Delegation.Properties
  ( dcertsAreTriggered
  , rejectDupSchedDelegs
  )
where

import Control.Arrow ((&&&))
import Control.Lens ((^.), makeLenses, (&), (.~), view)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog
  ( MonadTest
  , Property
  , (===)
  , assert
  , forAll
  , property
  , withTests
  )
import Hedgehog.Gen (integral)
import Hedgehog.Range (constant, linear)

import Control.State.Transition
  ( Environment
  , PredicateFailure
  , STS
  , Signal
  , State
  , initialRules
  , transitionRules
  , TRC (TRC)
  , judgmentContext
  , (?!)
  , trans
  , Embed
  , wrapFailed
  , applySTS
  )
import Control.State.Transition.Generator
  ( HasSizeInfo
  , HasTrace
  , initEnvGen
  , isTrivial
  , nonTrivialTrace
  , sigGen
  , suchThatLastState
  , trace
  )
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , lastState
  , traceEnv
  , traceSignals
  )
import Ledger.Core
  ( Epoch(Epoch)
  , Owner(Owner)
  , Sig(Sig)
  , Slot(Slot)
  , SlotCount(SlotCount)
  , VKey(VKey)
  , VKeyGenesis
  , VKeyGenesis(VKeyGenesis)
  , addSlot
  , owner
  )
import Ledger.Delegation
  ( DCert
  , DCert(DCert)
  , DELEG
  , DState(DState, _dStateDelegationMap, _dStateLastDelegation)
  , DSState(DSState)
  , _dSStateScheduledDelegations
  , _dSStateKeyEpochDelegations
  , DIState(DIState)
  , _dIStateDelegationMap
  , _dIStateLastDelegation
  , _dIStateScheduledDelegations
  , _dIStateKeyEpochDelegations
  , DSEnv(DSEnv)
  , DSEnv
  , PredicateFailure(IsAlreadyScheduled, SDelegFailure, SDelegSFailure)
  , _dbody
  , _depoch
  , _dwho
  , _dwit
  , delegate
  , delegationMap
  , delegator
  , liveness
  , scheduledDelegations
  , slot
  )
import Ledger.Core.Generator (vkGen)

--------------------------------------------------------------------------------
-- Delegation certification triggering tests
--------------------------------------------------------------------------------

-- | Initial state for the ADELEG and ADELEGS systems
initADelegsState :: DState
initADelegsState = DState
  { _dStateDelegationMap  = Map.empty
  , _dStateLastDelegation = Map.empty
  }

-- | Initial state for the ADELEG and ADELEGS systems
initSDelegsState :: DSState
initSDelegsState = DSState
  { _dSStateScheduledDelegations = []
  , _dSStateKeyEpochDelegations  = Set.empty
  }

-- | Initial state for the DELEG system
initialDIState :: DIState
initialDIState = DIState
  { _dIStateDelegationMap  = _dStateDelegationMap initADelegsState
  , _dIStateLastDelegation = _dStateLastDelegation initADelegsState
  , _dIStateScheduledDelegations = initSDelegsState ^. scheduledDelegations
  , _dIStateKeyEpochDelegations  = _dSStateKeyEpochDelegations initSDelegsState
  }

-- | Delegation blocks. Simple blockchain to test delegation.
data DBLOCK

-- | A delegation block.
data DBlock
  = DBlock
    { _blockSlot  :: Slot
    , _blockCerts :: [DCert]
    }
  deriving (Show, Eq)

makeLenses ''DBlock

-- | This corresponds to a state-transition rule where blocks with increasing
-- slot-numbers are produced.
instance STS DBLOCK where
  type Environment DBLOCK = ()
  type State DBLOCK = (DSEnv, DIState)
  type Signal DBLOCK = DBlock

  data PredicateFailure DBLOCK
    = DPF (PredicateFailure DELEG)
    | NotIncreasingBlockSlot
    deriving (Eq, Show)

  initialRules = [pure (initDSEnv, initialDIState)]

  transitionRules
    = [ do
          TRC (_, (env, st), dblock) <- judgmentContext
          env ^. slot < dblock ^. blockSlot ?! NotIncreasingBlockSlot
          stNext <- trans @DELEG $ TRC (env, st, dblock ^. blockCerts)
          return (env & slot .~ dblock ^. blockSlot, stNext)
      ]

instance Embed DELEG DBLOCK where
  wrapFailed = DPF

-- | Check that all the delegation certificates in the trace were correctly
-- applied.
dcertsAreTriggeredInTrace :: MonadTest m => Trace DBLOCK -> m ()
dcertsAreTriggeredInTrace tr
  = lastDms === trExpectedDms
  where
    lastDms = st ^. delegationMap

    trExpectedDms
      = expectedDms (env ^. slot) (env ^. liveness) (traceSignals OldestFirst tr)

    (env, st) = lastState tr

-- | Compute the expected delegation map after applying the sequence of
-- delegation certificates contained in the given blocks.
--
-- Delegation certificates are applied in the order they appear in the within a
-- block, and blocks are considered in the order they appear on the list passed
-- as parameter.
expectedDms
  :: Slot
  -- ^ Current slot
  -> SlotCount
  -- ^ Delegation certificate liveness parameter.
  -> [DBlock]
  -- ^ Delegation certificates to apply.
  -> Map VKeyGenesis VKey
expectedDms s d cs = Map.fromList (fmap (delegator &&& delegate) activeCerts)
  where
    -- | We keep all the blocks whose certificates should be active given the
    -- current slot.
    activeBlocks :: [DBlock]
    activeBlocks = filter (\b -> b ^. blockSlot <= activationSlot) cs

    -- | Slot at which the certificates become active.
    activationSlot :: Slot
    activationSlot = s `minusSlotCount` d

    -- | Get the active certificates from each block, and concatenate them all
    -- together.
    activeCerts :: [DCert]
    activeCerts = concatMap _blockCerts activeBlocks

minusSlotCount :: Slot -> SlotCount -> Slot
minusSlotCount (Slot s) (SlotCount c)
  | s <= c    = Slot 0
  | otherwise = Slot $ s - c

-- | An initial delegation scheduling environment to be used in the traces
-- produced by the @DBLOCK@ transition system.
initDSEnv :: DSEnv
initDSEnv
  = DSEnv
      (Set.fromAscList $ gk <$> [0..6])
      (Epoch 0)
      (Slot 0)
      (SlotCount 1)
  where
    gk = VKeyGenesis . k
    k n = VKey (Owner n)

instance HasTrace DBLOCK where

  initEnvGen = return ()

  sigGen _ (env, st) = do
    c <- integral (constant 1 10)
    let newSlot = (env ^.slot) `addSlot` SlotCount c
    delegs <- sigGen @DELEG env st
    return $ DBlock newSlot delegs

instance HasSizeInfo DBlock where
  isTrivial = null . view blockCerts

dcertsAreTriggered :: Property
dcertsAreTriggered = withTests 300 $ property $
  -- The number of tests was determined ad-hoc, since the default failed to
  -- uncover the presence of errors.
  forAll nonTrivialTrace >>= dcertsAreTriggeredInTrace

--------------------------------------------------------------------------------
-- Properties related to the transition rules
--------------------------------------------------------------------------------

-- | Reject delegation certificates where a genesis key tries to delegate in
-- the same slot.
--
-- This property tries to generate a trace where the last state contains a
-- non-empty sequence of scheduled delegations. If such trace cannot be
-- generated, then the test will fail when the heap limit is reached, or
-- hedgehog gives up.
rejectDupSchedDelegs :: Property
rejectDupSchedDelegs = property $ do
  (tr, dcert) <- forAll $ do
    tr <- trace @DELEG
          `suchThatLastState` (not . null . view scheduledDelegations)
    let vkS =
          case lastState tr ^. scheduledDelegations of
            (_, (res, _)):_ -> res
            _ -> error $  "This should not happen: "
                       ++ "tr is guaranteed to contain a non-empty sequence of scheduled delegations"
    vkD <- vkGen
    epo <- Epoch <$> integral (linear 0 100)
    let dcert
          = DCert
          { _dbody = (vkD, epo)
          , _dwit = Sig vkS (owner vkS)
          , _dwho = (vkS, vkD)
          , _depoch = epo
          }
    return (tr, dcert)
  let pfs = case applySTS (TRC (tr ^. traceEnv, lastState tr, [dcert])) of
        Left res -> res
        Right _ -> []
  assert $ SDelegSFailure (SDelegFailure IsAlreadyScheduled) `elem` pfs
