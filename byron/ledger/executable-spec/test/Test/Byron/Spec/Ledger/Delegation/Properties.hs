{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Properties of the delegation traces induced by the transition systems
-- associated with this aspect of the ledger.
module Test.Byron.Spec.Ledger.Delegation.Properties
  ( dcertsAreTriggered,
    dcertsAreNotReplayed,
    rejectDupSchedDelegs,
    tracesAreClassified,
    dblockTracesAreClassified,
    relevantCasesAreCovered,
    onlyValidSignalsAreGenerated,
    invalidSignalsAreGenerated,
  )
where

import Byron.Spec.Ledger.Core
  ( Epoch (Epoch),
    Sig (Sig),
    Slot,
    SlotCount (SlotCount),
    VKey,
    VKeyGenesis,
    addSlot,
    mkVKeyGenesis,
    owner,
    unSlot,
    unSlotCount,
  )
import Byron.Spec.Ledger.Core.Generators (epochGen, slotGen, vkGen)
import qualified Byron.Spec.Ledger.Core.Generators as CoreGen
import Byron.Spec.Ledger.Delegation
import qualified Byron.Spec.Ledger.Delegation.Test
import Byron.Spec.Ledger.GlobalParams (slotsPerEpoch)
import Control.Arrow (first, (***))
import Control.State.Transition
  ( Embed,
    Environment,
    IRC (IRC),
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
    applySTS,
    initialRules,
    judgmentContext,
    trans,
    transitionRules,
    wrapFailed,
    (?!),
  )
import Control.State.Transition.Generator
  ( HasSizeInfo,
    HasTrace,
    SignalGenerator,
    TraceProfile (TraceProfile),
    classifySize,
    classifyTraceLength,
    envGen,
    failures,
    isTrivial,
    nonTrivialTrace,
    proportionOfValidSignals,
    sigGen,
    suchThatLastState,
    trace,
    traceLengthsAreClassified,
    traceWithProfile,
  )
import qualified Control.State.Transition.Generator as Transition.Generator
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (OldestFirst),
    lastState,
    preStatesAndSignals,
    traceEnv,
    traceLength,
    traceSignals,
  )
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Data (Data, Typeable)
import Data.List (foldl')
import Data.List.Unique (repeated)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog
  ( Gen,
    MonadTest,
    Property,
    assert,
    cover,
    forAll,
    property,
    success,
    withTests,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro (to, (&), (.~), (^.))
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

--------------------------------------------------------------------------------
-- Delegation certification triggering tests
--------------------------------------------------------------------------------

-- | Initial state for the ADELEG and ADELEGS systems
initADelegsState :: DState
initADelegsState =
  DState
    { _dStateDelegationMap = Bimap.empty,
      _dStateLastDelegation = Map.empty
    }

-- | Initial state for the ADELEG and ADELEGS systems
initSDelegsState :: DSState
initSDelegsState =
  DSState
    { _dSStateScheduledDelegations = [],
      _dSStateKeyEpochDelegations = Set.empty
    }

-- | Initial state for the DELEG system
initialDIState :: DIState
initialDIState =
  DIState
    { _dIStateDelegationMap = _dStateDelegationMap initADelegsState,
      _dIStateLastDelegation = _dStateLastDelegation initADelegsState,
      _dIStateScheduledDelegations = initSDelegsState ^. scheduledDelegations,
      _dIStateKeyEpochDelegations = _dSStateKeyEpochDelegations initSDelegsState
    }

-- | Delegation blocks. Simple blockchain to test delegation.
data DBLOCK deriving (Data, Typeable)

-- | A delegation block.
data DBlock = DBlock
  { _blockSlot :: Slot,
    _blockCerts :: [DCert]
  }
  deriving (Show, Eq)

makeLenses ''DBlock

data DBlockPredicateFailure
  = DPF (PredicateFailure DELEG)
  | NotIncreasingBlockSlot
  deriving (Eq, Show, Data, Typeable)

-- | This corresponds to a state-transition rule where blocks with increasing
-- slot-numbers are produced.
instance STS DBLOCK where
  type Environment DBLOCK = DSEnv -- The initial environment is only used to bootstrap the initial state.
  type State DBLOCK = (DSEnv, DIState)
  type Signal DBLOCK = DBlock
  type PredicateFailure DBLOCK = DBlockPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        pure (env, initialDIState)
    ]

  transitionRules =
    [ do
        TRC (_, (env, st), dblock) <- judgmentContext
        let nextSlot = dblock ^. blockSlot
        env ^. slot < nextSlot ?! NotIncreasingBlockSlot
        stNext <- trans @DELEG $ TRC (env, st, dblock ^. blockCerts)
        let nextEpoch =
              if _dSEnvK env == 0
                then 0
                else Epoch $ unSlot nextSlot `div` slotsPerEpoch (_dSEnvK env)
        return
          ( env & slot .~ nextSlot
              & epoch .~ nextEpoch,
            stNext
          )
    ]

instance Embed DELEG DBLOCK where
  wrapFailed = DPF

-- | Check that all the delegation certificates in the trace were correctly
-- applied.
dcertsAreTriggeredInTrace :: MonadTest m => Trace DBLOCK -> m ()
dcertsAreTriggeredInTrace tr =
  lastDms === trExpectedDms
  where
    lastDms = st ^. delegationMap

    trExpectedDms =
      expectedDms
        lastSlot
        ((fromIntegral . unSlotCount . liveAfter) (_dSEnvK env))
        slotsAndDcerts

    (env, st) = lastState tr

    lastSlot :: Int
    lastSlot = fst . last $ slotsAndDcerts

    slotsAndDcerts :: [(Int, DBlock)]
    slotsAndDcerts =
      first (view (to fst . slot . to unSlot . to fromIntegral))
        <$> preStatesAndSignals OldestFirst tr

-- | Compute the expected delegation map after applying the sequence of
-- delegation certificates contained in the given blocks.
--
-- Delegation certificates are applied in the order they appear in the within a
-- block, and blocks are considered in the order they appear on the list passed
-- as parameter.
expectedDms ::
  -- | Last slot that should have been considered for certificate activation.
  Int ->
  -- | Delegation certificate liveness parameter.
  Int ->
  -- | Delegation certificates to apply, and the slot at which these
  -- certificates where scheduled.
  [(Int, DBlock)] ->
  Bimap VKeyGenesis VKey
expectedDms s d sbs =
  foldl' insertIfInjective Bimap.empty (fmap delegatorDelegate activeCerts)
  where
    insertIfInjective ::
      Bimap VKeyGenesis VKey ->
      (VKeyGenesis, VKey) ->
      Bimap VKeyGenesis VKey
    insertIfInjective m (k, v) =
      if Bimap.memberR v m
        then m
        else Bimap.insert k v m
    activeCerts :: [DCert]
    activeCerts = concatMap _blockCerts activeBlocks

    activeBlocks :: [DBlock]
    activeBlocks =
      snd
        <$> filter ((<= activationSlot) . fst) sbs

    activationSlot :: Int
    activationSlot = s - d

-- | Check that there are no duplicated certificates in the trace.
dcertsAreNotReplayed :: Property
dcertsAreNotReplayed = withTests 300 $
  property $ do
    let (thisTraceLength, step) = (100, 10)
    sample <- forAll (traceWithProfile @DBLOCK () thisTraceLength profile)
    classifyTraceLength sample thisTraceLength step
    dcertsAreNotReplayedInTrace sample
  where
    dcertsAreNotReplayedInTrace ::
      MonadTest m =>
      Trace DBLOCK ->
      m ()
    dcertsAreNotReplayedInTrace traceSample =
      repeated traceDelegationCertificates === []
      where
        traceDelegationCertificates =
          traceSignals OldestFirst traceSample
            & fmap _blockCerts
            & concat

profile :: TraceProfile DBLOCK
profile =
  TraceProfile
    { proportionOfValidSignals = 95,
      failures = [(5, invalidDBlockGen)]
    }
  where
    invalidDBlockGen :: SignalGenerator DBLOCK
    invalidDBlockGen env (diEnv, diState) =
      DBlock
        <$> nextSlotGen env
        <*> tamperedDcerts diEnv diState -- Gen.list (Range.constant 0 10) (randomDCertGen env)

instance HasTrace DBLOCK where
  envGen
    chainLength =
      DSEnv
        <$> allowedDelegators'
        -- We do not expect the current epoch to have an influence on the tests, so
        -- we chose a small value here.
        <*> epochGen 0 10
        -- As with epochs, the current slot should not have influence in the tests.
        <*> slotGen 0 10
        -- 2160 the value of @k@ used in production. However, delegation certificates are activated
        -- @2*k@ slots from the slot in which they are issued. This means that if we want to see
        -- delegation activations, we need to choose a small value for @k@ since we do not want to blow
        -- up the testing time by using large trace lengths.
        --
        -- Choosing a small @k@ value amounts to picking a large number of epochs. Given a trace length
        -- of @n@, if we have @10k@ slots per-epoch, we can have at most @n `div` 10@ epochs (by
        -- choosing @k == 1@).
        --
        <*> CoreGen.k chainLength (chainLength `div` 10)
      where
        -- We scale the number of delegators linearly up to twice the number of genesis keys we use in
        -- production. Factor 2 is chosen ad-hoc here.
        allowedDelegators' = do
          n <- Gen.integral (Range.linear 0 13)
          pure $! Set.fromAscList $ mkVKeyGenesis <$> [0 .. n]

  sigGen _ (env, st) =
    DBlock <$> nextSlotGen env <*> sigGen @DELEG env st

-- | Generate a next slot. We want the resulting trace to include a large number of epoch changes,
-- so we generate an epoch change with higher frequency.
nextSlotGen :: DSEnv -> Gen Slot
nextSlotGen env =
  incSlot
    <$> Gen.frequency
      [ (1, Gen.integral (Range.constant 1 10)),
        (2, pure $! slotsPerEpoch (_dSEnvK env))
      ]
  where
    incSlot c = (env ^. slot) `addSlot` SlotCount c

instance HasSizeInfo DBlock where
  isTrivial = null . view blockCerts

dcertsAreTriggered :: Property
dcertsAreTriggered =
  withTests 300 $
    property $
      -- The number of tests was determined ad-hoc, since the default failed to
      -- uncover the presence of errors.
      forAll (nonTrivialTrace () 100) >>= dcertsAreTriggeredInTrace

dblockTracesAreClassified :: Property
dblockTracesAreClassified = withTests 200 $
  property $ do
    let (tl, step) = (100, 10)
    tr <- forAll (trace @DBLOCK () tl)
    classifyTraceLength tr tl step
    -- Classify the traces by the total number of delegation certificates on
    -- them.
    classifySize "total dcerts" (traceDCerts tr) (fromIntegral . length) tl step
    success

-- | Extract the delegation certificates in the blocks, in the order they would
-- have been applied.
traceDCertsByBlock :: Trace DBLOCK -> [[DCert]]
traceDCertsByBlock tr = _blockCerts <$> traceSignals OldestFirst tr

-- | Flattended list of DCerts for the given Trace
traceDCerts :: Trace DBLOCK -> [DCert]
traceDCerts = concat . traceDCertsByBlock

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withTests 400 $
  property $ do
    let tl = 100
    tr <- forAll (trace @DBLOCK () tl)

    -- 40% of the traces must contain as many delegation certificates as blocks.
    cover
      40
      "there are at least as many delegation certificates as blocks"
      (traceLength tr <= length (traceDCerts tr))

    -- 50% of the traces must contain at most 25% of blocks with empty delegation payload.
    cover
      50
      "at most 25% of the blocks can contain empty delegation payload"
      (0.25 >= emptyDelegationPayloadRatio (traceDCertsByBlock tr))

    -- 50% of the traces must contain at least 30% of delegations to this epoch.
    cover
      50
      "at least 30% of all certificates delegate in this epoch"
      (0.3 <= thisEpochDelegationsRatio (epochDelegationEpoch tr))

    -- 70% of the traces must contain at least 50% of delegations to the next
    -- epoch.
    cover
      70
      "at least 50% of the certificates delegate in the next epoch"
      (0.5 <= nextEpochDelegationsRatio (epochDelegationEpoch tr))

    -- 30% of the traces must contain at least 30% of self-delegations.
    cover
      30
      "at least 30% of the certificates self delegate"
      (0.3 <= selfDelegationsRatio (traceDCerts tr))

    -- 15% of the traces must contain at least 10% of delegations to the same
    -- delegate.
    cover
      50
      "at least 25% of delegates have multiple delegators"
      (0.05 <= multipleDelegationsRatio (traceDCerts tr))
  where
    -- Get the epoch in which the delegation certificates of the trace were
    -- applied, paired with the epoch of the delegation certificate.
    epochDelegationEpoch :: Trace DBLOCK -> [(Epoch, Epoch)]
    epochDelegationEpoch tr =
      preStatesAndSignals @DBLOCK OldestFirst tr
        & fmap (_dSEnvEpoch . fst *** (fmap depoch . _blockCerts))
        & fmap (\(e, es) -> zip (repeat e) es)
        & concat

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ Transition.Generator.onlyValidSignalsAreGenerated @DBLOCK () 100

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
rejectDupSchedDelegs = withTests 300 $
  property $ do
    (tr, dcert) <- forAll $ do
      tr <-
        trace @DELEG () 100
          `suchThatLastState` (not . null . view scheduledDelegations)
      let vkS =
            case lastState tr ^. scheduledDelegations of
              (_, (res, _)) : _ -> res
              _ ->
                error $
                  "This should not happen: "
                    ++ "tr is guaranteed to contain a non-empty sequence of scheduled delegations"
      vkD <- vkGen
      epo <- Epoch <$> Gen.integral (Range.linear 0 100)
      let dcert = mkDCert vkS (Sig (vkD, epo) (owner vkS)) vkD epo
      return (tr, dcert)
    let pfs = case applySTS @DELEG (TRC (tr ^. traceEnv, lastState tr, [dcert])) of
          Left res -> res
          Right _ -> []
    assert $ SDelegSFailure (SDelegFailure IsAlreadyScheduled) `elem` pfs

-- | Classify the traces.
tracesAreClassified :: Property
tracesAreClassified = traceLengthsAreClassified @DELEG () 1000 100

-- | The signal generator generates invalid signals with high probability when
-- invalid signals are requested.
invalidSignalsAreGenerated :: Property
invalidSignalsAreGenerated =
  withTests 300 $
    Transition.Generator.invalidSignalsAreGenerated
      @DBLOCK
      ()
      (failures profile)
      100
      -- We have 6 failures we're interested in. However demanding an uniform
      -- distribution of predicate failures requires precise tweaking, which is
      -- difficult to guarantee. For this reason we allow for an order of
      -- magnitude deviation from the uniform distribution:
      --
      -- > 1/6 * 0.1 * 100 ~ 1.67
      --
      -- which we round up to 2.
      (Byron.Spec.Ledger.Delegation.Test.coverDelegFailures 2)
