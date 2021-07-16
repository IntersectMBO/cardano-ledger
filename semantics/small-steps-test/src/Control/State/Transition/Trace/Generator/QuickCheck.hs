{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.State.Transition.Trace.Generator.QuickCheck
  ( HasTrace (BaseEnv, envGen, sigGen, shrinkSignal, interpretSTS),
    traceFrom,
    traceFromInitState,
    trace,
    shrinkTrace,

    -- * Trace generator properties
    forAllTrace,
    forAllTraceFromInitState,
    onlyValidSignalsAreGenerated,
    onlyValidSignalsAreGeneratedFromInitState,

    -- * Trace classification
    traceLengthsAreClassified,
    classifyTraceLength,
    classifySize,

    -- * Internal
    mkIntervals,
  )
where

import Control.State.Transition (Environment, IRC (IRC), STS, Signal, State, TRC (TRC))
import qualified Control.State.Transition.Extended as STS
import Control.State.Transition.Trace (Trace)
import qualified Control.State.Transition.Trace as Trace
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Test.QuickCheck as QuickCheck

-- | State transition systems for which traces can be generated, given a trace
-- generation environment.
--
-- The trace generation environment allows to pass relevant data to the trace
-- generation algorithm.
class STS sts => HasTrace sts traceGenEnv where
  type BaseEnv sts :: Type
  type BaseEnv s = ()

  -- | Interpret the action from the base monad into a pure value, given some
  -- initial environment. This obviously places some contraints on the nature of
  -- the base monad for a trace to be completed.
  interpretSTS :: forall a. (BaseEnv sts -> STS.BaseM sts a -> a)
  default interpretSTS :: (STS.BaseM sts ~ Identity) => forall a. BaseEnv sts -> STS.BaseM sts a -> a
  interpretSTS _ (Identity x) = x

  envGen :: traceGenEnv -> QuickCheck.Gen (Environment sts)

  sigGen ::
    traceGenEnv ->
    Environment sts ->
    State sts ->
    QuickCheck.Gen (Signal sts)

  shrinkSignal :: Signal sts -> [Signal sts]

-- | Generate a random trace starting in the given environment and initial state.
traceFrom ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  Environment sts ->
  State sts ->
  QuickCheck.Gen (Trace sts)
traceFrom traceEnv maxTraceLength traceGenEnv env st0 = do
  chosenTraceLength <- QuickCheck.choose (0, maxTraceLength)
  Trace.mkTrace env st0 <$> loop chosenTraceLength st0 []
  where
    loop ::
      Word64 ->
      State sts ->
      [(State sts, Signal sts)] ->
      QuickCheck.Gen [(State sts, Signal sts)]
    loop 0 _ acc = pure $! acc
    loop !d sti stSigs = do
      sig <- sigGen @sts @traceGenEnv traceGenEnv env sti
      case interpretSTS @sts @traceGenEnv traceEnv (Trace.applySTSTest @sts (TRC (env, sti, sig))) of
        Left _predicateFailures ->
          loop (d - 1) sti stSigs
        Right sti' ->
          loop (d - 1) sti' ((sti', sig) : stSigs)

-- | Generate a random trace.
trace ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv,
    Show (Environment sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  QuickCheck.Gen (Trace sts)
trace traceEnv maxTraceLength traceGenEnv =
  traceFromInitState traceEnv maxTraceLength traceGenEnv Nothing

-- | Generate a random trace given a generator for initial state.
--
-- Takes an optional generator for initial state, or defaults to 'applySTS'
-- if no initial state is required by the STS.
traceFromInitState ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv,
    Show (Environment sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  -- | Optional generator of STS initial state
  Maybe (IRC sts -> QuickCheck.Gen (Either [STS.PredicateFailure sts] (State sts))) ->
  QuickCheck.Gen (Trace sts)
traceFromInitState baseEnv maxTraceLength traceGenEnv genSt0 = do
  env <- envGen @sts @traceGenEnv traceGenEnv
  res <-
    fromMaybe
      ( pure . interpretSTS @sts @traceGenEnv baseEnv
          . Trace.applySTSTest
      )
      genSt0
      $ (IRC env)

  case res of
    Left pf ->
      error $
        "Failed to apply the initial rule to the generated environment.\n"
          ++ "Generated environment: "
          ++ show env
          ++ "Failure: "
          ++ show pf
    Right st0 -> traceFrom baseEnv maxTraceLength traceGenEnv env st0

-- | Check a property on the 'sts' traces.
--
-- Takes an optional generator for initial state of the STS.
forAllTraceFromInitState ::
  forall sts traceGenEnv prop.
  ( HasTrace sts traceGenEnv,
    QuickCheck.Testable prop,
    Show (Environment sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  -- | Optional generator of STS initial state
  Maybe (IRC sts -> QuickCheck.Gen (Either [STS.PredicateFailure sts] (State sts))) ->
  (Trace sts -> prop) ->
  QuickCheck.Property
forAllTraceFromInitState baseEnv maxTraceLength traceGenEnv genSt0 prop =
  QuickCheck.forAllShrinkBlind
    (traceFromInitState @sts @traceGenEnv baseEnv maxTraceLength traceGenEnv genSt0)
    (shrinkTrace @sts @traceGenEnv baseEnv)
    prop

-- | Check a property on the 'sts' traces.
forAllTrace ::
  forall sts traceGenEnv prop.
  ( HasTrace sts traceGenEnv,
    QuickCheck.Testable prop,
    Show (Environment sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  (Trace sts -> prop) ->
  QuickCheck.Property
forAllTrace baseEnv maxTraceLength traceGenEnv =
  forAllTraceFromInitState baseEnv maxTraceLength traceGenEnv Nothing

-- | Shrink a trace by shrinking the list of signals and reconstructing traces from these
-- shrunk lists of signals.
--
-- When shrinking a trace that is failing some property (often stated in terms of a signal in the trace)
-- then the most recent signal is likely crucial to the failure of the property and must be preserved
-- in the shrunk traces.
shrinkTrace ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv
  ) =>
  BaseEnv sts ->
  Trace sts ->
  [Trace sts]
shrinkTrace baseEnv tr =
  interpretSTS @sts @traceGenEnv baseEnv $
    Trace.closure env st0 `traverse` shrinkSignals signals
  where
    env = Trace._traceEnv tr
    st0 = Trace._traceInitState tr
    signals = Trace.traceSignals Trace.NewestFirst tr

    -- Shrink a list of signals such that we preserve the most recent signal in the shrunk lists.
    -- This shrinker
    --   - recursively omits all but the most recent signal
    --   - builds up lists of signals starting with the most recent signal and
    --     building up to a list excluding the first (oldest) signal
    --   - explicitly shrinks in order from small to larger lists of signals
    --     (i.e. ordered by most to least aggressive shrinking)
    shrinkSignals (sn : _last : []) =
      [[sn]]
    shrinkSignals (sn : sm : sigs) =
      [[sn]] -- a trace with only the most recent signal
        ++ ((sn :) <$> shrinkSignals (sm : sigs)) -- shrink the tail
        ++ [sn : sigs] -- discard the second most recent signal

    -- shrink to [] if there is one or no signals
    shrinkSignals _ = []

-- | Property that asserts that only valid signals are generated.
onlyValidSignalsAreGenerated ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv,
    Show (Environment sts),
    Show (Signal sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  QuickCheck.Property
onlyValidSignalsAreGenerated baseEnv maxTraceLength traceGenEnv =
  onlyValidSignalsAreGeneratedFromInitState @sts baseEnv maxTraceLength traceGenEnv Nothing

-- | Property that asserts that only valid signals are generated.
--
-- Takes an optional generator for initial state of the STS.
onlyValidSignalsAreGeneratedFromInitState ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv,
    Show (Environment sts),
    Show (Signal sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length.
  Word64 ->
  traceGenEnv ->
  -- | Optional generator of STS initial state
  Maybe (IRC sts -> QuickCheck.Gen (Either [STS.PredicateFailure sts] (State sts))) ->
  QuickCheck.Property
onlyValidSignalsAreGeneratedFromInitState baseEnv maxTraceLength traceGenEnv genSt0 =
  forAllTraceFromInitState baseEnv maxTraceLength traceGenEnv genSt0 validSignalsAreGenerated
  where
    validSignalsAreGenerated ::
      Trace sts ->
      QuickCheck.Property
    validSignalsAreGenerated someTrace =
      QuickCheck.forAllShrink
        (sigGen @sts @traceGenEnv traceGenEnv env lastState)
        (shrinkSignal @sts @traceGenEnv)
        signalIsValid
      where
        signalIsValid signal =
          case interpretSTS @sts @traceGenEnv baseEnv (Trace.applySTSTest @sts (TRC (env, lastState, signal))) of
            Left pf -> QuickCheck.counterexample (show (signal, pf)) False
            Right _ -> QuickCheck.property True
        env = Trace._traceEnv someTrace
        lastState = Trace.lastState someTrace

--------------------------------------------------------------------------------
-- Trace classification
--------------------------------------------------------------------------------

-- | Property that simply classifies the lengths of the generated traces.
traceLengthsAreClassified ::
  forall sts traceGenEnv.
  ( HasTrace sts traceGenEnv,
    Show (Environment sts)
  ) =>
  BaseEnv sts ->
  -- | Maximum trace length that the signal generator of 's' can generate.
  Word64 ->
  -- | Lengths of the intervals in which the lengths range should be split.
  Word64 ->
  -- | Trace generation environment
  traceGenEnv ->
  QuickCheck.Property
traceLengthsAreClassified baseEnv maxTraceLength intervalSize traceGenEnv =
  forAllTrace @sts baseEnv maxTraceLength traceGenEnv (classifyTraceLength maxTraceLength intervalSize)

-- | Classify the trace length as either:
--
-- - being empty
-- - being a singleton
-- - having the given maximum size
-- - belonging to one of the intervals between 2 and the maximum size - 1. The
--   number of intervals are determined by the @step@ parameter.
classifyTraceLength ::
  -- | Maximum size of the traces
  Word64 ->
  -- | Steps used to divide the interval
  Word64 ->
  Trace s ->
  QuickCheck.Property
classifyTraceLength maxTraceLength step tr =
  classifySize "trace length:" tr (fromIntegral . Trace.traceLength) maxTraceLength step

-- | Classify the value size as either:
--
-- - being empty
-- - being a singleton
-- - having the given maximum size
-- - belonging to one of the intervals between 2 and the maximum size - 1. The
--   number of intervals are determined by the @step@ parameter.
classifySize ::
  (Ord n, Show n, Integral n) =>
  -- | Prefix to be added to the label intervals
  String ->
  -- | Value to classify
  a ->
  -- | Size function
  (a -> n) ->
  -- | Maximum value size
  n ->
  -- | Steps used to divide the size interval
  n ->
  QuickCheck.Property
classifySize prefixLabel value sizeF upBound step =
  QuickCheck.classify (sizeF value == 0) (mkLabel "empty") $
    QuickCheck.classify (sizeF value == 1) (mkLabel "singleton") $
      QuickCheck.classify (sizeF value == upBound) upBoundLabel $
        foldr classifySizeInterval (QuickCheck.property True) (mkIntervals 2 (upBound - 1) step)
  where
    upBoundLabel = mkLabel $ show upBound
    mkLabel = ((prefixLabel ++ " ") ++)
    classifySizeInterval (low, high) prop =
      QuickCheck.classify (low <= sizeF value && sizeF value < high) desc prop
      where
        desc = "[" ++ show low ++ ", " ++ show high ++ ")"

-- | Given a lower bound @low@,  an upper bound @high@ and a step size @step@
-- (both of which must be positive), divide the interval @[0, ub]@ into
-- sub-intervals of @step@ size.
--
-- If any of these values is negative the empty list will be returned.
--
-- Examples:
--
-- >>> mkIntervals 0 0 0 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals 0 10 2 :: [(Int, Int)]
-- [(0,2),(2,4),(4,6),(6,8),(8,10)]
--
-- >>> mkIntervals 1 10 2 :: [(Int, Int)]
-- [(1,3),(3,5),(5,7),(7,9),(9,10)]
--
--
-- >>> mkIntervals 3 10 3 :: [(Int, Int)]
-- [(3,6),(6,9),(9,10)]
--
-- >>> mkIntervals 5 2 3 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals (-1) 10 3 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals 1 (-10) 3 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals 1 1000 (-100) :: [(Int, Int)]
-- []
mkIntervals ::
  Integral n =>
  -- | Interval lower bound
  n ->
  -- | Interval upper bound
  n ->
  -- | Step size, used to divide the interval in sub-intervals of the same
  -- length.
  n ->
  [(n, n)]
mkIntervals low high step
  | 0 <= low && low <= high && 0 < step =
    [(low + i * step, high `min` (low + (i + 1) * step)) | i <- [0 .. n - 1]]
  | otherwise = []
  where
    highNorm = high - low
    n = highNorm `div` step + 1 `min` (highNorm `mod` step)
