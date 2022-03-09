{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generators for transition systems.
--
--   How should these work?
--   - We start with some initial environment.
--   - We start with some initial base state.
--   - We generate a stream of signals. These might be influenced by some running state
--   - We run each signal through
module Control.State.Transition.Generator
  ( HasTrace,
    SignalGenerator,
    BaseEnv,
    interpretSTS,
    envGen,
    sigGen,
    traceSigGen,
    genTrace,
    trace,
    traceWithProfile,
    traceOfLength,
    traceOfLengthWithInitState,
    traceSuchThat,
    ofLengthAtLeast,
    suchThatLastState,
    nonTrivialTrace,
    HasSizeInfo,
    isTrivial,
    sampleMaxTraceSize,
    randomTrace,
    randomTraceOfSize,
    TraceLength (Maximum, Desired),
    TraceProfile (TraceProfile, proportionOfValidSignals, failures),
    proportionOfInvalidSignals,

    -- * Invalid trace generation
    invalidTrace,

    -- * Trace classification
    classifyTraceLength,
    classifySize,
    mkIntervals,
    ratio,

    -- * Trace properties
    traceLengthsAreClassified,
    onlyValidSignalsAreGenerated,
    onlyValidSignalsAreGeneratedForTrace,
    invalidSignalsAreGenerated,

    -- * Helpers
    tinkerWithSigGen,
    coverFailures,
  )
where

import Control.Arrow (second)
import Control.Monad (forM, void)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.State.Strict (evalState)
import Control.State.Transition.Extended
  ( BaseM,
    Environment,
    IRC (IRC),
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
  )
import qualified Control.State.Transition.Invalid.Trace as Invalid
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (OldestFirst),
    applySTSTest,
    closure,
    extractValues,
    lastState,
    mkTrace,
    traceLength,
    traceSignals,
    _traceEnv,
  )
import Data.Data (Constr, Data, toConstr)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Hedgehog
  ( Gen,
    MonadTest,
    Property,
    PropertyT,
    classify,
    cover,
    evalEither,
    footnoteShow,
    forAll,
    property,
    success,
  )
import Hedgehog.Extra.Manual (Manual)
import qualified Hedgehog.Extra.Manual as Manual
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen (integral_, runDiscardEffectT)
import Hedgehog.Internal.Property (CoverPercentage)
import Hedgehog.Internal.Tree (NodeT (NodeT), TreeT, nodeChildren, treeValue)
import Hedgehog.Range (Size (Size))
import qualified Hedgehog.Range as Range
import Test.Goblin (Goblin (..), GoblinData, SeedGoblin (..))

class STS s => HasTrace s where
  type BaseEnv s :: Type
  type BaseEnv s = ()

  -- | Interpret the action from the base monad into a pure value, given some
  -- initial environment. This obviously places some contraints on the nature of
  -- the base monad for a trace to be completed.
  interpretSTS :: forall a. (BaseEnv s -> BaseM s a -> a)
  default interpretSTS :: (BaseM s ~ Identity) => forall a. BaseEnv s -> BaseM s a -> a
  interpretSTS _ (Identity x) = x

  -- | Generate an initial environment that is based on the given trace length.
  envGen ::
    -- | Trace length that will be used by 'trace' or 'traceOfLength'.
    Word64 ->
    Gen (Environment s)

  -- | Generate a (valid) signal given an environment and a pre-state.
  sigGen ::
    SignalGenerator s

  trace ::
    BaseEnv s ->
    -- | Maximum length of the generated traces. The actual length will be
    -- between 0 and this maximum.
    Word64 ->
    Gen (Trace s)
  trace baseEnv n = traceWithProfile @s baseEnv n allValid

  traceWithProfile ::
    BaseEnv s ->
    Word64 ->
    TraceProfile s ->
    Gen (Trace s)
  traceWithProfile baseEnv n p = traceSigGenWithProfile baseEnv (Maximum n) p (sigGen @s)

  traceOfLength ::
    BaseEnv s ->
    -- | Desired length of the generated trace. If the signal generator can generate invalid signals
    -- then the resulting trace might not have the given length.
    Word64 ->
    Gen (Trace s)
  traceOfLength baseEnv n = traceSigGenWithProfile baseEnv (Desired n) allValid (sigGen @s)

  traceOfLengthWithInitState ::
    BaseEnv s ->
    -- | Desired length of the generated trace. If the signal generator can generate invalid signals
    -- then the resulting trace might not have the given length.
    Word64 ->
    -- | A generator for Initial State, given the STS environment
    (Environment s -> Gen (State s)) ->
    Gen (Trace s)
  traceOfLengthWithInitState baseEnv n mkSt0 =
    traceSigGenWithProfileAndInitState baseEnv (Desired n) allValid (sigGen @s) mkSt0

type SignalGenerator s = Environment s -> State s -> Gen (Signal s)

data TraceLength = Maximum Word64 | Desired Word64

data TraceProfile s = TraceProfile
  { -- | Proportion of valid signals to generate.
    proportionOfValidSignals :: !Int,
    -- | List of failure conditions to try generate when generating an invalid signal, and the
    -- proportion of each failure.
    failures :: ![(Int, SignalGenerator s)]
  }

proportionOfInvalidSignals :: TraceProfile s -> Int
proportionOfInvalidSignals = sum . fmap fst . failures

allValid :: TraceProfile s
allValid =
  TraceProfile
    { proportionOfValidSignals = 1,
      failures = []
    }

-- | Generate a signal by combining the generators using @hedgehog@'s
-- 'frequency' combinator.
generateSignalWithFailureProportions ::
  -- | Failure proportions. See 'failures' in 'TraceProfile'.
  [(Int, SignalGenerator s)] ->
  SignalGenerator s
generateSignalWithFailureProportions proportions env st =
  Gen.frequency $ second aSigGenWithFailure <$> proportions
  where
    aSigGenWithFailure invalidSigGen = invalidSigGen env st

-- | Extract the maximum or desired integer value of the trace length.
traceLengthValue :: TraceLength -> Word64
traceLengthValue (Maximum n) = n
traceLengthValue (Desired n) = n

traceSigGen ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  TraceLength ->
  SignalGenerator s ->
  Gen (Trace s)
traceSigGen baseEnv aTraceLength = traceSigGenWithProfile baseEnv aTraceLength allValid

traceSigGenWithProfile ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  TraceLength ->
  TraceProfile s ->
  SignalGenerator s ->
  Gen (Trace s)
traceSigGenWithProfile baseEnv aTraceLength profile gen = do
  env <- envGen @s (traceLengthValue aTraceLength)
  case interpretSTS @s baseEnv $ applySTSTest @s (IRC env) of
    -- Hedgehog will give up if the generators fail to produce any valid
    -- initial state, hence we don't have a risk of entering an infinite
    -- recursion.
    Left _pf -> traceSigGen baseEnv aTraceLength gen
    -- Applying an initial rule with an environment and state will simply
    -- validate that state, so we do not care which state 'applySTS' returns.
    Right st -> genTraceOfMaxOrDesiredLength baseEnv aTraceLength profile env st gen

-- | A variation of 'traceSigGenWithProfile' which takes an argument generator
-- for the initial state of the given trace
traceSigGenWithProfileAndInitState ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  TraceLength ->
  TraceProfile s ->
  SignalGenerator s ->
  (Environment s -> Gen (State s)) ->
  Gen (Trace s)
traceSigGenWithProfileAndInitState baseEnv aTraceLength profile gen mkSt0 = do
  env <- envGen @s (traceLengthValue aTraceLength)
  st0 <- mkSt0 env

  genTraceOfMaxOrDesiredLength baseEnv aTraceLength profile env st0 gen

genTraceOfMaxOrDesiredLength ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  TraceLength ->
  TraceProfile s ->
  Environment s ->
  State s ->
  SignalGenerator s ->
  Gen (Trace s)
genTraceOfMaxOrDesiredLength baseEnv aTraceLength profile env st0 gen =
  case aTraceLength of
    Maximum n -> genTraceWithProfile baseEnv n profile env st0 gen
    Desired n -> genTraceOfLength baseEnv n profile env st0 gen

-- | Return a (valid) trace generator given an initial state, environment, and
-- signal generator.
genTrace ::
  forall s.
  (HasTrace s) =>
  BaseEnv s ->
  -- | Trace upper bound. This will be linearly scaled as a function of the
  -- generator size.
  Word64 ->
  -- | Environment, which remains constant in the system.
  Environment s ->
  -- | Initial state.
  State s ->
  -- | Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  SignalGenerator s ->
  Gen (Trace s)
genTrace baseEnv ub = genTraceWithProfile baseEnv ub allValid

-- | Return a trace generator given an initial state, environment, and signal generator.
genTraceWithProfile ::
  forall s.
  (HasTrace s) =>
  BaseEnv s ->
  -- | Trace upper bound. This will be linearly scaled as a function of the
  -- generator size.
  Word64 ->
  TraceProfile s ->
  -- | Environment, which remains constant in the system.
  Environment s ->
  -- | Initial state.
  State s ->
  -- | Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  SignalGenerator s ->
  Gen (Trace s)
genTraceWithProfile baseEnv ub profile env st0 aSigGen =
  do
    -- Generate the initial size of the trace, but don't shrink it (notice the
    -- use of 'integral_') since we will be shrinking the traces manually (so it
    -- doesn't make sense to shrink the trace size).
    --
    -- Note that the length of the resulting trace might be less than the
    -- generated value if invalid signals (according to some current state) are
    -- generated in 'loop'.
    --
    -- A linear range will generate about one third of empty traces, which does
    -- not seem sensible. Furthermore, in most cases it won't generate any trace
    -- of size @ub@. Hence we need to tweak the frequency of the trace lengths.
    n <-
      Gen.frequency
        [ (5, pure 0),
          (85, integral_ $ Range.linear 1 ub),
          (5, pure ub)
        ]
    genTraceOfLength baseEnv n profile env st0 aSigGen

-- | Return a (valid) trace generator that generates traces of the given size. If the signal
-- generator can generate invalid signals, then the size of resulting trace is not guaranteed.
genTraceOfLength ::
  forall s.
  (HasTrace s) =>
  BaseEnv s ->
  -- | Desired trace length.
  Word64 ->
  TraceProfile s ->
  -- | Environment, which remains constant in the system.
  Environment s ->
  -- | Initial state.
  State s ->
  -- | Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  SignalGenerator s ->
  Gen (Trace s)
genTraceOfLength baseEnv aTraceLength profile env st0 aSigGen =
  Manual.fromManual $ fmap interleaveSigs $ loop aTraceLength st0 []
  where
    loop ::
      Word64 ->
      State s ->
      [(State s, TreeT (MaybeT Identity) (Signal s))] ->
      Manual [(State s, TreeT (MaybeT Identity) (Signal s))]
    loop 0 _ acc = pure acc
    loop !d sti acc = do
      sigTree :: TreeT (MaybeT Identity) (Signal s) <-
        Manual.toManual $
          Gen.frequency
            [ ( proportionOfValidSignals profile,
                aSigGen env sti
              ),
              ( proportionOfInvalidSignals profile,
                generateSignalWithFailureProportions @s (failures profile) env sti
              )
            ]
      let --  Take the root of the next-state signal tree.
          mSig = treeValue $ runDiscardEffectT sigTree
      case mSig of
        Nothing ->
          loop (d - 1) sti acc
        Just sig ->
          case interpretSTS @s baseEnv $ applySTSTest @s (TRC (env, sti, sig)) of
            Left _err -> loop (d - 1) sti acc
            Right sti' -> loop (d - 1) sti' ((sti', sigTree) : acc)

    interleaveSigs ::
      [(State s, TreeT (MaybeT Identity) (Signal s))] ->
      TreeT (MaybeT Identity) (Trace s)
    interleaveSigs stateSignalTrees =
      Manual.wrapTreeT $
        Just $
          NodeT
            rootTrace
            (fmap (fmap (interpretSTS @s baseEnv . closure @s env st0)) signalShrinksChilren)
      where
        rootStates :: [State s]
        signalTrees :: [TreeT (MaybeT Identity) (Signal s)]
        (rootStates, signalTrees) = unzip stateSignalTrees
        rootSignals :: [Signal s]
        rootSignals = fmap (fromMaybe err . treeValue . runDiscardEffectT) signalTrees
        err = error "genTraceOfLength: the tree nodes must always contain a signal"
        -- The states ensuing the root signals were calculated at 'loop'
        -- already, so there is no need to apply the STS again.
        rootTrace :: Trace s
        rootTrace = mkTrace env st0 (zip rootStates rootSignals)
        signalShrinks :: TreeT (MaybeT Identity) [Signal s]
        signalShrinks = Manual.interleave signalTrees
        -- The signals at the root of 'signalShrinks' are already included in
        -- the 'rootTrace' so there is no need to include them again in the tree
        -- of traces. Thus we only need to apply 'closure' to the children of
        -- the shrink tree.
        signalShrinksChilren :: [TreeT (MaybeT Identity) [Signal s]]
        signalShrinksChilren = nodeChildren $ fromMaybe err $ Manual.unwrapTreeT signalShrinks

-- | Generate an invalid trace
invalidTrace ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  -- | Maximum length of the generated traces.
  Word64 ->
  -- | Trace failure profile to be used to get an invalid signal.
  [(Int, SignalGenerator s)] ->
  Gen (Invalid.Trace s)
invalidTrace baseEnv maxTraceLength failureProfile = do
  tr <- trace @s baseEnv maxTraceLength
  let env = _traceEnv tr
      st = lastState tr
  iSig <- generateSignalWithFailureProportions @s failureProfile env st
  let est' = interpretSTS @s baseEnv $ applySTSTest @s $ TRC (env, st, iSig)
  pure
    $! Invalid.Trace
      { Invalid.validPrefix = tr,
        Invalid.signal = iSig,
        Invalid.errorOrLastState = est'
      }

traceSuchThat ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  Word64 ->
  (Trace s -> Bool) ->
  Gen (Trace s)
traceSuchThat baseEnv n cond = Gen.filter cond (trace @s baseEnv n)

ofLengthAtLeast :: Gen (Trace s) -> Int -> Gen (Trace s)
ofLengthAtLeast traceGen minLength =
  Gen.filter ((minLength <=) . traceLength) traceGen

suchThatLastState ::
  forall s.
  Gen (Trace s) ->
  (State s -> Bool) ->
  Gen (Trace s)
suchThatLastState traceGen cond = Gen.filter (cond . lastState) traceGen

-- | Generate a trace that contains at least one non-trivial signal. See
-- 'HasSizeInfo'.
nonTrivialTrace ::
  forall s.
  (HasTrace s, HasSizeInfo (Signal s)) =>
  BaseEnv s ->
  Word64 ->
  Gen (Trace s)
nonTrivialTrace baseEnv ub =
  Gen.filter (any (not . isTrivial) . traceSignals OldestFirst) (trace baseEnv ub)

class HasSizeInfo sig where
  isTrivial :: sig -> Bool

instance HasSizeInfo [a] where
  isTrivial = null

--------------------------------------------------------------------------------
-- Trace sampling utilities
--------------------------------------------------------------------------------

-- | Sample the maximum trace size, given the generator size and number of
-- samples.
sampleMaxTraceSize ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  -- | Trace's upper bound
  Word64 ->
  -- | Generator size
  Int ->
  -- | Number of samples to take
  Word64 ->
  IO Int
sampleMaxTraceSize baseEnv ub d n =
  maximum
    <$> forM [0 .. n] (const $ traceLength <$> Gen.sample (Gen.resize (Size d) (trace @s baseEnv ub)))

randomTrace ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  Word64 ->
  IO (Trace s)
randomTrace baseEnv ub = Gen.sample (trace baseEnv ub)

randomTraceOfSize ::
  forall s.
  HasTrace s =>
  BaseEnv s ->
  Word64 ->
  IO (Trace s)
randomTraceOfSize baseEnv desiredTraceLength = Gen.sample (traceOfLength baseEnv desiredTraceLength)

--------------------------------------------------------------------------------
-- Trace classification
--------------------------------------------------------------------------------

-- | Classify the trace length as either:
--
-- - being empty
-- - being a singleton
-- - having the given maximum size
-- - belonging to one of the intervals between 2 and the maximum size - 1. The
--   number of intervals are determined by the @step@ parameter.
classifyTraceLength ::
  Trace s ->
  -- | Maximum size of the traces
  Word64 ->
  -- | Steps used to divide the interval
  Word64 ->
  PropertyT IO ()
classifyTraceLength tr = classifySize "trace length:" tr (fromIntegral . traceLength)

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
  PropertyT IO ()
classifySize prefixLabel value sizeF upBound step = do
  classify (mkLabel "empty") $ sizeF value == 0
  classify (mkLabel "singleton") $ sizeF value == 1
  traverse_ classifySizeInterval $ mkIntervals 2 (upBound - 1) step
  classify upBoundLabel $ sizeF value == upBound
  where
    upBoundLabel = mkLabel $ show upBound
    mkLabel = fromString . ((prefixLabel ++ " ") ++)
    classifySizeInterval (low, high) =
      classify desc $! low <= sizeF value && sizeF value < high
      where
        -- Hedgehog's LabelName doesn't have a monoid instance at the moment...
        desc = mkLabel $ "[" <> show low <> ", " <> show high <> ")"

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

-- | Given a function that computes an integral value from a trace, return that
-- value as a ratio of the trace length.
ratio ::
  Integral a =>
  (Trace s -> a) ->
  Trace s ->
  Double
ratio f tr = fromIntegral (f tr) / fromIntegral (traceLength tr)

--------------------------------------------------------------------------------
-- Trace properties
--------------------------------------------------------------------------------

-- | Property that simply classifies the lengths of the generated traces.
traceLengthsAreClassified ::
  forall s.
  (HasTrace s, Show (Environment s), Show (State s), Show (Signal s)) =>
  BaseEnv s ->
  -- | Maximum trace length that the signal generator of 's' can generate.
  Word64 ->
  -- | Lengths of the intervals in which the lengths range should be split.
  Word64 ->
  Property
traceLengthsAreClassified baseEnv maximumTraceLength intervalSize =
  property $ do
    traceSample <- forAll (trace @s baseEnv maximumTraceLength)
    classifyTraceLength traceSample maximumTraceLength intervalSize
    success

-- | Check that the signal generator of 's' only generate valid signals.
onlyValidSignalsAreGenerated ::
  forall s.
  (HasTrace s, Show (Environment s), Show (State s), Show (Signal s), HasCallStack) =>
  BaseEnv s ->
  -- | Maximum trace length.
  Word64 ->
  Property
onlyValidSignalsAreGenerated baseEnv maximumTraceLength =
  onlyValidSignalsAreGeneratedForTrace baseEnv (trace @s baseEnv maximumTraceLength)

-- | Check that the signal generator of 's' only generate valid signals.
onlyValidSignalsAreGeneratedForTrace ::
  forall s.
  (HasTrace s, Show (Environment s), Show (State s), Show (Signal s), HasCallStack) =>
  BaseEnv s ->
  Gen (Trace s) ->
  Property
onlyValidSignalsAreGeneratedForTrace baseEnv traceGen = property $ do
  tr <- forAll traceGen
  let env :: Environment s
      env = _traceEnv tr

      st' :: State s
      st' = lastState tr
  sig <- forAll (sigGen @s env st')
  let result = interpretSTS @s baseEnv $ applySTSTest @s (TRC (env, st', sig))
  -- TODO: For some reason the result that led to the failure is not shown
  -- (even without using tasty, and setting the condition to True === False)
  footnoteShow st'
  footnoteShow sig
  footnoteShow result
  void $ evalEither result

coverFailures ::
  forall m s a.
  ( MonadTest m,
    HasCallStack,
    Data (PredicateFailure s),
    Data a
  ) =>
  CoverPercentage ->
  -- | Target predicate failures
  [PredicateFailure s] ->
  -- | Structure containing the failures
  a ->
  m ()
coverFailures coverPercentage targetFailures failureStructure = do
  traverse_ coverFailure (toConstr <$> targetFailures)
  where
    coverFailure predicateFailureConstructor =
      cover
        coverPercentage
        (fromString $ show predicateFailureConstructor)
        (predicateFailureConstructor `elem` failuresConstructors)
      where
        subFailures :: [PredicateFailure s]
        subFailures = extractValues failureStructure

        failuresConstructors :: [Constr]
        failuresConstructors = toConstr <$> subFailures

invalidSignalsAreGenerated ::
  forall s.
  (HasTrace s, Show (Environment s), Show (State s), Show (Signal s), HasCallStack) =>
  BaseEnv s ->
  -- | Failure profile.
  [(Int, SignalGenerator s)] ->
  -- | Maximum trace length.
  Word64 ->
  -- | Action to run when the an invalid signal is generated.
  ([PredicateFailure s] -> PropertyT IO ()) ->
  Property
invalidSignalsAreGenerated baseEnv failureProfile maximumTraceLength checkFailures = property $ do
  tr <- forAll (invalidTrace @s baseEnv maximumTraceLength failureProfile)

  cover
    80
    "Invalid signals are generated when requested"
    (isLeft $ Invalid.errorOrLastState tr)

  either checkFailures (const success) (Invalid.errorOrLastState tr)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

tinkerWithSigGen ::
  forall g sts.
  ( HasTrace sts,
    Goblin g (Signal sts),
    SeedGoblin (Environment sts),
    SeedGoblin (State sts)
  ) =>
  GoblinData g ->
  Environment sts ->
  State sts ->
  Gen (Signal sts)
tinkerWithSigGen gd env state =
  flip evalState gd $
    seeder env >> seeder state >> tinker (sigGen @sts env state)
