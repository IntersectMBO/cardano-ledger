{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
--
module Control.State.Transition.Generator
  ( HasTrace
  , SignalGenerator
  , envGen
  , sigGen
  , traceSigGen
  , genTrace
  , trace
  , traceWithProfile
  , traceOfLength
  , traceOfLengthWithInitState
  , traceSuchThat
  , ofLengthAtLeast
  , suchThatLastState
  , nonTrivialTrace
  , HasSizeInfo
  , isTrivial
  , sampleMaxTraceSize
  , randomTrace
  , randomTraceOfSize
  , TraceLength (Maximum, Desired)
  , TraceProfile (TraceProfile, proportionOfValidSignals, failures)
  , proportionOfInvalidSignals
  -- * Invalid trace generation
  , invalidTrace
  -- * Trace classification
  , classifyTraceLength
  , classifySize
  , mkIntervals
  , ratio
  -- * Trace properties
  , traceLengthsAreClassified
  , onlyValidSignalsAreGenerated
  , onlyValidSignalsAreGeneratedForTrace
  , invalidSignalsAreGenerated
  -- * Helpers
  , tinkerWithSigGen
  , coverFailures
  )
where

import           Control.Arrow (second)
import           Control.Monad (forM, void)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.State.Strict (evalState)
import           Data.Data (Constr, Data, toConstr)
import           Data.Either (isLeft)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Hedgehog (Gen, MonadTest, Property, PropertyT, classify, cover, evalEither,
                     footnoteShow, forAll, property, success)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Property (CoverPercentage)
import           Hedgehog.Range (Size (Size))
import qualified Hedgehog.Range as Range

import           Hedgehog.Internal.Gen (integral_, runDiscardEffectT)
import           Hedgehog.Internal.Tree (NodeT (NodeT), TreeT, nodeChildren, treeValue)

import           Control.State.Transition (BaseM, Environment, IRC (IRC), PredicateFailure, STS, Signal,
                     State, TRC (TRC), applySTS)
import qualified Control.State.Transition.Invalid.Trace as Invalid
import           Control.State.Transition.Trace (Trace, TraceOrder (OldestFirst), closure,
                     extractValues, lastState, mkTrace, traceLength, traceSignals, _traceEnv)
import           Hedgehog.Extra.Manual (Manual)
import qualified Hedgehog.Extra.Manual as Manual

import           Test.Goblin (Goblin (..), GoblinData, SeedGoblin (..))

class (STS s, BaseM s ~ Identity) => HasTrace s where
  -- | Generate an initial environment that is based on the given trace length.
  envGen
    :: Word64
    -- ^ Trace length that will be used by 'trace' or 'traceOfLength'.
    -> Gen (Environment s)

  -- | Generate a (valid) signal given an environment and a pre-state.
  --
  sigGen
    :: SignalGenerator s

  trace
    :: Word64
    -- ^ Maximum length of the generated traces. The actual length will be
    -- between 0 and this maximum.
    -> Gen (Trace s)
  trace n = traceWithProfile @s n allValid

  traceWithProfile
    :: Word64
    -> TraceProfile s
    -> Gen (Trace s)
  traceWithProfile n p = traceSigGenWithProfile (Maximum n) p (sigGen @s)

  traceOfLength
    :: Word64
    -- ^ Desired length of the generated trace. If the signal generator can generate invalid signals
    -- then the resulting trace might not have the given length.
    -> Gen (Trace s)
  traceOfLength n = traceSigGenWithProfile (Desired n) allValid (sigGen @s)

  traceOfLengthWithInitState
    :: Word64
    -- ^ Desired length of the generated trace. If the signal generator can generate invalid signals
    -- then the resulting trace might not have the given length.
    -> (Environment s -> Gen (State s))
    -- ^ A generator for Initial State, given the STS environment
    -> Gen (Trace s)
  traceOfLengthWithInitState n mkSt0
    = traceSigGenWithProfileAndInitState (Desired n) allValid (sigGen @s) mkSt0

type SignalGenerator s = Environment s -> State s -> Gen (Signal s)

data TraceLength = Maximum Word64 | Desired Word64

data TraceProfile s
  = TraceProfile
  { proportionOfValidSignals :: !Int
    -- ^ Proportion of valid signals to generate.
  , failures :: ![(Int, SignalGenerator s)]
    -- ^ List of failure conditions to try generate when generating an invalid signal, and the
    -- proportion of each failure.
  }

proportionOfInvalidSignals :: TraceProfile s -> Int
proportionOfInvalidSignals = sum . fmap fst . failures

allValid :: TraceProfile s
allValid
  = TraceProfile
    { proportionOfValidSignals = 1
    , failures = []
    }

-- | Generate a signal by combining the generators using @hedgehog@'s
-- 'frequency' combinator.
generateSignalWithFailureProportions
  :: [(Int, SignalGenerator s)]
  -- ^ Failure proportions. See 'failures' in 'TraceProfile'.
  -> SignalGenerator s
generateSignalWithFailureProportions proportions env st =
  Gen.frequency $ second aSigGenWithFailure <$> proportions
  where
    aSigGenWithFailure invalidSigGen = invalidSigGen env st

-- | Extract the maximum or desired integer value of the trace length.
traceLengthValue :: TraceLength -> Word64
traceLengthValue (Maximum n) = n
traceLengthValue (Desired n) = n

traceSigGen
  :: forall s
   . HasTrace s
  => TraceLength
  -> SignalGenerator s
  -> Gen (Trace s)
traceSigGen aTraceLength = traceSigGenWithProfile aTraceLength allValid

traceSigGenWithProfile
  :: forall s
   . HasTrace s
  => TraceLength
  -> TraceProfile s
  -> SignalGenerator s
  -> Gen (Trace s)
traceSigGenWithProfile aTraceLength profile gen = do
  env <- envGen @s (traceLengthValue aTraceLength)
  case applySTS @s (IRC env) of
    -- Hedgehog will give up if the generators fail to produce any valid
    -- initial state, hence we don't have a risk of entering an infinite
    -- recursion.
    Left _pf -> traceSigGen aTraceLength gen
    -- Applying an initial rule with an environment and state will simply
    -- validate that state, so we do not care which state 'applySTS' returns.
    Right st -> genTraceOfMaxOrDesiredLength aTraceLength profile env st gen

-- | A variation of 'traceSigGenWithProfile' which takes an argument generator
-- for the initial state of the given trace
traceSigGenWithProfileAndInitState
  :: forall s
   . HasTrace s
  => TraceLength
  -> TraceProfile s
  -> SignalGenerator s
  -> (Environment s -> Gen (State s))
  -> Gen (Trace s)
traceSigGenWithProfileAndInitState aTraceLength profile gen mkSt0 = do
  env <- envGen @s (traceLengthValue aTraceLength)
  st0 <- mkSt0 env

  genTraceOfMaxOrDesiredLength aTraceLength profile env st0 gen

genTraceOfMaxOrDesiredLength
  :: forall s
   . HasTrace s
  => TraceLength
  -> TraceProfile s
  -> Environment s
  -> State s
  -> SignalGenerator s
  -> Gen (Trace s)
genTraceOfMaxOrDesiredLength aTraceLength profile env st0 gen =
  case aTraceLength of
    Maximum n -> genTraceWithProfile n profile env st0 gen
    Desired n -> genTraceOfLength n profile env st0 gen

-- | Return a (valid) trace generator given an initial state, environment, and
-- signal generator.
--
genTrace
  :: forall s
   . (STS s, BaseM s ~ Identity)
  => Word64
  -- ^ Trace upper bound. This will be linearly scaled as a function of the
  -- generator size.
  -> Environment s
  -- ^ Environment, which remains constant in the system.
  -> State s
  -- ^ Initial state.
  -> SignalGenerator s
  -- ^ Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  -> Gen (Trace s)
genTrace ub = genTraceWithProfile ub allValid

-- | Return a trace generator given an initial state, environment, and signal generator.
--
genTraceWithProfile
  :: forall s
   . (STS s, BaseM s ~ Identity)
  => Word64
  -- ^ Trace upper bound. This will be linearly scaled as a function of the
  -- generator size.
  -> TraceProfile s
  -> Environment s
  -- ^ Environment, which remains constant in the system.
  -> State s
  -- ^ Initial state.
  -> SignalGenerator s
  -- ^ Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  -> Gen (Trace s)
genTraceWithProfile ub profile env st0 aSigGen =
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
  n <- Gen.frequency [ (5, pure 0)
                     , (85, integral_ $ Range.linear 1 ub)
                     , (5, pure ub)
                     ]
  genTraceOfLength n profile env st0 aSigGen

-- | Return a (valid) trace generator that generates traces of the given size. If the signal
-- generator can generate invalid signals, then the size of resulting trace is not guaranteed.
--
genTraceOfLength
  :: forall s
   . (STS s, BaseM s ~ Identity)
  => Word64
  -- ^ Desired trace length.
  -> TraceProfile s
  -> Environment s
  -- ^ Environment, which remains constant in the system.
  -> State s
  -- ^ Initial state.
  -> SignalGenerator s
  -- ^ Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  -> Gen (Trace s)
genTraceOfLength aTraceLength profile env st0 aSigGen =
  Manual.fromManual $ fmap interleaveSigs $ loop aTraceLength st0 []
  where
    loop
      :: Word64
      -> State s
      -> [(State s, TreeT (MaybeT Identity) (Signal s))]
      -> Manual [(State s, TreeT (MaybeT Identity) (Signal s))]
    loop 0 _ acc = pure acc
    loop !d sti acc = do
      sigTree :: TreeT (MaybeT Identity) (Signal s)
        <- Manual.toManual $
             Gen.frequency
               [ ( proportionOfValidSignals profile
                 , aSigGen env sti
                 )
               , ( proportionOfInvalidSignals profile
                 , generateSignalWithFailureProportions @s (failures profile) env sti
                 )
               ]
      let
        --  Take the root of the next-state signal tree.
        mSig = treeValue $ runDiscardEffectT sigTree
      case mSig of
        Nothing ->
          loop (d - 1) sti acc
        Just sig ->
          case applySTS @s (TRC(env, sti, sig)) of
            Left _err  -> loop (d - 1) sti acc
            Right sti' -> loop (d - 1) sti' ((sti', sigTree) : acc)

    interleaveSigs
      :: [(State s, TreeT (MaybeT Identity) (Signal s))]
      -> TreeT (MaybeT Identity) (Trace s)
    interleaveSigs stateSignalTrees
      = Manual.wrapTreeT
      $ Just
      $ NodeT
          rootTrace
          (fmap (fmap (closure @s env st0)) signalShrinksChilren)
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
--
invalidTrace
  :: forall s
   . HasTrace s
  => Word64
  -- ^ Maximum length of the generated traces.
  -> [(Int, SignalGenerator s)]
  -- ^ Trace failure profile to be used to get an invalid signal.
  -> Gen (Invalid.Trace s)
invalidTrace maxTraceLength failureProfile = do
  tr <- trace @s maxTraceLength
  let env = _traceEnv tr
      st = lastState tr
  iSig <- generateSignalWithFailureProportions @s failureProfile env st
  let est' = applySTS @s $ TRC (env, st, iSig)
  pure $! Invalid.Trace
            { Invalid.validPrefix = tr
            , Invalid.signal = iSig
            , Invalid.errorOrLastState = est'
            }


traceSuchThat
  :: forall s
   . HasTrace s
  => Word64
  -> (Trace s -> Bool)
  -> Gen (Trace s)
traceSuchThat n cond = Gen.filter cond (trace @s n)


ofLengthAtLeast :: Gen (Trace s) -> Int -> Gen (Trace s)
ofLengthAtLeast traceGen minLength =
  Gen.filter ((minLength <=) . traceLength) traceGen


suchThatLastState
  :: forall s
   . Gen (Trace s)
  -> (State  s -> Bool)
  -> Gen (Trace s)
suchThatLastState traceGen cond = Gen.filter (cond . lastState) traceGen

-- | Generate a trace that contains at least one non-trivial signal. See
-- 'HasSizeInfo'.
nonTrivialTrace
  :: forall s
   . (HasTrace s, HasSizeInfo (Signal s))
  => Word64
  -> Gen (Trace s)
nonTrivialTrace ub =
  Gen.filter (any (not . isTrivial) . traceSignals OldestFirst) (trace ub)

class HasSizeInfo sig where
  isTrivial :: sig -> Bool

instance HasSizeInfo [a] where
  isTrivial = null

--------------------------------------------------------------------------------
-- Trace sampling utilities
--------------------------------------------------------------------------------

-- | Sample the maximum trace size, given the generator size and number of
-- samples.
sampleMaxTraceSize
  :: forall s
   . HasTrace s
  => Word64
  -- ^ Trace's upper bound
  -> Int
  -- ^ Generator size
  -> Word64
  -- ^ Number of samples to take
  -> IO Int
sampleMaxTraceSize ub d n =
  maximum <$>
    forM [0..n] (const $ traceLength <$> Gen.sample (Gen.resize (Size d) (trace @s ub)))

randomTrace
  :: forall s
   . HasTrace s
  => Word64
  -> IO (Trace s)
randomTrace ub = Gen.sample (trace ub)


randomTraceOfSize
  :: forall s
   . HasTrace s
  => Word64
  -> IO (Trace s)
randomTraceOfSize desiredTraceLength = Gen.sample (traceOfLength desiredTraceLength)


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
--
classifyTraceLength
  :: Trace s
  -> Word64
  -- ^ Maximum size of the traces
  -> Word64
  -- ^ Steps used to divide the interval
  -> PropertyT IO ()
classifyTraceLength tr = classifySize "trace length:" tr (fromIntegral . traceLength)

-- | Classify the value size as either:
--
-- - being empty
-- - being a singleton
-- - having the given maximum size
-- - belonging to one of the intervals between 2 and the maximum size - 1. The
--   number of intervals are determined by the @step@ parameter.
--
classifySize
  :: (Ord n, Show n, Integral n)
  => String
  -- ^ Prefix to be added to the label intervals
  -> a
  -- ^ Value to classify
  -> (a -> n)
  -- ^ Size function
  -> n
  -- ^ Maximum value size
  -> n
  -- ^ Steps used to divide the size interval
  -> PropertyT IO ()
classifySize prefixLabel value sizeF upBound step = do
  classify (mkLabel "empty")     $ sizeF value == 0
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
        desc = mkLabel $  "[" <> show low <> ", " <> show high <> ")"


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
--
mkIntervals
  :: Integral n
  => n
  -- ^ Interval lower bound
  -> n
  -- ^ Interval upper bound
  -> n
  -- ^ Step size, used to divide the interval in sub-intervals of the same
  -- length.
  -> [(n, n)]
mkIntervals low high step
  | 0 <= low && low <= high && 0 < step =
    [(low + i * step, high `min` (low + (i + 1) * step)) | i <- [0 .. n - 1]]
  | otherwise = []
  where
    highNorm = high - low
    n = highNorm `div` step + 1 `min` (highNorm `mod` step)

-- | Given a function that computes an integral value from a trace, return that
-- value as a ratio of the trace length.
ratio
  :: Integral a
  => (Trace s -> a)
  -> Trace s
  -> Double
ratio f tr = fromIntegral (f tr) / fromIntegral (traceLength tr)

--------------------------------------------------------------------------------
-- Trace properties
--------------------------------------------------------------------------------

-- | Property that simply classifies the lengths of the generated traces.
traceLengthsAreClassified
  :: forall s
   . (HasTrace s, Show (Environment s), Show (State s), Show (Signal s))
  => Word64
  -- ^ Maximum trace length that the signal generator of 's' can generate.
  -> Word64
  -- ^ Lengths of the intervals in which the lengths range should be split.
  -> Property
traceLengthsAreClassified maximumTraceLength intervalSize =
  property $ do
    traceSample <- forAll (trace @s maximumTraceLength)
    classifyTraceLength traceSample maximumTraceLength intervalSize
    success

-- | Check that the signal generator of 's' only generate valid signals.
onlyValidSignalsAreGenerated
  :: forall s
   . (HasTrace s, Show (Environment s), Show (State s), Show (Signal s), HasCallStack)
  => Word64
  -- ^ Maximum trace length.
  -> Property
onlyValidSignalsAreGenerated maximumTraceLength =
  onlyValidSignalsAreGeneratedForTrace (trace @s maximumTraceLength)

-- | Check that the signal generator of 's' only generate valid signals.
onlyValidSignalsAreGeneratedForTrace
  :: forall s
   . (HasTrace s, Show (Environment s), Show (State s), Show (Signal s), HasCallStack)
  => Gen (Trace s)
  -> Property
onlyValidSignalsAreGeneratedForTrace traceGen = property $ do
  tr <- forAll traceGen
  let
    env :: Environment s
    env = _traceEnv tr

    st' :: State s
    st' = lastState tr
  sig <- forAll (sigGen @s env st')
  let result = applySTS @s (TRC(env, st', sig))
  -- TODO: For some reason the result that led to the failure is not shown
  -- (even without using tasty, and setting the condition to True === False)
  footnoteShow st'
  footnoteShow sig
  footnoteShow result
  void $ evalEither result

coverFailures
  :: forall m s a
   . ( MonadTest m
     , HasCallStack
     , Data (PredicateFailure s)
     , Data a
     )
  => CoverPercentage
  -> [PredicateFailure s]
  -- ^ Target predicate failures
  -> a
  -- ^ Structure containing the failures
  -> m ()
coverFailures coverPercentage targetFailures failureStructure = do
  traverse_ coverFailure (toConstr <$> targetFailures)
  where
    coverFailure predicateFailureConstructor =
      cover coverPercentage
            (fromString $ show predicateFailureConstructor)
            (predicateFailureConstructor `elem` failuresConstructors)
      where
        subFailures :: [PredicateFailure s]
        subFailures = extractValues failureStructure

        failuresConstructors :: [Constr]
        failuresConstructors = toConstr <$> subFailures


invalidSignalsAreGenerated
  :: forall s
   . (HasTrace s, Show (Environment s), Show (State s), Show (Signal s), HasCallStack)
  => [(Int, SignalGenerator s)]
  -- ^ Failure profile.
  -> Word64
  -- ^ Maximum trace length.
  -> ([[PredicateFailure s]] -> PropertyT IO ())
  -- ^ Action to run when the an invalid signal is generated.
  -> Property
invalidSignalsAreGenerated failureProfile maximumTraceLength checkFailures = property $ do

  tr <- forAll (invalidTrace @s maximumTraceLength failureProfile)

  cover 80
    "Invalid signals are generated when requested"
    (isLeft $ Invalid.errorOrLastState tr)

  either checkFailures (const success) (Invalid.errorOrLastState tr)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

tinkerWithSigGen
  :: forall g sts
   . ( HasTrace sts, Goblin g (Signal sts)
     , SeedGoblin (Environment sts)
     , SeedGoblin (State sts) )
  => GoblinData g
  -> Environment sts
  -> State sts
  -> Gen (Signal sts)
tinkerWithSigGen gd env state = flip evalState gd $
  seeder env >> seeder state >> tinker (sigGen @sts env state)
