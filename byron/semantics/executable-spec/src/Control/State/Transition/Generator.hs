{-# LANGUAGE AllowAmbiguousTypes #-}
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
  , envGen
  , sigGen
  , trace
  , traceSigGen
  , genTrace
  , traceOfLength
  , traceSuchThat
  , suchThatLastState
  , nonTrivialTrace
  , HasSizeInfo
  , isTrivial
  , sampleMaxTraceSize
  , randomTrace
  , randomTraceOfSize
  , TraceLength (Maximum, Desired)
  -- * Trace classification
  , classifyTraceLength
  , classifySize
  , mkIntervals
  , ratio
  -- * Trace properties
  , traceLengthsAreClassified
  , onlyValidSignalsAreGenerated
  )
where

import           Control.Monad (forM, void)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.String (fromString)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Hedgehog (Gen, Property, PropertyT, classify, evalEither, footnoteShow, forAll,
                     property, success)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Range (Size (Size))
import qualified Hedgehog.Range as Range

--------------------------------------------------------------------------------
-- Temporary imports till hedgehog exposes interleaveTreeT and withGenT
--------------------------------------------------------------------------------
import           Hedgehog.Internal.Gen
import           Hedgehog.Internal.Tree
--------------------------------------------------------------------------------
-- END: Temporary imports till hedgehog exposes interleaveTreeT and withGenT
--------------------------------------------------------------------------------

import           Control.State.Transition (Environment, IRC (IRC), STS, Signal, State, TRC (TRC),
                     applySTS)
import           Control.State.Transition.Trace (Trace, TraceOrder (OldestFirst), closure,
                     lastState, mkTrace, traceLength, traceSignals, _traceEnv)


class STS s => HasTrace s where
  -- | Generate an initial environment that is based on the given trace length.
  envGen
    :: Word64
    -- ^ Trace length that will be used by 'trace' or 'traceOfLength'.
    -> Gen (Environment s)

  sigGen :: Environment s -> State s -> Gen (Signal s)

  trace
    :: Word64
    -- ^ Maximum length of the generated traces. The actual length will be between 0 and this
    -- maximum.
    -> Gen (Trace s)
  trace n = traceSigGen (Maximum n) (sigGen @s)

  traceOfLength
    :: Word64
    -- ^ Desired length of the generated trace. If the signal generator can generate invalid signals
    -- then the resulting trace might not have the given length.
    -> Gen (Trace s)
  traceOfLength n = traceSigGen (Desired n) (sigGen @s)

data TraceLength = Maximum Word64 | Desired Word64

-- | Extract the maximum or desired integer value of the trace length.
traceLengthValue :: TraceLength -> Word64
traceLengthValue (Maximum n) = n
traceLengthValue (Desired n) = n

traceSigGen
  :: forall s
   . HasTrace s
  => TraceLength
  -> (Environment s -> State s -> Gen (Signal s))
  -> Gen (Trace s)
traceSigGen aTraceLength gen = do
  env <- envGen @s (traceLengthValue aTraceLength)
  case applySTS @s (IRC env) of
    -- Hedgehog will give up if the generators fail to produce any valid
    -- initial state, hence we don't have a risk of entering an infinite
    -- recursion.
    Left _pf  -> traceSigGen aTraceLength gen
    -- Applying an initial rule with an environment and state will simply
    -- validate that state, so we do not care which state 'applySTS' returns.
    Right st ->
      case aTraceLength of
        Maximum n -> genTrace n env st gen
        Desired n -> genTraceOfLength n env st gen


-- | Return a (valid) trace generator given an initial state, environment, and
-- signal generator.
--
genTrace
  :: forall s
   . STS s
  => Word64
  -- ^ Trace upper bound. This will be linearly scaled as a function of the
  -- generator size.
  -> Environment s
  -- ^ Environment, which remains constant in the system.
  -> State s
  -- ^ Initial state.
  -> (Environment s -> State s -> Gen (Signal s))
  -- ^ Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  -> Gen (Trace s)
genTrace ub env st0 aSigGen = do
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
  genTraceOfLength n env st0 aSigGen

-- | Return a (valid) trace generator that generates traces of the given size. If the signal
-- generator can generate invalid signals, then the size of resulting trace is not guaranteed.
--
genTraceOfLength
  :: forall s
   . STS s
  => Word64
  -- ^ Desired trace length.
  -> Environment s
  -- ^ Environment, which remains constant in the system.
  -> State s
  -- ^ Initial state.
  -> (Environment s -> State s -> Gen (Signal s))
  -- ^ Signal generator. This generator relies on an environment and a state to
  -- generate a signal.
  -> Gen (Trace s)
genTraceOfLength aTraceLength env st0 aSigGen =
  mapGenT (TreeT . interleaveSigs . runTreeT) $ loop aTraceLength st0 []
  where
    loop
      :: Word64
      -> State s
      -> [(State s, TreeT (MaybeT Identity) (Signal s))]
      -> Gen [(State s, TreeT (MaybeT Identity) (Signal s))]
    loop 0 _ acc = pure acc
    loop d sti acc = do
      sigTree :: TreeT (MaybeT Identity) (Signal s)
        <- toTreeMaybeT $ aSigGen env sti
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
      :: MaybeT Identity (NodeT (MaybeT Identity) [(State s, TreeT (MaybeT Identity) (Signal s))])
      -> MaybeT Identity (NodeT (MaybeT Identity) (Trace s))
    interleaveSigs mst = do
      nodeT :: NodeT (MaybeT Identity) [(State s, TreeT (MaybeT Identity) (Signal s))] <- mst
      let (rootStates, trees) = unzip (nodeValue nodeT)
      NodeT rootSignals children <- interleaveTreeT trees
      pure $! NodeT
        (mkTrace env st0 (zip rootStates rootSignals))
        (fmap (fmap (closure @s env st0)) children)

traceSuchThat
  :: forall s
   . HasTrace s
  => Word64
  -> (Trace s -> Bool)
  -> Gen (Trace s)
traceSuchThat n cond = Gen.filter cond (trace @s n)

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
onlyValidSignalsAreGenerated maximumTraceLength = property $ do
  tr <- forAll (trace @s maximumTraceLength)
  let
    env :: Environment s
    env = _traceEnv tr

    st' :: State s
    st' = lastState tr
  sig <- forAll (sigGen @s env st')
  let result = applySTS @s (TRC(env, st', sig))
  -- TODO: For some reason the result that led to the failure is not shown
  -- (even without using tasty, and setting the condition to True === False)
  footnoteShow result
  void $ evalEither $ result


--------------------------------------------------------------------------------
-- Temporary definitions till hedgehog exposes these
--------------------------------------------------------------------------------

interleaveTreeT :: Monad m => [TreeT m a] -> m (NodeT m [a])
interleaveTreeT =
  fmap interleave . traverse runTreeT

--------------------------------------------------------------------------------
-- END: Temporary definitions till hedgehog exposes these
--------------------------------------------------------------------------------
