{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , initEnvGen
  , sigGen
  , trace
  , traceSuchThat
  , suchThatLastState
  , nonTrivialTrace
  , HasSizeInfo
  , isTrivial
  , sampleMaxTraceSize
  , randomTrace
  )
where

import Control.Lens ((^.))
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.List (subsequences)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (Size(Size), linear)

import Control.State.Transition
  ( Environment
  , IRC(IRC)
  , STS
  , Signal
  , State
  , TRC(TRC)
  , applySTS
  )
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , lastState
  , mkTrace
  , runTrace
  , traceLength
  , traceSignals
  , traceEnv
  , traceInitState
  )

class STS s => HasTrace s where
  initEnvGen :: Gen (Environment s)

  sigGen :: Environment s -> State s -> Gen (Signal s)

  trace :: Gen (Trace s)
  trace = do
    env <- initEnvGen @s
    case applySTS @s (IRC env) of
      -- Hedgehog will give up if the generators fail to produce any valid
      -- initial state, hence we don't have a risk of entering an infinite
      -- recursion.
      Left _pf  -> trace
      -- Applying an initial rule with an environment and state will simply
      -- validate that state, so we do not care which state 'applySTS' returns.
      Right st -> genTrace env st (sigGen @s)

-- | Return a (valid) trace generator given an initial state, environment, and
-- signal generator.
genTrace
  :: forall s
   . STS s
  => Environment s
  -> State s
  -> (Environment s -> State s -> Gen (Signal s))
  -> Gen (Trace s)
genTrace env st aSigGen =
  Gen.shrink shrinkTrace $ Gen.prune $ do
    d <- Gen.integral (linear 0 100)
    mkTrace env st <$> go d st []
  where
    go
      :: Int
      -> State s
      -> [(State s, Signal s)]
      -> Gen [(State s, Signal s)]
    go 0 _   acc = return acc
    go d sti acc = do
      mStSig <- genSigSt @s env sti aSigGen
      case mStSig of
        Nothing ->
          go (d - 1) sti acc
        Just (stNext, sig) ->
          go (d - 1) stNext ((stNext, sig): acc)

shrinkTrace
  :: forall s
   . STS s
  => Trace s
  -> [Trace s]
shrinkTrace tr = mkTrace env st0 <$> stSigs
  where
    (_, stSigs) = partitionEithers applied
    applied = (runTrace @s env st0) <$> (subsequences sigs)
    env = tr ^. traceEnv
    st0 = tr ^. traceInitState
    sigs = traceSignals OldestFirst tr

  -- An alternate way to generate a trace of the size of the generator might
  -- be:
  --
  -- >>>  go 0 _   acc = return acc
  -- >>>  go d sti acc = do
  -- >>>    mStSig <- genSigSt @s env sti aSigGen
  -- >>>    case mStSig of
  -- >>>      Nothing ->
  -- >>>        go (d - 1) sti acc
  -- >>>      Just (stNext, sig) ->
  -- >>>        go (d - 1) stNext ((stNext, sig): acc)
  --

-- | Return a signal-and-ensuing-state generator, given an initial state,
-- environment and signal generator.
genSigSt
  :: forall s
   . STS s
  => Environment s
  -> State s
  -> (Environment s -> State s -> Gen (Signal s))
  -> Gen (Maybe (State s, Signal s))
genSigSt env st aSigGen = do
  sig <- aSigGen env st
  -- TODO: we might want to know why are we getting a given failure...
  case applySTS @s (TRC(env, st, sig)) of
    Left _ -> pure Nothing
    Right nextSt -> pure $ Just (nextSt, sig)

traceSuchThat
  :: forall s
   . HasTrace s
  => (Trace s -> Bool)
  -> Gen (Trace s)
traceSuchThat cond = Gen.filter cond (trace @s)

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
  => Gen (Trace s)
nonTrivialTrace =
  Gen.filter (any (not . isTrivial) . traceSignals OldestFirst) trace

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
  => Int
  -- ^ Generator size
  -> Int
  -- ^ Number of samples to take
  -> IO Int
sampleMaxTraceSize d n =
  maximum <$>
    forM [0..n] (const $ traceLength <$> Gen.sample (Gen.resize (Size d) (trace @s)))

randomTrace
  :: forall s
   . HasTrace s
  => IO (Trace s)
randomTrace = Gen.sample trace
