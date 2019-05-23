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

import Control.Monad (forM)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Functor.Identity (Identity)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Range (Size(Size))

--------------------------------------------------------------------------------
-- Temporary imports till hedgehog exposes interleaveTreeT and withGenT
--------------------------------------------------------------------------------
import Hedgehog.Internal.Gen
import Hedgehog.Internal.Tree
--------------------------------------------------------------------------------
-- END: Temporary imports till hedgehog exposes interleaveTreeT and withGenT
--------------------------------------------------------------------------------

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
  , traceLength
  , traceSignals
  , closure
  )

class STS s => HasTrace s where
  initEnvGen :: Gen (Environment s)

  sigGen :: Environment s -> State s -> Gen (Signal s)

  trace
    :: Int
    -- ^ Length of the generated trace.
    -> Gen (Trace s)
  trace n = do
    env <- initEnvGen @s
    case applySTS @s (IRC env) of
      -- Hedgehog will give up if the generators fail to produce any valid
      -- initial state, hence we don't have a risk of entering an infinite
      -- recursion.
      Left _pf  -> trace n
      -- Applying an initial rule with an environment and state will simply
      -- validate that state, so we do not care which state 'applySTS' returns.
      Right st -> genTrace n env st (sigGen @s)

-- | Return a (valid) trace generator given an initial state, environment, and
-- signal generator.
--
genTrace
  :: forall s
   . STS s
  => Int
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
  n <- integral_ $ Range.linear 0 ub
  mapGenT (TreeT . interleaveSigs . runTreeT) $ loop n st0 []
  where
    loop
      :: Int
      -> State s
      -> [TreeT (MaybeT Identity) (Signal s)]
      -> Gen [TreeT (MaybeT Identity) (Signal s)]
    loop 0 _ acc = pure acc
    loop d sti acc = do
      sigTree :: TreeT (MaybeT Identity) (Signal s)
        <- toTreeMaybeT $ aSigGen env sti
      let
        --  Take the root of the next-state signal tree.
        mSig = treeValue <$> runDiscardEffect sigTree
      case mSig of
        Nothing ->
          loop (d - 1) sti acc
        Just sig ->
          case applySTS @s (TRC(env, sti, sig)) of
            Left _     -> loop (d - 1) sti acc
            Right sti' -> loop (d - 1) sti' (sigTree : acc)

    interleaveSigs
      :: MaybeT Identity (NodeT (MaybeT Identity) [TreeT (MaybeT Identity) (Signal s)])
      -> MaybeT Identity (NodeT (MaybeT Identity) (Trace s))
    interleaveSigs mst = do
      nodeT :: NodeT (MaybeT Identity) [TreeT (MaybeT Identity) (Signal s)]  <- mst
      lts <- interleaveTreeT (nodeValue nodeT)
      pure $! closure @s env st0 <$> lts

traceSuchThat
  :: forall s
   . HasTrace s
  => Int
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
  => Int
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
  => Int
  -- ^ Trace's upper bound
  -> Int
  -- ^ Generator size
  -> Int
  -- ^ Number of samples to take
  -> IO Int
sampleMaxTraceSize ub d n =
  maximum <$>
    forM [0..n] (const $ traceLength <$> Gen.sample (Gen.resize (Size d) (trace @s ub)))

randomTrace
  :: forall s
   . HasTrace s
  => Int
  -> IO (Trace s)
randomTrace ub = Gen.sample (trace ub)

--------------------------------------------------------------------------------
-- Temporary definitions till hedgehog exposes these
--------------------------------------------------------------------------------

interleaveTreeT :: Monad m => [TreeT m a] -> m (NodeT m [a])
interleaveTreeT =
  fmap interleave . traverse runTreeT

--------------------------------------------------------------------------------
-- END: Temporary definitions till hedgehog exposes these
--------------------------------------------------------------------------------
