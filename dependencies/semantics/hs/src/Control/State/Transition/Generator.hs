{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Generators for transition systems.
--
--   How should these work?
--   - We start with some initial environment.
--   - We start with some initial base state.
--   - We generate a stream of signals. These might be influenced by some running state
--   - We run each signal through
module Control.State.Transition.Generator where

import           Capability.Reader
import           Capability.State
import           Capability.Writer
import Control.Monad (join)
import Control.Monad.Morph (hoist)
import qualified Control.Monad.State.Strict as MS
import           Control.State.Transition
import Control.Monad.Trans.Class (lift)
import           Data.Bifunctor
    (second)
import           Data.Default
import           Data.List
    (partition)
import           Data.Proxy
    (Proxy)
import           GHC.Generics
    (Generic)
import           Hedgehog
import qualified Hedgehog.Gen as Gen

-- | Progressive generator for STS systems, using
--   state threading underneath.
class STS a => ProgressiveGen a where
  -- | Underlying state for generation
  data GenState a :: *

data Bookkeeping s = Bookkeeping
  { bkGenState    :: GenState s
  , bkState       :: State s
  , bkEnvironment :: Environment s
  , bkTrace       :: [(Signal s, [PredicateFailure s])]
  } deriving Generic

-- | Generation monad
newtype GenM s a = GenM (GenT (MS.State (Bookkeeping s)) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "bkGenState" (GenState s))
    via Field "bkGenState" () (MonadState (GenT (MS.State (Bookkeeping s))))

deriving via Field "bkState" () (MonadState (GenT (MS.State (Bookkeeping s))))
  instance (st ~ State s) => HasState "bkState" st (GenM s)

deriving via Field "bkEnvironment" () (MonadState (GenT (MS.State (Bookkeeping s))))
  instance (env ~ Environment s) => HasState "bkEnvironment" env (GenM s)

deriving via WriterLog (Field "bkTrace" () (MonadState (GenT (MS.State (Bookkeeping s)))))
  instance (sig ~ Signal s) => HasWriter "bkTrace" [(sig, [PredicateFailure s])] (GenM s)

-- | Run the generator, given a generator for the environment and an initial generator state.
runGenM
  :: forall a s. STS s
  => Gen (Environment s)
  -> GenState s
  -> GenM s a
  -> Gen (a, Bookkeeping s)
runGenM envGen genState (GenM act) = do
  env <- envGen
  let bk = Bookkeeping genState (head $ initialStates @s) env []
      trick = ((,) <$> act <*> lift MS.get)
  hoist (return . flip MS.evalState bk) trick

-- | Step an STS generator, using a given signal generator.
stsStepGen
  :: forall s. ProgressiveGen s
     -- | Generator for signals
  => GenM s (Signal s)
     -- | Resulting state
  -> GenM s (State s)
stsStepGen sigGen = do
  env <- get @"bkEnvironment"
  st <- get @"bkState"
  signal <- sigGen
  -- We attempt to apply all (non-base) rules
  let
    jc :: JudgmentContext s
    jc = (env, st, signal)
    (passed, failed) = partition (null . snd)
      $ applyRuleIndifferently @s jc <$> filter (not . isInitial) rules
  st' <- case passed of
    [(st', _)] -> do
      tell @"bkTrace" [(signal, [])]
      return st'
    (st', _):_ -> do
      -- TODO More than one rule could be applied. This is bad.
      tell @"bkTrace" [(signal, [])]
      return st'
    [] -> do
      (st', pfs) <- GenM $ Gen.element failed-- TODO Why is `MonadGen` not immediately derivable for `GenM`?
      tell @"bkTrace" [(signal, pfs)]
      return st'
  put @"bkState" st'
  return st'
