{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Invalid transition system traces.
--
-- An invalid trace consists of an valid prefix, and a last signal that
-- __might__ be invalid. The validity of the signal depends on the probability
-- of the trace generators of generating invalid signals.
module Test.Control.State.Transition.Invalid.Trace where

import Control.State.Transition (Environment, PredicateFailure, Signal, State)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import qualified Test.Control.State.Transition.Trace as Trace

data Trace s = Trace
  { validPrefix :: !(Trace.Trace s)
  , signal :: !(Signal s)
  -- ^ Last signal in the trace. This might cause a predicate failure, but it
  -- isn't guaranteed to do so, since invalid trace generation is
  -- probabilistic.
  , errorOrLastState :: !(Either (NonEmpty (PredicateFailure s)) (State s))
  }
  deriving (Generic)

deriving instance
  ( Eq (Environment s)
  , Eq (State s)
  , Eq (Signal s)
  , Eq (PredicateFailure s)
  ) =>
  (Eq (Trace s))

deriving instance
  ( Show (Environment s)
  , Show (State s)
  , Show (Signal s)
  , Show (PredicateFailure s)
  ) =>
  (Show (Trace s))

instance
  ( NoThunks (Environment s)
  , NoThunks (State s)
  , NoThunks (Signal s)
  , NoThunks (PredicateFailure s)
  ) =>
  (NoThunks (Trace s))
