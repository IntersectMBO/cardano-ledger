{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Traces of transition systems and associated operators.
--
-- This module also includes a minimal domain-specific-language to specify
-- expectations on traces.
module Control.State.Transition.Trace
  ( (.-)
  , (.->)
  , checkTrace
  , Trace
  , TraceOrder (NewestFirst, OldestFirst)
  , mkTrace
  , traceEnv
  , traceInitState
  , traceSignals
  , traceStates
  , preStatesAndSignals
  , traceLength
  , lastState
  )
where

import Control.Lens (makeLenses, (^.), (^..), _1, _2, to)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Test.Tasty.HUnit ((@?=), assertFailure)

import Control.State.Transition
  ( Environment
  , STS
  , Signal
  , State
  , TRC(TRC)
  , applySTS
  , PredicateFailure
  )

-- | A successful trace of a transition system.
--
data Trace s
  = Trace
    { _traceEnv :: !(Environment s)
      -- ^ Environment under which the trace was run.
      , _traceInitState :: !(State s)
      -- ^ Initial state in the trace
      , _traceTrans :: ![(State s, Signal s)]
      -- ^ Signals and resulting states observed in the trace. New elements are
      -- put in front of the list.
    }

makeLenses ''Trace

deriving instance
  (Eq (State s), Eq (Signal s), Eq (Environment s)) => (Eq (Trace s))

deriving instance
  (Show (State s), Show (Signal s), Show (Environment s)) => (Show (Trace s))

-- | Make a trace given an environment and initial state.
mkTrace :: Environment s -> State s -> [(State s, Signal s)] -> Trace s
mkTrace = Trace

-- $setup
-- |
-- >>> :set -XTypeFamilies
-- >>> import Control.State.Transition (initialRules, transitionRules)
-- >>> :{
-- data DUMMY
-- instance STS DUMMY where
--   type Environment DUMMY = Bool
--   type State DUMMY = Int
--   type Signal DUMMY = String
--   data PredicateFailure DUMMY = CeciNEstPasUnePredicateFailure deriving (Eq, Show)
--   initialRules = []
--   transitionRules = []
-- :}

-- | Extract the last state of a trace. Since a trace has at least an initial
-- state, the last state of a trace is always defined.
--
-- Examples:
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> lastState tr0
-- 0
--
-- >>> tr01 = mkTrace True 0 [(1, "one")] :: Trace DUMMY
-- >>> lastState tr01
-- 1
--
-- >>> tr012 = mkTrace True 0 [(2, "two"), (1, "one")] :: Trace DUMMY
-- >>> lastState tr012
-- 2
--
lastState :: Trace s -> State s
lastState tr = case tr ^. traceTrans of
  (st, _):_ -> st
  _ -> tr ^. traceInitState

data TraceOrder = NewestFirst | OldestFirst deriving (Eq)

fromNewestFirst :: TraceOrder -> [a] -> [a]
fromNewestFirst NewestFirst = id
fromNewestFirst OldestFirst = reverse

-- | Retrieve all the signals in the trace, in the order specified.
--
-- Examples:
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> traceSignals NewestFirst tr0
-- []
--
-- >>> tr01 = mkTrace True 0 [(1, "one")] :: Trace DUMMY
-- >>> traceSignals NewestFirst tr01
-- ["one"]
--
-- >>> traceSignals OldestFirst tr01
-- ["one"]
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> traceSignals NewestFirst tr0123
-- ["three","two","one"]
--
-- >>> traceSignals OldestFirst tr0123
-- ["one","two","three"]
--
traceSignals :: TraceOrder -> Trace s -> [Signal s]
traceSignals order tr = fromNewestFirst order (tr ^.. traceTrans . traverse . _2)

-- | Retrieve all the states in the trace, in the order specified.
--
-- Examples:
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> traceStates NewestFirst tr0
-- [0]
--
-- >>> traceStates OldestFirst tr0
-- [0]
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> traceStates NewestFirst tr0123
-- [3,2,1,0]
--
-- >>> traceStates OldestFirst tr0123
-- [0,1,2,3]
--
traceStates :: TraceOrder -> Trace s -> [State s]
traceStates order tr = fromNewestFirst order (xs ++ [x])
  where
    x = tr ^. traceInitState
    xs = tr ^.. traceTrans . traverse . _1

-- | Compute the length of a trace, defined as the number of signals it
-- contains.
--
-- Examples:
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> traceLength tr0
-- 0
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> traceLength tr0123
-- 3
--
traceLength :: Trace s -> Int
traceLength tr = tr ^. traceTrans . to length


-- | Retrieve all the signals in the trace paired with the state prior to the
-- application of the signal.
--
-- Note that the last state in the trace will not be returned, since there is
-- no corresponding signal, i.e. the last state is not the pre-state of any
-- signal in the trace.
--
-- Examples
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> preStatesAndSignals NewestFirst tr0
-- []
--
-- >>> preStatesAndSignals OldestFirst tr0
-- []
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> preStatesAndSignals OldestFirst tr0123
-- [(0,"one"),(1,"two"),(2,"three")]
--
-- >>> preStatesAndSignals NewestFirst tr0123
-- [(2,"three"),(1,"two"),(0,"one")]
--
preStatesAndSignals :: TraceOrder -> Trace s -> [(State s, Signal s)]
preStatesAndSignals OldestFirst tr
  = zip (traceStates OldestFirst tr) (traceSignals OldestFirst tr)
preStatesAndSignals NewestFirst tr
  = reverse $ preStatesAndSignals OldestFirst tr

--------------------------------------------------------------------------------
-- Minimal DSL to specify expectations on traces
--------------------------------------------------------------------------------

-- | Bind the state inside the first argument, and apply the transition
-- function in the @Reader@ environment to that state and given signal,
-- obtaining the resulting state, or an assertion failure if the transition
-- function fails.
(.-)
  :: forall m st sig err
   . ( MonadIO m
     , MonadReader (st -> sig -> Either err st) m
     , Show err
     )
  => m st -> sig -> m st
mSt .- sig = do
  st       <- mSt
  validate <- ask -- Get the validation function from the environment
  case validate st sig of
    Left pfs -> liftIO . assertFailure . show $ pfs
    Right st' -> pure st'

-- | Bind the state inside the first argument, and check whether it is equal to
-- the expected state, given in the second argument.
(.->)
  :: forall m st
   . (MonadIO m, Eq st, Show st)
  => m st -> st -> m st
mSt .-> stExpected = do
  stActual <- mSt
  liftIO $ stActual @?= stExpected
  return stActual

checkTrace
  :: forall s
   . (STS s)
  => Environment s
  -> ReaderT (State s -> Signal s -> Either [PredicateFailure s] (State s)) IO (State s)
  -> IO ()
checkTrace env act =
  void $ runReaderT act (\st sig -> applySTS (TRC(env, st, sig)))
