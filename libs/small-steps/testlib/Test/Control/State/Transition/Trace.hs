{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Traces of transition systems and associated operators.
--
-- This module also includes a minimal domain-specific-language to specify
-- expectations on traces.
module Test.Control.State.Transition.Trace (
  SigState (..),

  -- * Trace checking
  (.-),
  (.->),
  (.->>),
  checkTrace,

  -- * Trace
  Trace (..),
  TraceOrder (NewestFirst, OldestFirst),
  mkTrace,
  traceEnv,
  traceInitState,
  traceSignals,
  traceStates,
  preStatesAndSignals,
  SourceSignalTarget (..),
  sourceSignalTargets,
  traceLength,
  traceInit,
  lastState,
  lastSignal,
  firstAndLastState,
  closure,

  -- * Miscellaneous utilities
  extractValues,
  applySTSTest,
  getEvents,
  splitTrace,
) where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.State.Transition.Extended hiding (Assertion, trans)
import Data.Data (Data, Typeable, cast, gmapQ)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Sequence.Strict (StrictSeq (Empty, (:<|), (:|>)))
import qualified Data.Sequence.Strict as SS
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', lens, to, (^.), (^..))
import Lens.Micro.TH (makeLenses)
import NoThunks.Class (NoThunks (..))
import Test.Cardano.Ledger.Binary.TreeDiff (ToExpr, assertExprEqualWithMessage)
import Test.Tasty.HUnit (assertFailure, (@?=))

-- Signal and resulting state.
--
-- Strict in both arguments, unlike a tuple.
data SigState s = SigState !(State s) !(Signal s)
  deriving (Generic)

transSt :: Lens' (SigState s) (State s)
transSt = lens (\(SigState st _) -> st) (\(SigState _ x) st -> SigState st x)

transSig :: Lens' (SigState s) (Signal s)
transSig = lens (\(SigState _ sig) -> sig) (\(SigState x _) sig -> SigState x sig)

deriving instance
  (Eq (State s), Eq (Signal s)) => (Eq (SigState s))

deriving instance
  (Show (State s), Show (Signal s)) => (Show (SigState s))

instance
  ( NoThunks (State s)
  , NoThunks (Signal s)
  ) =>
  (NoThunks (SigState s))

-- | A successful trace of a transition system.
data Trace s = Trace
  { _traceEnv :: !(Environment s)
  -- ^ Environment under which the trace was run.
  , _traceInitState :: !(State s)
  -- ^ Initial state in the trace
  , _traceTrans :: !(StrictSeq (SigState s))
  -- ^ Signals and resulting states observed in the trace. New elements are
  -- put in front of the list.
  }
  deriving (Generic)

makeLenses ''Trace

deriving instance
  ( NFData (Environment s)
  , NFData (State s)
  , NFData (SigState s)
  ) =>
  (NFData (Trace s))

deriving instance
  (Eq (State s), Eq (Signal s), Eq (Environment s)) => (Eq (Trace s))

deriving instance
  (Show (State s), Show (Signal s), Show (Environment s)) => (Show (Trace s))

instance
  ( NoThunks (Environment s)
  , NoThunks (State s)
  , NoThunks (Signal s)
  ) =>
  (NoThunks (Trace s))

-- | Make a trace given an environment and initial state.
mkTrace :: Environment s -> State s -> [(State s, Signal s)] -> Trace s
mkTrace env initState sigs = Trace env initState sigs'
  where
    sigs' = uncurry SigState <$> StrictSeq.fromList sigs

-- $setup
-- |
-- >>> :set -XTypeFamilies
-- >>> import Control.State.Transition (initialRules, transitionRules)
-- >>> import Control.State.Transition.Extended (STS (..))
-- >>> :{
-- data DUMMY
-- data DummyPredicateFailure = CeciNEstPasUnePredicateFailure deriving (Eq, Show)
-- instance STS DUMMY where
--   type Environment DUMMY = Bool
--   type State DUMMY = Int
--   type Signal DUMMY = String
--   type PredicateFailure DUMMY = DummyPredicateFailure
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
lastState :: Trace s -> State s
lastState Trace {_traceInitState, _traceTrans} =
  case _traceTrans of
    SigState st _ :<| _ -> st
    _ -> _traceInitState

-- | Get the last applied signal in a trace (this is, the newest signal).
--
--
-- Examples:
--
-- >>> :set -XScopedTypeVariables
-- >>> import Control.Exception (catch, ErrorCall)
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> print (lastSignal tr0) `catch` (\(_ :: ErrorCall) -> putStrLn "error!")
-- "error!
--
-- dnadales: In the example above I don't know why the doctests is swallowing
-- the last @"@.
--
-- >>> tr01 = mkTrace True 0 [(1, "one")] :: Trace DUMMY
-- >>> lastSignal tr01
-- "one"
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> lastSignal tr0123
-- "three"
lastSignal :: HasCallStack => Trace s -> Signal s
lastSignal Trace {_traceTrans} =
  case _traceTrans of
    Empty -> error "lastSignal was called with a trace without signals"
    SigState _st signal :<| _ -> signal

-- | Return the first and last state of the trace.
--
-- The first state is returned in the first component of the result tuple.
--
-- Examples:
--
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> firstAndLastState tr0
-- (0,0)
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> firstAndLastState tr0123
-- (0,3)
firstAndLastState :: Trace s -> (State s, State s)
firstAndLastState tr = (_traceInitState tr, lastState tr)

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
traceSignals :: TraceOrder -> Trace s -> [Signal s]
traceSignals order tr = fromNewestFirst order (tr ^.. traceTrans . traverse . transSig)

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
traceStates :: TraceOrder -> Trace s -> [State s]
traceStates order tr = fromNewestFirst order (xs ++ [x])
  where
    x = tr ^. traceInitState
    xs = tr ^.. traceTrans . traverse . transSt

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
traceLength :: Trace s -> Int
traceLength tr = tr ^. traceTrans . to length

-- | Take all but the newest signal in the trace.
--
-- Precondition: the trace must contain at least one signal
--
-- Examples:
--
--
-- >>> :set -XScopedTypeVariables
-- >>> import Control.Exception (catch, ErrorCall)
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> print (traceInit tr0) `catch` (\(_ :: ErrorCall) -> print "error!")
-- "error!"
--
-- >>> tr01 = mkTrace True 0 [(1, "one")] :: Trace DUMMY
-- >>> traceInit tr01
-- Trace {_traceEnv = True, _traceInitState = 0, _traceTrans = StrictSeq {fromStrict = fromList []}}
--
-- >>> tr012 = mkTrace True 0 [(2, "two"), (1, "one")] :: Trace DUMMY
-- >>> traceInit tr012
-- Trace {_traceEnv = True, _traceInitState = 0, _traceTrans = StrictSeq {fromStrict = fromList [SigState 1 "one"]}}
traceInit :: HasCallStack => Trace s -> Trace s
traceInit tr@Trace {_traceTrans} =
  case _traceTrans of
    Empty -> error "traceInit was called with a trace without signals"
    _ :<| trans -> tr {_traceTrans = trans}

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
preStatesAndSignals :: TraceOrder -> Trace s -> [(State s, Signal s)]
preStatesAndSignals OldestFirst tr =
  zip (traceStates OldestFirst tr) (traceSignals OldestFirst tr)
preStatesAndSignals NewestFirst tr =
  reverse $ preStatesAndSignals OldestFirst tr

-- | Apply the signals in the list and elaborate a trace with the resulting
-- states.
--
-- If any of the signals cannot be applied, then it is discarded, and the next
-- signal is tried.
--
-- >>> :set -XTypeFamilies
-- >>> :set -XTypeApplications
-- >>> import Control.State.Transition (initialRules, transitionRules, judgmentContext)
-- >>> import Control.State.Transition.Extended (TRC (..))
-- >>> import Data.Functor.Identity
-- >>> :{
-- data ADDER
-- data AdderPredicateFailure = NoFailuresPossible deriving (Eq, Show)
-- instance STS ADDER where
--   type Environment ADDER = ()
--   type State ADDER = Int
--   type Signal ADDER = Int
--   type PredicateFailure ADDER = AdderPredicateFailure
--   initialRules = [ pure 0 ]
--   transitionRules =
--     [ do
--         TRC ((), st, inc) <- judgmentContext
--         pure $! st + inc
--     ]
-- :}
--
-- >>> runIdentity $ closure @ADDER () 0 [3, 2, 1]
-- Trace {_traceEnv = (), _traceInitState = 0, _traceTrans = StrictSeq {fromStrict = fromList [SigState 6 3,SigState 3 2,SigState 1 1]}}
--
-- >>> runIdentity $ closure @ADDER () 10 [-3, -2, -1]
-- Trace {_traceEnv = (), _traceInitState = 10, _traceTrans = StrictSeq {fromStrict = fromList [SigState 4 (-3),SigState 7 (-2),SigState 9 (-1)]}}
closure ::
  forall s m.
  (STS s, m ~ BaseM s) =>
  Environment s ->
  State s ->
  -- | List of signals to apply, where the newest signal comes first.
  [Signal s] ->
  m (Trace s)
closure env st0 sigs = mkTrace env st0 <$> loop st0 (reverse sigs) []
  where
    loop _ [] acc = pure acc
    loop sti (sig : sigs') acc =
      applySTSTest @s (TRC (env, sti, sig)) >>= \case
        Left _ -> loop sti sigs' acc
        Right sti' -> loop sti' sigs' ((sti', sig) : acc)

--------------------------------------------------------------------------------
-- Minimal DSL to specify expectations on traces
--------------------------------------------------------------------------------

-- | Bind the state inside the first argument, and apply the transition
-- function in the @Reader@ environment to that state and given signal,
-- obtaining the resulting state, or an assertion failure if the transition
-- function fails.
(.-) ::
  forall m st sig err.
  ( MonadIO m
  , MonadReader (st -> sig -> Either err st) m
  , Show err
  , HasCallStack
  ) =>
  m st ->
  sig ->
  m st
mSt .- sig = do
  st <- mSt
  validation <- ask -- Get the validation function from the environment
  case validation st sig of
    Left pfs -> liftIO . assertFailure . show $ pfs
    Right st' -> pure st'

-- | Bind the state inside the first argument, and check whether it is equal to
-- the expected state, given in the second argument. If it isn't raise an Assertion that
-- uses a tree-diff to describe the differences.
(.->>) ::
  forall m st.
  (MonadIO m, Eq st, ToExpr st, HasCallStack) =>
  m st ->
  st ->
  m st
mSt .->> stExpected = do
  stActual <- mSt
  liftIO $ assertExprEqualWithMessage "Check trace with (.->>) fails" stExpected stActual
  return stActual

-- | Bind the state inside the first argument, and check whether it is equal to
-- the expected state, given in the second argument. raises an Assertion if it does not.
(.->) ::
  forall m st.
  (MonadIO m, Eq st, Show st, HasCallStack) =>
  m st ->
  st ->
  m st
mSt .-> stExpected = do
  stActual <- mSt
  liftIO $ stActual @?= stExpected
  return stActual

checkTrace ::
  forall s m.
  (STS s, BaseM s ~ m) =>
  (forall a. m a -> a) ->
  Environment s ->
  ReaderT
    ( State s ->
      Signal s ->
      Either (NonEmpty (PredicateFailure s)) (State s)
    )
    IO
    (State s) ->
  IO ()
checkTrace interp env act =
  void $ runReaderT act (\st sig -> interp $ applySTSTest @s (TRC (env, st, sig)))

-- | Extract all the values of a given type.
--
-- Examples:
--
-- >>> extractValues "hello" :: [Char]
-- "hello"
--
-- >>> extractValues ("hello", " " ,"world") :: [Char]
-- "hello world"
--
-- >>> extractValues "hello" :: [Int]
-- []
--
-- >>> extractValues ([('a', 0 :: Int), ('b', 1)] :: [(Char, Int)]) :: [Int]
-- [0,1]
--
-- >>> extractValues (["hello"] :: [[Char]], 1, 'z') :: [[Char]]
-- ["hello","ello","llo","lo","o",""]
--
-- >>> extractValues ("hello", 'z') :: [Char]
-- "zhello"
extractValues :: forall d a. (Data d, Typeable a) => d -> [a]
extractValues d =
  catMaybes (gmapQ extractValue d)
    ++ concat (gmapQ extractValues d)
  where
    extractValue :: forall d1. Data d1 => d1 -> Maybe a
    extractValue d1 = cast d1

data SourceSignalTarget a = SourceSignalTarget
  { source :: State a
  , target :: State a
  , signal :: Signal a
  }

deriving instance (Eq (State a), Eq (Signal a)) => Eq (SourceSignalTarget a)

deriving instance (Show (State a), Show (Signal a)) => Show (SourceSignalTarget a)

-- | Extract triplets of the form [SourceSignalTarget {source = s, signal = sig, target =
-- t)] from a trace. For a valid trace, each source state can reach a target
-- state via the given signal.
--
-- Examples
--
--
-- >>> tr0 = mkTrace True 0 [] :: Trace DUMMY
-- >>> sourceSignalTargets tr0
-- []
--
-- >>> tr0123 = mkTrace True 0 [(3, "three"), (2, "two"), (1, "one")] :: Trace DUMMY
-- >>> sourceSignalTargets tr0123
-- [SourceSignalTarget {source = 0, target = 1, signal = "one"},SourceSignalTarget {source = 1, target = 2, signal = "two"},SourceSignalTarget {source = 2, target = 3, signal = "three"}]
sourceSignalTargets :: forall a. Trace a -> [SourceSignalTarget a]
sourceSignalTargets trace = zipWith3 SourceSignalTarget states statesTail signals
  where
    signals = traceSignals OldestFirst trace
    states = traceStates OldestFirst trace
    statesTail =
      case states of
        [] -> error "Control.State.Transition.Trace.sourceSignalTargets: Unexpected empty list"
        (_ : xs) -> xs

-- | Apply STS checking assertions.
applySTSTest ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (Either (NonEmpty (PredicateFailure s)) (State s))
applySTSTest = applySTSOptsEither defaultOpts
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = AssertionsAll
        , asoValidation = ValidateAll
        , asoEvents = EPDiscard
        }

-- | Extract the Events from a trace, by re-applying the signal with
--   assertions and vaidations turned off, but events turned on.
getEvents :: forall sts. STS sts => Trace sts -> BaseM sts [[Event sts]]
getEvents (Trace env s0 pairs) = mapM action (map make pairs2)
  where
    pairs2 = unstagger s0 (toList pairs)
    make (state, sig) = (env, state, sig)
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = AssertionsOff
        , asoValidation = ValidateNone
        , asoEvents = EPReturn
        }
    action :: (Environment sts, State sts, Signal sts) -> BaseM sts [Event sts]
    action x = do
      item <- applySTSOptsEither @sts @(BaseM sts) @'Transition defaultOpts (TRC x)
      case item of
        Left _ -> pure []
        Right (_, events) -> pure (reverse events)
    -- A Trace stores pairs of State and Signal (called a SigState). The pairs
    -- consist of the state that came about by appying the signal, so the index
    -- of the two parts don't match up. For example here is a sample
    -- [(s_n+1,dn), ... ,(s5,d4),(s4,d3),(s3,d2),(s2,d1)] si is a state and dj is a signal.
    -- Note how the indexes don't matchup. When we compute events we want the indices to align.
    -- We want it to look like this [(sn,dn), ... ,(s5,d5),(s4,d4),(s3,d3),(s2,d2),(s1,d1)]
    -- We call this transformation, the unstagger transformation.
    unstagger :: State s -> [SigState s] -> [(State s, Signal s)]
    unstagger _ [] = []
    unstagger s1 [(SigState _s2 d1)] = [(s1, d1)]
    unstagger s1 ((SigState _s4 d3) : (more@((SigState s3 _d2) : _))) = (s3, d3) : unstagger s1 more

-- | Given a StrictSeq of SigState (as found in a Trace, where earlier things are to the right), and
--   Given 'p' that compares later states to earlier states, split at the boundary where
--   the 'p' detects a change. Used to make a list of shorter Traces from a big Trace. Usually
--   'p' dectects changes in the Epoch (this marks the epoch boundary), but might have other uses.
--   Note that the last sub-Trace might be ill-formed, because we run out of Trace elements,
--   rather than detecting the changes guarded by 'p'
splitAtChange ::
  (State s -> State s -> Bool) ->
  StrictSeq (SigState s) ->
  State s ->
  StrictSeq (SigState s) ->
  [(StrictSeq (SigState s), State s)] ->
  [(StrictSeq (SigState s), State s)]
splitAtChange _p SS.Empty a0 ans1 ans2 = (ans1, a0) : ans2 -- Might be ill-formed
splitAtChange p (xs@(_ :|> x1) :|> x0) a0 ans1 ans2 =
  if p s1 s0
    then splitAtChange p xs a0 (x0 :<| ans1) ans2
    else splitAtChange p xs s0 SS.Empty ((x1 :<| (x0 :<| ans1), a0) : ans2)
  where
    (SigState s1 _) = x1
    (SigState s0 _) = x0
splitAtChange _p (SS.Empty :|> x1) a0 ans1 ans2 = (((x1 :<| ans1), a0) : ans2) -- Might be ill-formed

-- | Split a Trace into several shorter traces. The leftmost Trace
--   (at the front of the list) might be ill-formed, depending on what 'p' does.
splitTrace :: (State s -> State s -> Bool) -> Trace s -> [Trace s]
splitTrace p (Trace env s0 sigstates) = map f xs
  where
    f (sigstates2, s) = Trace env s sigstates2
    xs = splitAtChange p sigstates s0 Empty []
