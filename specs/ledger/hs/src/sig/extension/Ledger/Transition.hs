{-# LANGUAGE LambdaCase #-}
{- | Ledger transitions.

This module describes ledger transitions; that is, the rules which allow one to
define a valid ledger.

These serve the dual purpose of _generation_ and _validation_ of ledgers.

-}

module Ledger.Transition where

import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Data.Either (rights)
import Extension

-- | A 'Transition' specifies the conditions under which some signal might
-- effect a state transition over some state.
--
-- Given a state st and a signal s, we have two options:
--
-- - The signal is _invalid_ for this given state, and no transition is
--   effected.
--
-- - The signal is valid for the state, and some function is applied to the
--   state to generate a new state.
--
-- The question of validity is determined by a set of preconditions which apply
-- over the state and signal.
data Transition = Transition
  { _tPredicates :: [Predicate]
  , _tUncheckedApply :: State -> Signal -> State
  }

-- | Apply a transition, checking the preconditions.
applyTransition :: Transition -> State -> Signal -> Either [PredicateFailure] State
applyTransition (Transition preds app) st sig =
    case gatherFailures $ checkPredicate <$> preds of
      [] -> Right $ app st sig
      xs -> Left xs
  where
    checkPredicate (Predicate _ validate) = validate st sig
    gatherFailures :: [PredicateResult] -> [PredicateFailure]
    gatherFailures = mapMaybe (\case Passed -> Nothing; Failed pr -> Just pr)

-- | The result of a predicate
data PredicateResult
  = Passed
  | Failed PredicateFailure

-- | A precondition on the application of a signal to a state.
data Predicate = Predicate
  { _pName :: T.Text
  , _pValidate :: State -> Signal -> PredicateResult
  }

validateAll
  :: [Transition] -- ^ Transition rules for the system
  -> State -- ^ State to validate
  -> Bool -- TODO This should be something more like `Validity`
validateAll _ st | st `elem` baseStates = True
validateAll transitions st =
  let
    (oldSt, signal) = peelState st
    canTransition = st `elem` (rights $ (\t -> applyTransition t oldSt signal) <$> transitions)
  in canTransition && validateAll transitions st
