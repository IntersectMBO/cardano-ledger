{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Describes modes under which we might validate certain rules in the ledger.
--
--   What does this mean? Sometimes, we will want to check only certain
--   conditions specified in the rules. For example, when replaying a previously
--   validated chain, we do not care about rerunning _any_ checks, only making
--   the relevant changes to the ledger state.
module Cardano.Ledger.Rules.ValidationMode
  ( -- $static
    lblStatic,
    (?!#),
    (?!#:),
    failBecauseS,
    applySTSNonStatic,
    applySTSValidateSuchThat,

    -- * Interface with validation-selective libarary
    runValidation,
    runValidationTrans,
    runValidationTransMaybe,
    runValidationStatic,
    runValidationStaticTrans,
    runValidationStaticTransMaybe,
    mapMaybeValidation,

    -- * Interface for independent Tests
    Inject (..),
    InjectMaybe (..),
    Test,
    runTest,
    runTestMaybe,
    runTestOnSignal,
  )
where

import Control.State.Transition.Extended
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Validation

applySTSValidateSuchThat ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ([Label] -> Bool) ->
  RuleContext rtype s ->
  m (Either [PredicateFailure s] (State s))
applySTSValidateSuchThat cond =
  applySTSOptsEither opts
  where
    opts =
      ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateSuchThat cond,
          asoEvents = EPDiscard
        }

--------------------------------------------------------------------------------
-- Static checks
--------------------------------------------------------------------------------

-- * Static checks

--

-- $static
--
-- Static checks are used to indicate that a particular predicate depends only
-- on the signal to the transition, rather than the state or environment. This
-- is particularly relevant where the signal is something such as a transaction,
-- which is fixed, whereas the state and environment depend upon the chain tip
-- upon which we are trying to build a block.

-- | Indicates that this check depends only upon the signal to the transition,
-- not the state or environment.
lblStatic :: Label
lblStatic = "static"

-- | Construct a static predicate check.
--
--   The choice of '#' as a postfix here is made because often these are crypto
--   checks.
(?!#) :: Bool -> PredicateFailure sts -> Rule sts ctx ()
(?!#) = labeledPred [lblStatic]

infix 1 ?!#

-- | Construct a static predicate check with an explanation.
--
--   The choice of '#' as a postfix here is made because often these are crypto
--   checks.
(?!#:) :: Either e () -> (e -> PredicateFailure sts) -> Rule sts ctx ()
(?!#:) = labeledPredE [lblStatic]

infix 1 ?!#:

-- | Fail, if static checks are enabled.
failBecauseS :: PredicateFailure sts -> Rule sts ctx ()
failBecauseS = (False ?!#)

-- | Apply an STS system and do not validate any static checks.
applySTSNonStatic ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (Either [PredicateFailure s] (State s))
applySTSNonStatic = applySTSValidateSuchThat (notElem lblStatic)

-- | Fail with all `PredicateFailure`'s in STS if `Validation` was unsuccessful.
runValidation ::
  Validation (NonEmpty (PredicateFailure sts)) () -> Rule sts ctx ()
runValidation v = whenFailure_ v (traverse_ (\pf -> pf `seq` failBecause pf))

-- | Same as `runValidation`, except with ability to translate opaque failures
-- into `PredicateFailure`s with a help of supplied function.
runValidationTrans ::
  (e -> PredicateFailure sts) ->
  Validation (NonEmpty e) () ->
  Rule sts ctx ()
runValidationTrans toPredicateFailure v =
  whenFailure_ v (traverse_ (\e -> failBecause $! toPredicateFailure e))

-- | Same as `runValidationTrans`, but makes it possible to filter out failures
-- with the translating function when it returns `Nothing`.
runValidationTransMaybe ::
  (e -> Maybe (PredicateFailure sts)) ->
  Validation (NonEmpty e) () ->
  Rule sts ctx ()
runValidationTransMaybe toPredicateFailureMaybe =
  runValidation . mapMaybeValidation toPredicateFailureMaybe

-- | Same as `runValidation`, but will label predicate failures as @"static"@
runValidationStatic ::
  Validation (NonEmpty (PredicateFailure sts)) () -> Rule sts ctx ()
runValidationStatic v = whenFailure_ v (traverse_ failBecauseS)

-- | Same as `runValidationTrans`, but will label predicate failures as @"static"@
runValidationStaticTrans ::
  (e -> PredicateFailure sts) ->
  Validation (NonEmpty e) () ->
  Rule sts ctx ()
runValidationStaticTrans toPredicateFailure v =
  whenFailure_ v (traverse_ (failBecauseS . toPredicateFailure))

-- | Same as `runValidationTransMaybe`, but will label predicate failures as @"static"@
runValidationStaticTransMaybe ::
  (e -> Maybe (PredicateFailure sts)) ->
  Validation (NonEmpty e) () ->
  Rule sts ctx ()
runValidationStaticTransMaybe toPredicateFailure =
  runValidationStatic . mapMaybeValidation toPredicateFailure

-- | Helper function to filter out unused failures
mapMaybeValidation ::
  (e -> Maybe e') ->
  Validation (NonEmpty e) () ->
  Validation (NonEmpty e') ()
mapMaybeValidation toPredicateFailureMaybe =
  maybe (Success ()) Failure
    . NE.nonEmpty
    . fromFailure []
    . first (mapMaybe toPredicateFailureMaybe . NE.toList)

-- ===========================================================

class Inject t s where
  inject :: t -> s

class InjectMaybe t s where
  injectMaybe :: t -> Maybe s

type Test failure = Validation (NonEmpty failure) ()

runTest :: Inject t (PredicateFailure sts) => Test t -> Rule sts ctx ()
runTest x = runValidationTrans inject x

runTestOnSignal :: Inject t (PredicateFailure sts) => Test t -> Rule sts ctx ()
runTestOnSignal x = runValidationStaticTrans inject x

runTestMaybe :: InjectMaybe t (PredicateFailure sts) => Test t -> Rule sts ctx ()
runTestMaybe x = runValidationTransMaybe injectMaybe x
