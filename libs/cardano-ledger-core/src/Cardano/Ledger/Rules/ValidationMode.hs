{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Describes modes under which we might validate certain rules in the ledger.
--
--   What does this mean? Sometimes, we will want to check only certain
--   conditions specified in the rules. For example, when replaying a previously
--   validated chain, we do not care about rerunning _any_ checks, only making
--   the relevant changes to the ledger state.
module Cardano.Ledger.Rules.ValidationMode (
  -- $static
  lblStatic,
  (?!#),
  (?!#:),
  failBecauseS,

  -- * Interface for independent Tests
  Inject (..),
  Test,
  runTest,
  runTestOnSignal,
  failOnJustStatic,

  -- * Validation with injection of predicate failures
  (?!.),
  checkFailOnJust,
  checkFailOnNonEmpty,
  checkFailOnNonEmptySet,
  checkFailOnNonEmptyMap,

  -- ** Static
  (?!#.),
  checkFailOnLeftStatic,
  checkFailOnJustStatic,
) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Core
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty as NEM
import Data.Map.Strict (Map)
import qualified Data.Set.NonEmpty as NES
import Validation

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
(?!#) = labeledPred $ lblStatic NE.:| []

infix 1 ?!#

-- | Construct a static predicate check with an explanation.
--
--   The choice of '#' as a postfix here is made because often these are crypto
--   checks.
(?!#:) :: Either e () -> (e -> PredicateFailure sts) -> Rule sts ctx ()
(?!#:) = labeledPredE $ lblStatic NE.:| []

infix 1 ?!#:

-- | Fail, if static checks are enabled.
failBecauseS :: PredicateFailure sts -> Rule sts ctx ()
failBecauseS = (False ?!#)

-- ===========================================================

type Test failure = Validation (NonEmpty failure) ()

runTest :: InjectRuleFailure rule f era => Test (f era) -> Rule (EraRule rule era) ctx ()
runTest = validateTrans injectFailure

runTestOnSignal :: InjectRuleFailure rule f era => Test (f era) -> Rule (EraRule rule era) ctx ()
runTestOnSignal = validateTransLabeled injectFailure $ lblStatic NE.:| []

failOnJustStatic :: Maybe a -> (a -> PredicateFailure sts) -> Rule sts ctx ()
failOnJustStatic cond onJust =
  validateTransLabeled id (lblStatic NE.:| []) $ failureOnJust cond onJust
{-# INLINE failOnJustStatic #-}

-- =========== Validation with injection of predicate failures ===========

-- | Same as `?!`, except accepts injectable predicate failure
(?!.) ::
  InjectRuleFailure rule t era =>
  Bool -> t era -> Rule (EraRule rule era) ctx ()
(?!.) cond predFailure = cond ?! injectFailure predFailure
{-# INLINE (?!.) #-}

-- | Same as `?!#`, except accepts injectable predicate failure
(?!#.) ::
  InjectRuleFailure rule t era =>
  Bool -> t era -> Rule (EraRule rule era) ctx ()
(?!#.) cond predFailure = cond ?!# injectFailure predFailure
{-# INLINE (?!#.) #-}

-- | Same as `failOnJust`, except accepts injectable predicate failure
checkFailOnJust ::
  InjectRuleFailure rule t era =>
  Maybe a -> (a -> t era) -> Rule (EraRule rule era) ctx ()
checkFailOnJust cond onJust = failOnJust cond (injectFailure . onJust)
{-# INLINE checkFailOnJust #-}

-- | Same as `failOnNonEmpty`, except accepts injectable predicate failure
checkFailOnNonEmpty ::
  InjectRuleFailure rule t era =>
  Foldable f =>
  f a -> (NonEmpty a -> t era) -> Rule (EraRule rule era) ctx ()
checkFailOnNonEmpty cond onJust = failOnNonEmpty cond (injectFailure . onJust)
{-# INLINE checkFailOnNonEmpty #-}

-- | Same as `failOnNonEmptySet`, except accepts injectable predicate failure
checkFailOnNonEmptySet ::
  (InjectRuleFailure rule t era, Foldable f, Ord a) =>
  f a -> (NES.NonEmptySet a -> t era) -> Rule (EraRule rule era) ctx ()
checkFailOnNonEmptySet cond onJust = failOnNonEmptySet cond (injectFailure . onJust)
{-# INLINE checkFailOnNonEmptySet #-}

-- | Same as `failOnNonEmptyMap`, except accepts injectable predicate failure
checkFailOnNonEmptyMap ::
  InjectRuleFailure rule t era =>
  Map k v -> (NEM.NonEmptyMap k v -> t era) -> Rule (EraRule rule era) ctx ()
checkFailOnNonEmptyMap cond onJust = failOnNonEmptyMap cond (injectFailure . onJust)
{-# INLINE checkFailOnNonEmptyMap #-}

-- | Same as `?!#:`, except accepts injectable predicate failure and is not designed to be used as
-- infix operator
checkFailOnLeftStatic ::
  InjectRuleFailure rule t era =>
  Either e () -> (e -> t era) -> Rule (EraRule rule era) ctx ()
checkFailOnLeftStatic cond predFailure = cond ?!#: (injectFailure . predFailure)
{-# INLINE checkFailOnLeftStatic #-}

-- | Same as `failOnJustStatic`, except accepts injectable predicate failure
checkFailOnJustStatic ::
  InjectRuleFailure rule t era =>
  Maybe a -> (a -> t era) -> Rule (EraRule rule era) ctx ()
checkFailOnJustStatic cond onJust = failOnJustStatic cond (injectFailure . onJust)
{-# INLINE checkFailOnJustStatic #-}
