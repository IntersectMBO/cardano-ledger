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
) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Core
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
