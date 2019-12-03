
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple state transition system over the Identity monad.
module Control.State.Transition.Simple
  ( applyRuleIndifferently
  , applySTSIndifferently
  , applySTS
  , module Extended
  )
where

import           Control.Monad.Identity (Identity(..))
import qualified Control.State.Transition.Extended as X
import           Control.State.Transition.Extended as Extended hiding (applySTS, applySTSIndifferently, applyRuleIndifferently)

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleIndifferently
  :: forall s rtype
   . (STS s, RuleTypeRep rtype, Identity ~ BaseM s)
  => RuleContext rtype s
  -> Rule s rtype (State s)
  -> (State s, [PredicateFailure s])
applyRuleIndifferently jc r = runIdentity $ X.applyRuleIndifferently jc r

applySTSIndifferently
  :: forall s rtype
   . (STS s, RuleTypeRep rtype, Identity ~ BaseM s)
  => RuleContext rtype s
  -> (State s, [[PredicateFailure s]])
applySTSIndifferently ctx = runIdentity $ X.applySTSIndifferently ctx

applySTS :: forall s rtype
   . (STS s, RuleTypeRep rtype, BaseM s ~ Identity)
  => RuleContext rtype s
  -> Either [[PredicateFailure s]] (State s)
applySTS ctx = runIdentity $ X.applySTS ctx
