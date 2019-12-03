
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple state transition system over the Identity monad.
module Control.State.Transition.Simple
  ( (?!)
  , failBecause
  , trans
  , applyRuleIndifferently
  , applySTSIndifferently
  , applySTS
  , module Extended
  )
where

import           Control.Monad.Identity (Identity(..))
import           Control.Monad.Free.Church
import qualified Control.State.Transition.Extended as X
import           Control.State.Transition.Extended as Extended hiding ((?!), failBecause, trans, applySTS, applySTSIndifferently, applyRuleIndifferently)

-- | Oh noes!
--
--   This takes a condition (a boolean expression) and a failure and results in
--   a clause which will throw that failure if the condition fails.
(?!) :: STS sts => Bool -> PredicateFailure sts -> Rule sts ctx ()
cond ?! orElse = (pure cond) X.?! orElse

infix 1 ?!

failBecause :: STS sts => PredicateFailure sts -> Rule sts ctx ()
failBecause = (False ?!)

trans
  :: Embed sub super => RuleContext rtype sub -> Rule super rtype (State sub)
trans ctx = wrap $ SubTrans ctx pure

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
