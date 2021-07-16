{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple state transition system over the Identity monad.
module Control.State.Transition.Simple
  ( applySTSIndifferently,
    applySTS,
    module Extended,
  )
where

import Control.Monad.Identity (Identity (..))
import Control.State.Transition.Extended as Extended hiding (applySTS, applySTSIndifferently)
import qualified Control.State.Transition.Extended as X

applySTSIndifferently ::
  forall s rtype.
  (STS s, RuleTypeRep rtype, Identity ~ BaseM s) =>
  RuleContext rtype s ->
  (State s, [PredicateFailure s])
applySTSIndifferently ctx = runIdentity $ X.applySTSIndifferently ctx

applySTS ::
  forall s rtype.
  (STS s, RuleTypeRep rtype, BaseM s ~ Identity) =>
  RuleContext rtype s ->
  Either [PredicateFailure s] (State s)
applySTS ctx = runIdentity $ X.applySTS ctx
