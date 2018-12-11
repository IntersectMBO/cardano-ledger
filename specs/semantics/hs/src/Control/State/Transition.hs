{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Small step state transition systems.
module Control.State.Transition where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Free.Church
import Control.Monad.Trans.State.Strict (modify, runState)
import qualified Control.Monad.Trans.State.Strict as MonadState
import Data.Foldable (find, traverse_)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Kind (Type)

data RuleType
  = Initial
  | Transition

-- | Singleton instances.
--
--   Since our case is so small we don't bother with the singletons library.
data SRuleType a where
  SInitial :: SRuleType Initial
  STransition :: SRuleType Transition

class RuleTypeRep t where
  rTypeRep :: SRuleType t

instance RuleTypeRep Initial where
  rTypeRep = SInitial

instance RuleTypeRep Transition where
  rTypeRep = STransition

-- | Context available to initial rules.
newtype IRC sts = IRC (Environment sts)
-- | Context available to transition rules.
newtype TRC sts = TRC (Environment sts, State sts, Signal sts)

type family RuleContext (t :: RuleType) = (ctx :: Type -> Type) | ctx -> t where
  RuleContext Initial = IRC
  RuleContext Transition = TRC

type InitialRule sts = Rule sts Initial (State sts)
type TransitionRule sts = Rule sts Transition (State sts)

-- | State transition system.
class ( Eq (PredicateFailure a)
      , Show (PredicateFailure a)
      )
  => STS a where
  -- | Type of the state which the system transitions between.
  type State a :: *
  -- | Signal triggering a state change.
  type Signal a :: *
  -- | Environment type.
  type Environment a :: *

  -- | Descriptive type for the possible failures which might cause a transition
  -- to fail.
  data PredicateFailure a :: *

  -- | Rules governing transition under this system.
  initialRules :: [InitialRule a]
  transitionRules :: [TransitionRule a]

-- | Embed one STS within another.
class (STS sub, STS super) => Embed sub super where
  -- | Wrap a predicate failure of the subsystem in a failure of the super-system.
  wrapFailed :: PredicateFailure sub -> PredicateFailure super

instance STS sts => Embed sts sts where
  wrapFailed = id

data Clause sts (rtype :: RuleType) a where
  GetCtx :: (RuleContext rtype sts -> a)
         -> Clause sts rtype a
  SubTrans :: Embed sub sts
           => RuleContext rtype sub
              -- Subsequent computation with state introduced
           -> (State sub -> a)
           -> Clause sts rtype a
  Predicate :: Bool
               -- Type of failure to return if the predicate fails
            -> PredicateFailure sts
            -> a
            -> Clause sts rtype a

deriving instance Functor (Clause sts rtype)

type Rule sts rtype = F (Clause sts rtype)

-- | Oh noes!
--
--   This takes a condition (a boolean expression) and a failure and results in
--   a clause which will throw that failure if the condition fails.
(?!) :: STS sts => Bool -> PredicateFailure sts -> Rule sts ctx ()
cond ?! orElse = wrap $ Predicate cond orElse (pure ())

infix 1 ?!

trans
  :: Embed sub super => RuleContext rtype sub -> Rule super rtype (State sub)
trans ctx = wrap $ SubTrans ctx pure

-- | Get the judgment context
judgmentContext :: Rule sts rtype (RuleContext rtype sts)
judgmentContext = wrap $ GetCtx pure

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleIndifferently
  :: forall s rtype
   . (STS s, RuleTypeRep rtype)
  => RuleContext rtype s
  -> Rule s rtype (State s)
  -> (State s, [PredicateFailure s])
applyRuleIndifferently jc r = flip runState [] $ foldF runClause r
 where
  runClause :: Clause s rtype a -> MonadState.State [PredicateFailure s] a
  runClause (GetCtx next              ) = next <$> pure jc
  runClause (Predicate cond orElse val) = do
    unless cond $ modify (orElse :)
    pure val
  runClause (SubTrans subCtx next) = do
    let (ss, sfails) = applySTSIndifferently subCtx
    traverse_ (\a -> modify (a :)) $ wrapFailed <$> sfails
    next <$> pure ss

applySTSIndifferently
  :: forall s rtype
   . (STS s, RuleTypeRep rtype)
  => RuleContext rtype s
  -> (State s, [PredicateFailure s])
applySTSIndifferently ctx =
  successOrFirstFailure $ applySTSIndifferently' @s @rtype rTypeRep ctx
 where
  successOrFirstFailure xs = fromMaybe (head xs) $ find (not . null . snd) xs
  applySTSIndifferently'
    :: forall s rtype
     . STS s
    => SRuleType rtype
    -> RuleContext rtype s
    -> [(State s, [PredicateFailure s])]
  applySTSIndifferently' SInitial env =
    map (applyRuleIndifferently env) initialRules
  applySTSIndifferently' STransition jc =
    map (applyRuleIndifferently jc) transitionRules
