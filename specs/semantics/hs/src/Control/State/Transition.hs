{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Small step state transition systems.
module Control.State.Transition where

import Control.Lens
import Data.Maybe (catMaybes)

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
  rules :: [Rule a]

-- | Does a rule generate an initial state?
isInitial :: STS a => Rule a -> Bool
isInitial (Rule _ (Base _)) = True
isInitial _                 = False

initialStates :: forall s . STS s => [State s]
initialStates = catMaybes $ applyBase <$> rules @s
 where
  applyBase (Rule _ (Base b)) = Just b
  applyBase _                 = Nothing

-- | The union of the components of the system available for making judgments.
type JudgmentContext sts = (Environment sts, State sts, Signal sts)

-- | A transition under a state transition system
data Transition sts where
  Transition
    :: (   JudgmentContext sts
        -> State sts
       )
    -> Transition sts

-- | Apply a transition
transition :: Transition sts -> JudgmentContext sts -> State sts
transition (Transition f) = f

-- | Embed one STS within another.
class (STS sub, STS super) => Embed sub super where
  -- | Wrap a predicate failure of the subsystem in a failure of the super-system.
  wrapFailed :: [PredicateFailure sub] -> PredicateFailure super

data PredicateResult sts
  = Passed
  | Failed (PredicateFailure sts)

deriving instance (Eq (PredicateFailure sts)) => Eq (PredicateResult sts)

gatherFailures :: [PredicateResult sts] -> [PredicateFailure sts]
gatherFailures xs = catMaybes $ prToMaybe <$> xs
 where
  prToMaybe Passed     = Nothing
  prToMaybe (Failed x) = Just x

-- | Embed a transition under a sub-system within a super-system.
data EmbeddedTransition sub sts where
  EmbeddedTransition
    :: Embed sub sts
    => Getter (JudgmentContext sts) (JudgmentContext sub)
    -> Rule sub
    -> EmbeddedTransition sub sts

-- | The antecedent to a transition rule.
data Antecedent sts where

  -- | This rule is predicated upon a transition under a (sub-)system.
  SubTrans
    :: Embed sub sts
    => EmbeddedTransition sub sts
    -> Antecedent sts

  Predicate
    :: (   JudgmentContext sts
        -> PredicateResult sts
       )
    -> Antecedent sts

-- | Check the antecedent
checkAntecedent :: JudgmentContext sts -> Antecedent sts -> PredicateResult sts
checkAntecedent jc ant = case ant of
  SubTrans (EmbeddedTransition gjc rule) -> case applyRule (jc ^. gjc) rule of
    Right _   -> Passed
    Left  pfs -> Failed $ wrapFailed pfs
  Predicate p -> p jc

-- | Get the result from a transition under a subsystem, regardless of whether
-- validation passed.
subTransResult
  :: Embed sub super
  => JudgmentContext super
  -> EmbeddedTransition sub super
  -> State sub
subTransResult jc (EmbeddedTransition jcg rule) =
  fst $ applyRuleIndifferently (jc ^. jcg) rule


-- | The consequent to a transition rule.
data Consequent sts where
  -- | The consequent describes a valid base state.
  Base :: State sts -> Consequent sts
  -- | The consequent describes a valid transition between two states.
  Extension :: Transition sts -> Consequent sts

applyConsequent :: STS s => JudgmentContext s -> Consequent s -> State s
applyConsequent jc con = case con of
  Base      st'            -> st'
  Extension (Transition f) -> f jc

-- | A rule within a transition system.
data Rule sts where
  Rule
    :: [Antecedent sts]
    -> Consequent sts
    -> Rule sts

applyRule
  :: STS s
  => JudgmentContext s
  -> Rule s
  -> Either [PredicateFailure s] (State s)
applyRule jc (Rule ants con) = case checkAntecedent jc <$> ants of
  xs | all (== Passed) xs -> Right $ applyConsequent jc con
  xs                      -> Left $ gatherFailures xs

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleIndifferently
  :: STS s => JudgmentContext s -> Rule s -> (State s, [PredicateFailure s])
applyRuleIndifferently jc (Rule ants con) = (res, prs)
 where
  prs = gatherFailures $ checkAntecedent jc <$> ants
  res = applyConsequent jc con
