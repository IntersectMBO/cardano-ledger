{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Small step state transition systems.
module Control.State.Transition where

import Control.Lens

-- | State transition system.
class (Show (PredicateFailure a))
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

-- | Rules generating initial states
initialRules :: STS a => [Rule a]
initialRules = filter isInitial rules
 where
  isInitial (Rule _ (Base _)) = True
  isInitial _                 = False

-- | The union of the components of the system available for making judgments.
type JudgmentContext sts = (Environment sts, State sts, Signal sts)

-- | A transition under a state transition system
data Transition sts where
  Transition
    :: (   Environment sts
        -> State sts
        -> Signal sts
        -> State sts
       )
    -> Transition sts

-- | Apply a transition
transition
  :: Transition sts -> Environment sts -> State sts -> Signal sts -> State sts
transition (Transition f) = f

-- | Embed one STS within another.
class (STS sub, STS super) => Embed sub super where
  -- | Extract the state of the subsystem as a component of the super-system.
  stateLens :: Lens' (State super) (State sub)

data PredicateResult sts
  = Passed
  | Failed (PredicateFailure sts)

-- | The antecedent to a transition rule.
data Antecedent sts where

  -- | This rule is predicated upon a transition under a (sub-)system.
  SubTrans
    :: Embed sub sts
    => Getter (JudgmentContext sts) (Environment sub)
    -> Getter (JudgmentContext sts) (Signal sub)
    -> Rule sub
    -> Antecedent sts

  Predicate
    :: (   Environment sts
        -> State sts
        -> Signal sts
        -> PredicateResult sts
       )
    -> Antecedent sts

-- | The consequent to a transition rule.
data Consequent sts where
  -- | The consequent describes a valid base state.
  Base :: State sts -> Consequent sts
  -- | The consequent describes a valid transition between two states.
  Extension :: Transition sts -> Consequent sts

-- | A rule within a transition system.
data Rule sts where
  Rule
    :: [Antecedent sts]
    -> Consequent sts
    -> Rule sts

--------------------------------------------------------------------------------
-- Testing state validity
--------------------------------------------------------------------------------
