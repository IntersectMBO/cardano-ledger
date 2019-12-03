
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Small step state transition systems.
module Control.State.Transition.Extended where

import           Control.Monad (unless)
import           Control.Monad.Identity (Identity(..))
import           Control.Monad.Free.Church
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (modify, runStateT)
import qualified Control.Monad.Trans.State.Strict as MonadState
import           Data.Data (Data, Typeable)
import           Data.Foldable (find, traverse_)
import           Data.Functor ((<&>))
import           Data.Kind (Type)

data RuleType
  = Initial
  | Transition

-- | Singleton instances.
--
--   Since our case is so small we don't bother with the singletons library.
data SRuleType a where
  SInitial :: SRuleType 'Initial
  STransition :: SRuleType 'Transition

class RuleTypeRep t where
  rTypeRep :: SRuleType t

instance RuleTypeRep 'Initial where
  rTypeRep = SInitial

instance RuleTypeRep 'Transition where
  rTypeRep = STransition

-- | Context available to initial rules.
newtype IRC sts = IRC (Environment sts)

-- | Context available to transition rules.
newtype TRC sts = TRC (Environment sts, State sts, Signal sts)

deriving instance
  ( Show (Environment sts)
  , Show (State sts)
  , Show (Signal sts)
  ) => Show (TRC sts)

type family RuleContext (t :: RuleType) = (ctx :: Type -> Type) | ctx -> t where
  RuleContext 'Initial = IRC
  RuleContext 'Transition = TRC

type InitialRule sts = Rule sts 'Initial (State sts)
type TransitionRule sts = Rule sts 'Transition (State sts)

-- | State transition system.
class ( Eq (PredicateFailure a)
      , Show (PredicateFailure a)
      , Monad (BaseM a)
      )
  => STS a where
  -- | Type of the state which the system transitions between.
  type State a :: *
  -- | Signal triggering a state change.
  type Signal a :: *
  -- | Environment type.
  type Environment a :: *

  -- | Monad into which to interpret the rules.
  type BaseM a :: * -> *
  type BaseM a = Identity

  -- | Descriptive type for the possible failures which might cause a transition
  -- to fail.
  --
  -- As a convention, `PredicateFailure`s which are "structural" (meaning that
  -- they are not "throwable" in practice, and are used to pass control from
  -- one transition rule to another) are prefixed with `S_`.
  --
  -- Structural `PredicateFailure`s represent conditions between rules where
  -- the disjunction of all rules' preconditions is equal to `True`. That is,
  -- either one rule will throw a structural `PredicateFailure` and the other
  -- will succeed, or vice-versa.
  data PredicateFailure a :: *

  -- | Rules governing transition under this system.
  initialRules :: [InitialRule a]
  transitionRules :: [TransitionRule a]

-- | Embed one STS within another.
class (STS sub, STS super, BaseM sub ~ BaseM super) => Embed sub super where
  -- | Wrap a predicate failure of the subsystem in a failure of the super-system.
  wrapFailed :: PredicateFailure sub -> PredicateFailure super

instance STS sts => Embed sts sts where
  wrapFailed = id

data Clause sts (rtype :: RuleType) a where
  Lift
    :: STS sts
    => (BaseM sts) a
    -> (a -> b)
    -> Clause sts rtype b
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
(?!) :: Bool -> PredicateFailure sts -> Rule sts ctx ()
cond ?! orElse = wrap $ Predicate cond orElse (pure ())

infix 1 ?!

failBecause :: PredicateFailure sts -> Rule sts ctx ()
failBecause = (False ?!)

trans
  :: Embed sub super => RuleContext rtype sub -> Rule super rtype (State sub)
trans ctx = wrap $ SubTrans ctx pure

liftSTS
  :: STS sts
  => (BaseM sts) a
  -> Rule sts ctx a
liftSTS f = wrap $ Lift f pure

-- | Get the judgment context
judgmentContext :: Rule sts rtype (RuleContext rtype sts)
judgmentContext = wrap $ GetCtx pure

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleIndifferently
  :: forall s m rtype
   . (STS s, RuleTypeRep rtype, m ~ BaseM s)
  => RuleContext rtype s
  -> Rule s rtype (State s)
  -> m (State s, [PredicateFailure s])
applyRuleIndifferently jc r = flip runStateT [] $ foldF runClause r
 where
  runClause :: Clause s rtype a -> MonadState.StateT [PredicateFailure s] m a
  runClause (Lift f next) = next <$> lift f
  runClause (GetCtx next              ) = next <$> pure jc
  runClause (Predicate cond orElse val) = do
    unless cond $ modify (orElse :)
    pure val
  runClause (SubTrans subCtx next) = do
    (ss, sfails) <- lift $ applySTSIndifferently subCtx
    traverse_ (\a -> modify (a :)) $ wrapFailed <$> concat sfails
    next <$> pure ss

applySTSIndifferently
  :: forall s m rtype
   . (STS s, RuleTypeRep rtype, m ~ BaseM s)
  => RuleContext rtype s
  -> m (State s, [[PredicateFailure s]])
applySTSIndifferently ctx =
  successOrFirstFailure <$> applySTSIndifferently' rTypeRep ctx
 where
  successOrFirstFailure xs =
    case find (null . snd) xs of
      Nothing ->
        case xs of
          [] -> error "applySTSIndifferently was called with an empty set of rules"
          (s, _): _ -> (s, snd <$> xs )
      Just (s, _) -> (s, [])

  applySTSIndifferently'
    :: SRuleType rtype
    -> RuleContext rtype s
    -> m [(State s, [PredicateFailure s])]
  applySTSIndifferently' SInitial env =
    applyRuleIndifferently env `traverse` initialRules
  applySTSIndifferently' STransition jc =
    applyRuleIndifferently jc `traverse` transitionRules

applySTS :: forall s m rtype
   . (STS s, RuleTypeRep rtype, m ~ BaseM s)
  => RuleContext rtype s
  -> m (Either [[PredicateFailure s]] (State s))
applySTS ctx = applySTSIndifferently ctx <&> \case
  (st, []) -> Right st
  (_, pfs) -> Left pfs

-- | This can be used to specify predicate failures in STS rules where a value
-- is beyond a certain threshold.
newtype Threshold a = Threshold a
  deriving (Eq, Ord, Show, Data, Typeable)
