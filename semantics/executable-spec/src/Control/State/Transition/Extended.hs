{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Small step state transition systems.
module Control.State.Transition.Extended
  ( RuleType (..),
    RuleTypeRep,
    RuleContext,
    IRC (..),
    TRC (..),
    Rule,
    TransitionRule,
    InitialRule,
    Assertion (..),
    STS (..),
    Embed (..),
    (?!),
    (?!:),
    failBecause,
    judgmentContext,
    trans,
    liftSTS,

    -- * Apply STS
    AssertionPolicy (..),
    ValidationPolicy (..),
    ApplySTSOpts (..),
    applySTSOpts,
    applySTS,
    applySTSIndifferently,
    reapplySTS,

    -- * Random thing
    Threshold (..),
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Exception (Exception (..), throw)
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (modify, runStateT)
import qualified Control.Monad.Trans.State.Strict as MonadState
import Data.Data (Data, Typeable)
import Data.Foldable (find, for_, traverse_)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)

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
  ( Show (Environment sts),
    Show (State sts),
    Show (Signal sts)
  ) =>
  Show (TRC sts)

type family RuleContext (t :: RuleType) = (ctx :: Type -> Type) | ctx -> t where
  RuleContext 'Initial = IRC
  RuleContext 'Transition = TRC

type InitialRule sts = Rule sts 'Initial (State sts)

type TransitionRule sts = Rule sts 'Transition (State sts)

-- | An assertion is a validation condition for the STS system in question. It
--   should be used to define properties of the system as a whole that cannot be
--   violated under normal circumstances - e.g. a violation implies a failing in
--   the rule logic.
--
--   Assertions should not check for conditions that may differ between
--   different rules in a system, since the interpreter may stop the system upon
--   presence of a failed assertion.
--
--   Whether assertions are checked is a matter for the STS interpreter.
data Assertion sts
  = -- | Pre-condition. Checked before the rule fires.
    PreCondition String (TRC sts -> Bool)
  | -- | Post-condition. Checked after the rule fires, and given access
    --   to the resultant state as well as the initial context.
    PostCondition String (TRC sts -> State sts -> Bool)

data AssertionViolation sts = AssertionViolation
  { avSTS :: String,
    avMsg :: String,
    avCtx :: TRC sts,
    avState :: Maybe (State sts)
  }

instance Show (AssertionViolation sts) where
  show (AssertionViolation sts msg _ _) =
    "AssertionViolation (" <> sts <> "): " <> msg

instance
  (Typeable sts) =>
  Exception (AssertionViolation sts)

-- | State transition system.
class
  ( Eq (PredicateFailure a),
    Show (PredicateFailure a),
    Monad (BaseM a),
    Typeable a
  ) =>
  STS a
  where
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

  -- | Assertions about the transition system.
  assertions :: [Assertion a]
  assertions = []

  -- | Render an assertion violation.
  --
  --   Defaults to using 'show', but note that this does not know how to render
  --   the context. So for more information you should define your own renderer
  --   here.
  renderAssertionViolation :: AssertionViolation sts -> String
  renderAssertionViolation = show

-- | Embed one STS within another.
class (STS sub, STS super, BaseM sub ~ BaseM super) => Embed sub super where
  -- | Wrap a predicate failure of the subsystem in a failure of the super-system.
  wrapFailed :: PredicateFailure sub -> PredicateFailure super

instance STS sts => Embed sts sts where
  wrapFailed = id

data Clause sts (rtype :: RuleType) a where
  Lift ::
    STS sts =>
    (BaseM sts) a ->
    (a -> b) ->
    Clause sts rtype b
  GetCtx ::
    (RuleContext rtype sts -> a) ->
    Clause sts rtype a
  SubTrans ::
    Embed sub sts =>
    RuleContext rtype sub ->
    -- Subsequent computation with state introduced
    (State sub -> a) ->
    Clause sts rtype a
  Predicate ::
    Either e a ->
    -- Type of failure to return if the predicate fails
    (e -> PredicateFailure sts) ->
    a ->
    Clause sts rtype a

deriving instance Functor (Clause sts rtype)

type Rule sts rtype = F (Clause sts rtype)

-- | Oh noes!
--
--   This takes a condition (a boolean expression) and a failure and results in
--   a clause which will throw that failure if the condition fails.
(?!) :: Bool -> PredicateFailure sts -> Rule sts ctx ()
cond ?! orElse = liftF $ Predicate (if cond then Right () else Left ()) (const orElse) ()

infix 1 ?!

failBecause :: PredicateFailure sts -> Rule sts ctx ()
failBecause = (False ?!)

-- | Oh noes with an explanation
--
--   We interpret this as "What?" "No!" "Because:"
(?!:) :: Either e () -> (e -> PredicateFailure sts) -> Rule sts ctx ()
cond ?!: orElse = liftF $ Predicate cond orElse ()

trans ::
  Embed sub super => RuleContext rtype sub -> Rule super rtype (State sub)
trans ctx = wrap $ SubTrans ctx pure

liftSTS ::
  STS sts =>
  (BaseM sts) a ->
  Rule sts ctx a
liftSTS f = wrap $ Lift f pure

-- | Get the judgment context
judgmentContext :: Rule sts rtype (RuleContext rtype sts)
judgmentContext = wrap $ GetCtx pure

{------------------------------------------------------------------------------
-- STS interpreters
------------------------------------------------------------------------------}

-- | Control which assertions are enabled.
data AssertionPolicy
  = AssertionsAll
  | -- | Only run preconditions
    AssertionsPre
  | -- | Only run postconditions
    AssertionsPost
  | AssertionsOff
  deriving (Eq, Show)

-- | Control which predicates are evaluated during rule processing.
data ValidationPolicy
  = ValidateAll
  | ValidateNone
  deriving (Eq, Show)

data ApplySTSOpts = ApplySTSOpts
  { -- | Enable assertions during STS processing.
    --   If this option is enabled, STS processing will terminate on violation
    --   of an assertion.
    asoAssertions :: AssertionPolicy,
    -- | Validation policy
    asoValidation :: ValidationPolicy
  }
  deriving (Eq, Show)

type STSInterpreter =
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (State s, [[PredicateFailure s]])

type RuleInterpreter =
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  Rule s rtype (State s) ->
  m (State s, [PredicateFailure s])

-- | Apply an STS with options. Note that this returns both the final state and
-- the list of predicate failures.
applySTSOpts ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ApplySTSOpts ->
  RuleContext rtype s ->
  m (State s, [[PredicateFailure s]])
applySTSOpts ApplySTSOpts {asoAssertions, asoValidation} ctx =
  let goRule :: RuleInterpreter
      goRule = applyRuleInternal asoValidation goSTS
      goSTS :: STSInterpreter
      goSTS = applySTSInternal asoAssertions goRule
   in goSTS ctx

applySTS ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (Either [[PredicateFailure s]] (State s))
applySTS ctx =
  applySTSOpts defaultOpts ctx <&> \case
    (st, []) -> Right st
    (_, pfs) -> Left pfs
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateAll
        }

-- | Re-apply an STS.
--
--   It is assumed that the caller of this function has previously applied this
--   STS, and can guarantee that it completed successfully. No predicates will
--   be checked when calling this function.
reapplySTS ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (State s)
reapplySTS ctx =
  applySTSOpts defaultOpts ctx <&> fst
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateNone
        }

applySTSIndifferently ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (State s, [[PredicateFailure s]])
applySTSIndifferently ctx =
  applySTSOpts opts ctx
  where
    opts =
      ApplySTSOpts
        { asoAssertions = AssertionsAll,
          asoValidation = ValidateAll
        }

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleInternal ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ValidationPolicy ->
  -- | Interpreter for subsystems
  STSInterpreter ->
  RuleContext rtype s ->
  Rule s rtype (State s) ->
  m (State s, [PredicateFailure s])
applyRuleInternal vp goSTS jc r = flip runStateT [] $ foldF runClause r
  where
    runClause :: Clause s rtype a -> MonadState.StateT [PredicateFailure s] m a
    runClause (Lift f next) = next <$> lift f
    runClause (GetCtx next) = next <$> pure jc
    runClause (Predicate cond orElse val) = case vp of
      ValidateAll -> do
        case catchError cond throwError of
          Left err -> modify (orElse err :) >> pure val
          Right x -> pure x
      ValidateNone -> pure val
    runClause (SubTrans subCtx next) = do
      (ss, sfails) <- lift $ goSTS subCtx
      traverse_ (\a -> modify (a :)) $ wrapFailed <$> concat sfails
      next <$> pure ss

applySTSInternal ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  AssertionPolicy ->
  -- | Interpreter for rules
  RuleInterpreter ->
  RuleContext rtype s ->
  m (State s, [[PredicateFailure s]])
applySTSInternal ap goRule ctx =
  successOrFirstFailure <$> applySTSInternal' rTypeRep ctx
  where
    successOrFirstFailure xs =
      case find (null . snd) xs of
        Nothing ->
          case xs of
            [] -> error "applySTSInternal was called with an empty set of rules"
            (s, _) : _ -> (s, snd <$> xs)
        Just (s, _) -> (s, [])

    applySTSInternal' ::
      SRuleType rtype ->
      RuleContext rtype s ->
      m [(State s, [PredicateFailure s])]
    applySTSInternal' SInitial env =
      goRule env `traverse` initialRules
    applySTSInternal' STransition jc = do
      when (assertPre ap) $
        for_ (assertions @s) $
          ( \case
              PreCondition msg cond ->
                if cond jc
                  then
                    throw $
                      AssertionViolation
                        { avSTS = show $ typeRep (Proxy @s),
                          avMsg = msg,
                          avCtx = jc,
                          avState = Nothing
                        }
                  else pure ()
              _ -> pure ()
          )
      res <- goRule jc `traverse` transitionRules
      -- We only care about running postconditions if the state transition was
      -- successful.
      case (assertPost ap, successOrFirstFailure res) of
        (True, (st, [])) ->
          for_ (assertions @s) $
            ( \case
                PostCondition msg cond ->
                  if cond jc st
                    then
                      throw $
                        AssertionViolation
                          { avSTS = show $ typeRep (Proxy @s),
                            avMsg = msg,
                            avCtx = jc,
                            avState = Just st
                          }
                    else pure ()
                _ -> pure ()
            )
        _ -> pure ()
      pure res

    assertPre :: AssertionPolicy -> Bool
    assertPre AssertionsAll = True
    assertPre AssertionsPre = True
    assertPre _ = False

    assertPost :: AssertionPolicy -> Bool
    assertPost AssertionsAll = True
    assertPost AssertionsPost = True
    assertPost _ = False

-- | This can be used to specify predicate failures in STS rules where a value
-- is beyond a certain threshold.
--
-- TODO move this somewhere more sensible
newtype Threshold a = Threshold a
  deriving (Eq, Ord, Show, Data, Typeable, NoUnexpectedThunks)
