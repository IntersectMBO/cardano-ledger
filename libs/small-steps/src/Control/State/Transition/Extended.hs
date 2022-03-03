{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
    AssertionViolation (..),
    AssertionException (..),
    STS (..),
    STUB,
    Embed (..),
    (?!),
    (?!:),
    Label,
    SingEP (..),
    EventPolicy (..),
    EventReturnType,
    labeledPred,
    labeledPredE,
    ifFailureFree,
    failBecause,
    judgmentContext,
    trans,
    liftSTS,
    tellEvent,
    tellEvents,
    EventReturnTypeRep,
    mapEventReturn,

    -- * Apply STS
    AssertionPolicy (..),
    ValidationPolicy (..),
    ApplySTSOpts (..),
    applySTSOpts,
    applySTSOptsEither,
    applySTS,
    applySTSIndifferently,
    reapplySTS,
    globalAssertionPolicy,

    -- * Exported to allow running rules independently
    applySTSInternal,
    applyRuleInternal,
    RuleInterpreter,
    STSInterpreter,

    -- * Random thing
    Threshold (..),
    sfor_,
  )
where

import Control.Exception (Exception (..), throw)
import Control.Monad (when)
import Control.Monad.Free.Church
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Class (MonadState (..), modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bifunctor (Bifunctor (second), first)
import Data.Coerce (Coercible, coerce)
import Data.Data (Data, Typeable)
import Data.Default.Class (Default, def)
import Data.Foldable (find, traverse_)
import Data.Functor (($>), (<&>))
import Data.Kind (Type)
import Data.Typeable (typeRep)
import Data.Void (Void)
import NoThunks.Class (NoThunks (..))

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

instance STS sts => Show (AssertionViolation sts) where
  show = renderAssertionViolation

data AssertionException where
  AssertionException :: forall sts. STS sts => AssertionViolation sts -> AssertionException

instance Show AssertionException where
  show (AssertionException exc) = show exc

instance Exception AssertionException

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
  type State a :: Type

  -- | Signal triggering a state change.
  type Signal a :: Type

  -- | Environment type.
  type Environment a :: Type

  -- | Monad into which to interpret the rules.
  type BaseM a :: Type -> Type

  type BaseM a = Identity

  -- | Event type.
  type Event a :: Type

  type Event a = Void

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
  type PredicateFailure a :: Type

  -- | Rules governing transition under this system.
  initialRules :: [InitialRule a]
  default initialRules :: Default (State a) => [InitialRule a]
  initialRules = [pure def]

  transitionRules :: [TransitionRule a]

  -- | Assertions about the transition system.
  assertions :: [Assertion a]
  assertions = []

  -- | Render an assertion violation.
  --
  --   Defaults to using 'show', but note that this does not know how to render
  --   the context. So for more information you should define your own renderer
  --   here.
  renderAssertionViolation :: AssertionViolation a -> String
  renderAssertionViolation (AssertionViolation sts msg _ _) =
    "AssertionViolation (" <> sts <> "): " <> msg

-- | Embed one STS within another.
class (STS sub, BaseM sub ~ BaseM super) => Embed sub super where
  -- | Wrap a predicate failure of the subsystem in a failure of the super-system.
  wrapFailed :: PredicateFailure sub -> PredicateFailure super

  wrapEvent :: Event sub -> Event super
  default wrapEvent :: Coercible (Event sub) (Event super) => Event sub -> Event super
  wrapEvent = coerce

instance STS sts => Embed sts sts where
  wrapFailed = id

data EventPolicy
  = EventPolicyReturn
  | EventPolicyDiscard

data SingEP ep where
  EPReturn :: SingEP 'EventPolicyReturn
  EPDiscard :: SingEP 'EventPolicyDiscard

type family EventReturnType ep sts a :: Type where
  EventReturnType 'EventPolicyReturn sts a = (a, [Event sts])
  EventReturnType _ _ a = a

class EventReturnTypeRep ert where
  eventReturnTypeRep :: SingEP ert

instance EventReturnTypeRep 'EventPolicyReturn where
  eventReturnTypeRep = EPReturn

instance EventReturnTypeRep 'EventPolicyDiscard where
  eventReturnTypeRep = EPDiscard

discardEvents :: forall ep a. SingEP ep -> forall s. EventReturnType ep s a -> a
discardEvents ep = case ep of
  EPReturn -> fst
  EPDiscard -> id

getEvents :: forall ep. SingEP ep -> forall s a. EventReturnType ep s a -> [Event s]
getEvents ep ert = case ep of
  EPReturn -> snd ert
  EPDiscard -> []

-- | Map over an arbitrary 'EventReturnType'.
mapEventReturn ::
  forall ep sts a b.
  EventReturnTypeRep ep =>
  (a -> b) ->
  EventReturnType ep sts a ->
  EventReturnType ep sts b
mapEventReturn f ert = case eventReturnTypeRep @ep of
  EPReturn -> first f ert
  EPDiscard -> f ert

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
  Writer ::
    [Event sts] ->
    a ->
    Clause sts rtype a
  Predicate ::
    [Label] ->
    Either e a ->
    -- Type of failure to return if the predicate fails
    (e -> PredicateFailure sts) ->
    a ->
    Clause sts rtype a
  IfFailureFree :: Rule sts rtype a -> Rule sts rtype a -> Clause sts rtype a

deriving instance Functor (Clause sts rtype)

type Rule sts rtype = F (Clause sts rtype)

-- | Label for a predicate. This can be used to control which predicates get
-- run.
type Label = String

-- | Oh noes!
--
--   This takes a condition (a boolean expression) and a failure and results in
--   a clause which will throw that failure if the condition fails.
(?!) :: Bool -> PredicateFailure sts -> Rule sts ctx ()
(?!) = labeledPred []

infix 1 ?!

failBecause :: PredicateFailure sts -> Rule sts ctx ()
failBecause = (False ?!)

-- | Oh noes with an explanation
--
--   We interpret this as "What?" "No!" "Because:"
(?!:) :: Either e () -> (e -> PredicateFailure sts) -> Rule sts ctx ()
(?!:) = labeledPredE []

-- | Labeled predicate. This may be used to control which predicates are run
-- using 'ValidateSuchThat'.
labeledPred :: [Label] -> Bool -> PredicateFailure sts -> Rule sts ctx ()
labeledPred lbls cond orElse =
  liftF $
    Predicate
      lbls
      (if cond then Right () else Left ())
      (const orElse)
      ()

-- | Labeled predicate with an explanation
labeledPredE ::
  [Label] ->
  Either e () ->
  (e -> PredicateFailure sts) ->
  Rule sts ctx ()
labeledPredE lbls cond orElse = liftF $ Predicate lbls cond orElse ()

trans ::
  Embed sub super => RuleContext rtype sub -> Rule super rtype (State sub)
trans ctx = wrap $ SubTrans ctx pure

ifFailureFree :: Rule sts rtype a -> Rule sts rtype a -> Rule sts rtype a
ifFailureFree x y = liftF (IfFailureFree x y)

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
  | ValidateSuchThat ([Label] -> Bool)

data ApplySTSOpts ep = ApplySTSOpts
  { -- | Enable assertions during STS processing.
    --   If this option is enabled, STS processing will terminate on violation
    --   of an assertion.
    asoAssertions :: AssertionPolicy,
    -- | Validation policy
    asoValidation :: ValidationPolicy,
    -- | Event policy
    asoEvents :: SingEP ep
  }

type STSInterpreter ep =
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (EventReturnType ep s (State s, [PredicateFailure s]))

type RuleInterpreter ep =
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  Rule s rtype (State s) ->
  m (EventReturnType ep s (State s, [PredicateFailure s]))

-- | Apply an STS with options. Note that this returns both the final state and
-- the list of predicate failures.
applySTSOpts ::
  forall s m rtype ep.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ApplySTSOpts ep ->
  RuleContext rtype s ->
  m (EventReturnType ep s (State s, [PredicateFailure s]))
applySTSOpts ApplySTSOpts {asoAssertions, asoValidation, asoEvents} ctx =
  let goRule :: RuleInterpreter ep
      goRule = applyRuleInternal asoEvents asoValidation goSTS
      goSTS :: STSInterpreter ep
      goSTS c =
        runExceptT (applySTSInternal asoEvents asoAssertions goRule c) >>= \case
          Left err -> throw $! AssertionException err
          Right res -> pure $! res
   in goSTS ctx

applySTSOptsEither ::
  forall s m rtype ep.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ApplySTSOpts ep ->
  RuleContext rtype s ->
  m (Either [PredicateFailure s] (EventReturnType ep s (State s)))
applySTSOptsEither opts ctx =
  let r1 = applySTSOpts opts ctx
   in case asoEvents opts of
        EPDiscard ->
          r1 <&> \case
            (st, []) -> Right st
            (_, pfs) -> Left pfs
        EPReturn ->
          r1 <&> \case
            ((st, []), evts) -> Right (st, evts)
            ((_, pfs), _) -> Left pfs

applySTS ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (Either [PredicateFailure s] (State s))
applySTS = applySTSOptsEither defaultOpts
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = globalAssertionPolicy,
          asoValidation = ValidateAll,
          asoEvents = EPDiscard
        }

globalAssertionPolicy :: AssertionPolicy

#ifdef STS_ASSERT
globalAssertionPolicy = AssertionsAll
#else
globalAssertionPolicy = AssertionsOff
#endif

-- | Re-apply an STS.
--
--   It is assumed that the caller of this function has previously applied this
--   STS, and can guarantee that it completed successfully. No predicates or
--   assertions will be checked when calling this function.
reapplySTS ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (State s)
reapplySTS ctx = applySTSOpts defaultOpts ctx <&> fst
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateNone,
          asoEvents = EPDiscard
        }

applySTSIndifferently ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (State s, [PredicateFailure s])
applySTSIndifferently =
  applySTSOpts
    ApplySTSOpts
      { asoAssertions = AssertionsAll,
        asoValidation = ValidateAll,
        asoEvents = EPDiscard
      }

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleInternal ::
  forall (ep :: EventPolicy) s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  SingEP ep ->
  ValidationPolicy ->
  -- | Interpreter for subsystems
  STSInterpreter ep ->
  RuleContext rtype s ->
  Rule s rtype (State s) ->
  m (EventReturnType ep s (State s, [PredicateFailure s]))
applyRuleInternal ep vp goSTS jc r = do
  (s, er) <- flip runStateT ([], []) $ foldF runClause r
  case ep of
    EPDiscard -> pure (s, fst er)
    EPReturn -> pure ((s, fst er), snd er)
  where
    runClause ::
      forall f t a.
      ( f ~ t m,
        MonadState ([PredicateFailure s], [Event s]) f,
        MonadTrans t
      ) =>
      Clause s rtype a ->
      t m a
    runClause (Lift f next) = next <$> lift f
    runClause (GetCtx next) = pure $ next jc
    runClause (IfFailureFree yesrule norule) = do
      failureFree <- (null . fst <$> get)
      if failureFree
        then foldF runClause yesrule
        else foldF runClause norule
    runClause (Predicate lbls cond orElse val) =
      if validateIf lbls
        then case cond of
          Left err -> modify (first (orElse err :)) >> pure val
          Right x -> pure x
        else pure val
    runClause (SubTrans (subCtx :: RuleContext _rtype sub) next) = do
      s <- lift $ goSTS subCtx
      let ss :: State sub
          sfails :: [PredicateFailure sub]
          sevs :: [Event sub]
          (ss, sfails) = discardEvents ep @sub s
          sevs = getEvents ep @sub @(State sub, [PredicateFailure sub]) s
      traverse_ (\a -> modify (first (a :))) $ wrapFailed @sub @s <$> sfails
      runClause $ Writer (wrapEvent @sub @s <$> sevs) ()
      pure $ next ss
    runClause (Writer w a) = case ep of
      EPReturn -> modify (second (<> w)) $> a
      EPDiscard -> pure a
    validateIf lbls = case vp of
      ValidateAll -> True
      ValidateNone -> False
      ValidateSuchThat f -> f lbls

applySTSInternal ::
  forall s m rtype ep.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  SingEP ep ->
  AssertionPolicy ->
  -- | Interpreter for rules
  RuleInterpreter ep ->
  RuleContext rtype s ->
  ExceptT (AssertionViolation s) m (EventReturnType ep s (State s, [PredicateFailure s]))
applySTSInternal ep ap goRule ctx =
  successOrFirstFailure <$> applySTSInternal' rTypeRep ctx
  where
    successOrFirstFailure ::
      [EventReturnType ep s (State s, [PredicateFailure s])] ->
      EventReturnType ep s (State s, [PredicateFailure s])
    successOrFirstFailure xs =
      case find (\x -> null $ snd (discardEvents ep @s x :: (State s, [PredicateFailure s]))) xs of
        Nothing ->
          case xs of
            [] -> error "applySTSInternal was called with an empty set of rules"
            s' : _ -> case ep of
              EPDiscard -> (fst s', concatMap snd xs)
              EPReturn -> (((fst . fst) s', concatMap (snd . fst) xs), snd s')
        Just s' -> case ep of
          EPDiscard -> (fst s', [])
          EPReturn -> ((fst $ fst s', []), snd s')
    applySTSInternal' ::
      SRuleType rtype ->
      RuleContext rtype s ->
      ExceptT (AssertionViolation s) m [EventReturnType ep s (State s, [PredicateFailure s])]
    applySTSInternal' SInitial env =
      lift (goRule env `traverse` initialRules)
    applySTSInternal' STransition jc = do
      when (assertPre ap) $
        sfor_ (assertions @s) $! \case
          PreCondition msg cond
            | not (cond jc) ->
                let assertion =
                      AssertionViolation
                        { avSTS = show $ typeRep assertion,
                          avMsg = msg,
                          avCtx = jc,
                          avState = Nothing
                        }
                 in throwE assertion
          _ -> pure ()
      res <- lift (goRule jc `traverse` transitionRules)
      -- We only care about running postconditions if the state transition was
      -- successful.
      when (assertPost ap) $
        let res' :: (EventReturnType ep s (State s, [PredicateFailure s]))
            res' = successOrFirstFailure res
         in case discardEvents ep @s res' :: (State s, [PredicateFailure s]) of
              (st, []) ->
                sfor_ (assertions @s) $! \case
                  PostCondition msg cond
                    | not (cond jc st) ->
                        let assertion =
                              AssertionViolation
                                { avSTS = show $ typeRep assertion,
                                  avMsg = msg,
                                  avCtx = jc,
                                  avState = Just st
                                }
                         in throwE assertion
                  _ -> pure ()
              _ -> pure ()
      pure $! res

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
  deriving (Eq, Ord, Show, Data, Typeable, NoThunks)

{------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------}

-- | A stub rule with no transitions to use as a placeholder
data STUB (e :: Type) (st :: Type) (si :: Type) (f :: Type) (m :: Type -> Type)

instance
  ( Eq f,
    Monad m,
    Show f,
    Typeable e,
    Typeable f,
    Typeable si,
    Typeable st,
    Typeable m
  ) =>
  STS (STUB e st si f m)
  where
  type Environment (STUB e st si f m) = e
  type State (STUB e st si f m) = st
  type Signal (STUB e st si f m) = si
  type PredicateFailure (STUB e st si f m) = f
  type BaseM (STUB e st si f m) = m

  transitionRules = []
  initialRules = []

-- | Map each element of a structure to an action, evaluate these actions from
-- left to right, and ignore the results. For a version that doesn't ignore the
-- results see 'Data.Traversable.traverse'.
--
-- This is a strict variant on 'Data.Foldable.traverse_', which evaluates each
-- element of the structure even in a monad which would otherwise allow this to
-- be lazy.
straverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
straverse_ f = foldr c (pure ())
  where
    -- See Note [List fusion and continuations in 'c']
    c !x !k = (*> k) $! f x
    {-# INLINE c #-}

-- | 'sfor_' is 'straverse_' with its arguments flipped. For a version
-- that doesn't ignore the results see 'Data.Traversable.for'.
--
-- >>> sfor_ ([1..4] :: [Int]) print
-- 1
-- 2
-- 3
-- 4
sfor_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
{-# INLINE sfor_ #-}
sfor_ = flip straverse_

tellEvent ::
  Event sts ->
  Rule sts ctx ()
tellEvent e = tellEvents [e]

tellEvents ::
  [Event sts] ->
  Rule sts ctx ()
tellEvents es = liftF $ Writer es ()
