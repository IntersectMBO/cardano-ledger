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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Small step state transition systems.
module Control.State.Transition.Extended (
  RuleType (..),
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
  validate,
  validateTrans,
  validateTransLabeled,
  Label,
  SingEP (..),
  EventPolicy (..),
  EventReturnType,
  labeled,
  labeledPred,
  labeledPredE,
  ifFailureFree,
  whenFailureFree,
  whenFailureFreeDefault,
  failBecause,
  failOnJust,
  failOnNonEmpty,
  failureOnJust,
  failureOnNonEmpty,
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
  STSResult (..),
  applySTSOpts,
  applySTSOptsResult,
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
  runRule,

  -- * Random thing
  Threshold (..),
  sfor_,
) where

import Control.Exception (Exception (..), throw)
import Control.Monad (when)
import Control.Monad.Free.Church (F, MonadFree (wrap), foldF, liftF)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Class (MonadState (..), modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bifunctor (Bifunctor (second), first)
import Data.Coerce (Coercible, coerce)
import Data.Data (Data, Typeable)
import Data.Default (Default, def)
import Data.Foldable as F (find, toList, traverse_)
import Data.Functor (($>), (<&>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Typeable (typeRep)
import Data.Void (Void)
import NoThunks.Class (NoThunks (..))
import Validation (Validation (..), eitherToValidation)

-- | In order to avoid boolean blindness we create specialized type for the
-- concept of any rule having information about overall state of the nested
-- clause.
data IsFailing
  = Failing
  | NotFailing
  deriving (Eq, Show)

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
  { avSTS :: String
  , avMsg :: String
  , avCtx :: TRC sts
  , avState :: Maybe (State sts)
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
  ( Eq (PredicateFailure a)
  , Show (PredicateFailure a)
  , Monad (BaseM a)
  , Typeable a
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
    Validation (NonEmpty e) a ->
    -- Type of failure to return if the predicate fails
    (e -> PredicateFailure sts) ->
    a ->
    Clause sts rtype a
  -- | Label part of a rule. The interpreter may be configured to only run parts
  -- of rules governed by (or by the lack of) certain labels.
  Label ::
    NonEmpty Label ->
    Rule sts rtype a ->
    a ->
    Clause sts rtype a
  IfFailureFree :: Rule sts rtype a -> Rule sts rtype a -> Clause sts rtype a

deriving instance Functor (Clause sts rtype)

type Rule sts rtype = F (Clause sts rtype)

-- | Label for a predicate. This can be used to control which predicates get
-- run.
type Label = String

-- | Fail with `PredicateFailure`'s in STS if `Validation` was unsuccessful.
validate :: Validation (NonEmpty (PredicateFailure sts)) () -> Rule sts ctx ()
validate = validateTrans id

-- | Same as `validation`, except with ability to transform opaque failures
-- into `PredicateFailure`s with a help of supplied function.
validateTrans ::
  -- | Transformation function for all errors
  (e -> PredicateFailure sts) ->
  Validation (NonEmpty e) () ->
  Rule sts ctx ()
validateTrans t v = liftF $ Predicate v t ()

-- | Same as `validation`, except with ability to translate opaque failures
-- into `PredicateFailure`s with a help of supplied function.
validateTransLabeled ::
  -- | Transformation function for all errors
  (e -> PredicateFailure sts) ->
  -- | Supply a list of labels to be used as filters when STS is executed
  NonEmpty Label ->
  -- | Actual validations to be executed
  Validation (NonEmpty e) () ->
  Rule sts ctx ()
validateTransLabeled t labels v = liftF $ Label labels (liftF $ Predicate v t ()) ()

-- | Oh noes!
--
--   This takes a condition (a boolean expression) and a failure and results in
--   a clause which will throw that failure if the condition fails.
(?!) :: Bool -> PredicateFailure sts -> Rule sts ctx ()
(?!) cond onFail =
  liftF $
    Predicate (if cond then Success () else Failure (onFail :| [])) id ()

infix 1 ?!

failBecause :: PredicateFailure sts -> Rule sts ctx ()
failBecause = (False ?!)

-- | Produce a predicate failure when condition contains a Just value, contents of which
-- can be used inside the predicate failure
failOnJust :: Maybe a -> (a -> PredicateFailure sts) -> Rule sts ctx ()
failOnJust cond onJust = liftF $ Predicate (failureOnJust cond onJust) id ()

-- | Produce a failure when condition contains a Just value, contents of which can be used
-- inside the failure
failureOnJust :: Maybe a -> (a -> e) -> Validation (NonEmpty e) ()
failureOnJust cond onJust =
  case cond of
    Nothing -> Success ()
    Just a -> Failure (onJust a :| [])

-- | Produce a predicate failure when supplied foldable is not empty, contents of which
-- will be converted to a NonEmpty list and can be used inside the predicate failure.
failOnNonEmpty :: Foldable f => f a -> (NonEmpty a -> PredicateFailure sts) -> Rule sts ctx ()
failOnNonEmpty cond onNonEmpty = liftF $ Predicate (failureOnNonEmpty cond onNonEmpty) id ()

-- | Produce a failure when supplied foldable is not empty, contents of which will be
-- converted to a NonEmpty list and theh can be used inside the  failure.
failureOnNonEmpty :: Foldable f => f a -> (NonEmpty a -> e) -> Validation (NonEmpty e) ()
failureOnNonEmpty cond = failureOnJust (NE.nonEmpty (F.toList cond))

-- | Oh noes with an explanation
--
--   We interpret this as "What?" "No!" "Because:"
(?!:) :: Either e () -> (e -> PredicateFailure sts) -> Rule sts ctx ()
(?!:) cond onFail =
  liftF $
    Predicate (eitherToValidation $ first pure cond) onFail ()

-- | Labeled predicate. This may be used to control which predicates are run
-- using 'ValidateSuchThat'.
labeledPred :: NonEmpty Label -> Bool -> PredicateFailure sts -> Rule sts ctx ()
labeledPred lbls cond orElse = labeled lbls (cond ?! orElse)

-- | Labeled predicate with an explanation
labeledPredE ::
  NonEmpty Label ->
  Either e () ->
  (e -> PredicateFailure sts) ->
  Rule sts ctx ()
labeledPredE lbls cond orElse = labeled lbls (cond ?!: orElse)

-- | Labeled clause. This will only be executed if the interpreter is set to
-- execute clauses with this label.
labeled :: NonEmpty Label -> Rule sts ctx () -> Rule sts ctx ()
labeled lbls subrule = liftF $ Label lbls subrule ()

trans ::
  Embed sub super => RuleContext rtype sub -> Rule super rtype (State sub)
trans ctx = wrap $ SubTrans ctx pure

ifFailureFree :: Rule sts rtype a -> Rule sts rtype a -> Rule sts rtype a
ifFailureFree x y = liftF (IfFailureFree x y)

whenFailureFree :: Rule sts rtype () -> Rule sts rtype ()
whenFailureFree = whenFailureFreeDefault ()

whenFailureFreeDefault :: a -> Rule sts rtype a -> Rule sts rtype a
whenFailureFreeDefault defValOnFailure actionOnNoFailure =
  ifFailureFree actionOnNoFailure (pure defValOnFailure)

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
  { asoAssertions :: AssertionPolicy
  -- ^ Enable assertions during STS processing.
  --   If this option is enabled, STS processing will terminate on violation
  --   of an assertion.
  , asoValidation :: ValidationPolicy
  -- ^ Validation policy
  , asoEvents :: SingEP ep
  -- ^ Event policy
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

data STSResult s = STSResult
  { stsResultState :: State s
  , stsResultFailures :: [PredicateFailure s]
  , stsResultEvents :: [Event s]
  }

applySTSOptsResult ::
  forall s m rtype ep.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ApplySTSOpts ep ->
  RuleContext rtype s ->
  m (STSResult s)
applySTSOptsResult opts ctx =
  case asoEvents opts of
    EPDiscard -> do
      (stsState, stsFailure) <- applySTSOpts opts ctx
      pure
        STSResult
          { stsResultState = stsState
          , stsResultFailures = stsFailure
          , stsResultEvents = []
          }
    EPReturn -> do
      ((stsState, stsFailure), stsEvents) <- applySTSOpts opts ctx
      pure
        STSResult
          { stsResultState = stsState
          , stsResultFailures = stsFailure
          , stsResultEvents = stsEvents
          }

-- | Apply an STS with options. Note that this returns both the final state and
-- the list of predicate failures.
applySTSOpts ::
  forall s m rtype ep.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ApplySTSOpts ep ->
  RuleContext rtype s ->
  m (EventReturnType ep s (State s, [PredicateFailure s]))
applySTSOpts ApplySTSOpts {asoAssertions, asoValidation, asoEvents} ctx =
  let goRule :: IsFailing -> RuleInterpreter ep
      goRule isFailing = applyRuleInternal isFailing asoEvents asoValidation goSTS
      goSTS :: IsFailing -> STSInterpreter ep
      goSTS isFailing c =
        runExceptT (applySTSInternal asoEvents asoAssertions (goRule isFailing) c) >>= \case
          Left err -> throw $! AssertionException err
          Right res -> pure $! res
   in goSTS NotFailing ctx

applySTSOptsEither ::
  forall s m rtype ep.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  ApplySTSOpts ep ->
  RuleContext rtype s ->
  m (Either (NonEmpty (PredicateFailure s)) (EventReturnType ep s (State s)))
applySTSOptsEither opts ctx =
  let r1 = applySTSOpts opts ctx
   in case asoEvents opts of
        EPDiscard ->
          r1 <&> \case
            (st, []) -> Right st
            (_, pf : pfs) -> Left $ pf :| pfs
        EPReturn ->
          r1 <&> \case
            ((st, []), evts) -> Right (st, evts)
            ((_, pf : pfs), _) -> Left $ pf :| pfs

applySTS ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (Either (NonEmpty (PredicateFailure s)) (State s))
applySTS = applySTSOptsEither defaultOpts
  where
    defaultOpts =
      ApplySTSOpts
        { asoAssertions = globalAssertionPolicy
        , asoValidation = ValidateAll
        , asoEvents = EPDiscard
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
        { asoAssertions = AssertionsOff
        , asoValidation = ValidateNone
        , asoEvents = EPDiscard
        }

applySTSIndifferently ::
  forall s m rtype.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  RuleContext rtype s ->
  m (State s, [PredicateFailure s])
applySTSIndifferently =
  applySTSOpts
    ApplySTSOpts
      { asoAssertions = AssertionsAll
      , asoValidation = ValidateAll
      , asoEvents = EPDiscard
      }

-- | Apply a rule even if its predicates fail.
--
--   If the rule successfully applied, the list of predicate failures will be
--   empty.
applyRuleInternal ::
  forall (ep :: EventPolicy) s m rtype result.
  (STS s, RuleTypeRep rtype, m ~ BaseM s) =>
  -- | We need to know if the current STS incurred at least one
  -- PredicateFailure.  This is necessary because `applyRuleInternal` is called
  -- recursively through the @goSTS@ argument, which will not have access to any
  -- of the predicate failures occured in other branches of STS rule execusion tree.
  IsFailing ->
  SingEP ep ->
  ValidationPolicy ->
  -- | Interpreter for subsystems
  (IsFailing -> STSInterpreter ep) ->
  RuleContext rtype s ->
  Rule s rtype result ->
  m (EventReturnType ep s (result, [PredicateFailure s]))
applyRuleInternal isAlreadyFailing ep vp goSTS jc r = do
  (s, er) <- flip runStateT ([], []) $ foldF runClause r
  case ep of
    EPDiscard -> pure (s, fst er)
    EPReturn -> pure ((s, fst er), snd er)
  where
    isFailing :: StateT ([PredicateFailure s], [Event s]) m IsFailing
    isFailing =
      case isAlreadyFailing of
        Failing -> pure Failing
        NotFailing -> do
          isFailingNow <- null . fst <$> get
          pure $ if isFailingNow then NotFailing else Failing
    runClause :: Clause s rtype a -> StateT ([PredicateFailure s], [Event s]) m a
    runClause (Lift f next) = next <$> lift f
    runClause (GetCtx next) = pure $ next jc
    runClause (IfFailureFree notFailingRule failingRule) = do
      isFailing >>= \case
        Failing -> foldF runClause failingRule
        NotFailing -> foldF runClause notFailingRule
    runClause (Predicate cond orElse val) =
      case vp of
        ValidateNone -> pure val
        _ -> case cond of
          Success x -> pure x
          Failure errs -> modify (first (map orElse (reverse (NE.toList errs)) <>)) >> pure val
    runClause (Label lbls subrule val) =
      if validateIf (NE.toList lbls)
        then foldF runClause subrule
        else pure val
    runClause (SubTrans (subCtx :: RuleContext _rtype sub) next) = do
      isFailingNow <- isFailing
      s <- lift $ goSTS isFailingNow subCtx
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
                        { avSTS = show $ typeRep assertion
                        , avMsg = msg
                        , avCtx = jc
                        , avState = Nothing
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
                                { avSTS = show $ typeRep assertion
                                , avMsg = msg
                                , avCtx = jc
                                , avState = Just st
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
  deriving (Eq, Ord, Show, Data, NoThunks)

{------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------}

-- | A stub rule with no transitions to use as a placeholder
data STUB (e :: Type) (st :: Type) (si :: Type) (f :: Type) (m :: Type -> Type)

instance
  ( Eq f
  , Monad m
  , Show f
  , Typeable e
  , Typeable f
  , Typeable si
  , Typeable st
  , Typeable m
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

-- =================================

-- | runRule :: RuleInterpreter 'EventPolicyDiscard
--   run a rule given its context, to get a BaseM computation
runRule ::
  (STS s, RuleTypeRep rtype) =>
  RuleContext rtype s ->
  Rule s rtype result ->
  BaseM s (result, [PredicateFailure s])
runRule cntxt rule = applyRuleInternal NotFailing EPDiscard ValidateAll goSTS cntxt rule
  where
    goRule isFailing c r = applyRuleInternal isFailing EPDiscard ValidateAll goSTS c r
    goSTS :: IsFailing -> STSInterpreter 'EventPolicyDiscard
    goSTS isFailing c =
      runExceptT (applySTSInternal EPDiscard AssertionsOff (goRule isFailing) c) >>= \case
        Left err -> throw $! AssertionException err
        Right res -> pure $! res
