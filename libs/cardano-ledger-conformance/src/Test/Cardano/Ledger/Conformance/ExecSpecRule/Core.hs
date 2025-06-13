{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  conformsToImpl,
  generatesWithin,
  inputsGenerateWithin,
  runConformance,
  checkConformance,
  defaultTestConformance,
  translateWithContext,
  ForAllExecSpecRep,
  ForAllExecTypes,
  diffConformance,
) where

import Cardano.Ledger.BaseTypes (Globals, Inject (..), ShelleyBase)
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Core (Era, EraRule, eraProtVerLow)
import qualified Constrained.API as CV2 (HasSpec, Specification, genFromSpec, genFromSpecT)
import Constrained.GenT (GE (..), GenMode (..))
import qualified Constrained.GenT as CV1 (runGenT)
import Constrained.Generation (shrinkWithSpec, simplifySpec)
import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Base (Constraint, Symbol, Type)
import GHC.TypeLits (KnownSymbol)
import Lens.Micro.Mtl (use)
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.FilePath ((<.>))
import Test.Cardano.Ledger.Api.DebugTools (writeCBOR)
import Test.Cardano.Ledger.Binary.TreeDiff (Pretty (..), ansiWlPretty, ediff, ppEditExpr)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (
  FixupSpecRep (..),
  OpaqueErrorString (..),
  SpecTranslate (..),
  runSpecTransM,
  showOpaqueErrorString,
  toTestRep,
 )
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestM,
  ShelleyEraImp,
  impAnn,
  impGlobalsL,
  logDoc,
  tryRunImpRule,
 )
import UnliftIO (MonadIO (..), evaluateDeep)
import UnliftIO.Directory (makeAbsolute)
import UnliftIO.Environment (lookupEnv)

type ForAllExecTypes (c :: Type -> Constraint) rule era =
  ( c (ExecEnvironment rule era)
  , c (ExecState rule era)
  , c (ExecSignal rule era)
  )

type ForAllExecSpecRep (c :: Type -> Constraint) rule era =
  ( c (SpecRep (ExecEnvironment rule era))
  , c (SpecRep (ExecState rule era))
  , c (SpecRep (ExecSignal rule era))
  )

class
  ( ForAllExecTypes CV2.HasSpec rule era
  , ForAllExecTypes ToExpr rule era
  , ForAllExecTypes NFData rule era
  , KnownSymbol rule
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , SpecTranslate (ExecContext rule era) (PredicateFailure (EraRule rule era))
  , Inject (ExecEnvironment rule era) (Environment (EraRule rule era))
  , Inject (ExecState rule era) (State (EraRule rule era))
  , Inject (ExecSignal rule era) (Signal (EraRule rule era))
  ) =>
  ExecSpecRule (rule :: Symbol) era
  where
  type ExecContext rule era
  type ExecContext rule era = ()

  type ExecEnvironment rule era
  type ExecEnvironment rule era = Environment (EraRule rule era)

  type ExecState rule era
  type ExecState rule era = State (EraRule rule era)

  type ExecSignal rule era
  type ExecSignal rule era = Signal (EraRule rule era)

  environmentSpec ::
    HasCallStack =>
    ExecContext rule era ->
    CV2.Specification (ExecEnvironment rule era)

  stateSpec ::
    HasCallStack =>
    ExecContext rule era ->
    ExecEnvironment rule era ->
    CV2.Specification (ExecState rule era)

  signalSpec ::
    HasCallStack =>
    ExecContext rule era ->
    ExecEnvironment rule era ->
    ExecState rule era ->
    CV2.Specification (ExecSignal rule era)

  classOf :: ExecSignal rule era -> Maybe String
  classOf _ = Nothing

  genExecContext :: HasCallStack => Gen (ExecContext rule era)
  default genExecContext ::
    Arbitrary (ExecContext rule era) =>
    Gen (ExecContext rule era)
  genExecContext = arbitrary

  runAgdaRule ::
    HasCallStack =>
    SpecRep (ExecEnvironment rule era) ->
    SpecRep (ExecState rule era) ->
    SpecRep (ExecSignal rule era) ->
    Either OpaqueErrorString (SpecRep (ExecState rule era))

  translateInputs ::
    HasCallStack =>
    ExecEnvironment rule era ->
    ExecState rule era ->
    ExecSignal rule era ->
    ExecContext rule era ->
    ImpTestM
      era
      ( SpecRep (ExecEnvironment rule era)
      , SpecRep (ExecState rule era)
      , SpecRep (ExecSignal rule era)
      )
  default translateInputs ::
    ( ForAllExecTypes (SpecTranslate (ExecContext rule era)) rule era
    , ForAllExecSpecRep ToExpr rule era
    ) =>
    ExecEnvironment rule era ->
    ExecState rule era ->
    ExecSignal rule era ->
    ExecContext rule era ->
    ImpTestM
      era
      ( SpecRep (ExecEnvironment rule era)
      , SpecRep (ExecState rule era)
      , SpecRep (ExecSignal rule era)
      )
  translateInputs env st sig ctx = do
    let
      expectRight' (Right x) = pure x
      expectRight' (Left e) = assertFailure (T.unpack e)
    agdaEnv <- expectRight' . runSpecTransM ctx $ toSpecRep env
    logDoc $ "agdaEnv:\n" <> ansiExpr agdaEnv
    agdaSt <- expectRight' . runSpecTransM ctx $ toSpecRep st
    logDoc $ "agdaSt:\n" <> ansiExpr agdaSt
    agdaSig <- expectRight' . runSpecTransM ctx $ toSpecRep sig
    logDoc $ "agdaSig:\n" <> ansiExpr agdaSig
    pure (agdaEnv, agdaSt, agdaSig)

  testConformance ::
    ( ShelleyEraImp era
    , SpecTranslate (ExecContext rule era) (State (EraRule rule era))
    , ForAllExecSpecRep NFData rule era
    , ForAllExecSpecRep ToExpr rule era
    , NFData (SpecRep (PredicateFailure (EraRule rule era)))
    , ToExpr (SpecRep (PredicateFailure (EraRule rule era)))
    , Eq (SpecRep (PredicateFailure (EraRule rule era)))
    , Eq (SpecRep (ExecState rule era))
    , Inject
        (State (EraRule rule era))
        (ExecState rule era)
    , SpecTranslate (ExecContext rule era) (ExecState rule era)
    , FixupSpecRep (SpecRep (PredicateFailure (EraRule rule era)))
    , FixupSpecRep (SpecRep (ExecState rule era))
    , Inject (ExecEnvironment rule era) (Environment (EraRule rule era))
    , Inject (ExecState rule era) (State (EraRule rule era))
    , Inject (ExecSignal rule era) (Signal (EraRule rule era))
    , EncCBOR (ExecContext rule era)
    , EncCBOR (Environment (EraRule rule era))
    , EncCBOR (State (EraRule rule era))
    , EncCBOR (Signal (EraRule rule era))
    , ToExpr (ExecContext rule era)
    , ToExpr (PredicateFailure (EraRule rule era))
    , NFData (PredicateFailure (EraRule rule era))
    , HasCallStack
    ) =>
    ExecContext rule era ->
    ExecEnvironment rule era ->
    ExecState rule era ->
    ExecSignal rule era ->
    Property
  testConformance = defaultTestConformance @era @rule

  extraInfo ::
    HasCallStack =>
    Globals ->
    ExecContext rule era ->
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    Signal (EraRule rule era) ->
    Either OpaqueErrorString (State (EraRule rule era), [Event (EraRule rule era)]) ->
    Doc AnsiStyle
  extraInfo _ _ _ _ _ = mempty

dumpCbor ::
  forall era a.
  ( EncCBOR a
  , Era era
  ) =>
  FilePath ->
  a ->
  String ->
  ImpTestM era ()
dumpCbor path x name = do
  fullPath <- makeAbsolute $ path <> "/" <> name <.> "cbor"
  writeCBOR (eraProtVerLow @era) fullPath x

diffConformance :: ToExpr a => a -> a -> Doc AnsiStyle
diffConformance implRes agdaRes =
  ppEditExpr conformancePretty (ediff implRes agdaRes)
  where
    delColor = Red
    insColor = Magenta
    conformancePretty =
      ansiWlPretty
        { ppDel = annotate (color delColor) . parens . ("Impl: " <>)
        , ppIns = annotate (color insColor) . parens . ("Agda: " <>)
        }

checkConformance ::
  forall rule era.
  ( Era era
  , ToExpr (SpecRep (ExecState rule era))
  , Eq (SpecRep (ExecState rule era))
  , EncCBOR (ExecContext rule era)
  , EncCBOR (Environment (EraRule rule era))
  , EncCBOR (State (EraRule rule era))
  , EncCBOR (Signal (EraRule rule era))
  , HasCallStack
  ) =>
  ExecContext rule era ->
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  Either OpaqueErrorString (SpecRep (ExecState rule era)) ->
  Either OpaqueErrorString (SpecRep (ExecState rule era)) ->
  ImpTestM era ()
checkConformance ctx env st sig implResTest agdaResTest = do
  let
    failMsg =
      annotate (color Yellow) . vsep $
        [ "===== DIFF ====="
        , diffConformance implResTest agdaResTest
        ]
  unless (implResTest == agdaResTest) $ do
    let envVarName = "CONFORMANCE_CBOR_DUMP_PATH"
    mbyCborDumpPath <- lookupEnv envVarName
    case mbyCborDumpPath of
      Just path -> do
        dumpCbor path ctx "conformance_dump_ctx"
        dumpCbor path env "conformance_dump_env"
        dumpCbor path st "conformance_dump_st"
        dumpCbor path sig "conformance_dump_sig"
        logDoc $ "Dumped the CBOR files to " <> ansiExpr path
      Nothing ->
        logDoc $
          "Run the test again with "
            <> fromString envVarName
            <> "=<path> to get a CBOR dump of the test data"
    expectationFailure . ansiDocToString $ failMsg

defaultTestConformance ::
  forall era rule.
  ( HasCallStack
  , ShelleyEraImp era
  , ExecSpecRule rule era
  , ForAllExecSpecRep NFData rule era
  , ForAllExecSpecRep ToExpr rule era
  , NFData (PredicateFailure (EraRule rule era))
  , Eq (SpecRep (ExecState rule era))
  , Inject (State (EraRule rule era)) (ExecState rule era)
  , SpecTranslate (ExecContext rule era) (ExecState rule era)
  , FixupSpecRep (SpecRep (ExecState rule era))
  , EncCBOR (ExecContext rule era)
  , EncCBOR (Environment (EraRule rule era))
  , EncCBOR (State (EraRule rule era))
  , EncCBOR (Signal (EraRule rule era))
  , ToExpr (ExecContext rule era)
  , ToExpr (PredicateFailure (EraRule rule era))
  ) =>
  ExecContext rule era ->
  ExecEnvironment rule era ->
  ExecState rule era ->
  ExecSignal rule era ->
  Property
defaultTestConformance ctx env st sig = property $ do
  (implResTest, agdaResTest, implRes) <- runConformance @rule @era ctx env st sig
  globals <- use impGlobalsL
  let
    extra =
      extraInfo @rule @era
        globals
        ctx
        (inject env)
        (inject st)
        (inject sig)
        (first showOpaqueErrorString implRes)
  logDoc extra
  checkConformance @rule @_
    ctx
    (inject env)
    (inject st)
    (inject sig)
    (first showOpaqueErrorString implResTest)
    agdaResTest

runConformance ::
  forall (rule :: Symbol) era.
  ( ExecSpecRule rule era
  , ForAllExecSpecRep NFData rule era
  , ForAllExecSpecRep ToExpr rule era
  , FixupSpecRep (SpecRep (ExecState rule era))
  , Inject (State (EraRule rule era)) (ExecState rule era)
  , SpecTranslate (ExecContext rule era) (ExecState rule era)
  , ToExpr (ExecContext rule era)
  , HasCallStack
  , NFData (PredicateFailure (EraRule rule era))
  ) =>
  ExecContext rule era ->
  ExecEnvironment rule era ->
  ExecState rule era ->
  ExecSignal rule era ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (SpecRep (ExecState rule era))
    , Either OpaqueErrorString (SpecRep (ExecState rule era))
    , Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (State (EraRule rule era), [Event (EraRule rule era)])
    )
runConformance execContext env st sig = do
  (specEnv, specSt, specSig) <-
    impAnn "Translating the inputs" $
      translateInputs @rule @era env st sig execContext
  logDoc $ "ctx:\n" <> ansiExpr execContext
  logDoc $ "implEnv:\n" <> ansiExpr env
  logDoc $ "implSt:\n" <> ansiExpr st
  logDoc $ "implSig:\n" <> ansiExpr sig
  logDoc $ "specEnv:\n" <> ansiExpr specEnv
  logDoc $ "specSt:\n" <> ansiExpr specSt
  logDoc $ "specSig:\n" <> ansiExpr specSig
  agdaResTest <-
    fmap (second fixup) $
      impAnn "Deep evaluating Agda output" $
        evaluateDeep $
          runAgdaRule @rule @era specEnv specSt specSig
  implRes <- tryRunImpRule @rule @era (inject env) (inject st) (inject sig)
  implResTest <-
    impAnn "Translating implementation values to SpecRep" $
      expectRightExpr $
        runSpecTransM execContext $
          bimapM pure (toTestRep . inject @_ @(ExecState rule era) . fst) implRes
  pure (implResTest, agdaResTest, implRes)

conformsToImpl ::
  forall (rule :: Symbol) era.
  ( ShelleyEraImp era
  , ExecSpecRule rule era
  , ForAllExecSpecRep NFData rule era
  , ForAllExecSpecRep ToExpr rule era
  , NFData (SpecRep (PredicateFailure (EraRule rule era)))
  , NFData (ExecContext rule era)
  , ToExpr (SpecRep (PredicateFailure (EraRule rule era)))
  , ToExpr (ExecContext rule era)
  , SpecTranslate (ExecContext rule era) (State (EraRule rule era))
  , Eq (SpecRep (PredicateFailure (EraRule rule era)))
  , Inject (State (EraRule rule era)) (ExecState rule era)
  , Eq (SpecRep (ExecState rule era))
  , SpecTranslate (ExecContext rule era) (ExecState rule era)
  , FixupSpecRep (SpecRep (PredicateFailure (EraRule rule era)))
  , FixupSpecRep (SpecRep (ExecState rule era))
  , EncCBOR (ExecContext rule era)
  , EncCBOR (Environment (EraRule rule era))
  , EncCBOR (State (EraRule rule era))
  , EncCBOR (Signal (EraRule rule era))
  , HasCallStack
  , NFData (PredicateFailure (EraRule rule era))
  , ToExpr (PredicateFailure (EraRule rule era))
  ) =>
  Property
conformsToImpl = property @(ImpTestM era Property) . (`runContT` pure) $ do
  let
    deepEvalAnn s = "Deep evaluating " <> s
    deepEval x s = do
      _ <- lift $ impAnn (deepEvalAnn s) (liftIO (evaluateDeep x))
      pure ()
  ctx <- ContT $ \c ->
    pure $ forAllShow (genExecContext @rule @era) showExpr c
  deepEval ctx "context"
  let
    forAllSpec spec = do
      let
        simplifiedSpec = simplifySpec spec
        generator = CV1.runGenT (CV2.genFromSpecT simplifiedSpec) Loose []
        shrinker (Result x) = pure <$> shrinkWithSpec simplifiedSpec x
        shrinker _ = []
      res :: GE a <- ContT $ \c ->
        pure $ forAllShrinkBlind generator shrinker c
      case res of
        Result x -> pure x
        _ -> ContT . const . pure $ property Discard
  env <- forAllSpec $ environmentSpec @rule @era ctx
  deepEval env "environment"
  st <- forAllSpec $ stateSpec @rule @era ctx env
  deepEval st "state"
  sig <- forAllSpec $ signalSpec @rule @era ctx env st
  deepEval sig "signal"
  let classification =
        case classOf @rule @era sig of
          Nothing -> classify False "None"
          Just c -> classify True c
  pure . classification $
    testConformance @rule @era ctx env st sig

generatesWithin ::
  forall a.
  ( NFData a
  , ToExpr a
  , Typeable a
  , HasCallStack
  ) =>
  Gen a ->
  Int ->
  Spec
generatesWithin gen timeout =
  prop (aName <> " generates within " <> show timeout <> "μs")
    . forAllShow gen showExpr
    $ \x -> within timeout $ ioProperty (evaluateDeep x $> ())
  where
    aName = show (typeRep $ Proxy @a)

inputsGenerateWithin ::
  forall (rule :: Symbol) era.
  ExecSpecRule rule era =>
  Int ->
  Spec
inputsGenerateWithin timeout =
  describe (aName <> " input generation time") $ do
    let
      genEnv = do
        ctx <- genExecContext @rule @era
        CV2.genFromSpec $ environmentSpec @rule @era ctx
      genSt = do
        ctx <- genExecContext @rule @era
        env <- genEnv
        CV2.genFromSpec $ stateSpec @rule @era ctx env
      genSig = do
        ctx <- genExecContext @rule @era
        env <- genEnv
        st <- genSt
        CV2.genFromSpec $ signalSpec @rule @era ctx env st
    genEnv `generatesWithin` timeout
    genSt `generatesWithin` timeout
    genSig `generatesWithin` timeout
  where
    aName = show (typeRep $ Proxy @rule)

-- | Translate a Haskell type 'a' whose translation context is 'ctx' into its Agda type, in the ImpTest monad.
translateWithContext :: SpecTranslate ctx a => ctx -> a -> ImpTestM era (SpecRep a)
translateWithContext ctx x = do
  let
    expectRight' (Right y) = pure y
    expectRight' (Left e) = assertFailure (T.unpack e)
  expectRight' . runSpecTransM ctx $ toSpecRep x
