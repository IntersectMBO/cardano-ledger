{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  ConformanceResult (..),
  SpecTRC (..),
  generatesWithin,
  runConformance,
  checkConformance,
  testConformance,
  translateWithContext,
  ForAllExecSpecRep,
  ForAllExecTypes,
  diffConformance,
  runFromAgdaFunction,
) where

import Cardano.Ledger.BaseTypes (Globals, ShelleyBase)
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Core (Era, EraRule, eraProtVerLow)
import Control.State.Transition.Extended (STS (..), TRC (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.TreeDiff.Pretty (ansiWlExpr)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Base (Constraint, Symbol, Type)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import Lens.Micro.Mtl (use)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.FilePath ((<.>))
import Test.Cardano.Ledger.Api.DebugTools (writeCBOR)
import Test.Cardano.Ledger.Binary.TreeDiff (Pretty (..), ansiWlPretty, ediff, ppEditExpr)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecNormalize (..),
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult,
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
import UnliftIO (evaluateDeep)
import UnliftIO.Directory (makeAbsolute)
import UnliftIO.Environment (lookupEnv)

type ForAllExecTypes (c :: Type -> Constraint) rule era =
  ( c (Environment (EraRule rule era))
  , c (State (EraRule rule era))
  , c (Signal (EraRule rule era))
  )

type ForAllExecSpecRep (c :: Type -> Constraint) rule era =
  ( c (SpecEnvironment rule era)
  , c (SpecState rule era)
  , c (SpecSignal rule era)
  )

data SpecTRC rule era = SpecTRC
  { strcEnvironment :: SpecEnvironment rule era
  , strcState :: SpecState rule era
  , strcSignal :: SpecSignal rule era
  }
  deriving (Generic)

deriving instance ForAllExecSpecRep Eq rule era => Eq (SpecTRC rule era)

instance ForAllExecSpecRep NFData rule era => NFData (SpecTRC rule era)

instance ForAllExecSpecRep ToExpr rule era => ToExpr (SpecTRC rule era)

class
  ( ShelleyEraImp era
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , KnownSymbol rule
  , ForAllExecSpecRep NFData rule era
  , ForAllExecSpecRep ToExpr rule era
  , ForAllExecTypes NFData rule era
  , ForAllExecTypes ToExpr rule era
  , ForAllExecTypes EncCBOR rule era
  , EncCBOR (ExecContext rule era)
  , Eq (SpecState rule era)
  , SpecNormalize (SpecState rule era)
  , NFData (ExecContext rule era)
  , NFData (PredicateFailure (EraRule rule era))
  , NFData (SpecTRC rule era)
  , ToExpr (ExecContext rule era)
  , ToExpr (PredicateFailure (EraRule rule era))
  ) =>
  ExecSpecRule (rule :: Symbol) era
  where
  type ExecContext rule era
  type ExecContext rule era = ()

  type SpecEnvironment rule era
  type SpecEnvironment rule era = SpecRep (Environment (EraRule rule era))

  type SpecState rule era
  type SpecState rule era = SpecRep (State (EraRule rule era))

  type SpecSignal rule era
  type SpecSignal rule era = SpecRep (Signal (EraRule rule era))

  runAgdaRule ::
    HasCallStack =>
    SpecTRC rule era ->
    Either Text (SpecState rule era)

  translateInputs ::
    HasCallStack =>
    ExecContext rule era ->
    TRC (EraRule rule era) ->
    Either Text (SpecTRC rule era)
  default translateInputs ::
    ( SpecTranslate (ExecContext rule era) (Environment (EraRule rule era))
    , SpecTranslate (ExecContext rule era) (State (EraRule rule era))
    , SpecTranslate (ExecContext rule era) (Signal (EraRule rule era))
    , SpecRep (Environment (EraRule rule era)) ~ SpecEnvironment rule era
    , SpecRep (State (EraRule rule era)) ~ SpecState rule era
    , SpecRep (Signal (EraRule rule era)) ~ SpecSignal rule era
    ) =>
    ExecContext rule era ->
    TRC (EraRule rule era) ->
    Either Text (SpecTRC rule era)
  translateInputs ctx (TRC (env, st, sig)) = do
    runSpecTransM ctx $
      SpecTRC <$> toSpecRep env <*> toSpecRep st <*> toSpecRep sig

  translateOutput ::
    ExecContext rule era ->
    TRC (EraRule rule era) ->
    State (EraRule rule era) ->
    Either Text (SpecState rule era)
  default translateOutput ::
    ( SpecTranslate (ExecContext rule era) (State (EraRule rule era))
    , SpecRep (State (EraRule rule era)) ~ SpecState rule era
    ) =>
    ExecContext rule era ->
    TRC (EraRule rule era) ->
    State (EraRule rule era) ->
    Either Text (SpecState rule era)
  translateOutput ctx _ st = runSpecTransM ctx $ toSpecRep st

  extraInfo ::
    HasCallStack =>
    Globals ->
    ExecContext rule era ->
    TRC (EraRule rule era) ->
    Either Text (State (EraRule rule era), [Event (EraRule rule era)]) ->
    Doc AnsiStyle
  extraInfo _ _ _ _ = mempty

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

diffConformance :: ToExpr a => Either Text a -> Either Text a -> Doc AnsiStyle
diffConformance (Right implRes) (Right agdaRes) =
  ppEditExpr conformancePretty (ediff implRes agdaRes)
  where
    delColor = Red
    insColor = Magenta
    conformancePretty =
      ansiWlPretty
        { ppDel = annotate (color delColor) . parens . ("Impl: " <>)
        , ppIns = annotate (color insColor) . parens . ("Agda: " <>)
        }
diffConformance implRes agdaRes =
  let
    prettyRes True x = "Success):\n" <> annotate (colorDull White) (ansiWlExpr $ toExpr x)
    prettyRes False x = "Failure):\n" <> annotate (color Red) (ansiWlExpr $ toExpr x)
   in
    vsep
      [ "Impl ("
          <> nest
            2
            ( case implRes of
                Right x -> prettyRes True x
                Left x -> prettyRes False x
            )
      , "Agda ("
          <> nest
            2
            ( case agdaRes of
                Right x -> prettyRes True x
                Left x -> "Failure):\n" <> annotate (color Red) (pretty x)
            )
      ]

checkConformance ::
  forall rule era.
  ( HasCallStack
  , Era era
  , EncCBOR (ExecContext rule era)
  , EncCBOR (Environment (EraRule rule era))
  , EncCBOR (State (EraRule rule era))
  , EncCBOR (Signal (EraRule rule era))
  , ToExpr (SpecState rule era)
  , Eq (SpecState rule era)
  , SpecNormalize (SpecState rule era)
  ) =>
  ExecContext rule era ->
  TRC (EraRule rule era) ->
  Either Text (SpecState rule era) ->
  Either Text (SpecState rule era) ->
  ImpTestM era ()
checkConformance ctx (TRC (env, st, sig)) implResTest agdaResTest = do
  let
    implResNorm = specNormalize <$> implResTest
    agdaResNorm = specNormalize <$> agdaResTest
  case (implResNorm, agdaResNorm) of
    (Right agda, Right impl)
      | agda == impl -> pure ()
    (Left _, Left _) -> pure ()
    (agda, impl) -> do
      let
        envVarName = "CONFORMANCE_CBOR_DUMP_PATH"
        failMsg =
          annotate (color Yellow) . vsep $
            [ "===== DIFF ====="
            , diffConformance agda impl
            ]
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

testConformance ::
  forall rule era.
  ( HasCallStack
  , ExecSpecRule rule era
  ) =>
  ExecContext rule era ->
  TRC (EraRule rule era) ->
  Property
testConformance ctx trc = property $ do
  ConformanceResult implResTest agdaResTest implRes <- runConformance @rule @era ctx trc
  globals <- use impGlobalsL
  logDoc $ extraInfo @rule @era globals ctx trc (first (T.pack . show) implRes)
  checkConformance @rule @_ ctx trc (first (T.pack . show) implResTest) agdaResTest

data ConformanceResult rule era = ConformanceResult
  { crTranslationResult ::
      Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (SpecState rule era)
  , crSpecificationResult ::
      Either
        Text
        (SpecState rule era)
  , crImplementationResult ::
      Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (State (EraRule rule era), [Event (EraRule rule era)])
  }
  deriving (Generic)

deriving instance
  ( Show (SpecState rule era)
  , Show (PredicateFailure (EraRule rule era))
  , Show (State (EraRule rule era))
  , Show (Event (EraRule rule era))
  ) =>
  Show (ConformanceResult rule era)

instance
  ( ToExpr (SpecState rule era)
  , ToExpr (PredicateFailure (EraRule rule era))
  , ToExpr (State (EraRule rule era))
  , ToExpr (Event (EraRule rule era))
  ) =>
  ToExpr (ConformanceResult rule era)

runConformance ::
  forall (rule :: Symbol) era.
  ExecSpecRule rule era =>
  ExecContext rule era ->
  TRC (EraRule rule era) ->
  ImpTestM era (ConformanceResult rule era)
runConformance execContext trc@(TRC (env, st, sig)) = do
  SpecTRC specEnv specSt specSig <-
    impAnn "Translating the inputs" $
      expectRightDeepExpr $
        translateInputs @rule @era execContext trc
  logDoc $ "ctx:\n" <> ansiExpr execContext
  logDoc $ "implEnv:\n" <> ansiExpr env
  logDoc $ "implSt:\n" <> ansiExpr st
  logDoc $ "implSig:\n" <> ansiExpr sig
  logDoc $ "specEnv:\n" <> ansiExpr specEnv
  logDoc $ "specSt:\n" <> ansiExpr specSt
  logDoc $ "specSig:\n" <> ansiExpr specSig
  agdaResTest <-
    fmap (second specNormalize) $
      impAnn "Deep evaluating Agda output" $
        evaluateDeep $
          runAgdaRule @rule @era (SpecTRC specEnv specSt specSig)
  implRes <- tryRunImpRule @rule @era env st sig
  implResTest <-
    impAnn "Translating implementation values to SpecRep" $
      case implRes of
        Right (st', _) ->
          fmap (Right . specNormalize) . expectRightExpr $
            translateOutput @rule @era execContext trc st'
        Left err -> pure $ Left err
  pure $ ConformanceResult implResTest agdaResTest implRes

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

-- | Translate a Haskell type 'a' whose translation context is 'ctx' into its Agda type, in the ImpTest monad.
translateWithContext ::
  SpecTranslate ctx a => ctx -> a -> ImpTestM era (Either Text (SpecRep a))
translateWithContext ctx x = pure . runSpecTransM ctx $ toSpecRep x

runFromAgdaFunction ::
  ( SpecEnvironment rule era ->
    SpecState rule era ->
    SpecSignal rule era ->
    Agda.ComputationResult Text (SpecState rule era)
  ) ->
  SpecTRC rule era ->
  Either Text (SpecState rule era)
runFromAgdaFunction f (SpecTRC env st sig) = unComputationResult $ f env st sig
