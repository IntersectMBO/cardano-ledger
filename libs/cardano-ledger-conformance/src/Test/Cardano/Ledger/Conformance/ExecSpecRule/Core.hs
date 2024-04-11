{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  conformsToImpl,
  computationResultToEither,
  generatesWithin,
  runConformance,
  checkConformance,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Core (EraRule)
import qualified Constrained as CV2
import Constrained.Base (shrinkWithSpec, simplifySpec)
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.Functor (($>))
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Base (Constraint, NonEmpty, Symbol, Type)
import GHC.TypeLits (KnownSymbol)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (SpecTranslate (..), runSpecTransM)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestM,
  ShelleyEraImp,
  impAnn,
  logEntry,
  tryRunImpRule,
 )
import UnliftIO (evaluateDeep)

type ForAllRuleTypes (c :: Type -> Constraint) rule era =
  ( c (Environment (EraRule rule era))
  , c (State (EraRule rule era))
  , c (Signal (EraRule rule era))
  )

class
  ( ForAllRuleTypes (CV2.HasSpec fn) rule era
  , ForAllRuleTypes ToExpr rule era
  , ForAllRuleTypes NFData rule era
  , Eq (Event (EraRule rule era))
  , Typeable (Event (EraRule rule era))
  , KnownSymbol rule
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , NFData (TestRep (PredicateFailure (EraRule rule era)))
  , SpecTranslate (ExecContext fn rule era) (PredicateFailure (EraRule rule era))
  ) =>
  ExecSpecRule fn (rule :: Symbol) era
  where
  type ExecContext fn rule era
  type ExecContext fn rule era = ()

  environmentSpec ::
    ExecContext fn rule era ->
    CV2.Specification fn (Environment (EraRule rule era))

  stateSpec ::
    ExecContext fn rule era ->
    Environment (EraRule rule era) ->
    CV2.Specification fn (State (EraRule rule era))

  signalSpec ::
    ExecContext fn rule era ->
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    CV2.Specification fn (Signal (EraRule rule era))

  genExecContext :: Gen (ExecContext fn rule era)
  default genExecContext ::
    Arbitrary (ExecContext fn rule era) =>
    Gen (ExecContext fn rule era)
  genExecContext = arbitrary

  runAgdaRule ::
    SpecRep (Environment (EraRule rule era)) ->
    SpecRep (State (EraRule rule era)) ->
    SpecRep (Signal (EraRule rule era)) ->
    Either
      (NonEmpty (SpecRep (PredicateFailure (EraRule rule era))))
      (SpecRep (State (EraRule rule era)))

  translateInputs ::
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    Signal (EraRule rule era) ->
    ExecContext fn rule era ->
    ImpTestM
      era
      ( SpecRep (Environment (EraRule rule era))
      , SpecRep (State (EraRule rule era))
      , SpecRep (Signal (EraRule rule era))
      )
  default translateInputs ::
    ( ForAllRuleTypes (SpecTranslate (ExecContext fn rule era)) rule era
    , ToExpr (SpecRep (Environment (EraRule rule era)))
    , ToExpr (SpecRep (State (EraRule rule era)))
    , ToExpr (SpecRep (Signal (EraRule rule era)))
    ) =>
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    Signal (EraRule rule era) ->
    ExecContext fn rule era ->
    ImpTestM
      era
      ( SpecRep (Environment (EraRule rule era))
      , SpecRep (State (EraRule rule era))
      , SpecRep (Signal (EraRule rule era))
      )
  translateInputs env st sig ctx = do
    agdaEnv <- expectRight . runSpecTransM ctx $ toSpecRep env
    logEntry $ "agdaEnv:\n" <> showExpr agdaEnv
    agdaSt <- expectRight . runSpecTransM ctx $ toSpecRep st
    logEntry $ "agdaSt:\n" <> showExpr agdaSt
    agdaSig <- expectRight . runSpecTransM ctx $ toSpecRep sig
    logEntry $ "agdaSig:\n" <> showExpr agdaSig
    pure (agdaEnv, agdaSt, agdaSig)

  testConformance ::
    ( ShelleyEraImp era
    , SpecTranslate (ExecContext fn rule era) (State (EraRule rule era))
    , NFData (SpecRep (Environment (EraRule rule era)))
    , NFData (SpecRep (State (EraRule rule era)))
    , NFData (SpecRep (Signal (EraRule rule era)))
    , ToExpr (SpecRep (Environment (EraRule rule era)))
    , ToExpr (SpecRep (State (EraRule rule era)))
    , ToExpr (SpecRep (Signal (EraRule rule era)))
    ) =>
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    Signal (EraRule rule era) ->
    Property
  testConformance = defaultTestConformance @fn @era @rule

checkConformance ::
  ( ToExpr (TestRep (PredicateFailure (EraRule rule era)))
  , ToExpr (TestRep (State (EraRule rule era)))
  , Eq (TestRep (PredicateFailure (EraRule rule era)))
  , Eq (TestRep (State (EraRule rule era)))
  ) =>
  Either
    (NonEmpty (TestRep (PredicateFailure (EraRule rule era))))
    (TestRep (State (EraRule rule era))) ->
  Either
    (NonEmpty (TestRep (PredicateFailure (EraRule rule era))))
    (TestRep (State (EraRule rule era))) ->
  ImpTestM era ()
checkConformance implResTest agdaResTest = do
  let
    failMsg =
      unlines
        [ ""
        , "===== DIFF ====="
        , diffExpr implResTest agdaResTest
        , ""
        , "Legend:"
        , "\t\ESC[91mImplementation"
        , "\t\ESC[92mSpecification\ESC[39m"
        ]
  unless (implResTest == agdaResTest) $ expectationFailure failMsg

defaultTestConformance ::
  forall fn era rule.
  ( ShelleyEraImp era
  , SpecTranslate (ExecContext fn rule era) (State (EraRule rule era))
  , NFData (SpecRep (Environment (EraRule rule era)))
  , NFData (SpecRep (State (EraRule rule era)))
  , NFData (SpecRep (Signal (EraRule rule era)))
  , ToExpr (SpecRep (Environment (EraRule rule era)))
  , ToExpr (SpecRep (State (EraRule rule era)))
  , ToExpr (SpecRep (Signal (EraRule rule era)))
  , ExecSpecRule fn rule era
  ) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  Property
defaultTestConformance env st sig = property $ do
  (implResTest, agdaResTest) <- runConformance @rule @fn @era env st sig
  checkConformance @rule implResTest agdaResTest

runConformance ::
  forall (rule :: Symbol) (fn :: [Type] -> Type -> Type) era.
  ( ExecSpecRule fn rule era
  , NFData (SpecRep (Environment (EraRule rule era)))
  , NFData (SpecRep (State (EraRule rule era)))
  , NFData (SpecRep (Signal (EraRule rule era)))
  , ToExpr (SpecRep (Environment (EraRule rule era)))
  , ToExpr (SpecRep (State (EraRule rule era)))
  , ToExpr (SpecRep (Signal (EraRule rule era)))
  , SpecTranslate (ExecContext fn rule era) (State (EraRule rule era))
  ) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM
    era
    ( Either
        (NonEmpty (TestRep (PredicateFailure (EraRule rule era))))
        (TestRep (State (EraRule rule era)))
    , Either
        (NonEmpty (TestRep (PredicateFailure (EraRule rule era))))
        (TestRep (State (EraRule rule era)))
    )
runConformance env st sig = do
  (execContext :: ctx) <- liftGen $ genExecContext @fn @rule @era
  (specEnv, specSt, specSig) <-
    impAnn "Translating the inputs" $
      translateInputs @fn @rule @era env st sig execContext
  logEntry $ "specEnv:\n" <> showExpr specEnv
  logEntry $ "specSt:\n" <> showExpr specSt
  logEntry $ "specSig:\n" <> showExpr specSig
  let agdaRes = runAgdaRule @fn @rule @era specEnv specSt specSig
  implRes <- tryRunImpRule @rule @era env st sig
  implResTest <-
    impAnn "Translating implementation values to SpecRep" . expectRightExpr . runSpecTransM execContext $
      bimapM (traverse toTestRep) toTestRep (fst <$> implRes)
  let
    agdaResTest =
      bimap
        (fmap $ specToTestRep @ctx @(PredicateFailure (EraRule rule era)))
        (specToTestRep @ctx @(State (EraRule rule era)))
        agdaRes
  pure (implResTest, agdaResTest)

conformsToImpl ::
  forall (rule :: Symbol) fn era.
  ( ShelleyEraImp era
  , ExecSpecRule fn rule era
  , NFData (SpecRep (Environment (EraRule rule era)))
  , NFData (SpecRep (State (EraRule rule era)))
  , NFData (SpecRep (Signal (EraRule rule era)))
  , ToExpr (SpecRep (Environment (EraRule rule era)))
  , ToExpr (SpecRep (State (EraRule rule era)))
  , ToExpr (SpecRep (Signal (EraRule rule era)))
  , SpecTranslate (ExecContext fn rule era) (State (EraRule rule era))
  , ToExpr (ExecContext fn rule era)
  ) =>
  Property
conformsToImpl =
  let genCtx = genExecContext @fn @rule @era
   in forAllShow genCtx showExpr $ \ctx ->
        let envSpec = simplifySpec $ environmentSpec @fn @rule @era ctx
         in forAllShrinkShow (CV2.genFromSpec_ envSpec) (shrinkWithSpec envSpec) showExpr $ \env ->
              let stSpec = simplifySpec $ stateSpec @fn @rule @era ctx env
               in forAllShrinkShow (CV2.genFromSpec_ stSpec) (shrinkWithSpec stSpec) showExpr $ \st ->
                    let sigSpec = simplifySpec $ signalSpec @fn @rule @era ctx env st
                     in forAllShrinkShow (CV2.genFromSpec_ sigSpec) (shrinkWithSpec sigSpec) showExpr $ \sig ->
                          testConformance @fn @rule @era env st sig

generatesWithin ::
  forall a.
  ( NFData a
  , ToExpr a
  , Typeable a
  ) =>
  Gen a ->
  Int ->
  Spec
generatesWithin gen timeout =
  prop (aName <> " generates in reasonable time")
    . forAllShow gen showExpr
    $ \x -> within timeout $ ioProperty (evaluateDeep x $> ())
  where
    aName = show (typeRep $ Proxy @a)

computationResultToEither :: Agda.ComputationResult e a -> Either e a
computationResultToEither (Agda.Success x) = Right x
computationResultToEither (Agda.Failure e) = Left e
