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
) where

import Cardano.Ledger.BaseTypes (Inject, ShelleyBase)
import Cardano.Ledger.Core (EraRule)
import qualified Constrained as CV2
import Constrained.Base (Specification (..))
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.Typeable (Typeable)
import GHC.Base (Constraint, NonEmpty, Symbol, Type)
import GHC.TypeLits (KnownSymbol)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (SpecTranslate (..), runSpecTransM)
import Test.Cardano.Ledger.Imp.Common (
  MonadGen (..),
  NFData,
  ToExpr,
  expectRightExpr,
  shouldBeExpr,
 )
import Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestM,
  impAnn,
  logEntry,
  logToExpr,
  tryRunImpRule,
 )

type ForAllRuleTypes (c :: Type -> Constraint) rule era =
  ( c (Environment (EraRule rule era))
  , c (State (EraRule rule era))
  , c (Signal (EraRule rule era))
  )

class
  ( ForAllRuleTypes (CV2.HasSpec fn) rule era
  , ForAllRuleTypes (SpecTranslate (ExecContext fn rule era)) rule era
  , ForAllRuleTypes ToExpr rule era
  , ForAllRuleTypes NFData rule era
  , CV2.HasSpec fn (ExecContext fn rule era)
  , Inject (ExecContext fn rule era) (SpecTransContext (Environment (EraRule rule era)))
  , Inject (ExecContext fn rule era) (SpecTransContext (State (EraRule rule era)))
  , Inject (ExecContext fn rule era) (SpecTransContext (Signal (EraRule rule era)))
  , Inject (ExecContext fn rule era) (SpecTransContext (PredicateFailure (EraRule rule era)))
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
    CV2.Specification fn (Environment (EraRule rule era))

  stateSpec ::
    Environment (EraRule rule era) ->
    CV2.Specification fn (State (EraRule rule era))

  signalSpec ::
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    CV2.Specification fn (Signal (EraRule rule era))

  execContextSpec :: CV2.Specification fn (ExecContext fn rule era)
  default execContextSpec ::
    ExecContext fn rule era ~ () =>
    CV2.Specification fn (ExecContext fn rule era)
  execContextSpec = TrueSpec

  runAgdaRule ::
    SpecRep (Environment (EraRule rule era)) ->
    SpecRep (State (EraRule rule era)) ->
    SpecRep (Signal (EraRule rule era)) ->
    Either
      (NonEmpty (SpecRep (PredicateFailure (EraRule rule era))))
      (SpecRep (State (EraRule rule era)))

conformsToImpl ::
  forall (rule :: Symbol) fn era.
  ( ExecSpecRule fn rule era
  , NFData (SpecRep (PredicateFailure (EraRule rule era)))
  , NFData (SpecRep (State (EraRule rule era)))
  ) =>
  ImpTestM era ()
conformsToImpl = impAnn "conformsToImpl" . resize 5 $ do
  env <- liftGen . CV2.genFromSpec_ $ environmentSpec @fn @rule @era
  logToExpr env
  st <- liftGen . CV2.genFromSpec_ $ stateSpec @fn @rule @era env
  logToExpr st
  sig <- liftGen . CV2.genFromSpec_ $ signalSpec @fn @rule @era env st
  logToExpr sig
  logEntry . show $ signalSpec @fn @rule @era env st
  (execContext :: ctx) <- liftGen . CV2.genFromSpec_ $ execContextSpec @fn @rule @era
  agdaRes <-
    impAnn "Translating spec values to SpecRep" . expectRightExpr . runSpecTransM execContext $
      runAgdaRule @fn @rule @era <$> toSpecRep env <*> toSpecRep st <*> toSpecRep sig
  implRes <- fmap fst <$> tryRunImpRule @rule @era env st sig
  implResTest <-
    impAnn "Translating implementation values to SpecRep" . expectRightExpr . runSpecTransM execContext $
      bimapM (traverse toTestRep) toTestRep implRes
  let
    agdaResTest =
      bimap
        (fmap $ specToTestRep @ctx @(PredicateFailure (EraRule rule era)))
        (specToTestRep @ctx @(State (EraRule rule era)))
        agdaRes
  agdaResTest `shouldBeExpr` implResTest

computationResultToEither :: Agda.ComputationResult e a -> Either e a
computationResultToEither (Agda.Success x) = Right x
computationResultToEither (Agda.Failure e) = Left e
