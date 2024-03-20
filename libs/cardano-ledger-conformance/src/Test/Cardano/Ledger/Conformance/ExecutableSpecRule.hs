{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Ledger.Conformance.ExecutableSpecRule
  ( EraUniv
  , ExecutableSpecRule (..)
  , conformsToImpl
  ) where

import Data.Kind (Type)
import qualified Constrained as CV2
import Control.State.Transition.Extended (STS(..))
import Cardano.Ledger.Core (EraRule)
import Test.Cardano.Ledger.Conformance.SpecTranslate (SpecTranslate(..))
import GHC.Base (Symbol, NonEmpty)
import Test.Cardano.Ledger.Imp.Common (MonadGen(..)
  , NFData, ToExpr, expectRightExpr, shouldBeExpr)
import GHC.TypeLits (KnownSymbol)
import Data.Typeable (Typeable)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (bimapM)
import Test.Cardano.Ledger.Shelley.ImpTest
  ( ImpTestM
  , impAnn
  , tryRunImpRule
  )

type family EraUniv era :: [Type] -> Type -> Type

class
  ( CV2.HasSpec (EraUniv era) (Environment (EraRule rule era))
  , CV2.HasSpec (EraUniv era) (State (EraRule rule era))
  , CV2.HasSpec (EraUniv era) (Signal (EraRule rule era))
  , SpecTranslate (Environment (EraRule rule era))
  , SpecTranslate (State (EraRule rule era))
  , SpecTranslate (Signal (EraRule rule era))
  , SpecTranslate (PredicateFailure (EraRule rule era))
  , NFData (SpecRep (PredicateFailure (EraRule rule era)))
  , NFData (SpecRep (State (EraRule rule era)))
  , NFData (State (EraRule rule era))
  , NFData (Event (EraRule rule era))
  , NFData (TestRep (PredicateFailure (EraRule rule era)))
  , NFData (TestRep (State (EraRule rule era)))
  , ToExpr (Event (EraRule rule era))
  , ToExpr (TestRep (PredicateFailure (EraRule rule era)))
  , ToExpr (TestRep (State (EraRule rule era)))
  , Eq (Event (EraRule rule era))
  , Eq (TestRep (PredicateFailure (EraRule rule era)))
  , Eq (TestRep (State (EraRule rule era)))
  , Typeable (Event (EraRule rule era))
  , KnownSymbol rule
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  ) =>
  ExecutableSpecRule rule era
  where
  environmentSpec ::
    CV2.Spec (EraUniv era) (Environment (EraRule rule era))

  stateSpec ::
    Environment (EraRule rule era) ->
    CV2.Spec (EraUniv era) (State (EraRule rule era))

  signalSpec ::
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    CV2.Spec (EraUniv era) (Signal (EraRule rule era))

  runAgdaRule ::
    SpecRep (Environment (EraRule rule era)) ->
    SpecRep (State (EraRule rule era)) ->
    SpecRep (Signal (EraRule rule era)) ->
    Either
      (NonEmpty (SpecRep (PredicateFailure (EraRule rule era))))
      (SpecRep (State (EraRule rule era)))

conformsToImpl :: forall era (rule :: Symbol).
  ExecutableSpecRule rule era =>
  ImpTestM era ()
conformsToImpl = do
  env <- liftGen . CV2.genFromSpec_ $ environmentSpec @rule @era
  st <- liftGen . CV2.genFromSpec_ $ stateSpec @rule @era env
  sig <- liftGen . CV2.genFromSpec_ $ signalSpec @rule @era env st
  agdaRes <- impAnn "Translating spec values to SpecRep" . expectRightExpr $
    runAgdaRule @rule @era <$> toSpecRep env <*> toSpecRep st <*> toSpecRep sig
  implRes <- fmap fst <$> tryRunImpRule @rule @era env st sig
  implResTest <- impAnn "Translating implementation values to SpecRep" . expectRightExpr $
    bimapM (traverse toTestRep) toTestRep implRes
  let
    agdaResTest =
      bimap
        (fmap $ specToTestRep @(PredicateFailure (EraRule rule era)))
        (specToTestRep @(State (EraRule rule era)))
        agdaRes
  agdaResTest `shouldBeExpr` implResTest
