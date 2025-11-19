{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway.Ratify (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  EnactState,
  GovActionState (..),
  RatifyEnv (..),
  RatifyState (..),
  rsEnactStateL,
 )
import Cardano.Ledger.Conway.Rules (
  committeeAccepted,
  dRepAccepted,
  spoAccepted,
 )
import Constrained.Generation
import Data.Either (fromRight)
import Lens.Micro
import MAlonzo.Code.Ledger.Foreign.API qualified as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import Test.Cardano.Ledger.Conformance.Spec.Core
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Constrained.Conway.MiniTrace (
  ConstrainedGeneratorBundle (..),
  constrainedRatify,
 )
import Test.Cardano.Ledger.Imp.Common

conformsToImplAccepted ::
  era ~ ConwayEra =>
  (RatifyEnv era -> RatifyState era -> GovActionState era -> Bool) ->
  (SpecRep (RatifyEnv era) -> SpecRep (EnactState era) -> SpecRep (GovActionState era) -> Bool) ->
  Property
conformsToImplAccepted impl agda = property $ do
  let ConstrainedGeneratorBundle {..} = constrainedRatify
  govActions <- cgbContextGen
  ratifyEnv <- genFromSpec $ cgbEnvironmentSpec govActions
  ratifySt <- genFromSpec $ cgbStateSpec govActions ratifyEnv
  let specEnv = fromRight (error "conformsToImplAccepted") $ runSpecTransM @Coin 0 $ toSpecRep ratifyEnv
      specSt =
        fromRight (error "conformsToImplAccepted") $
          runSpecTransM govActions $
            toSpecRep (ratifySt ^. rsEnactStateL)
      specGovActions = fromRight (error "conformsToImplAccepted") $ runSpecTransM () $ toSpecRep govActions
  return $
    and $
      zipWith (\ga sga -> impl ratifyEnv ratifySt ga == agda specEnv specSt sga) govActions specGovActions

spec :: Spec
spec = describe "RATIFY" $ do
  prop "STS" $ conformsToImplConstrained_ constrainedRatify
  describe "Accepted" $ do
    forM_
      [ ("DRep", (dRepAccepted, Agda.acceptedByDRep))
      , ("SPO", (spoAccepted, Agda.acceptedBySPO))
      ]
      $ \(l, (impl, agda)) ->
        prop l $ conformsToImplAccepted impl agda
    -- https://github.com/IntersectMBO/cardano-ledger/issues/5418
    -- TODO: Re-enable after issue is resolved, by removing this override
    xprop "CC" $ conformsToImplAccepted committeeAccepted Agda.acceptedByCC
