{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Gov () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (EraPParams (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Lens.Micro ((&), (.~), (^.))
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common

instance
  ( NFData (SpecRep (ConwayGovPredFailure ConwayEra))
  , IsConwayUniv fn
  ) =>
  ExecSpecRule fn "GOV" ConwayEra
  where
  type ExecContext fn "GOV" ConwayEra = EnactState ConwayEra

  environmentSpec _ = govEnvSpec

  stateSpec _ = govProposalsSpec

  signalSpec _ = govProceduresSpec

  runAgdaRule env st sig = unComputationResult $ Agda.govStep env st sig

  translateInputs env@GovEnv {gePParams} st sig enactState = do
    agdaEnv <- expectRight $ runSpecTransM ctx $ toSpecRep env
    agdaSt <- expectRight $ runSpecTransM ctx $ toSpecRep st
    agdaSig <- expectRight $ runSpecTransM ctx $ toSpecRep sig
    pure (agdaEnv, agdaSt, agdaSig)
    where
      ctx =
        enactState
          & ensPrevGovActionIdsL
            .~ toPrevGovActionIds (st ^. pRootsL)
          & ensProtVerL
            .~ (gePParams ^. ppProtocolVersionL)
