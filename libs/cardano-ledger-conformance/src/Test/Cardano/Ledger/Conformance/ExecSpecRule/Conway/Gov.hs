{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Gov () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (EraPParams (..))
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Control.State.Transition.Extended (TRC (..))
import Lens.Micro ((&), (.~), (^.))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
import Test.Cardano.Ledger.Conway.Arbitrary ()

instance ExecSpecRule "GOV" ConwayEra where
  type ExecContext "GOV" ConwayEra = EnactState ConwayEra

  runAgdaRule (SpecTRC env st sig) = unComputationResult $ Agda.govStep env st sig

  translateInputs enactState (TRC (env@Conway.GovEnv {Conway.gePParams}, st, sig)) = do
    agdaEnv <- runSpecTransM ctx $ toSpecRep @ConwayEra env
    agdaSt <- runSpecTransM () $ toSpecRep @ConwayEra st
    agdaSig <- runSpecTransM () $ toSpecRep @ConwayEra sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig
    where
      ctx =
        enactState
          & ensPrevGovActionIdsL .~ toPrevGovActionIds (st ^. pRootsL)
          & ensProtVerL .~ (gePParams ^. ppProtocolVersionL)
