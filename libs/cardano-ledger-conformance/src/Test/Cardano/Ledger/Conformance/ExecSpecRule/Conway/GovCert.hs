{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert () where

import Cardano.Ledger.Conway (ConwayEra)
import Control.State.Transition.Extended (TRC (..))
import Data.Bifunctor (Bifunctor (..))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTRC (..),
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (ConwayCertExecContext (..))

instance ExecSpecRule "GOVCERT" ConwayEra where
  type ExecContext "GOVCERT" ConwayEra = ConwayCertExecContext ConwayEra

  translateInputs ConwayCertExecContext {..} (TRC (env, st, sig)) = do
    agdaEnv <- runSpecTransM (ccecVotes, ccecWithdrawals) $ toSpecRep @ConwayEra env
    agdaSt <- runSpecTransM () $ toSpecRep @ConwayEra st
    agdaSig <- runSpecTransM () $ toSpecRep @ConwayEra sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  runAgdaRule (SpecTRC env (Agda.MkCertState dState pState vState) sig) =
    second (Agda.MkCertState dState pState) . unComputationResult $
      Agda.govCertStep env vState sig
