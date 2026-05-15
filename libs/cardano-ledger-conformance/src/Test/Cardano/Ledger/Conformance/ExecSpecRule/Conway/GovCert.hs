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
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (ConwayCertExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  SpecTRC (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  unComputationResult,
  withCtxSpecTransM,
 )

instance ExecSpecRule "GOVCERT" ConwayEra where
  type ExecContext "GOVCERT" ConwayEra = ConwayCertExecContext ConwayEra

  translateInputs (TRC (env, st, sig)) = do
    ConwayCertExecContext {..} <- askSpecTransM
    agdaEnv <- withCtxSpecTransM (ccecVotes, ccecWithdrawals) $ toSpecRep env
    agdaSt <- withCtxSpecTransM () $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  runAgdaRule (SpecTRC env (Agda.MkCertState dState pState vState) sig) =
    second (Agda.MkCertState dState pState) . unComputationResult $
      Agda.govCertStep env vState sig
