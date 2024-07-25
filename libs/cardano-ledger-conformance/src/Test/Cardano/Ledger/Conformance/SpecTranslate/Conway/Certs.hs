{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Certs () where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules
import Control.Monad.Identity
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Constrained.Conway (CertsExecEnv (..))
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx (CertsExecEnv era)
  where
  type SpecRep (CertsExecEnv era) = Agda.CertEnv

  toSpecRep CertsExecEnv {..} = do
    Agda.MkCertEnv
      <$> toSpecRep (certsCurrentEpoch ceeCertEnv)
      <*> toSpecRep (certsPParams ceeCertEnv)
      <*> toSpecRep ceeVotes
      <*> toSpecRep ceeWithdrawals
      <*> toSpecRep ceeDeposits

instance
  ToExpr (PredicateFailure (EraRule "CERT" era)) =>
  SpecTranslate ctx (ConwayCertsPredFailure era)
  where
  type SpecRep (ConwayCertsPredFailure era) = OpaqueErrorString
  toSpecRep = pure . OpaqueErrorString . show . toExpr
