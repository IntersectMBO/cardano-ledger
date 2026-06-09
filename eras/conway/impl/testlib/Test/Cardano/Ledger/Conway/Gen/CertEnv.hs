{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Conway.Gen.CertEnv () where

import Cardano.Ledger.Conway.Core (ConwayEraPParams)
import Cardano.Ledger.Conway.Rules (CertEnv (..))
import Test.Cardano.Ledger.Imp.Common (MonadGen, arbitrary)
import Test.Cardano.Ledger.Conway.Gen.PParams (genPParams)

genCertEnv ::
  ( MonadGen m
  , ConwayEraPParams era
  ) =>
  m (CertEnv era)
genCertEnv = do
  ceCurrentEpoch <- arbitrary
  cePParams <- genPParams
  ceCurrentCommittee <- genCommittee
  ceCommitteeProposals <- _
  pure CertEnv {..}
