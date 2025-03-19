{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Certs (nameCerts) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Constrained
import Data.Bifunctor (first)
import Data.Sequence (Seq)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.Cardano.Ledger.Imp.Common hiding (context)

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "CERTS" ConwayEra
  where
  type ExecContext fn "CERTS" ConwayEra = (WitUniv ConwayEra, ConwayCertExecContext ConwayEra)

  genExecContext = do
    univ <- genWitUniv @ConwayEra 300
    ccec <- genFromSpec @fn (conwayCertExecContextSpec univ 5)
    pure (univ, ccec)

  environmentSpec _ = certsEnvSpec

  stateSpec (univ, context) _ =
    constrained $ \x ->
      match x $ \vstate pstate dstate ->
        [ satisfies vstate (vStateSpec @_ @ConwayEra univ (ccecDelegatees context))
        , satisfies pstate (pStateSpec @_ @ConwayEra univ)
        , -- temporary workaround because Spec does some extra tests, that the implementation does not, in the bootstrap phase.
          satisfies dstate (bootstrapDStateSpec univ (ccecDelegatees context) (ccecWithdrawals context))
        ]

  signalSpec (univ, _) env state = txCertsSpec @ConwayEra @fn univ env state

  runAgdaRule env st sig = unComputationResult $ Agda.certsStep env st sig
  classOf = Just . nameCerts

  testConformance ctx env st sig = property $ do
    (implResTest, agdaResTest, _) <- runConformance @"CERTS" @fn @ConwayEra ctx env st sig
    checkConformance @"CERTS" @ConwayEra @fn
      ctx
      env
      st
      sig
      (first showOpaqueErrorString implResTest)
      agdaResTest

nameCerts :: Seq (ConwayTxCert ConwayEra) -> String
nameCerts x = "Certs length " ++ show (length x)
