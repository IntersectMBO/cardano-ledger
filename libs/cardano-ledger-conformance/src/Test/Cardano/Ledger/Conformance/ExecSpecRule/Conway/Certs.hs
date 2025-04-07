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

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Constrained.API
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.Cardano.Ledger.Imp.Common hiding (context)

instance ExecSpecRule "CERTS" ConwayEra where
  type ExecContext "CERTS" ConwayEra = (WitUniv ConwayEra, ConwayCertExecContext ConwayEra)

  genExecContext = do
    univ <- genWitUniv @ConwayEra 300
    ccec <- genFromSpec (conwayCertExecContextSpec univ 5)
    pure (univ, ccec)

  environmentSpec _ = certsEnvSpec

  stateSpec (univ, context) _ =
    constrained $ \x ->
      match x $ \vstate pstate dstate ->
        [ satisfies vstate (vStateSpec @ConwayEra univ (ccecDelegatees context))
        , satisfies pstate (pStateSpec @ConwayEra univ)
        , -- temporary workaround because Spec does some extra tests, that the implementation does not, in the bootstrap phase.
          satisfies dstate (bootstrapDStateSpec univ (ccecDelegatees context) (ccecWithdrawals context))
        ]

  signalSpec (univ, _) env state = txCertsSpec @ConwayEra univ env state

  runAgdaRule env st sig = unComputationResult $ Agda.certsStep env st sig
  classOf = Just . nameCerts

  testConformance ctx@(_, ccec) env st sig = property $ do
    -- The results of runConformance are Agda types, the `ctx` is a Haskell type, we extract and translate the Withdrawal keys.
    specWithdrawalCredSet <-
      translateWithContext () (Map.keysSet (Map.mapKeys raCredential (ccecWithdrawals ccec)))
    (implResTest, agdaResTest, _) <- runConformance @"CERTS" @ConwayEra ctx env st sig
    case (implResTest, agdaResTest) of
      (Right haskell, Right spec) ->
        checkConformance @"CERTS" @ConwayEra
          ctx
          env
          st
          sig
          (Right (fixRewards specWithdrawalCredSet haskell))
          (Right spec)
        where
          -- Zero out the rewards for credentials that are the key of some withdrawal
          -- (found in the ctx) as this happens in the Spec, but not in the implementation.
          fixRewards (Agda.MkHSSet creds) x =
            x {Agda.dState = (Agda.dState x) {Agda.dsRewards = zeroRewards (Agda.dsRewards (Agda.dState x))}}
            where
              credsSet = Set.fromList creds
              zeroRewards (Agda.MkHSMap pairs) =
                Agda.MkHSMap (map (\(c, r) -> if c `Set.member` credsSet then (c, 0) else (c, r)) pairs)
      _ ->
        checkConformance @"CERTS" @ConwayEra
          ctx
          env
          st
          sig
          (first showOpaqueErrorString implResTest)
          agdaResTest

nameCerts :: Seq (ConwayTxCert ConwayEra) -> String
nameCerts x = "Certs length " ++ show (length x)
