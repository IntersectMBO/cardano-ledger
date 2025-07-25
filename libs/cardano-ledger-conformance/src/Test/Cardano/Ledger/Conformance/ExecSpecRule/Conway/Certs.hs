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
import Data.Sequence (Seq)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert ()
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

instance ExecSpecRule "CERTS" ConwayEra where
  type ExecContext "CERTS" ConwayEra = (WitUniv ConwayEra, ConwayCertExecContext ConwayEra)

  runAgdaRule = runFromAgdaFunction Agda.certsStep

-- TODO think of how to implement this
-- testConformance ctx@(_, ccec) env st sig = property $ do
--  -- The results of runConformance are Agda types, the `ctx` is a Haskell type, we extract and translate the Withdrawal keys.
--  specWithdrawalCredSet <-
--    translateWithContext () (Map.keysSet (Map.mapKeys raCredential (ccecWithdrawals ccec)))
--  (implResTest, agdaResTest, _) <- runConformance @"CERTS" @ConwayEra ctx env st sig
--  case (implResTest, agdaResTest) of
--    (Right haskell, Right spec) ->
--      checkConformance @"CERTS" @ConwayEra
--        ctx
--        env
--        st
--        sig
--        (Right (fixRewards specWithdrawalCredSet haskell))
--        (Right spec)
--      where
--        -- Zero out the rewards for credentials that are the key of some withdrawal
--        -- (found in the ctx) as this happens in the Spec, but not in the implementation.
--        fixRewards (Agda.MkHSSet creds) x =
--          x {Agda.dState = (Agda.dState x) {Agda.dsRewards = zeroRewards (Agda.dsRewards (Agda.dState x))}}
--          where
--            credsSet = Set.fromList creds
--            zeroRewards (Agda.MkHSMap pairs) =
--              Agda.MkHSMap (map (\(c, r) -> if c `Set.member` credsSet then (c, 0) else (c, r)) pairs)
--    _ ->
--      checkConformance @"CERTS" @ConwayEra
--        ctx
--        env
--        st
--        sig
--        (first showOpaqueErrorString implResTest)
--        agdaResTest

nameCerts :: Seq (ConwayTxCert ConwayEra) -> String
nameCerts x = "Certs length " ++ show (length x)
