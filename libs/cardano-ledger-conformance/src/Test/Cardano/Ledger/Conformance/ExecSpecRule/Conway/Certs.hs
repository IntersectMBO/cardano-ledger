{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Certs (nameCerts) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Shelley.LedgerState (CertState (..), DState (..))
import Cardano.Ledger.UMap (dRepMap)
import Constrained (constrained, match, satisfies)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Lib as Agda
import Prettyprinter (vsep)
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Generic.PrettyCore

-- import Cardano.Ledger.Conway.Governance(VotingProcedures(..))
-- import Test.QuickCheck

import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Imp.Common (property)

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "CERTS" Conway
  where
  type ExecContext fn "CERTS" Conway = ConwayCertExecContext Conway

  -- genExecContext = pure ( ConwayCertExecContext Map.empty (VotingProcedures Map.empty))
  environmentSpec _ = certsEnvSpec

  stateSpec context _ =
    constrained $ \x ->
      match x $ \vstate pstate dstate ->
        [ satisfies vstate vStateSpec
        , satisfies pstate pStateSpec
        , -- temporary workaround because Spec does some extra tests, that the implementation does not, in the bootstrap phase.
          satisfies dstate (bootstrapDStateSpec (ccecWithdrawals context))
        ]

  signalSpec _ env state = txCertsSpec env state

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.certsStep' env st sig
  classOf = Just . nameCerts

  testConformance ctx env st sig = property $ do
    -- The results of runConformance are Agda types, the `ctx` is a Haskell type, we extract and translate the Withdrawal keys.
    specWithdrawalCredSet <-
      translateWithContext () (Map.keysSet (Map.mapKeys snd (ccecWithdrawals ctx)))
    (implResTest, agdaResTest) <- runConformance @"CERTS" @fn @Conway ctx env st sig
    case (implResTest, agdaResTest) of
      (Right haskell, Right spec) -> checkConformance @"CERTS" @_ @fn (Right (fixRewards specWithdrawalCredSet haskell)) (Right spec)
        where
          -- Zero out the rewards for credentials that are the key of some withdrawal
          -- (found in the ctx) as this happens in the Spec, but not in the implementation.
          fixRewards (Agda.MkHSSet creds) x =
            x {Agda.dState' = (Agda.dState' x) {Agda.rewards' = zeroRewards (Agda.rewards' (Agda.dState' x))}}
            where
              zeroRewards (Agda.MkHSMap pairs) = Agda.MkHSMap (map (\(c, r) -> if elem c creds then (c, 0) else (c, r)) pairs)
      _ -> checkConformance @"CERTS" @_ @fn implResTest agdaResTest
  extraInfo context _env state signal =
    show $
      vsep
        [ "\nWithdrawals"
        , prettyA (ccecWithdrawals context)
        , "\nJust keyHashObj of withdrawals"
        , ppSet prettyA (Set.filter isKeyHash (Map.keysSet (Map.mapKeys snd (ccecWithdrawals context))))
        , "\nDRepDelegs"
        , prettyA (dRepMap (dsUnified (certDState state)))
        , "\nSignal"
        , prettyA signal
        ]

nameCerts :: Seq (ConwayTxCert Conway) -> String
nameCerts x = "Certs length " ++ show (length x)
