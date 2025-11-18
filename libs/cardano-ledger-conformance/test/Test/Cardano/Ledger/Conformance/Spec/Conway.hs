{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (spec) where

import Data.Map.Strict qualified as Map
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayCertExecContext (..))
import Test.Cardano.Ledger.Conformance.Spec.Core
import Test.Cardano.Ledger.Constrained.Conway (genUtxoExecContext)
import Test.Cardano.Ledger.Constrained.Conway.MiniTrace (
  ConwayCertGenContext (..),
  constrainedCert,
  constrainedCerts,
  constrainedDeleg,
  constrainedEnact,
  constrainedEpoch,
  constrainedGov,
  constrainedGovCert,
  constrainedPool,
  constrainedRatify,
  constrainedUtxo,
 )
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = do
  describe "Constrained Generators" $ do
    describe "Ticks transition graph" $ do
      prop "ENACT" $
        conformsToImplConstrained constrainedEnact $
          \curEpoch _ _ _ -> pure curEpoch
      prop "RATIFY" $ conformsToImplConstrained_ constrainedRatify
      xprop "EPOCH" $ conformsToImplConstrained_ constrainedEpoch
      xprop "NEWEPOCH" $ conformsToImplConstrained_ constrainedEpoch
    describe "Blocks transition graph" $ do
      prop "DELEG" $
        conformsToImplConstrained constrainedDeleg $
          \(_, ConwayCertGenContext {..}) _ _ _ -> pure $ Map.keysSet ccccDelegatees
      prop "GOVCERT" $ conformsToImplConstrained_ constrainedGovCert
      prop "POOL" $ conformsToImplConstrained_ constrainedPool
      prop "CERT" $ conformsToImplConstrained_ constrainedCert
      prop "CERTS" $
        conformsToImplConstrained constrainedCerts $
          \(_, ConwayCertGenContext {..}) _ _ _ ->
            pure $
              ConwayCertExecContext
                { ccecVotes = ccccVotes
                , ccecWithdrawals = ccccWithdrawals
                }
      prop "GOV" $ conformsToImplConstrained_ constrainedGov
      -- UTXO is disabled due to: https://github.com/IntersectMBO/cardano-ledger/issues/4876
      xprop "UTXO" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
      xprop "UTXOW" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
      xprop "LEDGER" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
      xprop "LEDGERS" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
