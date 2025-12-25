{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway (
  ConwayEra,
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
  hardforkConwayDELEGIncorrectDepositsAndRefunds,
  hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule,
  Tx (..),
  ApplyTxError (..),
) where

import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Conway.BlockBody ()
import Cardano.Ledger.Conway.Era (
  ConwayEra,
  hardforkConwayBootstrapPhase,
  hardforkConwayDELEGIncorrectDepositsAndRefunds,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
  hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule,
 )
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.State ()
import Cardano.Ledger.Conway.Transition ()
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx (Tx (..))
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Shelley.API
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)

-- =====================================================

instance ApplyTx ConwayEra where
  newtype ApplyTxError ConwayEra = ConwayApplyTxError (NonEmpty (ConwayLedgerPredFailure ConwayEra))
    deriving (Eq, Show)
  applyTxValidation validationPolicy globals env state tx =
    first ConwayApplyTxError $
      ruleApplyTxValidation @"MEMPOOL" validationPolicy globals env state tx

instance ApplyBlock ConwayEra

instance RunConwayRatify ConwayEra

instance Inject (NonEmpty (ConwayLedgerPredFailure ConwayEra)) (ApplyTxError ConwayEra) where
  inject = ConwayApplyTxError
