{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway (
  Conway,
  ConwayEra,
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
  Tx (..),
) where

import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Conway.BlockBody ()
import Cardano.Ledger.Conway.Era (
  ConwayEra,
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
 )
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Conway.State ()
import Cardano.Ledger.Conway.Transition ()
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx (Tx (..))
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Shelley.API

type Conway = ConwayEra

{-# DEPRECATED Conway "In favor of `ConwayEra`" #-}

-- =====================================================

instance ApplyTx ConwayEra where
  applyTxValidation = ruleApplyTxValidation @"MEMPOOL"

instance ApplyBlock ConwayEra

instance RunConwayRatify ConwayEra
