{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway (
  Conway,
  ConwayEra,
)
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Conway.Core (EraRule)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Conway.Transition ()
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Shelley.API

type Conway = ConwayEra

{-# DEPRECATED Conway "In favor of `ConwayEra`" #-}

-- =====================================================

instance ApplyTx ConwayEra where
  type ApplyTxRule ConwayEra = EraRule "MEMPOOL" ConwayEra
  reapplyTx = reapplyAlonzoTx

instance ApplyBlock ConwayEra

instance RunConwayRatify ConwayEra
