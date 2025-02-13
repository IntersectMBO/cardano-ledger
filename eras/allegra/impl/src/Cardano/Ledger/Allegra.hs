{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra (
  Allegra,
  AllegraEra,
)
where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Rules ()
import Cardano.Ledger.Allegra.Scripts ()
import Cardano.Ledger.Allegra.Transition ()
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Allegra.TxSeq ()
import Cardano.Ledger.Allegra.UTxO ()
import Cardano.Ledger.Shelley.API

type Allegra = AllegraEra

{-# DEPRECATED Allegra "In favor of `AllegraEra`" #-}

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance ApplyTx AllegraEra where
  applyTxValidation = ruleApplyTxValidation @"LEDGER"

instance ApplyBlock AllegraEra
