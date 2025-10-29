{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra (
  AllegraEra,
  Tx (..),
) where

import Cardano.Ledger.Allegra.BlockBody ()
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Rules ()
import Cardano.Ledger.Allegra.Scripts ()
import Cardano.Ledger.Allegra.State ()
import Cardano.Ledger.Allegra.Transition ()
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Allegra.Tx (Tx (..))
import Cardano.Ledger.Allegra.UTxO ()
import Cardano.Ledger.Shelley.API

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance ApplyTx AllegraEra where
  applyTxValidation = ruleApplyTxValidation @"LEDGER"

instance ApplyBlock AllegraEra
