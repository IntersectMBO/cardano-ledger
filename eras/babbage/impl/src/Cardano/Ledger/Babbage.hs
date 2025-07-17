{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage (
  Babbage,
  BabbageEra,
  BabbageTxOut,
  TxBody (BabbageTxBody),
  Tx (..),
  AlonzoScript,
  AlonzoTxAuxData,
) where

import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Babbage.BlockBody ()
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.State ()
import Cardano.Ledger.Babbage.Transition ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.Tx (Tx (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut, TxBody (BabbageTxBody))
import Cardano.Ledger.Babbage.TxInfo ()
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.Shelley.API

type Babbage = BabbageEra

{-# DEPRECATED Babbage "In favor of `BabbageEra`" #-}

-- =====================================================

instance ApplyTx BabbageEra where
  applyTxValidation = ruleApplyTxValidation @"LEDGER"

instance ApplyBlock BabbageEra
