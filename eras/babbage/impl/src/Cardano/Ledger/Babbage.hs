{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage (
  Babbage,
  BabbageEra,
  BabbageTxOut,
  BabbageTxBody,
  AlonzoScript,
  AlonzoTxAuxData,
)
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Transition ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody, BabbageTxOut)
import Cardano.Ledger.Babbage.TxInfo ()
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.API

type Babbage = BabbageEra

-- =====================================================

instance DSignable (Hash EraIndependentTxBody) => ApplyTx BabbageEra where
  reapplyTx = reapplyAlonzoTx

instance DSignable (Hash EraIndependentTxBody) => ApplyBlock BabbageEra
