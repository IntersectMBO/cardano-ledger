{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo (
  Alonzo,
  AlonzoEra,
  AlonzoTxOut,
  MaryValue,
#if __GLASGOW_HASKELL__ >= 914
  data AlonzoTxBody,
#else
  pattern AlonzoTxBody,
#endif
  AlonzoScript,
  AlonzoTxAuxData,
  Tx (..),
) where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Plutus.TxInfo ()
import Cardano.Ledger.Alonzo.Rules ()
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.State ()
import Cardano.Ledger.Alonzo.Transition ()
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Alonzo.Tx (Tx (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut, TxBody (AlonzoTxBody))
import Cardano.Ledger.Alonzo.TxWits ()
import Cardano.Ledger.Alonzo.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data ()
import Cardano.Ledger.Shelley.API

type Alonzo = AlonzoEra

{-# DEPRECATED Alonzo "In favor of `AlonzoEra`" #-}

-- =====================================================

instance ApplyTx AlonzoEra where
  applyTxValidation = ruleApplyTxValidation @"LEDGER"

instance ApplyBlock AlonzoEra
