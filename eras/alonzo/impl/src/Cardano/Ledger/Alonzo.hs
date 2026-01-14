{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo (
  AlonzoEra,
  AlonzoTxOut,
  MaryValue,
  pattern AlonzoTxBody,
  AlonzoScript,
  AlonzoTxAuxData,
  Tx (..),
  ApplyTxError (..),
) where

import Cardano.Ledger.Alonzo.BlockBody ()
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
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data ()
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)

-- =====================================================

instance ApplyTx AlonzoEra where
  newtype ApplyTxError AlonzoEra = AlonzoApplyTxError (NonEmpty (ShelleyLedgerPredFailure AlonzoEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup)
  applyTxValidation validationPolicy globals env state tx =
    first AlonzoApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

instance ApplyBlock AlonzoEra
