{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage (
  BabbageEra,
  BabbageTxOut,
  TxBody (BabbageTxBody),
  Tx (..),
  ApplyTxError (..),
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
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)

-- =====================================================

instance ApplyTx BabbageEra where
  newtype ApplyTxError BabbageEra = BabbageApplyTxError (NonEmpty (ShelleyLedgerPredFailure BabbageEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup)
  applyTxValidation validationPolicy globals env state tx =
    first BabbageApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

instance ApplyBlock BabbageEra
