{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  AlonzoStAnnTx (..),
  mkAlonzoStAnnTx,
) where

import Cardano.Ledger.Alonzo.BlockBody ()
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Forecast ()
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext)
import Cardano.Ledger.Alonzo.Plutus.TxInfo ()
import Cardano.Ledger.Alonzo.Rules ()
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.State ()
import Cardano.Ledger.Alonzo.Transition ()
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Alonzo.Tx (AlonzoStAnnTx (..), Tx (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut, TxBody (AlonzoTxBody))
import Cardano.Ledger.Alonzo.TxWits ()
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Block (EraBlockHeader)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data ()
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure)
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro

instance ApplyTx AlonzoEra where
  newtype ApplyTxError AlonzoEra = AlonzoApplyTxError (NonEmpty (ShelleyLedgerPredFailure AlonzoEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx = mkAlonzoStAnnTx

  applyTxValidation validationPolicy globals env state tx =
    first AlonzoApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

instance ApplyTick AlonzoEra

instance EraBlockHeader h AlonzoEra => ApplyBlock h AlonzoEra

mkAlonzoStAnnTx ::
  ( AlonzoEraUTxO era
  , AlonzoEraTx era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  UTxO era ->
  Tx TopTx era ->
  AlonzoStAnnTx TopTx era
mkAlonzoStAnnTx ei sysStart pp utxo tx =
  let
    scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    scriptsProvided = getScriptsProvided utxo tx
   in
    AlonzoStAnnTx
      { asatTx = tx
      , asatProtocolVersion = pp ^. ppProtocolVersionL
      , asatScriptsNeeded = scriptsNeeded
      , asatScriptsProvided = scriptsProvided
      , asatPlutusScriptsWithContext = collectPlutusScriptsWithContext ei sysStart pp tx utxo
      }
