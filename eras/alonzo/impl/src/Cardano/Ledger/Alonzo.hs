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
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext,
  LedgerTxInfo (..),
  SupportedPlutusRunnable (..),
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  scriptsWithContextFromLedgerTxInfo,
 )
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
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO,
  AlonzoScriptsNeeded,
  resolveNeededPlutusScriptsWithPurpose,
 )
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Block (EraBlockHeader)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus (plutusLanguage)
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ledgerPpL, ledgerSlotNoL)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (EraUTxO (..), utxoG)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.State.Transition.Extended (ValidationPolicy (..))
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro

instance ApplyTx AlonzoEra where
  newtype ApplyTxError AlonzoEra = AlonzoApplyTxError (NonEmpty (Shelley.ShelleyLedgerPredFailure AlonzoEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx = mkAlonzoStAnnTx

  internalApplyTxWithValidation validationPolicy globals env state tx =
    let stAnnTx =
          mkStAnnTx
            (unsafeLinearExtendEpochInfo (env ^. ledgerSlotNoL) (epochInfo globals))
            (systemStart globals)
            (env ^. ledgerPpL)
            (state ^. utxoG)
            tx
     in first AlonzoApplyTxError $
          ruleApplyTxValidation @"LEDGER" validationPolicy globals env state stAnnTx

  internalReapplyValidatedTx globals env state vtx
    | getValidatedTxSlotNo vtx == env ^. ledgerSlotNoL =
        fst
          <$> first
            AlonzoApplyTxError
            ( ruleApplyTxValidation @"LEDGER"
                (ValidateSuchThat (notElem lblStatic))
                globals
                env
                state
                (getValidatedTxStAnnTx vtx)
            )
    | otherwise =
        fst
          <$> internalApplyTxWithValidation
            ValidateAll
            globals
            env
            state
            (getValidatedTxStAnnTx vtx ^. txStAnnTxG)

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
    protVer = pp ^. ppProtocolVersionL
    scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    scriptsProvided = getScriptsProvided utxo tx
    plutusScriptsUsed = resolveNeededPlutusScriptsWithPurpose protVer scriptsProvided scriptsNeeded
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = ei
        , ltiSystemStart = sysStart
        , ltiUTxO = utxo
        , ltiTx = tx
        , ltiMemoizedSubTransactions = mempty
        }
   in
    AlonzoStAnnTx
      { asatTx = tx
      , asatScriptsNeeded = scriptsNeeded
      , asatScriptsProvided = scriptsProvided
      , asatPlutusLanguagesUsed =
          Set.fromList [plutusLanguage spr | (_, SupportedPlutusRunnable spr) <- plutusScriptsUsed]
      , asatPlutusScriptsWithContext =
          scriptsWithContextFromLedgerTxInfo ledgerTxInfo (pp ^. ppCostModelsL) plutusScriptsUsed
      }
