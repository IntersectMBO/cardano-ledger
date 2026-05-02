{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra (
  DijkstraEra,
  ApplyTxError (..),
  mkDijkstraStAnnTopTx,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (mkTxInfoResult), LedgerTxInfo (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  scriptsWithContextFromLedgerTxInfo,
  scriptsWithContextFromLedgerTxInfoWithResult,
 )
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO,
  AlonzoScriptsNeeded,
  resolveNeededPlutusScriptsWithPurpose,
 )
import Cardano.Ledger.BaseTypes (Inject (inject))
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Block (EraBlockHeader)
import Cardano.Ledger.Conway.Governance (RunConwayRatify)
import Cardano.Ledger.Dijkstra.BlockBody ()
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Forecast ()
import Cardano.Ledger.Dijkstra.Genesis ()
import Cardano.Ledger.Dijkstra.Governance ()
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraLedgerPredFailure,
  DijkstraMempoolPredFailure (LedgerFailure),
 )
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.State.CertState ()
import Cardano.Ledger.Dijkstra.State.Stake ()
import Cardano.Ledger.Dijkstra.Transition ()
import Cardano.Ledger.Dijkstra.Translation ()
import Cardano.Ledger.Dijkstra.Tx (DijkstraStAnnTx (..))
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Dijkstra.TxInfo ()
import Cardano.Ledger.Dijkstra.TxWits ()
import Cardano.Ledger.Dijkstra.UTxO ()
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Shelley.API (
  ApplyBlock (..),
  ApplyTick (..),
  ApplyTx (..),
  ruleApplyTxValidation,
 )
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided, UTxO)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro

instance ApplyTx DijkstraEra where
  newtype ApplyTxError DijkstraEra = DijkstraApplyTxError (NonEmpty (DijkstraMempoolPredFailure DijkstraEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx = mkDijkstraStAnnTopTx

  applyTxValidation validationPolicy globals env state stAnnTx =
    first DijkstraApplyTxError $
      ruleApplyTxValidation @"MEMPOOL" validationPolicy globals env state stAnnTx

instance ApplyTick DijkstraEra

-- Even though `EraBlockHeader` looks like it is implied there is a
-- loopy superclasses warning that suggests to add it here
instance (EraBlockHeader h DijkstraEra, DijkstraEraBlockHeader h DijkstraEra) => ApplyBlock h DijkstraEra where
  wrapBlockSignal = DijkstraBbodySignal

instance RunConwayRatify DijkstraEra

instance Inject (NonEmpty (DijkstraMempoolPredFailure DijkstraEra)) (ApplyTxError DijkstraEra) where
  inject = DijkstraApplyTxError

instance Inject (NonEmpty (DijkstraLedgerPredFailure DijkstraEra)) (ApplyTxError DijkstraEra) where
  inject = DijkstraApplyTxError . fmap LedgerFailure

mkDijkstraStAnnTopTx ::
  ( AlonzoEraUTxO era
  , AlonzoEraTx era
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  UTxO era ->
  Tx TopTx era ->
  DijkstraStAnnTx TopTx era
mkDijkstraStAnnTopTx ei sysStart pp utxo tx =
  let
    txBody = tx ^. bodyTxL
    scriptsNeeded = getScriptsNeeded utxo txBody
    scriptsProvided = getScriptsProvided utxo tx
    plutusScriptsUsed = resolveNeededPlutusScriptsWithPurpose scriptsProvided scriptsNeeded
    stAnnSubTxs =
      map
        (mkDijkstraStAnnSubTx ei sysStart pp utxo scriptsProvided)
        (toList (txBody ^. subTransactionsTxBodyL))
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = pp ^. ppProtocolVersionL
        , ltiEpochInfo = ei
        , ltiSystemStart = sysStart
        , ltiUTxO = utxo
        , ltiTx = tx
        , ltiMemoizedSubTransactions =
            Map.fromList
              [ (txIdTx dsastTx, dsastTxInfoResult)
              | DijkstraStAnnSubTx {dsastTx, dsastTxInfoResult} <- stAnnSubTxs
              ]
        }
    languagesUsed = Set.fromList [plutusScriptLanguage s | (_, _, s) <- plutusScriptsUsed]
   in
    DijkstraStAnnTopTx
      { dsattTx = tx
      , dsattProtocolVersion = pp ^. ppProtocolVersionL
      , dsattScriptsNeeded = scriptsNeeded
      , dsattScriptsProvided = scriptsProvided
      , dsattPlutusLegacyMode = not $ Set.null $ Set.filter (<= PlutusV3) languagesUsed
      , dsattPlutusLanguagesUsed = languagesUsed
      , dsattPlutusScriptsWithContext =
          scriptsWithContextFromLedgerTxInfo ledgerTxInfo (pp ^. ppCostModelsL) plutusScriptsUsed
      , dsattStAnnSubTxs = stAnnSubTxs
      }

mkDijkstraStAnnSubTx ::
  ( AlonzoEraUTxO era
  , AlonzoEraTx era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  UTxO era ->
  ScriptsProvided era ->
  Tx SubTx era ->
  DijkstraStAnnTx SubTx era
mkDijkstraStAnnSubTx ei sysStart pp utxo scriptsProvided tx =
  let
    scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    plutusScriptsUsed = resolveNeededPlutusScriptsWithPurpose scriptsProvided scriptsNeeded
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = pp ^. ppProtocolVersionL
        , ltiEpochInfo = ei
        , ltiSystemStart = sysStart
        , ltiUTxO = utxo
        , ltiTx = tx
        , ltiMemoizedSubTransactions = mempty
        }
    txInfoResult = mkTxInfoResult ledgerTxInfo
   in
    DijkstraStAnnSubTx
      { dsastTx = tx
      , dsastScriptsNeeded = scriptsNeeded
      , dsastScriptsProvided = scriptsProvided
      , dsastTxInfoResult = txInfoResult
      , dsastPlutusLanguagesUsed =
          Set.fromList [plutusScriptLanguage s | (_, _, s) <- plutusScriptsUsed]
      , dsastPlutusScriptsWithContext =
          scriptsWithContextFromLedgerTxInfoWithResult
            ledgerTxInfo
            txInfoResult
            (pp ^. ppCostModelsL)
            plutusScriptsUsed
      }
