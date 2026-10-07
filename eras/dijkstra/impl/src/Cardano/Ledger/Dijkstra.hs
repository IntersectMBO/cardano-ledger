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
  evalDijkstraTxExUnits,
  evalDijkstraTxExUnitsWithLogs,
  DijkstraRedeemerReport,
  DijkstraRedeemerReportWithLogs,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (mkTxInfoResult),
  LedgerLevelTxInfo (..),
  LedgerTxInfo (..),
  SupportedPlutusRunnable (..),
  toScriptHashByPurpose,
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  TransactionScriptFailure,
  evalTxExUnitsWithLogsFromLedgerTxInfo,
  scriptsWithContextFromLedgerTxInfo,
  scriptsWithContextFromLedgerTxInfoWithResult,
 )
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO,
  AlonzoScriptsNeeded,
  resolveNeededPlutusScriptsWithPurpose,
 )
import Cardano.Ledger.BaseTypes (Inject (inject), StrictMaybe (..), TxIx (..))
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Block (EraBlockHeader, LeiosBbodySignal (..), LeiosEraBlockHeader)
import Cardano.Ledger.Conway.Governance (RunConwayRatify)
import Cardano.Ledger.Dijkstra.Block ()
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
import Cardano.Ledger.Plutus (ExUnits, Language (..), plutusLanguage)
import Cardano.Ledger.Shelley.API (
  ApplyBlock (..),
  ApplyTick (..),
  ApplyTx (..),
  defaultApplyTxWithValidation,
  defaultReapplyValidatedTx,
 )
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided, UTxO)
import Cardano.Ledger.TxIn (TxId)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
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

  internalApplyTxWithValidation = defaultApplyTxWithValidation @"MEMPOOL" DijkstraApplyTxError

  internalReapplyValidatedTx = defaultReapplyValidatedTx @"MEMPOOL" DijkstraApplyTxError

instance ApplyTick DijkstraEra

instance (EraBlockHeader h DijkstraEra, LeiosEraBlockHeader h DijkstraEra) => ApplyBlock h DijkstraEra where
  wrapBlockSignal = LeiosBbodySignal

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
  Map.Map ScriptHash (SupportedPlutusRunnable era) ->
  Tx TopTx era ->
  DijkstraStAnnTx TopTx era
mkDijkstraStAnnTopTx ei sysStart pp utxo stAnnTxCache tx =
  let
    txBody = tx ^. bodyTxL
    protVer = pp ^. ppProtocolVersionL
    scriptsNeeded = getScriptsNeeded utxo txBody
    scriptsProvided = getScriptsProvided utxo tx
    (newStAnnTxCache, plutusScriptsUsed) =
      resolveNeededPlutusScriptsWithPurpose protVer scriptsProvided scriptsNeeded stAnnTxCache
    -- We do not need to fold over sub-transactions in order to get updated cache, since
    -- `getScriptsProvided` is recursive and will collect all scripts from sub-transactions
    stAnnSubTxs =
      zipWith
        (mkDijkstraStAnnSubTx ei sysStart pp utxo scriptsProvided newStAnnTxCache)
        [TxIx 0 ..]
        (toList (txBody ^. subTransactionsTxBodyL))
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = ei
        , ltiSystemStart = sysStart
        , ltiUTxO = utxo
        , ltiTx = tx
        , ltiScriptsUsed = plutusScriptsUsed
        , ltiScriptHashesUsed = toScriptHashByPurpose plutusScriptsUsed
        , ltiLevelTxInfo =
            LedgerTopTxInfo $
              Map.fromList
                [ (txIdTx dsastTx, dsastTxInfoResult)
                | DijkstraStAnnSubTx {dsastTx, dsastTxInfoResult} <- stAnnSubTxs
                ]
        }
    languagesUsed =
      Set.fromList [plutusLanguage spr | (_, SupportedPlutusRunnable spr) <- plutusScriptsUsed]
   in
    DijkstraStAnnTopTx
      { dsattTx = tx
      , dsattScriptsNeeded = scriptsNeeded
      , dsattScriptsProvided = scriptsProvided
      , dsattPlutusLegacyMode = not $ Set.null $ Set.filter (<= PlutusV3) languagesUsed
      , dsattPlutusRunnableCache = newStAnnTxCache
      , dsattPlutusLanguagesUsed = languagesUsed
      , dsattPlutusScriptsWithContext =
          scriptsWithContextFromLedgerTxInfo ledgerTxInfo (pp ^. ppCostModelsL)
      , dsattSubTransactions = stAnnSubTxs
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
  Map.Map ScriptHash (SupportedPlutusRunnable era) ->
  TxIx ->
  Tx SubTx era ->
  DijkstraStAnnTx SubTx era
mkDijkstraStAnnSubTx ei sysStart pp utxo scriptsProvided plutusScriptsCache txIx tx =
  let
    protVer = pp ^. ppProtocolVersionL
    scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    (_, plutusScriptsUsed) =
      resolveNeededPlutusScriptsWithPurpose protVer scriptsProvided scriptsNeeded plutusScriptsCache
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = ei
        , ltiSystemStart = sysStart
        , ltiUTxO = utxo
        , ltiTx = tx
        , ltiScriptsUsed = plutusScriptsUsed
        , ltiScriptHashesUsed = toScriptHashByPurpose plutusScriptsUsed
        , ltiLevelTxInfo = LedgerSubTxInfo txIx
        }
    txInfoResult = mkTxInfoResult ledgerTxInfo
   in
    DijkstraStAnnSubTx
      { dsastTx = tx
      , dsastScriptsNeeded = scriptsNeeded
      , dsastScriptsHashesNeeded = getScriptsHashesNeeded scriptsNeeded
      , dsastScriptsProvided = scriptsProvided
      , dsastTxInfoResult = txInfoResult
      , dsastPlutusLanguagesUsed =
          Set.fromList [plutusLanguage spr | (_, SupportedPlutusRunnable spr) <- plutusScriptsUsed]
      , dsastPlutusRunnableCache = plutusScriptsCache
      , dsastPlutusScriptsWithContext =
          scriptsWithContextFromLedgerTxInfoWithResult
            ledgerTxInfo
            txInfoResult
            (pp ^. ppCostModelsL)
      }

-- | Execution estimates indexed by body identity and body-local redeemer
-- pointer. 'SNothing' identifies the top-level body; 'SJust' contains a child's
-- transaction id, so identical pointers in distinct bodies remain distinct.
type DijkstraRedeemerReport era =
  Map.Map
    (StrictMaybe TxId, PlutusPurpose AsIx era)
    (Either (TransactionScriptFailure era) ExUnits)

type DijkstraRedeemerReportWithLogs era =
  Map.Map
    (StrictMaybe TxId, PlutusPurpose AsIx era)
    (Either (TransactionScriptFailure era) ([Text], ExUnits))

-- | Estimate every body in a Dijkstra batch using its actual context.
evalDijkstraTxExUnits ::
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  DijkstraRedeemerReport era
evalDijkstraTxExUnits pp tx utxo ei sysStart =
  Map.map (fmap snd) $ evalDijkstraTxExUnitsWithLogs pp tx utxo ei sysStart

-- | Batch execution estimates with logs. Witness/reference scripts are shared
-- exactly as in validation. Each child's index and the parent's Guarding child
-- views are supplied through the existing ledger context interface.
evalDijkstraTxExUnitsWithLogs ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  DijkstraRedeemerReportWithLogs era
evalDijkstraTxExUnitsWithLogs pp tx utxo ei sysStart =
  Map.unions $
    estimate SNothing topInfo
      : [estimate (SJust childId) childInfo | (childId, childInfo) <- childInfos]
  where
    protVer = pp ^. ppProtocolVersionL
    provided = getScriptsProvided utxo tx
    -- Reuse the existing resolver cache across all bodies in this estimation.
    (scriptsCache, _) =
      resolveNeededPlutusScriptsWithPurpose
        protVer
        provided
        (getScriptsNeeded utxo (tx ^. bodyTxL))
        mempty
    childInfos =
      [ (txIdTx child, mkInfo (LedgerSubTxInfo txIx) child)
      | (txIx, child) <- zip [TxIx 0 ..] (toList (tx ^. bodyTxL . subTransactionsTxBodyL))
      ]
    topInfo =
      mkInfo
        (LedgerTopTxInfo (Map.fromList [(childId, mkTxInfoResult info) | (childId, info) <- childInfos]))
        tx
    mkInfo :: forall level. LedgerLevelTxInfo level era -> Tx level era -> LedgerTxInfo level era
    mkInfo levelInfo bodyTx =
      let
        needed = getScriptsNeeded utxo (bodyTx ^. bodyTxL)
        (_, scriptsUsed) = resolveNeededPlutusScriptsWithPurpose protVer provided needed scriptsCache
       in
        LedgerTxInfo
          { ltiProtVer = protVer
          , ltiEpochInfo = ei
          , ltiSystemStart = sysStart
          , ltiUTxO = utxo
          , ltiTx = bodyTx
          , ltiScriptsUsed = scriptsUsed
          , ltiScriptHashesUsed = toScriptHashByPurpose scriptsUsed
          , ltiLevelTxInfo = levelInfo
          }
    estimate ::
      forall level. StrictMaybe TxId -> LedgerTxInfo level era -> DijkstraRedeemerReportWithLogs era
    estimate bodyId =
      Map.mapKeysMonotonic (\pointer -> (bodyId, pointer))
        . evalTxExUnitsWithLogsFromLedgerTxInfo pp provided
