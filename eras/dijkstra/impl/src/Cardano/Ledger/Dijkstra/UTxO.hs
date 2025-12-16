{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.UTxO (
  getDijkstraScriptsNeeded,
) where

import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsHashesNeeded,
  zipAsIxItem,
 )
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getBabbageSupplementalDataHashes,
 )
import Cardano.Ledger.BaseTypes (inject)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.UTxO (
  conwayConsumed,
  conwayProducedValue,
  getConwayMinFeeTxUtxo,
  getConwayScriptsNeeded,
  getConwayWitsVKeyNeeded,
 )
import Cardano.Ledger.Credential (Credential, credScriptHash)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraEraScript (..), pattern GuardingPurpose)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Mary.UTxO (burnedMultiAssets, getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Val (Val (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Lens.Micro ((^.))

dijkstraSubTxsProducedValue ::
  ( Foldable t
  , Value era ~ MaryValue
  , DijkstraEraTxBody era
  , EraTx era
  ) =>
  PParams era ->
  (KeyHash StakePool -> Bool) ->
  t (Tx SubTx era) ->
  Value era
dijkstraSubTxsProducedValue pp isStakePool = F.foldMap $ \subTx ->
  dijkstraSubTxProducedValue pp isStakePool (subTx ^. bodyTxL)

dijkstraProducedValue ::
  forall era.
  ( DijkstraEraTxBody era
  , EraTx era
  , Value era ~ MaryValue
  ) =>
  PParams era ->
  (KeyHash StakePool -> Bool) ->
  TxBody TopTx era ->
  Value era
dijkstraProducedValue pp isStakePool txBody =
  conwayProducedValue pp isStakePool txBody
    <> dijkstraSubTxsProducedValue pp isStakePool (txBody ^. subTransactionsTxBodyL)

getConsumedSubTxsValue ::
  ( Foldable t
  , DijkstraEraTxBody era
  , EraTx era
  , Value era ~ MaryValue
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  UTxO era ->
  t (Tx SubTx era) ->
  Value era
getConsumedSubTxsValue pp initialUtxo = fst . foldl' go (zero, initialUtxo)
  where
    go (val, utxo) subTx = first (val <>) $ getConsumedSubTxValueAndUTxO pp utxo subTx

getConsumedSubTxValueAndUTxO ::
  ( Value era ~ MaryValue
  , DijkstraEraTxBody era
  , EraTx era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  UTxO era ->
  Tx SubTx era ->
  (Value era, UTxO era)
getConsumedSubTxValueAndUTxO pp utxo tx =
  (getConsumedDijkstraValue pp undefined undefined utxo txBody, undefined)
  where
    txBody = tx ^. bodyTxL

getConsumedDijkstraValue ::
  ( DijkstraEraTxBody era
  , EraTx era
  , Value era ~ MaryValue
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  (Credential Staking -> Maybe Coin) ->
  (Credential DRepRole -> Maybe Coin) ->
  UTxO era ->
  TxBody l era ->
  Value era
getConsumedDijkstraValue pp lookupStakingDeposit lookupDRepDeposit utxo txBody =
  getConsumedMaryValue pp lookupStakingDeposit lookupDRepDeposit utxo txBody
    <> withBothTxLevels txBody topTxConsumedValue undefined
  where
    topTxConsumedValue topTx = getConsumedSubTxsValue pp utxo (topTx ^. subTransactionsTxBodyL)

instance EraUTxO DijkstraEra where
  type ScriptsNeeded DijkstraEra = AlonzoScriptsNeeded DijkstraEra

  consumed = conwayConsumed

  getConsumedValue = getConsumedDijkstraValue

  getProducedValue pp isRegPoolId txBody =
    withBothTxLevels
      txBody
      (dijkstraProducedValue pp isRegPoolId)
      (dijkstraSubTxProducedValue pp isRegPoolId)

  getScriptsProvided = getBabbageScriptsProvided

  getScriptsNeeded = getDijkstraScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

  getMinFeeTxUtxo = getConwayMinFeeTxUtxo

getDijkstraScriptsNeeded ::
  (DijkstraEraTxBody era, DijkstraEraScript era) =>
  UTxO era -> TxBody l era -> AlonzoScriptsNeeded era
getDijkstraScriptsNeeded utxo txb =
  getConwayScriptsNeeded utxo txb
    <> guardingScriptsNeeded
  where
    guardingScriptsNeeded = AlonzoScriptsNeeded $
      catMaybes $
        zipAsIxItem (txb ^. guardsTxBodyL) $
          \(AsIxItem idx cred) -> (\sh -> (GuardingPurpose (AsIxItem idx sh), sh)) <$> credScriptHash cred

instance AlonzoEraUTxO DijkstraEra where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum

dijkstraSubTxProducedValue ::
  (ConwayEraTxBody era, Value era ~ MaryValue) =>
  PParams era ->
  (KeyHash StakePool -> Bool) ->
  TxBody SubTx era ->
  Value era
dijkstraSubTxProducedValue pp isRegPoolId txBody =
  sumAllValue (txBody ^. outputsTxBodyL)
    <> inject (getTotalDepositsTxBody pp isRegPoolId txBody <> txBody ^. treasuryDonationTxBodyL)
    <> burnedMultiAssets txBody
