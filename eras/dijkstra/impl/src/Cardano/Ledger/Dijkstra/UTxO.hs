{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.UTxO (
  DijkstraEraUTxO (..),
  dijkstraConsumed,
  getDijkstraScriptsNeeded,
  getDijkstraScriptsProvided,
  scriptsProvidedDijkstraStAnnTx,
  batchNonDistinctRefScriptsSize,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (CollectError)
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
  getConwayMinFeeTxUtxo,
  getConwayScriptsNeeded,
  getConwayWitsVKeyNeeded,
  txNonDistinctRefScriptsSize,
 )
import Cardano.Ledger.Credential (Credential, credScriptHash)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraEraScript (..), pattern GuardingPurpose)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.State.CertState (dijkstraBatchDepositsTxBody)
import Cardano.Ledger.Dijkstra.Tx (DijkstraStAnnTx (..))
import Cardano.Ledger.Mary.UTxO (burnedMultiAssets, getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus (Language, PlutusWithContext)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import qualified Data.OMap.Strict as OMap
import Data.Set (Set)
import Lens.Micro (SimpleGetter, to, (^.))
import Lens.Micro.Extras (view)

class AlonzoEraUTxO era => DijkstraEraUTxO era where
  subTransactionsStAnnTx :: StAnnTx TopTx era -> [StAnnTx SubTx era]
  plutusLegacyModeStAnnTxG :: SimpleGetter (StAnnTx TopTx era) Bool

-- | Unlike `shelleyConsumed`, this function does not need access to `Accounts` to produce accurate
-- information about refunds, hence is this simplification. Note that using `shelleyConsumed` in
-- Dijkstra era onwards will produce the same result as this one.
dijkstraConsumed ::
  EraUTxO era =>
  PParams era ->
  UTxO era ->
  TxBody l era ->
  Value era
dijkstraConsumed pp = getConsumedValue pp (const Nothing)

getConsumedDijkstraValue ::
  forall era l.
  ( DijkstraEraTxBody era
  , EraUTxO era
  , Value era ~ MaryValue
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  (Credential Staking -> Maybe Coin) ->
  UTxO era ->
  TxBody l era ->
  Value era
getConsumedDijkstraValue pp lookupStakingDeposit utxo txBody =
  withBothTxLevels
    txBody
    ( \topTxBody ->
        txBodyConsumedValue topTxBody <> subTransactionsConsumedValue topTxBody
    )
    txBodyConsumedValue
  where
    txBodyConsumedValue :: forall m. TxBody m era -> Value era
    txBodyConsumedValue = getConsumedMaryValue pp lookupStakingDeposit utxo
    subTransactionsConsumedValue topTxBody =
      foldMap'
        (getConsumedValue pp lookupStakingDeposit utxo . view bodyTxL)
        (topTxBody ^. subTransactionsTxBodyL)

dijkstraProducedValue ::
  forall era.
  ( DijkstraEraTxBody era
  , EraUTxO era
  , Value era ~ MaryValue
  ) =>
  PParams era ->
  (KeyHash StakePool -> Bool) ->
  TxBody TopTx era ->
  MaryValue
dijkstraProducedValue pp isRegPoolId topTxBody =
  commonProduced topTxBody
    <> foldMap' (commonProduced . (^. bodyTxL)) (topTxBody ^. subTransactionsTxBodyL)
    <> inject (topTxBody ^. feeTxBodyL)
    <> inject (dijkstraBatchDepositsTxBody pp isRegPoolId topTxBody) -- deposits produced by batch
  where
    -- the contribution produced identically across levels
    commonProduced :: TxBody l era -> MaryValue
    commonProduced txBody =
      sumAllValue (txBody ^. outputsTxBodyL)
        <> inject (txBody ^. treasuryDonationTxBodyL)
        <> burnedMultiAssets txBody

instance EraUTxO DijkstraEra where
  type ScriptsNeeded DijkstraEra = AlonzoScriptsNeeded DijkstraEra

  getConsumedValue = getConsumedDijkstraValue

  getProducedValue = dijkstraProducedValue

  getScriptsProvided = getDijkstraScriptsProvided

  getScriptsNeeded = getDijkstraScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

  getMinFeeTxUtxo = getConwayMinFeeTxUtxo

-- | Like 'getBabbageScriptsProvided', but for 'TopTx' also aggregates
-- scripts from all subtransactions.
getDijkstraScriptsProvided ::
  ( EraTx era
  , DijkstraEraTxBody era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  UTxO era ->
  Tx l era ->
  ScriptsProvided era
getDijkstraScriptsProvided utxo tx =
  withBothTxLevels
    tx
    ( \topTx ->
        ScriptsProvided $
          Map.unions $
            unScriptsProvided (getBabbageScriptsProvided utxo topTx)
              : [ unScriptsProvided (getBabbageScriptsProvided utxo subTx)
                | subTx <- OMap.elems (topTx ^. bodyTxL . subTransactionsTxBodyL)
                ]
    )
    (getBabbageScriptsProvided utxo)

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

  scriptsProvidedStAnnTx = scriptsProvidedDijkstraStAnnTx

  scriptsNeededStAnnTx = scriptsNeededDijkstraStAnnTx

  plutusScriptsWithContextStAnnTx = plutusScriptsWithContextDijkstraStAnnTx

  plutusLanguagesUsedStAnnTx = plutusLanguagesUsedDijkstraStAnnTx

scriptsProvidedDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era -> ScriptsProvided era
scriptsProvidedDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattScriptsProvided} -> dsattScriptsProvided)
    (\DijkstraStAnnSubTx {dsastScriptsProvided} -> dsastScriptsProvided)

scriptsNeededDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era -> ScriptsNeeded era
scriptsNeededDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattScriptsNeeded} -> dsattScriptsNeeded)
    (\DijkstraStAnnSubTx {dsastScriptsNeeded} -> dsastScriptsNeeded)

plutusScriptsWithContextDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era ->
  Either (NonEmpty (CollectError era)) [PlutusWithContext]
plutusScriptsWithContextDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattPlutusScriptsWithContext} -> dsattPlutusScriptsWithContext)
    (\DijkstraStAnnSubTx {dsastPlutusScriptsWithContext} -> dsastPlutusScriptsWithContext)

plutusLanguagesUsedDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era -> Set Language
plutusLanguagesUsedDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattPlutusLanguagesUsed} -> dsattPlutusLanguagesUsed)
    (\DijkstraStAnnSubTx {dsastPlutusLanguagesUsed} -> dsastPlutusLanguagesUsed)

instance DijkstraEraUTxO DijkstraEra where
  subTransactionsStAnnTx = subTransactionsDijkstraStAnnTx
  plutusLegacyModeStAnnTxG = to (\DijkstraStAnnTopTx {dsattPlutusLegacyMode} -> dsattPlutusLegacyMode)

subTransactionsDijkstraStAnnTx ::
  DijkstraStAnnTx TopTx era -> [DijkstraStAnnTx SubTx era]
subTransactionsDijkstraStAnnTx DijkstraStAnnTopTx {dsattSubTransactions} = dsattSubTransactions

-- | Total size of reference scripts across a top-level transaction and all its subtransactions.
batchNonDistinctRefScriptsSize ::
  ( EraTx era
  , DijkstraEraTxBody era
  ) =>
  UTxO era ->
  Tx TopTx era ->
  Int
batchNonDistinctRefScriptsSize utxo tx =
  txNonDistinctRefScriptsSize utxo tx
    + getSum
      ( foldMap'
          (Sum . txNonDistinctRefScriptsSize utxo)
          (tx ^. bodyTxL . subTransactionsTxBodyL)
      )
