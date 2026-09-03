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
  getDijkstraWitsVKeyNeeded,
  voterWitnessesExcept,
  scriptsProvidedDijkstraStAnnTx,
  batchNonDistinctRefScriptsSize,
  localProducedValue,
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
import Cardano.Ledger.Conway.Governance (Voter (..), unVotingProcedures)
import Cardano.Ledger.Conway.TxBody (conwayProposalsDeposits)
import Cardano.Ledger.Conway.UTxO (
  getConwayMinFeeTxUtxo,
  getConwayScriptsNeeded,
  getConwayWitsVKeyNeeded,
  txNonDistinctRefScriptsSize,
 )
import Cardano.Ledger.Credential (Credential, credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraEraScript (..), pattern GuardingPurpose)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.Tx (DijkstraStAnnTx (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.UTxO (burnedMultiAssets, getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus (Language, PlutusWithContext)
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeededNoGov)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import qualified Data.OMap.Strict as OMap
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (SimpleGetter, to, (^.))
import Lens.Micro.Extras (view)

class AlonzoEraUTxO era => DijkstraEraUTxO era where
  subTransactionsStAnnTx :: StAnnTx TopTx era -> [StAnnTx SubTx era]
  plutusLegacyModeStAnnTxG :: SimpleGetter (StAnnTx TopTx era) Bool
  scriptsHashesNeededStAnnTx :: StAnnTx SubTx era -> Set ScriptHash

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
  localProducedValue pp topTxBody
    <> foldMap' (localProducedValue pp . (^. bodyTxL)) subTxs
    <> inject (topTxBody ^. feeTxBodyL)
    <> inject (getTotalDepositsTxCerts pp isRegPoolId batchTxCerts)
  where
    -- add all values that are produced by both top and sub-transactions
    -- Certs are excluded, since they need to be processed separately
    -- while maintaining the state through all of the certs of a transactions.
    batchTxCerts =
      foldMap' (^. bodyTxL . certsTxBodyL) subTxs
        <> (topTxBody ^. certsTxBodyL)
    subTxs = topTxBody ^. subTransactionsTxBodyL

-- | Produced value that is local to a single transaction body, that is, the part
-- that can be summed independently for each body in a batch.
-- Excludes fees and certificate deposits, which are accounted for once per
-- batch.
localProducedValue ::
  ( DijkstraEraTxBody era
  , Value era ~ MaryValue
  ) =>
  PParams era ->
  TxBody l era ->
  MaryValue
localProducedValue pp txBody =
  sumAllValue (txBody ^. outputsTxBodyL)
    <> inject (txBody ^. treasuryDonationTxBodyL)
    <> inject (conwayProposalsDeposits pp txBody)
    <> burnedMultiAssets txBody
    <> inject (fold (unDirectDeposits (txBody ^. directDepositsTxBodyL)))

instance EraUTxO DijkstraEra where
  type ScriptsNeeded DijkstraEra = AlonzoScriptsNeeded DijkstraEra

  getConsumedValue = getConsumedDijkstraValue

  getProducedValue = dijkstraProducedValue

  getScriptsProvided = getDijkstraScriptsProvided

  getScriptsNeeded = getDijkstraScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

  getMinFeeTxUtxo = getConwayMinFeeTxUtxo

-- | Like 'getConwayWitsVKeyNeeded', except that SPO votes covered by a
-- pool-vote witness contribute no cold-key hash: they are authorized by the
-- pool's registered voting key instead, checked in the UTXOW rule. The waiver
-- is applied at the vote's contribution and never by subtraction, so a pool
-- cold key that is also needed for other reasons stays required.
getDijkstraWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  Set (KeyHash StakePool) ->
  UTxO era ->
  TxBody l era ->
  Set (KeyHash Witness)
getDijkstraWitsVKeyNeeded blsCovered utxo txBody =
  getShelleyWitsVKeyNeededNoGov utxo txBody
    `Set.union` Set.map asWitness (txBody ^. reqSignerHashesTxBodyG)
    `Set.union` voterWitnessesExcept blsCovered txBody

voterWitnessesExcept ::
  ConwayEraTxBody era =>
  Set (KeyHash StakePool) ->
  TxBody l era ->
  Set (KeyHash Witness)
voterWitnessesExcept blsCovered txb =
  Map.foldrWithKey' accum mempty (unVotingProcedures (txb ^. votingProceduresTxBodyL))
  where
    accum voter _ khs =
      maybe khs (`Set.insert` khs) $
        case voter of
          CommitteeVoter cred -> credKeyHashWitness cred
          DRepVoter cred -> credKeyHashWitness cred
          StakePoolVoter poolId
            | poolId `Set.member` blsCovered -> Nothing
            | otherwise -> Just $ asWitness poolId

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
  scriptsHashesNeededStAnnTx = dsastScriptsHashesNeeded

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
