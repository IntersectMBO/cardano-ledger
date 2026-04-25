{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Dijkstra.Examples (
  ledgerExamples,
  exampleDijkstraBasedTx,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (
  Exclusive (..),
  Inclusive (..),
  Network (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (ApplyTxError (..), DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules (DijkstraLEDGER, DijkstraMEMPOOL)
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraEraScript,
  pattern GuardingPurpose,
 )
import Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody,
  accountBalanceIntervalsTxBodyL,
  directDepositsTxBodyL,
  guardsTxBodyL,
  requiredTopLevelGuardsL,
  subTransactionsTxBodyL,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro ((%~), (&), (.~), (<>~), (^.))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState)
import Test.Cardano.Ledger.Conway.Examples (exampleConwayBasedTx)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Dijkstra.ImpTest (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  exampleStakeKey,
  exampleTxIns,
  mkKeyHash,
  mkScriptHash,
 )

ledgerExamples :: LedgerExamples DijkstraEra
ledgerExamples =
  mkAlonzoBasedLedgerExamples
    ( DijkstraApplyTxError $
        pure $
          wrapFailed @(DijkstraLEDGER DijkstraEra) @(DijkstraMEMPOOL DijkstraEra) $
            wrapFailed @(ConwayDELEG DijkstraEra) @(DijkstraLEDGER DijkstraEra) $
              DelegateeStakePoolNotRegisteredDELEG (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleDijkstraBasedTx
    exampleDijkstraGenesis

-- Complete transaction which is compatible with any era starting with Dijkstra.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleDijkstraBasedTx ::
  forall era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , Value era ~ MaryValue
  , DijkstraEraScript era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV4 era
  , EraPlutusTxInfo PlutusV3 era
  , AlonzoEraTxAuxData era
  ) =>
  Tx TopTx era
exampleDijkstraBasedTx =
  addDijkstraBasedTxFeatures exampleConwayBasedTx

addDijkstraBasedTxFeatures ::
  forall era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , AlonzoEraTxAuxData era
  , DijkstraEraScript era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV4 era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addDijkstraBasedTxFeatures tx =
  tx
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL
                .~ Map.singleton
                  (hashScript @era $ alwaysSucceeds @'PlutusV4 3)
                  (alwaysSucceeds @'PlutusV4 3)
              & rdmrsTxWitsL <>~ redeemers
          )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                <>~ Map.singleton PlutusV4 (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV4 3)))
        )
    & bodyTxL . outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV4 3)
        ]
    & bodyTxL . guardsTxBodyL
      .~ OSet.fromList
        [ KeyHashObj $ mkKeyHash 211
        , KeyHashObj $ mkKeyHash 212
        , ScriptHashObj $ mkScriptHash 213
        ]
    & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable [subTx]
    & bodyTxL . directDepositsTxBodyL .~ exampleDirectDeposits
    & bodyTxL . accountBalanceIntervalsTxBodyL .~ exampleAccountBalanceIntervals
  where
    redeemers =
      Redeemers $
        Map.fromList
          [ (GuardingPurpose $ AsIx 3, (redeemerData, ExUnits 5000 5000))
          ]
    redeemerData = Data @era (P.Constr 1 [P.List [P.I 1], P.Map [(P.I 2, P.B "2")]])
    subTx =
      mkBasicTx @era subTxBody
        & witsTxL
          <>~ ( mkBasicTxWits
                  & scriptTxWitsL
                    .~ Map.singleton
                      (hashScript @era $ alwaysSucceeds @'PlutusV4 3)
                      (alwaysSucceeds @'PlutusV4 3)
              )
        & auxDataTxL
          %~ fmap
            ( \auxData ->
                auxData
                  & plutusScriptsTxAuxDataL
                    <>~ Map.singleton PlutusV4 (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV4 3)))
            )
    txBody = tx ^. bodyTxL
    subTxBody =
      mkBasicTxBody
        & inputsTxBodyL .~ exampleTxIns
        & referenceInputsTxBodyL .~ exampleTxIns
        & outputsTxBodyL
          <>~ StrictSeq.fromList
            [ mkBasicTxOut
                (mkAddr examplePayKey exampleStakeKey)
                (exampleMultiAssetValue 2)
                & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
                & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV4 3)
            ]
        & vldtTxBodyL .~ txBody ^. vldtTxBodyL
        & mintTxBodyL .~ txBody ^. mintTxBodyL
        & withdrawalsTxBodyL .~ txBody ^. withdrawalsTxBodyL
        & auxDataHashTxBodyL .~ txBody ^. auxDataHashTxBodyL
        & scriptIntegrityHashTxBodyL .~ txBody ^. scriptIntegrityHashTxBodyL
        & networkIdTxBodyL .~ txBody ^. networkIdTxBodyL
        & certsTxBodyL .~ txBody ^. certsTxBodyL
        & votingProceduresTxBodyL .~ txBody ^. votingProceduresTxBodyL
        & proposalProceduresTxBodyL .~ txBody ^. proposalProceduresTxBodyL
        & currentTreasuryValueTxBodyL .~ txBody ^. currentTreasuryValueTxBodyL
        & treasuryDonationTxBodyL .~ txBody ^. treasuryDonationTxBodyL
        & requiredTopLevelGuardsL
          .~ Map.fromList
            [ (KeyHashObj $ mkKeyHash 212, SNothing)
            , (ScriptHashObj $ mkScriptHash 213, SJust $ exampleDatum @era)
            ]

exampleDirectDeposits :: DirectDeposits
exampleDirectDeposits =
  DirectDeposits $
    Map.singleton
      (AccountAddress Mainnet (AccountId $ KeyHashObj $ mkKeyHash 300))
      (Coin 1000000)

exampleAccountBalanceIntervals :: AccountBalanceIntervals era
exampleAccountBalanceIntervals =
  AccountBalanceIntervals $
    Map.fromList
      [ (AccountId $ KeyHashObj $ mkKeyHash 400, AccountBalanceLowerBound (Inclusive $ Coin 500))
      , (AccountId $ KeyHashObj $ mkKeyHash 401, AccountBalanceUpperBound (Exclusive $ Coin 10000))
      ,
        ( AccountId $ ScriptHashObj $ mkScriptHash 402
        , AccountBalanceBothBounds (Inclusive $ Coin 100) (Exclusive $ Coin 5000)
        )
      ]
