{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.Examples (
  ledgerExamples,
  mkDijkstraBasedExampleTx,
  mkDijkstraBasedExampleTxBody,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.BaseTypes (Exclusive (..), Inclusive (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (ApplyTxError (..), DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules (DijkstraLEDGER, DijkstraMEMPOOL)
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraPlutusPurpose (..),
 )
import Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody,
  accountBalanceIntervalsTxBodyL,
  directDepositsTxBodyL,
  guardsTxBodyL,
  subTransactionsTxBodyL,
 )
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro ((%~), (&), (.~))
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState)
import Test.Cardano.Ledger.Conway.Examples (exampleConwayBasedTxBody, mkConwayBasedExampleTx)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Dijkstra.ImpTest (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  exampleStakeKey,
  exampleStakePoolParams,
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
    exampleTxDijkstra
    exampleDijkstraGenesis

exampleTxDijkstra :: Tx TopTx DijkstraEra
exampleTxDijkstra =
  mkDijkstraBasedExampleTx
    (mkDijkstraBasedExampleTxBody $ exampleConwayBasedTxBody exampleDijkstraCerts)
    (DijkstraSpending $ AsIx 0)

-- | Reusable Tx builder for Dijkstra onwards with PlutusV4 script witness.
mkDijkstraBasedExampleTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraPlutusTxInfo 'PlutusV3 era
  , EraPlutusTxInfo 'PlutusV4 era
  ) =>
  TxBody TopTx era ->
  PlutusPurpose AsIx era ->
  Tx TopTx era
mkDijkstraBasedExampleTx txBody scriptPurpose =
  mkConwayBasedExampleTx txBody scriptPurpose
    & witsTxL
      %~ ( <>
             ( mkBasicTxWits
                 & scriptTxWitsL
                   .~ Map.singleton
                     (hashScript @era $ alwaysSucceeds @'PlutusV4 3)
                     (alwaysSucceeds @'PlutusV4 3)
             )
         )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                %~ (<> Map.singleton PlutusV4 (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV4 3))))
        )

mkDijkstraBasedExampleTxBody ::
  forall era.
  ( DijkstraEraTxBody era
  , EraTx era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV4 era
  ) =>
  TxBody TopTx era ->
  TxBody TopTx era
mkDijkstraBasedExampleTxBody txBody =
  txBody
    & outputsTxBodyL
      %~ ( <>
             StrictSeq.fromList
               [ mkBasicTxOut
                   (mkAddr examplePayKey exampleStakeKey)
                   (exampleMultiAssetValue 2)
                   & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
                   & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV4 3)
               ]
         )
    & guardsTxBodyL .~ OSet.fromList [KeyHashObj $ mkKeyHash 212, ScriptHashObj $ mkScriptHash 213]
    & subTransactionsTxBodyL .~ mempty -- Sub-transactions require complex recursive setup
    & directDepositsTxBodyL .~ exampleDirectDeposits
    & accountBalanceIntervalsTxBodyL .~ exampleAccountBalanceIntervals

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

exampleDijkstraCerts :: StrictSeq.StrictSeq (DijkstraTxCert era)
exampleDijkstraCerts =
  -- TODO should I add the new certs here?
  StrictSeq.fromList
    [ DijkstraTxCertPool (RegPool exampleStakePoolParams)
    ]
