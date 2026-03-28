{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Examples (
  ledgerExamples,
  mkConwayBasedExampleTx,
  exampleConwayBasedTxBody,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ApplyTxError (ConwayApplyTxError), ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..), ConwayLEDGER)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (
  exampleBabbageBasedTxBody,
  exampleBabbageNewEpochState,
  mkBabbageBasedExampleTx,
 )
import Test.Cardano.Ledger.Conway.Era ()
import Test.Cardano.Ledger.Conway.Genesis (expectedConwayGenesis)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  exampleStakeKey,
  exampleStakePoolParams,
  mkKeyHash,
 )

ledgerExamples :: LedgerExamples ConwayEra
ledgerExamples =
  mkAlonzoBasedLedgerExamples
    ( ConwayApplyTxError $
        pure $
          wrapFailed @(ConwayDELEG ConwayEra) @(ConwayLEDGER ConwayEra) $
            DelegateeStakePoolNotRegisteredDELEG @ConwayEra (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    ( mkConwayBasedExampleTx
        (exampleConwayBasedTxBody exampleConwayCerts)
        (ConwaySpending $ AsIx 0)
    )
    exampleConwayGenesis

mkConwayBasedExampleTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraPlutusTxInfo 'PlutusV3 era
  ) =>
  TxBody TopTx era ->
  PlutusPurpose AsIx era ->
  Tx TopTx era
mkConwayBasedExampleTx txBody scriptPurpose =
  mkBabbageBasedExampleTx
    txBody
    scriptPurpose
    & witsTxL
      %~ ( <>
             ( mkBasicTxWits
                 & scriptTxWitsL
                   .~ Map.singleton
                     (hashScript @era $ alwaysSucceeds @'PlutusV3 3)
                     (alwaysSucceeds @'PlutusV3 3)
             )
         )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                %~ (<> Map.singleton PlutusV3 (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV3 3))))
        )

exampleConwayBasedTxBody ::
  forall era.
  ( ConwayEraTxBody era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , Value era ~ MaryValue
  ) =>
  StrictSeq.StrictSeq (TxCert era) ->
  TxBody TopTx era
exampleConwayBasedTxBody certs = mkConwayBasedExampleTxBody certs exampleBabbageBasedTxBody

mkConwayBasedExampleTxBody ::
  forall era.
  ( ConwayEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV3 era
  ) =>
  StrictSeq.StrictSeq (TxCert era) ->
  TxBody TopTx era ->
  TxBody TopTx era
mkConwayBasedExampleTxBody certs txBody =
  txBody
    & outputsTxBodyL
      %~ ( <>
             StrictSeq.fromList
               [ mkBasicTxOut
                   (mkAddr examplePayKey exampleStakeKey)
                   (exampleMultiAssetValue 2)
                   & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
                   & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV3 3)
               ]
         )
    & certsTxBodyL .~ certs
    & votingProceduresTxBodyL .~ VotingProcedures mempty
    & proposalProceduresTxBodyL .~ mempty
    & currentTreasuryValueTxBodyL .~ SJust (Coin 867530900000)
    & treasuryDonationTxBodyL .~ mempty

exampleConwayCerts :: StrictSeq.StrictSeq (ConwayTxCert era)
exampleConwayCerts =
  -- TODO add all possible certificates
  StrictSeq.fromList
    [ ConwayTxCertPool (RegPool exampleStakePoolParams)
    ]

exampleConwayGenesis :: ConwayGenesis
exampleConwayGenesis = expectedConwayGenesis
