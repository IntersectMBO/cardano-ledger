{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.Examples (
  ledgerExamples,
  mkBabbageBasedExampleTx,
  exampleBabbageBasedTxBody,
  exampleBabbageNewEpochState,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Babbage (ApplyTxError (BabbageApplyTxError), BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure (DelegateeNotRegisteredDELEG),
  ShelleyDelegsPredFailure (DelplFailure),
  ShelleyDelplPredFailure (DelegFailure),
  ShelleyLedgerPredFailure (DelegsFailure),
 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleAlonzoBasedShelleyTxBody,
  exampleAlonzoBasedTxBody,
  exampleDatum,
  mkAlonzoBasedExampleTx,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  exampleNewEpochState,
  examplePayKey,
  exampleStakeKey,
  exampleTxIns,
  mkKeyHash,
 )

ledgerExamples :: LedgerExamples BabbageEra
ledgerExamples =
  mkAlonzoBasedLedgerExamples
    ( BabbageApplyTxError $
        pure $
          DelegsFailure $
            DelplFailure $
              DelegFailure $
                DelegateeNotRegisteredDELEG (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    ( mkBabbageBasedExampleTx
        exampleBabbageBasedShelleyTxBody
        (AlonzoSpending $ AsIx 0)
    )
    NoGenesis

exampleBabbageNewEpochState ::
  ( BabbageEraTest era
  , Value era ~ MaryValue
  ) =>
  NewEpochState era
exampleBabbageNewEpochState =
  exampleNewEpochState
    (exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 1))

mkBabbageBasedExampleTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  ) =>
  TxBody TopTx era ->
  PlutusPurpose AsIx era ->
  Tx TopTx era
mkBabbageBasedExampleTx txBody scriptPurpose =
  mkAlonzoBasedExampleTx txBody scriptPurpose
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL
                .~ Map.singleton
                  (hashScript @era $ alwaysSucceeds @'PlutusV2 3)
                  (alwaysSucceeds @'PlutusV2 3)
          )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                <>~ Map.singleton PlutusV2 (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV2 3)))
        )

exampleBabbageBasedShelleyTxBody ::
  forall era.
  ( BabbageEraTxBody era
  , ShelleyEraTxBody era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era
exampleBabbageBasedShelleyTxBody =
  mkBabbageBasedExampleTxBody exampleAlonzoBasedShelleyTxBody

exampleBabbageBasedTxBody ::
  forall era.
  ( BabbageEraTxBody era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era
exampleBabbageBasedTxBody =
  mkBabbageBasedExampleTxBody exampleAlonzoBasedTxBody

mkBabbageBasedExampleTxBody ::
  forall era.
  ( BabbageEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  ) =>
  TxBody TopTx era ->
  TxBody TopTx era
mkBabbageBasedExampleTxBody txBody =
  txBody
    & referenceInputsTxBodyL .~ exampleTxIns
    & outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum) -- inline datum
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV1 3)
        , mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum) -- inline datum
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV2 3)
        ]
    & collateralReturnTxBodyL .~ SJust exampleCollateralOutput
    & totalCollateralTxBodyL .~ SJust (Coin 8675309)

exampleCollateralOutput ::
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  ) =>
  TxOut era
exampleCollateralOutput =
  mkBasicTxOut
    (mkAddr examplePayKey exampleStakeKey)
    (MaryValue (Coin 8675309) mempty)
