{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Babbage.Examples (
  ledgerExamples,
  exampleBabbageNewEpochState,
  exampleBabbageTx,
  exampleBabbageBasedTx,
  exampleBabbageToConwayTx,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
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
  exampleAlonzoBasedTx,
  exampleAlonzoToBabbageTx,
  exampleAlonzoToConwayTx,
  exampleDatum,
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
  exampleShelleyScript,
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
    exampleBabbageTx
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

-- Complete transaction which is compatible with any era starting with Babbage.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleBabbageBasedTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  , BabbageEraTxBody era
  ) =>
  Tx TopTx era
exampleBabbageBasedTx =
  addBabbageBasedTxFeatures exampleAlonzoBasedTx

-- Complete Babbage transaction that is only compatible until Babbage era
-- ('ConwayEra' is not an instance of 'ShelleyEraTxBody').
exampleBabbageTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  , ShelleyEraTxBody era
  , BabbageEraTxBody era
  ) =>
  Tx TopTx era
exampleBabbageTx =
  addBabbageBasedTxFeatures exampleAlonzoToBabbageTx

-- Complete Babbage transaction that is compatible until Conway era
-- ('DijkstraEra' is not an instance of 'ShelleyEraTxCert').
exampleBabbageToConwayTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , Value era ~ MaryValue
  , BabbageEraTxBody era
  , ShelleyEraTxCert era
  ) =>
  Tx TopTx era
exampleBabbageToConwayTx =
  addBabbageBasedTxFeatures exampleAlonzoToConwayTx

addBabbageBasedTxFeatures ::
  forall era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addBabbageBasedTxFeatures tx =
  tx
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
    & bodyTxL . referenceInputsTxBodyL .~ exampleTxIns
    & bodyTxL . outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 1)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV1 3)
        , mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV2 3)
        , mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 3)
            & referenceScriptTxOutL .~ SJust (fromNativeScript exampleShelleyScript)
        ]
    & bodyTxL . collateralReturnTxBodyL .~ SJust exampleCollateralOutput
    & bodyTxL . totalCollateralTxBodyL .~ SJust (Coin 8675309)

exampleCollateralOutput ::
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  ) =>
  TxOut era
exampleCollateralOutput =
  mkBasicTxOut
    (mkAddr examplePayKey exampleStakeKey)
    (MaryValue (Coin 8675309) mempty)
