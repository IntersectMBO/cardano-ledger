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
  exampleBabbageBasedTx,
  exampleBabbageBasedTopTx,
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
import qualified Data.MapExtras as Map
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  addAlonzoToConwayExampleReqSigners,
  exampleAlonzoBasedTopTx,
  exampleAlonzoBasedTx,
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  addShelleyBasedTopTxExampleFee,
  addShelleyToBabbageExampleProposedPUpdates,
  addShelleyToBabbageTxCerts,
  addShelleyToConwayTxCerts,
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
  where
    exampleBabbageTx =
      exampleBabbageBasedTopTx
        & addShelleyBasedTopTxExampleFee
        & addShelleyToBabbageExampleProposedPUpdates
        & addShelleyToBabbageTxCerts
        & addShelleyToConwayTxCerts
        & addAlonzoToConwayExampleReqSigners

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

exampleBabbageBasedTopTx ::
  forall era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  ) =>
  Tx TopTx era
exampleBabbageBasedTopTx =
  exampleAlonzoBasedTopTx
    & addBabbageBasedTxFeatures
    & addBabbageBasedTopTxFeatures

exampleBabbageBasedTx ::
  forall era l.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , Typeable l
  ) =>
  Tx l era
exampleBabbageBasedTx =
  exampleAlonzoBasedTx
    & addBabbageBasedTxFeatures

addBabbageBasedTopTxFeatures ::
  forall era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addBabbageBasedTopTxFeatures tx =
  tx
    & bodyTxL . collateralReturnTxBodyL .~ SJust exampleCollateralOutput
    & bodyTxL . totalCollateralTxBodyL .~ SJust (Coin 8675309)

addBabbageBasedTxFeatures ::
  forall era l.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  ) =>
  Tx l era ->
  Tx l era
addBabbageBasedTxFeatures tx =
  tx
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL <>~ Map.fromElems hashScript [alwaysSucceeds @'PlutusV2 3]
          )
    & modifyTxAuxData
      ( plutusScriptsTxAuxDataL
          %~ Map.insertWith
            (<>)
            PlutusV2
            (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV2 3)))
      )
    & bodyTxL . referenceInputsTxBodyL <>~ exampleTxIns
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

exampleCollateralOutput ::
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  ) =>
  TxOut era
exampleCollateralOutput =
  mkBasicTxOut
    (mkAddr examplePayKey exampleStakeKey)
    (MaryValue (Coin 8675309) mempty)
