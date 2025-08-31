{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest (
  babbageFixupTx,
  impBabbageExpectTxSuccess,
  module Test.Cardano.Ledger.Alonzo.ImpTest,
  produceRefScript,
  produceRefScripts,
  produceRefScriptsTx,
  mkTxWithRefInputs,
  submitTxWithRefInputs,
) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Collateral (collOuts)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (
  UTxO (..),
  curPParamsEpochStateL,
  nesEsL,
  utxoL,
 )
import Cardano.Ledger.Tools (ensureMinCoinTxOut, setMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import Control.Monad (forM, (>=>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.Era ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Plutus (testingCostModels)

instance ShelleyEraImp BabbageEra where
  initNewEpochState =
    defaultInitNewEpochState
      (nesEsL . curPParamsEpochStateL . ppCostModelsL <>~ testingCostModels [PlutusV2])
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess
  modifyImpInitProtVer = shelleyModifyImpInitProtVer
  genRegTxCert = shelleyGenRegTxCert
  genUnRegTxCert = shelleyGenUnRegTxCert

babbageFixupTx ::
  ( HasCallStack
  , AlonzoEraImp era
  , BabbageEraTxBody era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
babbageFixupTx =
  addNativeScriptTxWits
    >=> fixupAuxDataHash
    >=> addCollateralInput
    >=> addRootTxIn
    >=> fixupScriptWits
    >=> fixupOutputDatums
    >=> fixupDatums
    >=> fixupRedeemerIndices
    >=> fixupTxOuts
    >=> fixupCollateralReturn
    >=> alonzoFixupFees
    >=> fixupRedeemers
    >=> fixupPPHash
    >=> updateAddrTxWits

fixupCollateralReturn ::
  ( ShelleyEraImp era
  , BabbageEraTxBody era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupCollateralReturn tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  pure $ tx & bodyTxL . collateralReturnTxBodyL %~ fmap (ensureMinCoinTxOut pp)

impBabbageExpectTxSuccess ::
  ( HasCallStack
  , AlonzoEraImp era
  , BabbageEraTxBody era
  ) =>
  Tx era -> ImpTestM era ()
impBabbageExpectTxSuccess tx = do
  impAlonzoExpectTxSuccess tx
  -- Check that the balance of the collateral was returned
  let returns = Map.toList . unUTxO . collOuts $ tx ^. bodyTxL
  utxo <- getsNES utxoL
  if tx ^. isValidTxL == IsValid True
    then do
      impAnn "Collateral return should not be in UTxO" $
        expectUTxOContent utxo [(txIn, isNothing) | (txIn, _txOut) <- returns]
    else do
      impAnn "Collateral return should be in UTxO" $
        expectUTxOContent utxo [(txIn, (== Just txOut)) | (txIn, txOut) <- returns]

instance MaryEraImp BabbageEra

instance AlonzoEraImp BabbageEra where
  scriptTestContexts = plutusTestScripts SPlutusV1 <> plutusTestScripts SPlutusV2

produceRefScript ::
  (ShelleyEraImp era, BabbageEraTxOut era) =>
  Script era ->
  ImpTestM era TxIn
produceRefScript script = do
  txIn :| [] <- produceRefScripts $ script :| []
  pure txIn

produceRefScripts ::
  (ShelleyEraImp era, BabbageEraTxOut era) =>
  NonEmpty (Script era) ->
  ImpTestM era (NonEmpty TxIn)
produceRefScripts scripts = do
  txId <- txIdTx <$> produceRefScriptsTx scripts
  pure $ NE.zipWith (\_ -> mkTxInPartial txId) scripts (0 :| [1 ..])

produceRefScriptsTx ::
  (ShelleyEraImp era, BabbageEraTxOut era) =>
  NonEmpty (Script era) ->
  ImpTestM era (Tx era)
produceRefScriptsTx scripts = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  txOuts <- forM scripts $ \script -> do
    addr <- freshKeyAddr_
    let txOutZero =
          mkBasicTxOut addr mempty & referenceScriptTxOutL .~ SJust script
    pure $ setMinCoinTxOut pp txOutZero
  let txBody = mkBasicTxBody & outputsTxBodyL .~ SSeq.fromList (NE.toList txOuts)
  submitTx (mkBasicTx txBody)

mkTxWithRefInputs ::
  (ShelleyEraImp era, BabbageEraTxBody era) =>
  TxIn ->
  NonEmpty TxIn ->
  Tx era
mkTxWithRefInputs txIn refIns =
  mkBasicTx $
    mkBasicTxBody
      & referenceInputsTxBodyL .~ Set.fromList (NE.toList refIns)
      & inputsTxBodyL .~ [txIn]

submitTxWithRefInputs ::
  (ShelleyEraImp era, BabbageEraTxBody era) =>
  TxIn ->
  NonEmpty TxIn ->
  ImpTestM era (Tx era)
submitTxWithRefInputs txIn refIns = submitTx $ mkTxWithRefInputs txIn refIns
