{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (NoCostModel))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (<>~))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsWithDatum,
  alwaysSucceedsWithDatum,
  datumIsWellformed,
  inputsOutputsAreNotEmptyWithDatum,
  purposeIsWellformedWithDatum,
  redeemerSameAsDatum,
 )

spec ::
  forall era.
  ( AlonzoEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOS" $
  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let redeemerSameAsDatumHash = hashPlutusScript $ redeemerSameAsDatum slang
            alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang

        let scripts =
              [ ("redeemerSameAsDatum", redeemerSameAsDatum)
              , ("purposeIsWellformedWithDatum", purposeIsWellformedWithDatum)
              , ("datumIsWellformed", datumIsWellformed)
              , ("inputsOutputsAreNotEmptyWithDatum", inputsOutputsAreNotEmptyWithDatum)
              ]

        describe "Spending scripts with a Datum" $ do
          forM_ scripts $ \(name, script) -> do
            it name $ do
              let sHash = hashPlutusScript (script slang)
              txIn0 <- produceScript sHash
              submitTxAnn_ "Submit a transaction that consumes the script output" $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL
                    .~ Set.singleton txIn0
              passEpoch

        it "Valid transaction marked as invalid" $ do
          let tx = mkBasicTx mkBasicTxBody & isValidTxL .~ IsValid False
          submitFailingTx tx [injectFailure (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]

        it "Invalid transaction marked as valid" $ do
          txIn <- produceScript . hashPlutusScript $ alwaysFailsWithDatum slang
          expectPhase2Invalid $ mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

        it "Invalid plutus script fails in phase 2" $ do
          txIn0 <- produceScript redeemerSameAsDatumHash
          exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
          submitTxAnn_ "Submitting consuming transaction" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0
              & isValidTxL .~ IsValid False
              & witsTxL . rdmrsTxWitsL
                .~ Redeemers
                  ( Map.singleton
                      (mkSpendingPurpose $ AsIx 0)
                      (Data $ P.I 32, exUnits)
                  )

        describe "Scripts pass in phase 2" $ do
          let scripts' = drop 1 scripts
          forM_ scripts' $ \(name, script) -> do
            it name $ do
              let sHash = hashPlutusScript (script slang)
              txIn0 <- produceScript sHash
              submitTxAnn_ "Submitting consuming transaction" $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0

        it "No cost model" $ do
          txIn <- produceScript alwaysSucceedsWithDatumHash
          let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL <>~ [txIn]
          modifyPParams $ ppCostModelsL .~ mempty
          submitFailingTx
            tx
            [injectFailure (CollectErrors [NoCostModel lang])]
