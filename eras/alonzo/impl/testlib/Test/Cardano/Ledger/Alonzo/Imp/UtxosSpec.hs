{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  CollectError (NoCostModel),
  TransactionScriptFailure (RedeemerPointsToUnknownScriptHash),
  evalTxExUnits,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Globals (..), StrictMaybe (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Control.Monad.Reader (asks)
import Data.Either (isLeft)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro (set, (%~), (&), (.~), (<>~), (^.), _2)
import Lens.Micro.Mtl (use)
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V1 as PV1
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
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $
  forM_ (eraLanguages @era) $ \lang ->
    describe (show lang) $
      withSLanguage lang $ \slang -> do
        let redeemerSameAsDatumHash = hashPlutusScript $ redeemerSameAsDatum slang
            alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang

        let scripts =
              [ ("redeemerSameAsDatum", redeemerSameAsDatum)
              , ("purposeIsWellformedWithDatum", purposeIsWellformedWithDatum)
              , ("datumIsWellformed", datumIsWellformed)
              , ("inputsOutputsAreNotEmptyWithDatum", inputsOutputsAreNotEmptyWithDatum)
              ]

        describe "ExUnits" $ do
          it "Calculate ExUnits" $ do
            let
              overrideExUnits tx = do
                pp <- getsNES $ nesEsL . curPParamsEpochStateL
                utxo <- getUTxO
                Globals {epochInfo, systemStart} <- use impGlobalsL
                purposeUnits <-
                  either (fail . show) pure . sequence $
                    evalTxExUnits pp tx utxo epochInfo systemStart
                pure $ tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ spliceUnits purposeUnits
              spliceUnits =
                Map.merge
                  Map.dropMissing -- Ignore purposes not already in the redeemers
                  Map.preserveMissing -- Don't touch purposes not being updated
                  (Map.zipWithMatched $ \_ -> set _2) -- Replace the units, keep the datum
              redoAddrWits = updateAddrTxWits . (witsTxL . addrTxWitsL .~ mempty)

            txIn <- produceScript alwaysSucceedsWithDatumHash
            withPostFixup (overrideExUnits >=> fixupPPHash >=> redoAddrWits) $
              submitTx_ $
                mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

          it "Attempt to calculate ExUnits with an invalid tx" $ do
            txIn <- produceScript alwaysSucceedsWithDatumHash
            let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

            txFixed <- (tx &) =<< asks iteFixup
            logToExpr txFixed

            let
              twiddleIx (SJust (SpendingPurpose (AsIx 0))) = SpendingPurpose (AsIx 1)
              twiddleIx _ = SpendingPurpose (AsIx 0)
              badPurpose =
                twiddleIx $
                  redeemerPointer (txFixed ^. bodyTxL) (SpendingPurpose $ AsItem txIn)
              du = (Data $ PV1.I 42, ExUnits 5000 5000)
              txBorked =
                txFixed
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert badPurpose du
            logToExpr txBorked

            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            utxo <- getUTxO
            Globals {epochInfo, systemStart} <- use impGlobalsL
            let report = evalTxExUnits pp txBorked utxo epochInfo systemStart
            logToExpr report

            Map.filter isLeft report
              `shouldBe` Map.singleton badPurpose (Left (RedeemerPointsToUnknownScriptHash badPurpose))

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
          submitPhase2Invalid_ $ mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

        it "Invalid plutus script fails in phase 2" $ do
          txIn0 <- produceScript redeemerSameAsDatumHash
          exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
          submitTxAnn_ "Submitting consuming transaction" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0
              & isValidTxL .~ IsValid False
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton (mkSpendingPurpose $ AsIx 0) (Data $ P.I 32, exUnits)

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
