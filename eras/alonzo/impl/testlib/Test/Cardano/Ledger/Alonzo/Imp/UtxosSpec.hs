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
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusTxInfo,
  LedgerTxInfo (..),
  toPlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (NoCostModel))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  natVersion,
 )
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (hashPlutusScript, withSLanguage)
import qualified Cardano.Ledger.Plutus.Language as L
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Slotting.Time (SystemStart (SystemStart))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Lens.Micro (to, (&), (.~), (<>~))
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
  , EraPlutusTxInfo L.PlutusV1 era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  it
    "transaction validity interval has closed upper bound when protocol version < 9 and open otherwise"
    $ do
      ei <- use $ impGlobalsL . to epochInfo
      ss@(SystemStart sysStart) <- use $ impGlobalsL . to systemStart
      SlotNo currentSlot <- use impLastTickG
      protVer <- getProtVer
      utxo <- getUTxO
      let txValidity = 7200
          -- We must provide a non-Nothing upper bound so that the "closed" vs "open" case can be tested.
          interval = ValidityInterval SNothing $ SJust $ SlotNo $ currentSlot + txValidity
          startPOSIX = floor $ utcTimeToPOSIXSeconds sysStart
          expectedUpperBound = (startPOSIX + fromIntegral (currentSlot + txValidity)) * 1000
          tx = mkBasicTx mkBasicTxBody & bodyTxL . vldtTxBodyL .~ interval
          lti =
            LedgerTxInfo
              { ltiProtVer = protVer
              , ltiEpochInfo = ei
              , ltiSystemStart = ss
              , ltiUTxO = utxo
              , ltiTx = tx
              }
      case toPlutusTxInfo (Proxy @L.PlutusV1) lti of
        Left e -> assertFailure $ "No translation error was expected, but got: " <> show e
        Right txInfo ->
          PV1.txInfoValidRange txInfo
            `shouldBe` PV1.Interval
              (PV1.LowerBound PV1.NegInf True)
              ( PV1.UpperBound
                  ( PV1.Finite
                      (PV1.POSIXTime expectedUpperBound)
                  )
                  (protVer < ProtVer (natVersion @9) 0) -- The upper bound.
              )

  forM_ (eraLanguages @era) $ \lang -> do
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
