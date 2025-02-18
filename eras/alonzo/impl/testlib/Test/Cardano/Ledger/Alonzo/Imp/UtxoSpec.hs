{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxAuxData (mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (Mismatch (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..), toDeltaCoin)
import qualified Cardano.Ledger.Metadata as M
import Cardano.Ledger.Plutus (Data (..), ExUnits (..), hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Lens.Micro (to, (&), (.~), (<>~), (^.))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

spec ::
  forall era.
  ( AlonzoEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  it "Wrong network ID" $ do
    submitFailingTx
      (mkBasicTx mkBasicTxBody & bodyTxL . networkIdTxBodyL .~ SJust Mainnet)
      [ injectFailure $
          WrongNetworkInTxBody Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
      ]

  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        it "Too many execution units for tx" $ do
          txIn <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum slang
          maxExUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
          let
            txExUnits = maxExUnits <> ExUnits 1 1
            prp = mkSpendingPurpose (AsIx 0)
            dat = Data $ P.I 32
            tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [txIn]
                & witsTxL . rdmrsTxWitsL <>~ Redeemers (Map.singleton prp (dat, txExUnits))
          submitFailingTx
            tx
            [ injectFailure $
                ExUnitsTooBigUTxO Mismatch {mismatchSupplied = txExUnits, mismatchExpected = maxExUnits}
            ]

        it "Insufficient collateral" $ do
          scriptInput <- produceScript $ hashPlutusScript $ alwaysSucceedsWithDatum slang
          collateralAddr <- freshKeyAddr_
          collateralInput <- sendCoinTo collateralAddr mempty -- 0 will be changed to MinUTxO
          collateral <- (^. coinTxOutL) <$> impGetUTxO collateralInput
          -- We need to artificially blow up the fee to increase the required collateral.
          -- Unfortunately we do not have expensive enough scripts yet, so one other way
          -- to achieve the same thing is by increasing the size of the transactions by
          -- including random garbage. Auxiliary data fits the bill quite nicely
          metadata <-
            Map.fromList
              <$> forM [1 .. (12 * 1024 `div` 64)] (\ix -> (,) ix . M.B <$> uniformByteStringM 64)
          let auxData = mkAlonzoTxAuxData @[] @era metadata []
              tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL <>~ [scriptInput]
                  & bodyTxL . collateralInputsTxBodyL <>~ [collateralInput]
                  & auxDataTxL .~ SJust auxData
          percentage <-
            getsNES $ nesEsL . curPParamsEpochStateL . ppCollateralPercentageL . to toInteger
          submitFailingTxM tx $ \txFixed -> do
            let expectedCollateral =
                  Coin . ceiling $ txFixed ^. bodyTxL . feeTxBodyL . to unCoin * percentage % 100
            pure [injectFailure $ InsufficientCollateral (toDeltaCoin collateral) expectedCollateral]
