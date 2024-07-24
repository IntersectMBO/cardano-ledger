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
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..), toDeltaCoin)
import Cardano.Ledger.Plutus (Data (..), ExUnits (..), hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
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
  SpecWith (ImpTestState era)
spec = describe "UTXO" $ do
  it "Wrong network ID" $ do
    submitFailingTx
      (mkBasicTx mkBasicTxBody & bodyTxL . networkIdTxBodyL .~ SJust Mainnet)
      [injectFailure $ WrongNetworkInTxBody Testnet Mainnet]

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
          submitFailingTx tx [injectFailure $ ExUnitsTooBigUTxO maxExUnits txExUnits]

        it "Insufficient collateral" $ do
          scriptInput <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum slang
          let collateral = Coin 123
          collateralAddr <- freshKeyAddr_
          collateralInput <- sendCoinTo collateralAddr collateral
          let tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL <>~ [scriptInput]
                  & bodyTxL . collateralInputsTxBodyL <>~ [collateralInput]
          percentage <-
            getsNES $ nesEsL . curPParamsEpochStateL . ppCollateralPercentageL . to toInteger
          submitFailingTxM tx $ \txFixed -> do
            let expectedCollateral =
                  Coin . ceiling $ txFixed ^. bodyTxL . feeTxBodyL . to unCoin * percentage % 100
            pure [injectFailure $ InsufficientCollateral (toDeltaCoin collateral) expectedCollateral]
