{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxAuxData
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import qualified Cardano.Ledger.Metadata as M
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Tools
import Cardano.Ledger.TxIn
import Control.Monad (forM)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro (to, (%~), (&), (.~), (<>~), (^.))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec :: forall era. AlonzoEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  it "Wrong network ID" $ do
    submitFailingTx
      (mkBasicTx mkBasicTxBody & bodyTxL . networkIdTxBodyL .~ SJust Mainnet)
      [ injectFailure $
          WrongNetworkInTxBody Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
      ]

  it "Collateral input not in UTxO" $ do
    let neverSubmittedTx = mkBasicTx mkBasicTxBody :: Tx TopTx era
        bogusCollateralTxIn = mkTxInPartial (txIdTx neverSubmittedTx) 0
        tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . collateralInputsTxBodyL .~ [bogusCollateralTxIn]
    submitFailingTx
      tx
      [injectFailure $ BadInputsUTxO (NES.singleton bogusCollateralTxIn)]

  it "Collateral input already spent" $ do
    addr <- freshKeyAddr_
    spentTxIn <- sendCoinTo addr (Coin 10_000_000)
    submitAnyTx_ $
      mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [spentTxIn]
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . collateralInputsTxBodyL .~ [spentTxIn]
    submitFailingTx
      tx
      [injectFailure $ BadInputsUTxO (NES.singleton spentTxIn)]

  it "OutputTooBigUTxO" $ do
    restrictMaxValSizeToAdaOnly
    pp <- getsPParams id
    (multiAsset, txOut) <- freshAssetOutput
    let tooBigTxOut = setMinCoinTxOut pp txOut
    submitFailingTx
      ( mkBasicTx mkBasicTxBody
          & bodyTxL . mintTxBodyL .~ multiAsset
          & bodyTxL . outputsTxBodyL .~ [tooBigTxOut]
      )
      [injectFailure $ OutputTooBigUTxO [outputTooBigEntry pp tooBigTxOut]]

  -- The formal specification does not check the number of collateral inputs
  -- TODO: Re-enable once it does, by removing this override
  disableInConformanceIt "TooManyCollateralInputs" $ do
    maxCollateralInputs <- getsPParams ppMaxCollateralInputsL
    collateralInputs <- replicateM (fromIntegral maxCollateralInputs + 1) freshFundedTxIn
    submitFailingTx
      ( mkBasicTx mkBasicTxBody
          & bodyTxL . collateralInputsTxBodyL .~ Set.fromList collateralInputs
      )
      [ injectFailure $
          TooManyCollateralInputs
            Mismatch
              { mismatchSupplied = maxCollateralInputs + 1
              , mismatchExpected = maxCollateralInputs
              }
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
                & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert prp (dat, txExUnits)
          submitFailingTx
            tx
            [ injectFailure $
                ExUnitsTooBigUTxO
                  Mismatch {mismatchSupplied = OrdExUnits txExUnits, mismatchExpected = OrdExUnits maxExUnits}
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
              <$> forM
                [1 .. (12 * 1024 `div` 64)]
                (\ix -> (,) ix . M.B <$> uniformByteArrayM 64)
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

        it "ScriptsNotPaidUTxO" $ do
          let scriptHash = hashPlutusScript $ alwaysSucceedsWithDatum slang
          scriptInput <- produceScript scriptHash
          collateralInput <- sendCoinTo (mkAddr scriptHash StakeRefNull) (Coin 30_000_000)
          collateralTxOut <- impGetUTxO collateralInput
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [scriptInput]
                & bodyTxL . collateralInputsTxBodyL .~ [collateralInput]
            )
            [injectFailure . ScriptsNotPaidUTxO $ NEM.singleton collateralInput collateralTxOut]

        -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1279
        -- TODO: Re-enable after issue is resolved, by removing this override
        disableInConformanceIt "CollateralContainsNonADA" $ do
          scriptInput <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum slang
          (multiAsset, txOut) <- freshAssetOutput
          let collateralTxOut = txOut & coinTxOutL .~ Coin 30_000_000
          collateralInput <-
            txInAt 0
              <$> submitTx
                ( mkBasicTx mkBasicTxBody
                    & bodyTxL . mintTxBodyL .~ multiAsset
                    & bodyTxL . outputsTxBodyL .~ [collateralTxOut]
                )
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [scriptInput]
                & bodyTxL . collateralInputsTxBodyL .~ [collateralInput]
            )
            [injectFailure $ CollateralContainsNonADA (collateralTxOut ^. valueTxOutL)]

        it "NoCollateralInputs" $ do
          scriptInput <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum slang
          percentage <- toInteger <$> getsPParams ppCollateralPercentageL
          withPostFixup (rederiveAddrTxWits . (bodyTxL . collateralInputsTxBodyL .~ mempty)) $
            submitFailingTxM (mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [scriptInput]) $
              \txFixed -> do
                let expectedCollateral =
                      Coin . ceiling $ txFixed ^. bodyTxL . feeTxBodyL . to unCoin * percentage % 100
                pure
                  [ injectFailure $ InsufficientCollateral (DeltaCoin 0) expectedCollateral
                  , injectFailure NoCollateralInputs
                  ]
