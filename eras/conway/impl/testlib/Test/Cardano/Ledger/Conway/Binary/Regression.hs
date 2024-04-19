{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Binary.Regression where

import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe (..), TxIx (..))
import Cardano.Ledger.Binary (
  EncCBOR (..),
  decCBOR,
  decodeFull,
  decodeFullAnnotatorFromHexText,
  mkVersion,
  serialize,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (
  BabbageEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  coinTxOutL,
  eraProtVerLow,
  txIdTx,
 )
import Cardano.Ledger.Conway.Rules (
  ConwayLedgerPredFailure (..),
  ConwayUtxoPredFailure (..),
  ConwayUtxowPredFailure (..),
 )
import Cardano.Ledger.Plutus.Language (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.Monad ((<=<))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkScriptAddr)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)

spec ::
  forall era.
  ( EraTx era
  , NFData (Tx era)
  ) =>
  Spec
spec = describe "Regression" $ do
  it "DeserialiseFailure on resubmitting Conway Tx with invalid plutus script #4198" $ do
    io . expectRightDeep_ $
      decodeFullAnnotatorFromHexText @(Tx era) (eraProtVerLow @era) "Unwitnessed Tx" decCBOR $
        mconcat
          [ "84a700d9010282825820745f04573e7429be1404f9b936d208b81159f3fc4b300"
          , "37b9d630187eec1875600825820745f04573e7429be1404f9b936d208b81159f3"
          , "fc4b30037b9d630187eec18756020dd9010281825820745f04573e7429be1404f"
          , "9b936d208b81159f3fc4b30037b9d630187eec1875601018282581d60fdfaa525"
          , "1e9ed2186a52eeea05ac1d39834eeef09b3e41dc151577a01a001e848082581d6"
          , "0fe920c980dbc1113a01db0156955479f3b91f6fb6a51bdc0c383c1b91a3b586d"
          , "e61082581d60fe920c980dbc1113a01db0156955479f3b91f6fb6a51bdc0c383c"
          , "1b91a001a65b0111a00041ed0021a0002bf350b5820878c73eb6ec7171b23396f"
          , "71d7e5adee98b3f72cfc1c0662453ea724a4e27ad5a303d9010281581e581c010"
          , "0003322323222235004007123500235300300149849848004800504d9010281d8"
          , "799f182aff0581840000d8799f182aff820000f4f6"
          ]
    expectRightDeep_ $
      decodeFullAnnotatorFromHexText @(Tx era) (eraProtVerLow @era) "Witnessed Tx" decCBOR $
        mconcat
          [ "84a700d9010282825820745f04573e7429be1404f9b936d208b81159f3fc4b300"
          , "37b9d630187eec1875600825820745f04573e7429be1404f9b936d208b81159f3"
          , "fc4b30037b9d630187eec18756020dd9010281825820745f04573e7429be1404f"
          , "9b936d208b81159f3fc4b30037b9d630187eec1875601018282581d60fdfaa525"
          , "1e9ed2186a52eeea05ac1d39834eeef09b3e41dc151577a01a001e848082581d6"
          , "0fe920c980dbc1113a01db0156955479f3b91f6fb6a51bdc0c383c1b91a3b586d"
          , "e61082581d60fe920c980dbc1113a01db0156955479f3b91f6fb6a51bdc0c383c"
          , "1b91a001a65b0111a00041ed0021a0002bf350b5820878c73eb6ec7171b23396f"
          , "71d7e5adee98b3f72cfc1c0662453ea724a4e27ad5a400d9010282825820119ca"
          , "69d7aadd28f1e182176cbaa35f4e08d580b79ee749103f4106768594343584057"
          , "de8c067f7b806001e94f740c9c96c51f884e264dd0b2d0cff501ad67f1d269b7a"
          , "7af5adf92148f4a10855fe3b2090bc88f045603cfe14c8a5f3fed6c4008038258"
          , "20468ed75ae68f72233e33b0a869ae5f00cfabe477f186184782e5a1994d189a9"
          , "b58408395b8e91540804ce1860272ac72b4ecc682f567a33c33da8e835d736f1f"
          , "c039ff86ee5aae0ac0e9c9d50506132e209f62a02fe04906b66a3392d48d4d627"
          , "d0403d9010281581e581c01000033223232222350040071235002353003001498"
          , "49848004800504d9010281d8799f182aff0581840000d8799f182aff820000f4f6"
          ]
  describe "ImpTest" $
    withImpState @Conway $
      it "InsufficientCollateral is not encoded with negative coin #4198" $ do
        let lockedVal = inject $ Coin 100
        (_, collateralAddress) <- freshKeyAddr
        (_, skp) <- freshKeyPair
        let
          plutusVersion = SPlutusV2
          scriptHash = hashPlutusScript $ guessTheNumber3 plutusVersion
          lockScriptAddress = mkScriptAddr scriptHash skp
        (_, collateralReturnAddr) <- freshKeyAddr
        lockedTx <-
          submitTxAnn @Conway "Script locked tx" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . outputsTxBodyL
                .~ SSeq.fromList
                  [ mkBasicTxOut lockScriptAddress lockedVal
                  , mkBasicTxOut collateralAddress (inject $ Coin 1)
                  ]
              & bodyTxL . collateralReturnTxBodyL
                .~ SJust (mkBasicTxOut collateralReturnAddr . inject $ Coin 1)
        let
          modifyRootCoin = coinTxOutL .~ Coin 989482376
          modifyRootTxOut (x SSeq.:<| SSeq.Empty) =
            modifyRootCoin x SSeq.:<| SSeq.Empty
          modifyRootTxOut (x SSeq.:<| xs) = x SSeq.:<| modifyRootTxOut xs
          modifyRootTxOut (xs SSeq.:|> x) = xs SSeq.:|> modifyRootCoin x
          modifyRootTxOut SSeq.Empty = SSeq.Empty
          breakCollaterals tx =
            pure $
              tx
                & bodyTxL . collateralReturnTxBodyL
                  .~ SJust (mkBasicTxOut collateralReturnAddr . inject $ Coin 1_000_000_000)
                & bodyTxL . feeTxBodyL .~ Coin 178349
                & bodyTxL . outputsTxBodyL %~ modifyRootTxOut
                & witsTxL . addrTxWitsL .~ mempty
        res <-
          impAnn "Consume the script locked output" $
            withPostFixup (updateAddrTxWits <=< breakCollaterals) $ do
              trySubmitTx @Conway $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL .~ Set.singleton (TxIn (txIdTx lockedTx) $ TxIx 0)
        pFailure <- impAnn "Expecting failure" $ expectLeftDeepExpr res
        let
          hasInsufficientCollateral
            (ConwayUtxowFailure (UtxoFailure (InsufficientCollateral _ _))) = True
          hasInsufficientCollateral _ = False
        impAnn "Fails with InsufficientCollateral" $
          pFailure `shouldSatisfyExpr` any hasInsufficientCollateral
        let encoding = encCBOR pFailure
        version <- mkVersion (11 :: Int)
        let
          bs = serialize version encoding
          decoded = decodeFull version bs
        impAnn "Expecting deserialization of predicate failure to succeed" $
          decoded `shouldBe` Right pFailure
