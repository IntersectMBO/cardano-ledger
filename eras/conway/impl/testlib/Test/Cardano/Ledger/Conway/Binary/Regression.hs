{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Binary.Regression where

import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotatorFromHexText)
import Cardano.Ledger.Conway.Core (EraTx (..), eraProtVerLow)
import Test.Cardano.Ledger.Common (NFData, Spec, describe, expectRightDeep_, it)

spec ::
  forall era.
  ( EraTx era
  , NFData (Tx era)
  ) =>
  Spec
spec = describe "Regression" $ do
  it "DeserialiseFailure on resubmitting Conway Tx with invalid plutus script #4198" $ do
    expectRightDeep_ $
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
