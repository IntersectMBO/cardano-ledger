{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Conway.Binary.Bugs where

import Cardano.Ledger.Conway.Core (EraTx(..), eraProtVerLow)
import Test.Cardano.Ledger.Common (Spec, describe, it, expectRightDeep_, NFData)
import Cardano.Ledger.Binary (decodeFullAnnotatorFromHexText, decCBOR)

spec :: forall era.
  ( EraTx era
  , NFData (Tx era)
  ) =>
  Spec
spec = describe "Bugs" $ do
  it "DeserialiseFailure on resubmitting Conway Tx with invalid plutus script #4198" $ do
    expectRightDeep_ $
      decodeFullAnnotatorFromHexText @(Tx era) (eraProtVerLow @era) "Tx" decCBOR $ mconcat
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
