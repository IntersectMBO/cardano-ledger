{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.JSON.GoldenSpec (
  spec,
) where

import Cardano.Ledger.Allegra.Core (
  ValidityInterval (ValidityInterval, invalidBefore, invalidHereafter),
 )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams (CoinPerWord (CoinPerWord))
import Cardano.Ledger.Alonzo.Rules (FailureDescription (..), TagMismatchDescription (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid))
import Cardano.Ledger.BaseTypes (SlotNo (..), StrictMaybe (..), boundRational)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Plutus.CostModels (CostModel, mkCostModel)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..))
import Cardano.Ledger.Plutus.Language (Language (PlutusV1))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (fromRight)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Paths_cardano_ledger_alonzo (getDataFileName)
import Test.Cardano.Ledger.Alonzo.Era ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common (
  Spec,
  aesonGoldenSpec,
  beforeAll,
  describe,
  it,
  itGoldenToJSON,
  shouldBe,
 )
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)

spec :: Spec
spec = do
  beforeAll (getDataFileName "golden/pparams.json") $
    goldenJsonPParamsSpec @AlonzoEra
  beforeAll (getDataFileName "golden/pparams-update.json") $
    goldenJsonPParamsUpdateSpec @AlonzoEra
  aesonGoldenSpec
    getDataFileName
    "golden/ValidityInterval.json"
    [ ValidityInterval
        { invalidBefore = SNothing
        , invalidHereafter = SNothing
        }
    , ValidityInterval
        { invalidBefore = SJust (SlotNo 12345)
        , invalidHereafter = SNothing
        }
    , ValidityInterval
        { invalidBefore = SNothing
        , invalidHereafter = SJust (SlotNo 12354)
        }
    , ValidityInterval
        { invalidBefore = SJust (SlotNo 12345)
        , invalidHereafter = SJust (SlotNo 12354)
        }
    ]
  aesonGoldenSpec
    getDataFileName
    "golden/IsValid.json"
    [ IsValid True
    , IsValid False
    ]
  itGoldenToJSON
    getDataFileName
    "golden/FailureDescription.json"
    $ PlutusFailure "A description" "A reconstruction"
  itGoldenToJSON
    getDataFileName
    "golden/TagMismatchDescription.json"
    [ PassedUnexpectedly
    , FailedUnexpectedly (NE.fromList [PlutusFailure "A description" "A reconstruction"])
    ]
  goldenGenesisSerialization

goldenGenesisSerialization :: Spec
goldenGenesisSerialization =
  describe "Alonzo Genesis" $
    it "deserializes mainnet-alonzo-genesis.json" $ do
      file <- getDataFileName "golden/mainnet-alonzo-genesis.json"
      deserialized <- (eitherDecodeFileStrict file :: IO (Either String AlonzoGenesis))
      deserialized `shouldBe` Right expectedGenesis

expectedGenesis :: AlonzoGenesis
expectedGenesis =
  AlonzoGenesis
    { agCoinsPerUTxOWord = CoinPerWord $ Coin 34482
    , agPrices = Prices (fromJust $ boundRational 0.0577) (fromJust $ boundRational 0.0000721)
    , agPlutusV1CostModel = expectedCostModel
    , agMaxTxExUnits = ExUnits 10000000 10000000000
    , agMaxBlockExUnits = ExUnits 50000000 40000000000
    , agMaxValSize = 5000
    , agCollateralPercentage = 150
    , agMaxCollateralInputs = 3
    , agExtraConfig = Nothing
    }

expectedCostModel :: CostModel
expectedCostModel =
  fromRight
    (error ("Error creating CostModel from known parameters" <> show expectedPParams))
    (mkCostModel PlutusV1 expectedPParams)

expectedPParams :: [Int64]
expectedPParams =
  [ 197209
  , 0
  , 1
  , 1
  , 396231
  , 621
  , 0
  , 1
  , 150000
  , 1000
  , 0
  , 1
  , 150000
  , 32
  , 2477736
  , 29175
  , 4
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 29773
  , 100
  , 100
  , 100
  , 29773
  , 100
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 1000
  , 0
  , 1
  , 150000
  , 32
  , 150000
  , 1000
  , 0
  , 8
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 150000
  , 1000
  , 0
  , 8
  , 150000
  , 112536
  , 247
  , 1
  , 150000
  , 10000
  , 1
  , 136542
  , 1326
  , 1
  , 1000
  , 150000
  , 1000
  , 1
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 1
  , 1
  , 150000
  , 1
  , 150000
  , 4
  , 103599
  , 248
  , 1
  , 103599
  , 248
  , 1
  , 145276
  , 1366
  , 1
  , 179690
  , 497
  , 1
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 61516
  , 11218
  , 0
  , 1
  , 150000
  , 32
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 148000
  , 425507
  , 118
  , 0
  , 1
  , 1
  , 2477736
  , 29175
  , 4
  , 0
  , 82363
  , 4
  , 150000
  , 5000
  , 0
  , 1
  , 150000
  , 32
  , 197209
  , 0
  , 1
  , 1
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 150000
  , 32
  , 3345831
  , 1
  , 1
  ]
