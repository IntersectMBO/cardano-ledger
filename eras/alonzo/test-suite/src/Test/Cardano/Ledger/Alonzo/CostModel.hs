{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.CostModel (
  costModelParamsCount,
  freeCostModel,
  freeV1CostModels,
  freeV1V2CostModels,
)
where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts as Alonzo (
  CostModel,
  CostModels (..),
  mkCostModel,
 )
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.V1 as PV1 (ParamName)
import qualified PlutusLedgerApi.V2 as PV2 (ParamName)
import PlutusPrelude (enumerate)

costModelParamsCount :: Language -> Int
costModelParamsCount lang = case lang of
  PlutusV1 -> length (enumerate @PV1.ParamName)
  PlutusV2 -> length (enumerate @PV2.ParamName)

-- | A cost model that sets everything as being free
freeCostModel :: Language -> CostModel
freeCostModel lang =
  fromRight (error "freeCostModel is not well-formed") $
    Alonzo.mkCostModel lang (replicate (costModelParamsCount lang) 0)

freeV1CostModels :: CostModels
freeV1CostModels = CostModels (Map.singleton PlutusV1 (freeCostModel PlutusV1)) mempty mempty

freeV1V2CostModels :: CostModels
freeV1V2CostModels =
  CostModels (Map.fromList [(l, freeCostModel l) | l <- [PlutusV1, PlutusV2]]) mempty mempty
