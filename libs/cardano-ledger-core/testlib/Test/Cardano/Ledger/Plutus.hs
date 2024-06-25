{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Ledger.Plutus (
  PlutusArgs (..),
  ScriptTestContext (..),

  -- * Plutus
  alwaysSucceedsPlutus,
  alwaysFailsPlutus,

  -- * CostModel
  mkCostModelConst,
  zeroTestingCostModel,
  zeroTestingCostModelV1,
  zeroTestingCostModelV2,
  zeroTestingCostModelV3,
  testingCostModel,
  testingCostModelV1,
  testingCostModelV2,
  testingCostModelV3,
  zeroTestingCostModelV4,
  testingEvaluationContext,

  -- * CostModels
  testingCostModels,
  zeroTestingCostModels,
) where

import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  CostModels,
  getCostModelEvaluationContext,
  mkCostModel,
  mkCostModels,
 )
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusBinary (..),
 )
import qualified Data.Map.Strict as Map
import GHC.Stack
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.Test.Examples as P (
  alwaysFailingNAryFunction,
  alwaysSucceedingNAryFunction,
 )
import qualified PlutusLedgerApi.Test.V1.EvaluationContext as PV1
import qualified PlutusLedgerApi.Test.V2.EvaluationContext as PV2
import qualified PlutusLedgerApi.Test.V3.EvaluationContext as PV3
import PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Plutus.ScriptTestContext (
  PlutusArgs (..),
  ScriptTestContext (..),
 )

-- | Construct a test cost model where all parameters are set to the same value
mkCostModelConst :: HasCallStack => Language -> Integer -> CostModel
mkCostModelConst lang x =
  case lang of
    PlutusV1 -> mkCostModel' lang (x <$ PV1.costModelParamsForTesting)
    PlutusV2 -> mkCostModel' lang (x <$ PV2.costModelParamsForTesting)
    PlutusV3 -> mkCostModel' lang (x <$ PV3.costModelParamsForTesting)
    PlutusV4 -> mkCostModel' lang (x <$ PV3.costModelParamsForTesting) -- TODO WG

mkCostModel' :: HasCallStack => Language -> [Integer] -> CostModel
mkCostModel' lang params =
  case mkCostModel lang params of
    Left err ->
      error $
        "Number of CostModel parameters "
          ++ show (length params)
          ++ " is not well-formed for "
          ++ show lang
          ++ ": "
          ++ show err
    Right costModel -> costModel

-- | Test CostModels for all available languages with zero values for all parameters
zeroTestingCostModels :: HasCallStack => [Language] -> CostModels
zeroTestingCostModels =
  foldMap $ \lang -> mkCostModels (Map.singleton lang (zeroTestingCostModel lang))

zeroTestingCostModel :: HasCallStack => Language -> CostModel
zeroTestingCostModel lang = mkCostModelConst lang 0

zeroTestingCostModelV1 :: HasCallStack => CostModel
zeroTestingCostModelV1 = zeroTestingCostModel PlutusV1

zeroTestingCostModelV2 :: HasCallStack => CostModel
zeroTestingCostModelV2 = zeroTestingCostModel PlutusV2

zeroTestingCostModelV3 :: HasCallStack => CostModel
zeroTestingCostModelV3 = zeroTestingCostModel PlutusV3

zeroTestingCostModelV4 :: HasCallStack => CostModel
zeroTestingCostModelV4 = zeroTestingCostModel PlutusV4

-- | Test CostModels for all available languages
testingCostModels :: HasCallStack => [Language] -> CostModels
testingCostModels =
  foldMap $ \lang -> mkCostModels (Map.singleton lang (testingCostModel lang))

testingCostModel :: HasCallStack => Language -> CostModel
testingCostModel = \case
  PlutusV1 -> testingCostModelV1
  PlutusV2 -> testingCostModelV2
  PlutusV3 -> testingCostModelV3
  PlutusV4 -> testingCostModelV4

testingCostModelV1 :: HasCallStack => CostModel
testingCostModelV1 = mkCostModel' PlutusV1 $ toInteger . snd <$> PV1.costModelParamsForTesting

testingCostModelV2 :: HasCallStack => CostModel
testingCostModelV2 = mkCostModel' PlutusV2 $ toInteger . snd <$> PV2.costModelParamsForTesting

testingCostModelV3 :: HasCallStack => CostModel
testingCostModelV3 = mkCostModel' PlutusV3 $ toInteger . snd <$> PV3.costModelParamsForTesting

testingCostModelV4 :: HasCallStack => CostModel
testingCostModelV4 = mkCostModel' PlutusV4 $ toInteger . snd <$> PV3.costModelParamsForTesting -- TODO WG

testingEvaluationContext :: Language -> PV1.EvaluationContext
testingEvaluationContext = getCostModelEvaluationContext . testingCostModel

alwaysSucceedsPlutus :: Natural -> Plutus l
alwaysSucceedsPlutus n = Plutus (PlutusBinary (P.alwaysSucceedingNAryFunction n))

alwaysFailsPlutus :: Natural -> Plutus l
alwaysFailsPlutus n = Plutus (PlutusBinary (P.alwaysFailingNAryFunction n))
