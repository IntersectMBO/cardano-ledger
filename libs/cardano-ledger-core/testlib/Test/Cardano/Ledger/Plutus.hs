{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Ledger.Plutus (
  -- * Plutus
  alwaysSucceedsPlutus,
  alwaysFailsPlutus,

  -- * CostModel
  zeroTestingCostModel,
  zeroTestingCostModelV1,
  zeroTestingCostModelV2,
  zeroTestingCostModelV3,
  testingCostModel,
  testingCostModelV1,
  testingCostModelV2,
  testingCostModelV3,
  testingEvaluationContext,
) where

import Cardano.Ledger.Plutus.CostModels (CostModel, getCostModelEvaluationContext, mkCostModel)
import Cardano.Ledger.Plutus.Language (Language (..), Plutus (..), PlutusBinary (..))
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

zeroTestingCostModel :: HasCallStack => Language -> CostModel
zeroTestingCostModel = \case
  PlutusV1 -> zeroTestingCostModelV1
  PlutusV2 -> zeroTestingCostModelV2
  PlutusV3 -> zeroTestingCostModelV3

zeroTestingCostModelV1 :: HasCallStack => CostModel
zeroTestingCostModelV1 = mkCostModel' PlutusV1 (0 <$ PV1.costModelParamsForTesting)

zeroTestingCostModelV2 :: HasCallStack => CostModel
zeroTestingCostModelV2 = mkCostModel' PlutusV2 (0 <$ PV2.costModelParamsForTesting)

zeroTestingCostModelV3 :: HasCallStack => CostModel
zeroTestingCostModelV3 = mkCostModel' PlutusV3 (0 <$ PV3.costModelParamsForTesting)

testingCostModel :: HasCallStack => Language -> CostModel
testingCostModel = \case
  PlutusV1 -> testingCostModelV1
  PlutusV2 -> testingCostModelV2
  PlutusV3 -> testingCostModelV3

testingCostModelV1 :: HasCallStack => CostModel
testingCostModelV1 = mkCostModel' PlutusV1 $ snd <$> PV1.costModelParamsForTesting

testingCostModelV2 :: HasCallStack => CostModel
testingCostModelV2 = mkCostModel' PlutusV2 $ snd <$> PV2.costModelParamsForTesting

testingCostModelV3 :: HasCallStack => CostModel
testingCostModelV3 = mkCostModel' PlutusV3 $ snd <$> PV3.costModelParamsForTesting

testingEvaluationContext :: Language -> PV1.EvaluationContext
testingEvaluationContext = getCostModelEvaluationContext . testingCostModel

alwaysSucceedsPlutus :: Natural -> Plutus l
alwaysSucceedsPlutus n = Plutus (PlutusBinary (P.alwaysSucceedingNAryFunction n))

alwaysFailsPlutus :: Natural -> Plutus l
alwaysFailsPlutus n = Plutus (PlutusBinary (P.alwaysFailingNAryFunction n))
