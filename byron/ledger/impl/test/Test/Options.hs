{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Options
  ( TestScenario (..),
    mainWithTestScenario,
    scenarioScaled,
    scenarioScaleDefault,
    eachOfTS,
    withTestsTS,
    TSProperty,
    TSGroup,
    concatGroups,
    concatTSGroups,
    tsGroupToTree,
    ShouldAssertNF (..),
  )
where

import Cardano.Prelude hiding (Option)
import Hedgehog (Gen, Group (..), Property, PropertyT, TestLimit, withTests)
import Hedgehog.Internal.Property (GroupName (..), PropertyName (..))
import Test.Cardano.Prelude
import Test.Tasty
  ( TestTree,
    askOption,
    defaultMainWithIngredients,
    includingOptions,
    testGroup,
  )
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Ingredients (Ingredient (..), composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter, listingTests)
import Test.Tasty.Options
  ( IsOption (..),
    OptionDescription (..),
    lookupOption,
    safeRead,
  )

--------------------------------------------------------------------------------
-- TestScenario
--------------------------------------------------------------------------------

data TestScenario
  = ContinuousIntegration
  | Development
  | QualityAssurance
  deriving (Read, Show)

instance IsOption TestScenario where
  defaultValue = Development
  parseValue = safeRead
  optionName = pure "scenario"
  optionHelp = pure helpText

logScenario :: Ingredient
logScenario = TestReporter [] $ \options _ -> Just $ \_ -> do
  let scenario = lookupOption @TestScenario options
  putTextLn $ "\nRunning in scenario: " <> show scenario
  pure (const (pure True))

mainWithTestScenario :: TestTree -> IO ()
mainWithTestScenario =
  defaultMainWithIngredients
    [ includingOptions [Option (Proxy @TestScenario)],
      listingTests,
      composeReporters logScenario consoleTestReporter
    ]

helpText :: [Char]
helpText =
  "Run under one of Development (default), ContinuousIntegration, or "
    <> "QualityAssurance, to affect how tests are run"

--------------------------------------------------------------------------------
-- TestLimit scaling functions & helpers
--------------------------------------------------------------------------------

-- | Convenient alias for TestScenario-dependent @Group@s
type TSGroup = TestScenario -> Group

concatGroups :: [Group] -> Group
concatGroups [] = panic "concatGroups: No tests in test Group"
concatGroups gs@(g : _) = Group (groupName g) (concat $ groupProperties <$> gs)

concatTSGroups :: [TSGroup] -> TSGroup
concatTSGroups gs ts = concatGroups $ ($ ts) <$> gs

tsGroupToTree :: TSGroup -> TestTree
tsGroupToTree tsGroup = askOption $ \scenario -> case tsGroup scenario of
  Group {groupName, groupProperties} ->
    testGroup
      (unGroupName groupName)
      (uncurry testProperty . first unPropertyName <$> groupProperties)

-- | Convenient alias for TestScenario-dependent @Property@s
type TSProperty = TestScenario -> Property

-- | Default ratio of tests in development
devTestDefault :: Ratio TestLimit
devTestDefault = 1 % 2

-- | Default ratio of tests in CI
ciTestDefault :: Ratio TestLimit
ciTestDefault = 1 % 1

-- | Default ratio of tests in QA
qaTestDefault :: Ratio TestLimit
qaTestDefault = 2 % 1

-- | Return an Int number of tests, using the above-defined defaults
scenarioScaleDefault :: TestScenario -> Ratio TestLimit
scenarioScaleDefault ts = case ts of
  Development -> devTestDefault
  ContinuousIntegration -> ciTestDefault
  QualityAssurance -> qaTestDefault

-- | Multiply the default scenario values by a scalar
scenarioScaled :: TestLimit -> TestScenario -> TestLimit
scenarioScaled count ts =
  if scaledCount > 0
    then scaledCount
    else
      panic $
        "scenarioScaled: produced a non-positive TestLimit: "
          <> show scaledCount
  where
    scaledCount :: TestLimit
    scaledCount = round . ((count % 1) *) $ scenarioScaleDefault ts

-- | A modified `eachOf` which uses the default TestScenario values,
-- multiplied by a scalar
eachOfTS ::
  (Show a, HasCallStack) =>
  TestLimit ->
  Gen a ->
  (a -> PropertyT IO ()) ->
  TestScenario ->
  Property
eachOfTS count gen predicate scenario =
  withFrozenCallStack $ eachOf (scenarioScaled count scenario) gen predicate

-- | A modified `withTests` which uses the default TestScenario values,
-- multiplied by a scalar
withTestsTS ::
  TestLimit ->
  Property ->
  TestScenario ->
  Property
withTestsTS count prop scenario =
  withTests (scenarioScaled count scenario) prop

--------------------------------------------------------------------------------
-- ShouldAssertNF
--------------------------------------------------------------------------------

data ShouldAssertNF
  = AssertNF
  | NoAssertNF
  deriving (Eq, Show)
