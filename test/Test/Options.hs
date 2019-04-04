{-# LANGUAGE OverloadedStrings #-}
module Test.Options
  ( Opts(..)
  , optsParser
  , TestScenario(..)
  , scenarioScaled
  , scenarioScaleDefault
  , eachOfTS
  , withTestsTS
  , TSProperty
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Options.Applicative as Opts
  ( Parser
  , ParserInfo
  , auto
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , short
  , value
  )

import Hedgehog (Gen, Property, PropertyT, TestLimit, withTests)


--------------------------------------------------------------------------------
-- Opts
--------------------------------------------------------------------------------

newtype Opts = Opts
  { optsTestScenario :: TestScenario
  }

optsParser :: ParserInfo Opts
optsParser = info (parser <**> helper) mempty
  where parser = Opts <$> testScenarioParser


--------------------------------------------------------------------------------
-- TestScenario
--------------------------------------------------------------------------------

data TestScenario
  = ContinuousIntegration
  | Development
  | QualityAssurance
  deriving (Read)

testScenarioParser :: Parser TestScenario
testScenarioParser = Opts.option
  auto
  (  short 's'
  <> long "scenario"
  <> metavar "TEST_SCENARIO"
  <> help helpText
  <> value Development
  )
 where
  helpText :: [Char]
  helpText =
    "Run under one of Development (default), ContinuousIntegration, or "
      <> "QualityAssurance, to affect how tests are run"


--------------------------------------------------------------------------------
-- TestLimit scaling functions & helpers
--------------------------------------------------------------------------------

-- | Convenient alias for TestScenario-dependent Property's
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
     else panic $ "scenarioScaled: produced a non-positive TestLimit: "
               <> show scaledCount
 where
  scaledCount :: TestLimit
  scaledCount = round . ((count%1) *) $ scenarioScaleDefault ts

-- | A modified `eachOf` which uses the default TestScenario values,
-- multiplied by a scalar
eachOfTS
  :: (Show a, HasCallStack)
  => TestLimit
  -> Gen a
  -> (a -> PropertyT IO ())
  -> TestScenario
  -> Property
eachOfTS count gen predicate scenario =
  eachOf (scenarioScaled count scenario) gen predicate

-- | A modified `withTests` which uses the default TestScenario values,
-- multiplied by a scalar
withTestsTS
  :: TestLimit
  -> Property
  -> TestScenario
  -> Property
withTestsTS count prop scenario =
  withTests (scenarioScaled count scenario) prop
