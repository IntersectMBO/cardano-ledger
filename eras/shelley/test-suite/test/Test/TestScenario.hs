{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestScenario where

import Cardano.Prelude hiding (Option)
import Test.Tasty (TestTree, defaultMainWithIngredients, includingOptions)
import Test.Tasty.Ingredients (Ingredient (..), composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter, listingTests)
import Test.Tasty.Options
  ( IsOption (..),
    OptionDescription (..),
    lookupOption,
    safeRead,
  )
import Prelude hiding (show)

data TestScenario
  = ContinuousIntegration
  | Development
  | Nightly
  | Fast
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
  "Run under one of Development (default), ContinuousIntegration, "
    <> "Nightly, or Fast, to affect how tests are run"
