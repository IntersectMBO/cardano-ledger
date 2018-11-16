module Test.Options
  ( Opts(..)
  , optsParser
  , TestScenario(..)
  )
where

import Cardano.Prelude

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
  helpText =
    "Run under one of Development (default), ContinuousIntegration, or "
      <> "QualityAssurance, to affect how tests are run"
