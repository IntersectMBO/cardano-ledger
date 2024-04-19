#!/usr/bin/env cabal
{- cabal:
  build-depends:
    base, cabal-plan, containers, directory, filepath, microlens, microlens-aeson,
    optparse-applicative, terminal-size, text, yaml
  ghc-options: -Wall -Wcompat -Wunused-packages
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Cabal.Plan
import Control.Monad (unless)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List ((\\))
import Data.Text (Text)
import Data.Yaml (Value, decodeFileThrow)
import Lens.Micro ((^..))
import Lens.Micro.Aeson (key, values, _String)
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))

import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as Text
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optProjectDir :: FilePath
  , optWorkflowFile :: FilePath
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optProjectDir <-
                strOption $
                  long "project"
                    <> metavar "DIR"
                    <> value "."
                    <> help "The project directory, or a subdirectory of it"
                    <> showDefaultWith id
              optWorkflowFile <-
                strOption $
                  long "workflow"
                    <> metavar "FILENAME"
                    <> value "haskell.yml"
                    <> help "The workflow file name (relative to .github/workflows)"
                    <> showDefaultWith id
              pure Options {..}
          )
          ( fullDesc
              <> header "Check that the test jobs in a GitHub workflow match the tests in a Cabal project"
          )
      )

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= bool (die $ "Project directory " <> optProjectDir <> " doesn't exist") (pure ())

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  workflow <- decodeFileThrow $ root </> ".github/workflows" </> optWorkflowFile

  let expected = planTests plan
      actual = workflowTests workflow
      missing = expected \\ actual
      extra = actual \\ expected

  unless (null missing) $ do
    putStrLn "The following tests are missing from the workflow:"
    for_ missing $ Text.putStrLn . ("* " <>)
  unless (null extra) $ do
    putStrLn "The following tests should not be in the workflow:"
    for_ extra $ Text.putStrLn . ("* " <>)

  unless (null missing && null extra) $
    exitFailure

planTests :: PlanJson -> [Text]
planTests plan =
  let localUnits = filter ((UnitTypeLocal ==) . uType) . Map.elems . pjUnits $ plan
      unitsWithTests = filter (any isTestComp . Map.keys . uComps) localUnits
      isTestComp (CompNameTest _) = True
      isTestComp _ = False
      pIdName (PkgId (PkgName name) _) = name
   in pIdName . uPId <$> unitsWithTests

workflowTests :: Value -> [Text]
workflowTests v = v ^.. key "jobs" . key "test" . key "strategy" . key "matrix" . key "package" . values . _String
