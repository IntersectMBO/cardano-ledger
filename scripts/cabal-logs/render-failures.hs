{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson qualified as JSON
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import LogResults
import Options.Applicative hiding (Failure)
import System.Console.Terminal.Size qualified as TS
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optOutput :: FilePath
  , optInputs :: [FilePath]
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
              optOutput <-
                strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
                    <> value "/dev/stdout"
                    <> showDefaultWith id
              optInputs <-
                many . strArgument $
                  help "JSON files containing failures"
                    <> metavar "FILE ..."
              pure Options {..}
          )
          (fullDesc <> header "Render failure information from Cabal test logs")
      )

  (errs, inputs) <- partitionEithers <$> for optInputs (JSON.eitherDecodeFileStrict @LogResults)

  for_ errs $ hPutStrLn stderr

  let
    groupedFailures =
      NE.groupWith fst . sort $
        [ (suiteName, (optionValue failureSelector, logCompilerVersion, optionValue failureSeed))
        | LogResults {..} <- inputs
        , SuiteRun {..} <- logSuiteRuns
        , Failure {..} <- suiteFailures
        ]

  let
    prefix = ["## Test Failures ##"]
    body =
      concat
        [ [ ""
          , "### `" <> suite <> "` ###"
          , ""
          , "| Test                                         | Compiler | Seed     |"
          , "|:-------------------------------------------- |:-------- |:-------- |"
          ]
            <> [ T.unwords ["|", selector, "|", compilerName, "|", seed, "|"]
               | (selector, compiler, seed) <- map snd $ NE.toList g
               , let compilerName = T.intercalate "." $ map (T.pack . show) compiler
               ]
        | g@((suite, _) :| _) <- groupedFailures
        ]

  T.writeFile optOutput . T.unlines $ prefix <> body
