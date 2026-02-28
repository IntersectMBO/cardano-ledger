{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (unless)
import Data.Aeson qualified as JSON
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
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
    selectorEntry Failure {..} = (optionValue failureSelector, [optionValue failureSeed])
    logResults = Map.fromListWith (<>) . map selectorEntry <$> Map.unionsWith (<>) inputs

  unless (null logResults) $ do
    let
      prefix =
        [ "## Test Failures ##"
        , ""
        , "| Target | Pattern | Seeds |"
        , "|:------ |:------- |:----- |"
        ]
      body =
        [ T.unwords ["|", target, "|", selector, "|", T.unwords seeds, "|"]
        | (target, selectors) <- Map.toList logResults
        , (selector, seeds) <- Map.toList selectors
        ]
    T.writeFile optOutput . T.unlines $ prefix <> body
