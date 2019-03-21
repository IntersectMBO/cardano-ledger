{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import Safe
import System.Exit (exitWith)
import Turtle


main :: IO ()
main = do
  buildResult <- buildSubdir "crypto" .&&. buildStep
    (Just ["--scenario=ContinuousIntegration"])

  when (buildResult == ExitSuccess) coverageUploadStep

  exitWith buildResult
 where
  buildSubdir :: Turtle.FilePath -> IO ExitCode
  buildSubdir dir = do
    cd dir
    res <- buildStep Nothing
    cd ".."
    pure res

buildStep :: Maybe [Text] -> IO ExitCode
buildStep testArgs = do
  echo "+++ Build and test"
  build .&&. test
 where
  cfg = ["--dump-logs", "--color", "always"]
  stackBuild args = run "stack" $ cfg ++ ["build", "--fast"] ++ args
  buildArgs =
    [ "--bench"
    , "--no-run-benchmarks"
    , "--haddock"
    , "--haddock-internal"
    , "--no-haddock-deps"
    ]
  buildAndTest = stackBuild $ "--tests" : buildArgs
  build        = stackBuild $ "--no-run-tests" : buildArgs
  test =
    stackBuild
      $  ["--test", "--coverage", "--jobs", "1"]
      ++ maybe [] ("--ta" :) testArgs

-- | Upload coverage information to coveralls
coverageUploadStep :: IO ()
coverageUploadStep = do
  echo "--- Uploading Coverage Information"
  need "CARDANO_CHAIN_COVERALLS_REPO_TOKEN" >>= \case
    Nothing -> printf
      "Missing coverall repo token. Not uploading coverage information.\n"
    Just repoToken -> do
      result <- proc
        "shc"
        ["--repo-token", repoToken, "cardano-ledger", "cardano-ledger-test"]
        empty
      case result of
        ExitSuccess   -> printf "Coverage information upload successful.\n"
        ExitFailure _ -> printf "Coverage information upload failed.\n"

run :: Text -> [Text] -> IO ExitCode
run cmd args = do
  printf (s % " " % s % "\n") cmd (T.unwords args)
  res <- proc cmd args empty
  case res of
    ExitSuccess      -> pure ()
    ExitFailure code -> eprintf
      ("error: Command exited with code " % d % "!\nContinuing...\n")
      code
  pure res
