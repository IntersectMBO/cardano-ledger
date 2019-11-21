{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | To test this you can run:
--
-- > nix-shell .buildkite --run "ghci .buildkite/rebuild.hs"
--
-- from the @skeleton@ folder.

import           Build (LibraryName (LibraryName), Optimizations (Standard),
                     ShouldUploadCoverage (ShouldUploadCoverage),
                     StackExtraTestArgs (StackExtraTestArgs), TestRun (TestRun), doBuild)
import           BuildArgs (BuildArgs (BuildArgs, command, options),
                     Command (Build, CleanupCache, PurgeCache),
                     RebuildOpts (RebuildOpts, optBuildDirectory, optCacheDirectory, optDryRun),
                     parseArgs)
import           CommonBuild (Bool (True), CoverallsConfig (CoverallsConfig),
                     CoverallsTokenEnvVar (CoverallsTokenEnvVar), ExtraShcArgs (ExtraShcArgs),
                     ExtraTixFilesDirectory (ExtraTixFilesDirectory), IO, const, ($))

import           Data.Maybe (fromMaybe)
import           System.Exit (exitWith)

import qualified Data.Text as T


main :: IO ()
main =
  doBuild
    (LibraryName "cardano-ledger-specs")
    Standard
    (ShouldUploadCoverage (const True))
    [TestRun $ StackExtraTestArgs $ const []]
    (CoverallsConfig
       (CoverallsTokenEnvVar "CARDANO_LEDGER_SPECS_COVERALLS_REPO_TOKEN")
       (ExtraShcArgs [])
       (ExtraTixFilesDirectory ".")
    )
