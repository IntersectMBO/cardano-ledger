{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Cabal.Plan
import Control.Monad (guard, unless)
import Data.Aeson qualified as JSON
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Traversable (for)
import LogResults
import Options.Applicative hiding (Failure)
import Parse
import System.Console.Terminal.Size qualified as TS
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optVerbosity :: Int
  , optProjectDir :: FilePath
  , optOutput :: FilePath
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  let counter = fmap length . many . flag' ()

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optVerbosity <-
                counter $
                  help "Increase output verbosity (repeatable)"
                    <> short 'v'
                    <> long "verbose"
              optProjectDir <-
                strOption $
                  help "The project directory, or a subdirectory of it"
                    <> short 'p'
                    <> long "project"
                    <> metavar "DIR"
                    <> value "."
                    <> showDefaultWith id
              optOutput <-
                strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
                    <> value "/dev/stdout"
                    <> showDefaultWith id
              pure Options {..}
          )
          (fullDesc <> header "Extract failure information from Cabal test logs")
      )

  let trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= (`unless` die ("Project directory " <> optProjectDir <> " doesn't exist"))

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  let
    targetLogs = do
      -- List monad
      unit <- Map.elems $ pjUnits plan
      guard $ uType unit == UnitTypeLocal
      Just dir <- [uDistDir unit]
      comp@(CompNameTest tName) <- Map.keys (uComps unit)
      let
        pId = uPId unit
        PkgId pName _ = pId
        PkgName name = pName
        target = name <> ":" <> dispCompNameTarget pName comp
        file = dir </> "test" </> T.unpack (dispPkgId pId <> "-" <> tName) <.> "log"
      pure (target, file)

  trace 1 $ show (length targetLogs) <> " Cabal targets found"

  targetFailures <-
    for targetLogs $ \(target, file) -> do
      exists <- doesFileExist file
      failures <-
        if exists
          then do
            trace 2 $ "Examining " <> file
            parseLog file
          else
            pure mempty
      pure (target, failures)

  let logResults = Map.fromList $ filter (not . null . snd) targetFailures

  trace 1 $ show (Map.size logResults) <> " logs with failures found"

  JSON.encodeFile @LogResults optOutput logResults
