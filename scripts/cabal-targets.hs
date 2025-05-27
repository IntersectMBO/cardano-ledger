#!/usr/bin/env -S cabal run -v0 --
{- cabal:
  build-depends:
    base, cabal-plan, containers, directory,
    optparse-applicative, terminal-size, text
  ghc-options: -Wall -Wcompat -Wunused-packages
-}
{- project:
  allow-newer: cabal-plan:*
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Cabal.Plan
import Data.Bool (bool)
import Data.Char (toLower, toUpper)
import Data.List (intercalate, stripPrefix, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import qualified System.Console.Terminal.Size as TS
import System.Directory (doesDirectoryExist)
import System.Exit (die)
import Text.Read (readMaybe)

data Options = Options
  { optProjectDir :: FilePath
  , optCompTypes :: [CompType]
  , optPackages :: [Text]
  }
  deriving (Show)

parseArgs :: IO Options
parseArgs = do
  cols <- maybe 100 TS.width <$> TS.size

  customExecParser
    (prefs $ columns cols)
    ( info
        ( helper <*> do
            project <-
              strOption $
                short 'p'
                  <> long "project"
                  <> metavar "DIR"
                  <> value "."
                  <> help "The project directory, or a subdirectory of it"
                  <> showDefaultWith id
            include <-
              many . option readType $
                short 'i'
                  <> long "include"
                  <> metavar "TYPE"
                  <> help (inExHelp "Include")
            exclude <-
              many . option readType $
                short 'x'
                  <> long "exclude"
                  <> metavar "TYPE"
                  <> help (inExHelp "Exclude")
            packages <-
              many . strArgument $
                metavar "PACKAGE ..."
                  <> help "Show targets for PACKAGE ... (default: all packages)"
            pure
              Options
                { optProjectDir = project
                , optCompTypes = (if null include then allCompTypes else include) \\ exclude
                , optPackages = packages
                }
        )
        (fullDesc <> header "List the targets in a Cabal project")
    )
  where
    inExHelp op =
      op
        <> " targets of type TYPE (repeatable; one of: "
        <> intercalate ", " (map showType allCompTypes)
        <> ")"
    readType :: ReadM CompType
    readType = maybeReader $ readMaybe . ("CompType" <>) . initial toUpper
    showType :: CompType -> String
    showType = initial toLower . stripPrefix' "CompType" . show
    stripPrefix' p s = fromMaybe s $ stripPrefix p s
    initial f (c : s) = f c : s
    initial _ s = s

main :: IO ()
main = do
  Options {..} <- parseArgs

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= bool (die $ "Project directory " <> optProjectDir <> " doesn't exist") (pure ())

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  Text.putStr . Text.unlines $
    [ dispCompNameTargetFull (pIdName . uPId $ u) c
    | u <- Map.elems $ pjUnitsWithType UnitTypeLocal plan
    , c <- Map.keys $ uComps u
    , compType c `elem` optCompTypes
    , null optPackages || (unPkgName . pIdName . uPId) u `elem` optPackages
    ]

data CompType
  = CompTypeLib
  | CompTypeFlib
  | CompTypeExe
  | CompTypeTest
  | CompTypeBench
  | CompTypeSetup
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

compType :: CompName -> CompType
compType = \case
  CompNameLib -> CompTypeLib
  CompNameSubLib _ -> CompTypeLib
  CompNameFLib _ -> CompTypeFlib
  CompNameExe _ -> CompTypeExe
  CompNameTest _ -> CompTypeTest
  CompNameBench _ -> CompTypeBench
  CompNameSetup -> CompTypeSetup

allCompTypes :: [CompType]
allCompTypes = [minBound .. maxBound]

dispCompNameTargetFull :: PkgName -> CompName -> Text
dispCompNameTargetFull p c = unPkgName p <> ":" <> dispCompNameTarget p c

pjUnitsWithType :: UnitType -> PlanJson -> Map.Map UnitId Unit
pjUnitsWithType t = Map.filter ((t ==) . uType) . pjUnits

pIdName :: PkgId -> PkgName
pIdName (PkgId name _) = name

unPkgName :: PkgName -> Text
unPkgName (PkgName name) = name
