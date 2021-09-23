{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Mirror
  ( mainnetEpochFiles,
  )
where

import Cardano.Prelude
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (lookupEnv)
import System.FilePath (isExtensionOf, (</>))

-- Failing here (with 'exitFailure') is fine because this function is only ever
-- used to test maiinnet validaton. It is never used in production code.
mainnetEpochFiles :: IO [FilePath]
mainnetEpochFiles =
  lookupEnv "CARDANO_MAINNET_MIRROR" >>= \case
    Nothing -> do
      putStrLn "mainnetEpochFiles: CARDANO_MAINNET_MIRROR variable is not set"
      exitFailure
    Just fpath -> do
      exists <- doesDirectoryExist fpath
      if exists
        then
          sort
            . fmap (fpath </>)
            . filter ("epoch" `isExtensionOf`)
            <$> getDirectoryContents fpath
        else do
          putStrLn $
            "mainnetEpochFiles: directory '"
              ++ fpath
              ++ "' does not exist."
          exitFailure
