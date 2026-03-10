{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Crypto.Hash (Blake2b_256)
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Ledger.Shelley.Genesis (InjectionData (..), foldInjectionData)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getTemporaryDirectory,
  removeDirectoryRecursive,
 )
import System.Exit (exitFailure)
import System.FS.API (mkFsPath)
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.FilePath ((</>))
import Weigh (Column (..), Grouped (..), Weight (..), io, setColumns, weighResults)

-- | This tests verifies that 'foldInjectionData' can stream a large JSON file
-- in constant space: max residency must stay under 1MB regardless of file size.
main :: IO ()
main = do
  sodiumInit
  dir <- benchDirPath
  generateJsonFile dir

  (results, _) <- weighResults $ do
    setColumns [Case, Max, Allocated, GCs]
    io "InjectionFromFile" benchStreaming (dir, fileName, expectedHash)

  -- Cleanup temp dir
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir

  let weights = flattenResults results
      failures =
        filter (\(Weight {weightMaxBytes}, _) -> weightMaxBytes > maxResidencyBytes) weights

  mapM_
    ( \(w, _) ->
        putStrLn $
          weightLabel w
            <> ": max="
            <> show (weightMaxBytes w)
            <> " allocated="
            <> show (weightAllocatedBytes w)
    )
    weights

  unless (null failures) $ do
    putStrLn ""
    mapM_
      ( \(Weight {weightLabel, weightMaxBytes}, _) ->
          putStrLn $
            "FAIL: "
              <> weightLabel
              <> " max residency "
              <> show weightMaxBytes
              <> " bytes > threshold "
              <> show maxResidencyBytes
              <> " bytes"
      )
      failures
    exitFailure
  where
    -- 1MB — streaming should never need more regardless of file size
    maxResidencyBytes :: Word64
    maxResidencyBytes = 1024 * 1024

    entryCount :: Int
    entryCount = 1_000_000

    fileName :: String
    fileName = "injection-test.json"

    -- Precomputed Blake2b-256 hash of the generated JSON file.
    -- Recompute with: Hash.hashWith @Blake2b_256 id <$> BS.readFile "injection-test.json"
    expectedHash :: Hash.Hash Blake2b_256 BS.ByteString
    expectedHash = case Hash.hashFromTextAsHex "106ca86c08ac211e069daf1f66cc04f8d82b17b37f272bd50b21ff774d884cf8" of
      Just h -> h
      Nothing -> error "invalid hardcoded hash"

    benchDirPath :: IO FilePath
    benchDirPath = do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "cardano-shelley-injection-memory-test"
      createDirectoryIfMissing True dir
      pure dir

    generateJsonFile :: FilePath -> IO ()
    generateJsonFile dir = do
      let fp = dir </> fileName
      exists <- doesFileExist fp
      unless exists $ do
        let obj :: Map.Map Text Text
            obj =
              Map.fromList
                [ (Text.pack ("key_" <> show i), Text.pack ("val_" <> show i))
                | i <- [1 .. entryCount]
                ]
        BSL.writeFile fp (Aeson.encode obj)

    -- Fold over streaming file, counting entries. Accumulator is just an Int.
    benchStreaming :: (FilePath, String, Hash.Hash Blake2b_256 BS.ByteString) -> IO Int
    benchStreaming (dir, fn, fileHash) =
      foldInjectionData
        (ioHasFS (MountPoint dir))
        (InjectionFromFile (mkFsPath [fn]) fileHash :: InjectionData Text Text)
        (\(!acc) _ -> acc + 1)
        0

    flattenResults :: [Grouped (Weight, Maybe String)] -> [(Weight, Maybe String)]
    flattenResults = concatMap go
      where
        go (Singleton x) = [x]
        go (Grouped _ xs) = concatMap go xs
