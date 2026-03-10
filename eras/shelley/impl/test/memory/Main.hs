{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Cardano.Crypto.Hash (Blake2b_256)
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Ledger.Shelley.Genesis (InjectionData (..), foldInjectionData)
import Control.Monad (forM_, unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FS.API (mkFsPath)
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Weigh (Column (..), Grouped (..), Weight (..), io, setColumns, weighResults)

-- | This tests verifies that 'foldInjectionData' can stream a large JSON file
-- in constant space: max residency must stay under 1MB regardless of file size.
main :: IO ()
main = do
  sodiumInit
  results <- withSystemTempDirectory "cardano-shelley-injection-memory-test" $ \dir -> do
    generateJsonFile dir
    fmap fst $ weighResults $ do
      setColumns [Case, Max, Allocated, GCs]
      io "InjectionFromFile" benchStreaming (dir, fileName, expectedHash)

  let weights = flattenResults results
      failures =
        filter (\Weight {weightMaxBytes} -> weightMaxBytes > maxResidencyBytes) weights

  forM_ weights $ \w ->
    putStrLn $
      weightLabel w
        <> ": max="
        <> show (weightMaxBytes w)
        <> " allocated="
        <> show (weightAllocatedBytes w)

  unless (null failures) $ do
    putStrLn ""
    forM_ failures $ \Weight {weightLabel, weightMaxBytes} ->
      putStrLn $
        "FAIL: "
          <> weightLabel
          <> " max residency "
          <> show weightMaxBytes
          <> " bytes > threshold "
          <> show maxResidencyBytes
          <> " bytes"
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
    expectedHash = $$"106ca86c08ac211e069daf1f66cc04f8d82b17b37f272bd50b21ff774d884cf8"

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
        (\acc !_ -> acc + 1)
        0

    flattenResults :: [Grouped (Weight, Maybe String)] -> [Weight]
    flattenResults = concatMap go
      where
        go (Singleton (w, _)) = [w]
        go (Grouped _ xs) = concatMap go xs
