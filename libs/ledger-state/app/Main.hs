{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State.Massiv
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Query
import Control.Monad
import Data.IORef
import Options.Applicative
import System.IO
import System.Mem

data Opts = Opts
  { -- | Json file to use UTxO state from.
    optsUtxoJsonFile :: Maybe FilePath,
    -- | Path to the CBOR encoded NewEpochState data type, which will be used to
    -- load into sqlite database
    optsLedgerStateBinaryFile :: Maybe FilePath,
    -- | Path to Sqlite database file.
    optsSqliteDbFile :: Maybe FilePath
  }
  deriving (Show)


optsParser :: Parser Opts
optsParser =
  Opts <$>
  option
    (Just <$> str)
    (long "utxo-json" <>
     value Nothing <>
     help
       "Json file with UTxO. It will be loaded into memory and stats printed to stdout.") <*>
  option
    (Just <$> str)
    (long "new-epoch-state-cbor" <>
     value Nothing <>
     help
       ("Path to the CBOR encoded NewEpochState data type. " <>
        "Can be produced by `cardano-cli query ledger-state` command. " <>
        "When supplied stats about the state will be printed to stdout")) <*>
  option
    (Just <$> str)
    (long "new-epoch-state-sqlite" <>
     value Nothing <>
     help
       ("Path to Sqlite database file. When supplied then new-epoch-state " <>
        "will be loaded into the databse. Requires --new-epoch-state-cbor"))


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <-
    execParser $
    info
      (optsParser <*
       abortOption
         (ShowHelpText Nothing)
         (long "help" <> short 'h' <> help "Display this message."))
      (header "ledger-state - Tool for analyzing ledger state")
  forM_ (optsLedgerStateBinaryFile opts) $ \binFp -> do
    ls <- loadNewEpochState binFp
    forM_ (optsSqliteDbFile opts) $ \dbFp -> do
      storeLedgerState dbFp $ esLState $ nesEs ls
      putStrLn "Loaded LedgerState into the database"
    printNewEpochStateStats $ countNewEpochStateStats ls
  -- forM_ (optsUtxoJsonFile opts) $ \fp -> do
  --   _ <- observeMemoryOriginalMap fp
  --   pure ()

-- getChar
-- ---collectStats fp
-- -- -- putStrLn $ "Counted: " ++ show (length utxo) ++ " entries"
-- --putStrLn $ "Total ADA: " ++ show (totalADA utxo) ++ " entries"
-- -- collectStats fp

-- observeMemoryOriginalMap fp = do
--   ref <- newIORef Nothing
--   utxo <- loadUTxOihm' fp
--   utxo `seq` putStrLn "Loaded"
--   performGC
--   _ <- getChar
--   writeIORef ref $ Just utxo -- ensure utxo doesn't get GCed
--   pure ref

-- --observeMemory :: FilePath -> IO (IORef (Maybe UTxOs))
-- observeMemory fp = do
--   ref <- newIORef Nothing
--   utxo <- loadMassivUTxO fp
--   utxo `seq` putStrLn "Loaded"
--   performGC
--   _ <- getChar
--   writeIORef ref $ Just utxo -- ensure utxo doesn't get GCed
--   pure ref

-- testRoundTrip :: [Char] -> IO ()
-- testRoundTrip fp = do
--   putStrLn $ "Loading file: " <> fp
--   utxoOriginalMap <- loadUTxO fp
--   putStrLn "Loaded"
--   utxo <- utxoFromMap utxoOriginalMap
--   putStrLn "Converted"
--   testMassivUTxO utxoOriginalMap utxo
--   putStrLn "Tested"

-- $ cabal build ledger-state && cabal exec -- ledger-state --utxo-json="/path/to/mainnet-utxo-2021-09-15.json" +RTS -s
