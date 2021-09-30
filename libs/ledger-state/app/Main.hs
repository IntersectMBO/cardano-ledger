module Main where

import Control.DeepSeq
import Options.Applicative
import System.IO
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Massiv
import Control.Monad
import Data.IORef
import System.Mem

data Opts = Opts
  { -- | Json file to import UTxO state from.
    optsUtxoJsonFile :: Maybe FilePath,
    -- | Path to Sqlite database file. Defaults to in memory.
    optsSqliteDbFile :: Maybe FilePath
  }
  deriving (Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <-
    execParser $
      info
        ( Opts
            <$> option
              (Just <$> str)
              ( long "utxo-json"
                  <> value Nothing
                  <> help "Json file to import UTxO state from"
              )
            <*> option
              (Just <$> str)
              ( long "sqlite-db"
                  <> value Nothing
                  <> help "Path to Sqlite database file. Default is in memory"
              )
            <* abortOption
              (ShowHelpText Nothing)
              (long "help" <> short 'h' <> help "Display this message.")
        )
        (header "ledger-state - Tool for analyzing ledger state")
  forM_ (optsUtxoJsonFile opts) $ \fp -> do
    _ <- observeMemory fp
    pure ()
    -- getChar
    -- ---collectStats fp
    -- -- -- putStrLn $ "Counted: " ++ show (length utxo) ++ " entries"
    -- --putStrLn $ "Total ADA: " ++ show (totalADA utxo) ++ " entries"
    -- -- collectStats fp

observeMemoryOriginalMap fp = do
  ref <- newIORef Nothing
  utxo <- loadUTxOn fp
  utxo `seq` putStrLn "Loaded"
  performGC
  _ <- getChar
  writeIORef ref $ Just utxo -- ensure utxo doesn't GCed
  pure ref


observeMemory :: FilePath -> IO (IORef (Maybe UTxOs))
observeMemory fp = do
  ref <- newIORef Nothing
  utxo <- loadMassivUTxO fp
  utxo `deepseq` putStrLn "Loaded"
  performGC
  _ <- getChar
  printStats utxo
  pure ref

testRoundTrip :: [Char] -> IO ()
testRoundTrip fp = do
  putStrLn $ "Loading file: " <> fp
  utxoOriginalMap <- loadUTxO fp
  putStrLn "Loaded"
  utxo <- utxoFromMap utxoOriginalMap
  putStrLn "Converted"
  testMassivUTxO utxoOriginalMap utxo
  putStrLn "Tested"


-- $ cabal build ledger-state && cabal exec -- ledger-state --utxo-json="/path/to/mainnet-utxo-2021-09-15.json" +RTS -s
