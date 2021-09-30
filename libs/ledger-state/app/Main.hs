module Main where

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
  print opts
  --ref <- newIORef Nothing
  forM_ (optsUtxoJsonFile opts) $ \fp -> do
    utxo <- loadMassivUTxO fp
    utxo `seq` putStrLn "Loaded"
    printStats utxo
    performGC
    -- getChar
    --writeIORef ref $ Just utxo
    getChar
    ---collectStats fp
    -- -- putStrLn $ "Counted: " ++ show (length utxo) ++ " entries"
    --putStrLn $ "Total ADA: " ++ show (totalADA utxo) ++ " entries"
    -- collectStats fp


-- $ cabal build ledger-state && cabal exec -- ledger-state --utxo-json="/path/to/mainnet-utxo-2021-09-15.json" +RTS -s
