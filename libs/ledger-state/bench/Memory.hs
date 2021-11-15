{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Ledger.State.Query
--import Cardano.Ledger.State.UTxO
import Control.Monad
import qualified Data.Text as T
import Options.Applicative as O
import Weigh

data Opts = Opts
  { -- | Path to the CBOR encoded NewEpochState data type, which will be used to
    -- load into sqlite database
    optsLedgerStateBinaryFile :: Maybe FilePath,
    -- | Path to Sqlite database file.
    optsSqliteDbFile :: Maybe FilePath
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> option
      (Just <$> str)
      ( long "new-epoch-state-cbor"
          <> O.value Nothing
          <> help
            ("Benchmark loading CBOR encoded NewEpochState into memory.")
      )
    <*> option
      (Just <$> str)
      ( long "new-epoch-state-sqlite"
          <> O.value Nothing
          <> help
            ("Run various benchmarks on LedgerState representations")
      )

main :: IO ()
main = do
  opts <-
    execParser $
      info
        ( optsParser
            <* abortOption
              (ShowHelpText Nothing)
              (long "help" <> short 'h' <> help "Display this message.")
        )
        (header "ledger-state:memory - Tool for analyzing memory consumption of ledger state")
  let cols = [Case, Max, MaxOS, Live, Allocated, GCs, WallTime]
  !mEpochStateEntity <- mapM (loadEpochStateEntity . T.pack) (optsSqliteDbFile opts)
  mainWith $ do
    setColumns cols
    -- forM_ (optsLedgerStateBinaryFile opts) $ \binFp -> do
    --   io "NewEpochState" loadNewEpochState binFp
    forM_ (optsSqliteDbFile opts) $ \dbFpStr -> do
      let dbFp = T.pack dbFpStr
      forM_ mEpochStateEntity $ \_ese ->
        -- wgroup "EpochState" $ do
        --   io "SnapShots - no sharing" (loadSnapShotsNoSharingM dbFp) _ese
        --   io "SnapShots - with sharing" (loadSnapShotsWithSharingM dbFp) _ese
        --   io "SnapShots (Vector) - no sharing" (loadSnapShotsNoSharing dbFp) _ese
        --   io "SnapShots (Vector) - with sharing" (loadSnapShotsWithSharing dbFp) _ese
        wgroup "DState+UTxO" $ do
          io "IntMap (KeyMap TxId TxOut) (original)" getLedgerStateNoSharingOrigKeyMap' dbFp
          io "IntMap (KeyMap TxId TxOut)" getLedgerStateNoSharingKeyMap dbFp
          io "IntMap (KeyMap TxId TxOut) (sharing)" getLedgerStateWithSharingKeyMap dbFp
          io "IntMap (KeyMap TxId TxOut')" getLedgerStateNoSharingKeyMap' dbFp
          io "IntMap (KeyMap TxId TxOut') (sharing)" getLedgerStateWithSharingKeyMap' dbFp

-- io "KeyMap TxId (IntMap TxOut)" getLedgerStateDStateTxIdSharingKeyMap dbFp
-- io "IntMap (Map TxId TxOut)" getLedgerStateDStateTxIxSharing dbFp
-- io "Map TxIn TxOut" getLedgerStateDStateSharing dbFp

-- wgroup "Baseline" $ do
--   io "DState" loadDStateNoSharing dbFp
--   io "UTxO" loadUTxONoSharing dbFp
--   io "LedgerState" getLedgerStateNoSharing dbFp
-- wgroup "UTxO (No TxOut)" $ do
--   io "IntMap (KeyMap TxId ())" (loadDbUTxO txIxSharingKeyMap_) dbFp
--   io "KeyMap TxId (IntMap TxId ())" (loadDbUTxO txIdSharingKeyMap_) dbFp
--   io "IntMap (Map TxId ())" (loadDbUTxO txIxSharing_) dbFp
--   io "Map TxIn ()" (loadDbUTxO noSharing_) dbFp
-- wgroup "LedgerState" $ do
--   wgroup "UTxO (Share DState)" $ do
--     io "IntMap (KeyMap TxId TxOut)" getLedgerStateDStateTxIxSharingKeyMap dbFp
--     io "KeyMap TxId (IntMap TxOut)" getLedgerStateDStateTxIdSharingKeyMap dbFp
--     io "IntMap (Map TxId TxOut)" getLedgerStateDStateTxIxSharing dbFp
--     io "Map TxIn TxOut" getLedgerStateDStateSharing dbFp

--   wgroup "Share TxOut StakeCredential" $ do
--     io "Map TxIn TxOut'" getLedgerStateDStateTxOutSharing dbFp
-- wgroup "Share TxOut StakeCredential" $ do
--   io "Map TxIn TxOut'" getLedgerStateTxOutSharing dbFp
-- wgroup "No Sharing" $ do
-- wgroup "Share TxOut StakeCredential" $ do
--   io "IntMap (KeyMap TxId TxOut')" getLedgerStateWithSharingKeyMap dbFp
--   io "IntMap (Map TxId TxOut')" getLedgerStateWithSharing dbFp
