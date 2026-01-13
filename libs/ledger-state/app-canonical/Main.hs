{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.State.Query (queryConstitution, queryProposals)
import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.SCLS
import Cardano.Ledger.Conway.SCLS.Namespace.GovProposals as Proposals
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.TxOut as Shelley ()
import Cardano.Ledger.State.UTxO
import Cardano.SCLS.Internal.Entry.ChunkEntry
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Serializer.Dump.Plan
import qualified Cardano.SCLS.Internal.Serializer.External.Impl as External (serialize)
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import Control.Exception (throwIO)
import Control.Monad
import Data.Bifunctor (first)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.VMap as VMap
import Lens.Micro
import Options.Applicative
import qualified Streaming.Prelude as S
import System.IO

-- | Insight into options:
--
-- * `optsNewEpochStateBinaryFile` is for reading a previously serialized
-- * `NewEpochState` produced by cardano-cli` and is used to populate sqlite
-- * database
--
-- * `optsEpochStateBinaryFile` is used for grabbing data from sqlite,
-- * constructing `EpochState` (in a new format) and writing it into the cbor
-- * serialized file
data Opts = Opts
  { optsNewEpochStateBinaryFile :: Maybe FilePath
  -- ^ Path to the CBOR encoded NewEpochState data type, which will be used to
  -- load into sqlite database
  , optsEpochStateBinaryFile :: Maybe FilePath
  -- ^ Path to the CBOR encoded EpochState data type, which will have data
  -- from sqlite database written into it.
  , optsSqliteDbFile :: Maybe FilePath
  -- ^ Path to Sqlite database file.
  }
  deriving (Show)

data Cmd
  = CmdCreateStateFile FilePath FilePath FilePath
  | Cat FilePath
  | Tee FilePath FilePath
  deriving (Show)

optsParser :: Parser Cmd
optsParser =
  hsubparser
    ( command "create" (info createStateCommand (progDesc "Create canonical file for ledger state"))
        <> command "cat" (info catCommand (progDesc "Display contents of canonical ledger state file"))
        <> command
          "tee"
          ( info
              teeCommand
              (progDesc "Display contents of canonical ledger state file and store it to the new one")
          )
    )
  where
    createStateCommand =
      CmdCreateStateFile
        <$> argument str (metavar "STATE_BIN_FILE")
        <*> argument str (metavar "UTXO_HEX_FILE")
        <*> argument str (metavar "SCLS_FILE")
    catCommand = Cat <$> argument str (metavar "SCLS_FILE")
    teeCommand =
      Tee
        <$> argument str (metavar "SCLS_FILE")
        <*> argument str (metavar "SCLS_FILE")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cmd <-
    execParser $
      info
        ( optsParser
            <* abortOption
              (ShowHelpText Nothing)
              (long "help" <> short 'h' <> help "Display this message.")
        )
        (header "canonical-state - Tool for working with canonical ledger state representation")
  case cmd of
    Cat fileName -> do
      putStrLn $ "Reading canonical state from " ++ fileName
      withKnownNamespacedData fileName (Proxy @"utxo/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"blocks/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"gov/committee/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"gov/constitution/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"gov/pparams/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"gov/proposals/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"pool_stake/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"pots/v0") $ \stream ->
        S.print stream
      withKnownNamespacedData fileName (Proxy @"snapshots/v0") $ \stream ->
        S.print stream
    Tee oldFile newFile -> do
      withFile oldFile ReadMode $ \hdl -> do
        External.serialize
          newFile
          Mainnet
          (SlotNo 1)
          ( defaultSerializationPlan
              & ( let p = (Proxy @"utxo/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"blocks/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"gov/committee/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"gov/constitution/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"gov/pparams/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"gov/proposals/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"pool_stake/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"pots/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
              & ( let p = (Proxy @"snapshots/v0")
                   in addNamespacedChunks p (knownNamespacedData hdl p & S.duplicate & S.print)
                )
          )
    CmdCreateStateFile stateFilePath utxoFilePath fileName -> do
      putStrLn "Creating state file..."
      putStrLn $ "Reading State from " ++ stateFilePath
      nes <- readNewEpochState stateFilePath
      UTxO utxo0 <- localReadDecCBORHex utxoFilePath
      let epoch = nesEL nes
      print epoch
      External.serialize
        fileName
        Mainnet
        (SlotNo 1)
        ( defaultSerializationPlan
            & addNamespacedChunks
              (Proxy @"utxo/v0")
              ( S.each
                  [ ChunkEntry (UtxoKeyIn txin) (UtxoOutBabbage txout)
                  | (txin, txout) <- Map.toList utxo0
                  ]
              )
            & addNamespacedChunks
              (Proxy @"blocks/v0")
              ( S.each
                  [ ChunkEntry (BlockIn (key_hash, pred epoch)) (BlockOut natural)
                  | let BlocksMade mpPrev = nesBprev nes
                  , (key_hash, natural) <- Map.toList mpPrev
                  ]
                  <> S.each
                    [ ChunkEntry (BlockIn (key_hash, epoch)) (BlockOut natural)
                    | let BlocksMade mpCurrent = nesBcur nes
                    , (key_hash, natural) <- Map.toList mpCurrent
                    ]
              )
            & addNamespacedChunks
              (Proxy @"pots/v0")
              ( S.each
                  [ ChunkEntry
                      (PotsIn epoch)
                      PotsOut
                        { poFee = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosFeesL
                        , poDeposit = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
                        , poDonation = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosDonationL
                        , poReserves = nes ^. nesEsL . to (esChainAccountState) . casReservesL
                        , poTreasury = nes ^. nesEsL . to (esChainAccountState) . casReservesL
                        }
                  ]
              )
            & addNamespacedChunks
              (Proxy @"snapshots/v0")
              ( S.each
                  [ ChunkEntry
                      (SnapShotInCred SnapShotStageSet cred SnapShotValueAddress)
                      (SnapShotOutAddress stakeHash)
                  | (cred, stakeHash) <-
                      nes ^. nesEsL . esSnapshotsL . ssStakeSetL . ssDelegationsL . to (VMap.toList)
                  ]
                  <> S.each
                    [ ChunkEntry
                        (SnapShotInKey SnapShotStageSet poolHash SnapShotValuePoolParams)
                        (SnapShotOutPoolParams poolParams)
                    | (poolHash, poolParams) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeSetL . ssPoolParamsL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry (SnapShotInCred SnapShotStageSet poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                    | (poolHash, fromCompact -> coin) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeSetL . ssStakeDistrL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry
                        (SnapShotInCred SnapshotStageMark cred SnapShotValueAddress)
                        (SnapShotOutAddress stakeHash)
                    | (cred, stakeHash) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeMarkL . ssDelegationsL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry
                        (SnapShotInKey SnapshotStageMark poolHash SnapShotValuePoolParams)
                        (SnapShotOutPoolParams poolParams)
                    | (poolHash, poolParams) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeMarkL . ssPoolParamsL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry (SnapShotInCred SnapshotStageMark poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                    | (poolHash, fromCompact -> coin) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeMarkL . ssStakeDistrL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry (SnapShotInCred SnapshotStageGo poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                    | (poolHash, fromCompact -> coin) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeGoL . ssStakeDistrL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry (SnapShotInCred SnapshotStageGo cred SnapShotValueAddress) (SnapShotOutAddress stakeHash)
                    | (cred, stakeHash) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeGoL . ssDelegationsL . to (VMap.toList)
                    ]
                  <> S.each
                    [ ChunkEntry (SnapShotInCred SnapshotStageGo poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                    | (poolHash, fromCompact -> coin) <-
                        nes ^. nesEsL . esSnapshotsL . ssStakeGoL . ssStakeDistrL . to (VMap.toList)
                    ]
              )
            & addNamespacedChunks
              (Proxy @"pool_stake/v0")
              ( S.each
                  [ ChunkEntry
                      (PoolStakeIn cred)
                      (PoolStakeOut (fromCompact individualTotalPoolStake) individualPoolStakeVrf)
                  | (cred, IndividualPoolStake {..}) <- nes ^. nesPdL . poolDistrDistrL . to (Map.toList)
                  ]
              )
            & addNamespacedChunks
              (Proxy @"gov/pparams/v0")
              ( S.each
                  [ ChunkEntry k (GovPParamsOut v)
                  | Just (k, v) <-
                      [ Just (GovPParamsInCurr, nes ^. nesEpochStateL . curPParamsEpochStateL)
                      , Just (GovPParamsInPrev, nes ^. nesEpochStateL . prevPParamsEpochStateL)
                      , case nes ^. nesEpochStateL . futurePParamsEpochStateL of
                          NoPParamsUpdate -> Nothing
                          DefinitePParamsUpdate pp -> Just (GovPParamsInDefiniteFuture, pp)
                          PotentialPParamsUpdate mp -> (GovPParamsInPossibleFuture,) <$> mp
                      ]
                  ]
              )
            & addNamespacedChunks
              (Proxy @"gov/constitution/v0")
              ( S.each
                  [ ChunkEntry (GovConstitutionIn epoch) (mkCanonicalConstitution $ nes & queryConstitution)
                  ]
              )
            & addNamespacedChunks
              (Proxy @"gov/proposals/v0")
              ( S.each
                  [ ChunkEntry (GovProposalIn gasId) (GovProposalOut $ Proposals.toWire g)
                  | let proposals = nes & flip queryProposals Set.empty
                  , g@GovActionState {..} <- toList proposals
                  ]
              )
            & addNamespacedChunks
              (Proxy @"gov/committee/v0")
              ( S.each
                  [ ChunkEntry (GovCommitteeIn epoch) (GovCommitteeOut cms)
                  | let cms = nes ^. nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
                  ]
              )
        )

localReadDecCBORHex :: FilePath -> IO (UTxO ConwayEra)
localReadDecCBORHex = either throwIO pure . decodeFullHex <=< LBS.readFile
  where
    decodeFullHex =
      Plain.decodeFull
        <=< first (DecoderErrorCustom "Invalid Hex encoding:" . T.pack) . Base16.decode
