{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Query where

import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Credential as Credential
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Shelley.EpochBoundary as EpochBoundary
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.State.Orphans
import Cardano.Ledger.State.Schema
import Cardano.Ledger.State.Transform
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Vector
import qualified Cardano.Ledger.TxIn as TxIn
import Conduit
import Control.Foldl (Fold (..))
import Control.Monad
import Control.Monad.Trans.Reader
import qualified Data.Compact.KeyMap as KeyMap
import qualified Data.Compact.ViewMap as VMap
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List (sourceList)
import Data.Functor
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Sharing
import qualified Data.Text as T
import Data.UMap (delView, ptrView, rewView, unify)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Database.Persist.Sqlite

-- Populate database

insertGetKey ::
  ( MonadIO m,
    PersistUniqueWrite backend,
    PersistRecordBackend record backend,
    AtLeastOneUniqueKey record
  ) =>
  record ->
  ReaderT backend m (Key record)
insertGetKey = fmap (either entityKey id) . insertBy

insertUTxOState ::
  MonadIO m =>
  Shelley.UTxOState CurrentEra ->
  ReaderT SqlBackend m (Key UtxoState)
insertUTxOState Shelley.UTxOState {..} = do
  insert $
    UtxoState
      { utxoStateDeposited = _deposited,
        utxoStateFees = _fees,
        utxoStatePpups = _ppups
      }

insertUTxO ::
  MonadIO m =>
  Shelley.UTxO CurrentEra ->
  Key UtxoState ->
  ReaderT SqlBackend m ()
insertUTxO utxo stateKey = do
  mapM_ insertTxOut $ Map.toList (Shelley.unUTxO utxo)
  where
    insertTxOut (TxIn.TxIn txId txIx, out) = do
      txKey <-
        insert $ Tx {txInIx = fromIntegral txIx, txInId = txId, txOut = out}
      txsKey <-
        insert $
          Txs
            { txsInIx = fromIntegral txIx,
              txsInId = txId,
              txsOut = out,
              txsStakeCredential = Nothing
            }
      insert_ $
        UtxoEntry
          { utxoEntryTxId = txKey,
            utxoEntryTxsId = txsKey,
            utxoEntryStateId = stateKey
          }

insertDState :: MonadIO m => Shelley.DState C -> ReaderT SqlBackend m DStateId
insertDState Shelley.DState {..} = do
  let irDeltaReserves = Shelley.deltaReserves _irwd
  let irDeltaTreasury = Shelley.deltaTreasury _irwd
  dstateId <- insert $ DState (Enc _fGenDelegs) _genDelegs irDeltaReserves irDeltaTreasury
  forM_ (Map.toList (rewView _unified)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (Reward dstateId credId c)
  forM_ (Map.toList (delView _unified)) $ \(cred, spKeyHash) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    keyHashId <- insertGetKey (KeyHash (Keys.asWitness spKeyHash))
    insert_ (Delegation dstateId credId keyHashId)
  forM_ (Map.toList (ptrView _unified)) $ \(ptr, cred) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (Ptr dstateId credId ptr)
  forM_ (Map.toList (Shelley.iRReserves _irwd)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (IRReserves dstateId credId c)
  forM_ (Map.toList (Shelley.iRTreasury _irwd)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (IRTreasury dstateId credId c)
  pure dstateId

insertLedgerState ::
  MonadIO m => EpochStateId -> Shelley.LedgerState CurrentEra -> ReaderT SqlBackend m ()
insertLedgerState epochStateKey Shelley.LedgerState {..} = do
  stateKey <- insertUTxOState _utxoState
  insertUTxO (Shelley._utxo _utxoState) stateKey
  dstateKey <- insertDState $ Shelley._dstate _delegationState
  insert_
    LedgerState
      { ledgerStateUtxoId = stateKey,
        ledgerStateDstateId = dstateKey,
        ledgerStatePstateBin = Shelley._pstate _delegationState,
        ledgerStateEpochStateId = epochStateKey
      }

insertSnapShot ::
  MonadIO m =>
  Key EpochState ->
  SnapShotType ->
  EpochBoundary.SnapShot C ->
  ReaderT SqlBackend m ()
insertSnapShot snapShotEpochStateId snapShotType EpochBoundary.SnapShot {..} = do
  snapShotId <- insert $ SnapShot {snapShotType, snapShotEpochStateId}
  VG.forM_ (VMap.unVMap (EpochBoundary.unStake _stake)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (SnapShotStake snapShotId credId c)
  VG.forM_ (VMap.unVMap _delegations) $ \(cred, spKeyHash) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    keyHashId <- insertGetKey (KeyHash (Keys.asWitness spKeyHash))
    insert_ (SnapShotDelegation snapShotId credId keyHashId)
  VG.forM_ (VMap.unVMap _poolParams) $ \(keyHash, pps) -> do
    keyHashId <- insertGetKey (KeyHash (Keys.asWitness keyHash))
    insert_ (SnapShotPool snapShotId keyHashId pps)

insertSnapShots ::
  MonadIO m =>
  Key EpochState ->
  EpochBoundary.SnapShots C ->
  ReaderT SqlBackend m ()
insertSnapShots epochStateKey EpochBoundary.SnapShots {..} = do
  mapM_
    (uncurry (insertSnapShot epochStateKey))
    [ (SnapShotMark, _pstakeMark),
      (SnapShotSet, _pstakeSet),
      (SnapShotGo, _pstakeGo)
    ]

insertEpochState ::
  MonadIO m => Shelley.EpochState CurrentEra -> ReaderT SqlBackend m ()
insertEpochState Shelley.EpochState {..} = do
  epochStateKey <-
    insert
      EpochState
        { epochStateTreasury = Shelley._treasury esAccountState,
          epochStateReserves = Shelley._reserves esAccountState,
          epochStatePrevPp = esPrevPp,
          epochStatePp = esPp,
          epochStateNonMyopic = esNonMyopic,
          epochStateSnapShotsFee = EpochBoundary._feeSS esSnapshots
        }
  insertSnapShots epochStateKey esSnapshots
  insertLedgerState epochStateKey esLState

-- Query database

-- Into vector

selectVMap ::
  ( Ord k,
    PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    VMap.Vector kv k,
    VMap.Vector vv v,
    MonadResource m
  ) =>
  [Filter record] ->
  (record -> ReaderT SqlBackend m (k, v)) ->
  ReaderT SqlBackend m (VMap.VMap kv vv k v)
selectVMap fs f = do
  n <- count fs
  mv <- liftIO $ VGM.unsafeNew n
  runConduit $
    zipSources (sourceList [0 ..]) (selectSource fs [])
      .| mapM_C (\(i, Entity _ a) -> liftIO . VGM.write mv i =<< f a)
  VMap.VMap <$> liftIO (VG.unsafeFreeze =<< VMap.normalizeM mv)
{-# INLINEABLE selectVMap #-}

getSnapShotNoSharingM ::
  MonadResource m =>
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m (SnapShotM C)
getSnapShotNoSharingM epochStateId snapShotType = do
  snapShotId <-
    selectFirst
      [SnapShotType ==. snapShotType, SnapShotEpochStateId ==. epochStateId]
      []
      <&> \case
        Nothing -> error $ "Missing a snapshot: " ++ show snapShotType
        Just (Entity snapShotId _) -> snapShotId
  stake <-
    selectMap [SnapShotStakeSnapShotId ==. snapShotId] $ \SnapShotStake {..} -> do
      Credential credential <- getJust snapShotStakeCredentialId
      pure (Keys.coerceKeyRole credential, snapShotStakeCoin)
  delegations <-
    selectMap [SnapShotDelegationSnapShotId ==. snapShotId] $ \SnapShotDelegation {..} -> do
      Credential credential <- getJust snapShotDelegationCredentialId
      KeyHash keyHash <- getJust snapShotDelegationKeyHash
      --TODO ^ rename snapShotDelegationKeyHashId
      pure (Keys.coerceKeyRole credential, Keys.coerceKeyRole keyHash)
  poolParams <-
    selectMap [SnapShotPoolSnapShotId ==. snapShotId] $ \SnapShotPool {..} -> do
      KeyHash keyHash <- getJust snapShotPoolKeyHashId
      pure (Keys.coerceKeyRole keyHash, snapShotPoolParams)
  pure
    SnapShotM
      { ssStake = stake,
        ssDelegations = delegations,
        ssPoolParams = poolParams
      }
{-# INLINEABLE getSnapShotNoSharingM #-}

getSnapShotWithSharingM ::
  MonadResource m =>
  [SnapShotM C] ->
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m (SnapShotM C)
getSnapShotWithSharingM otherSnapShots epochStateId snapShotType = do
  let internOtherStakes =
        interns
          (foldMap (internsFromMap . ssStake) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherPoolParams =
        interns (foldMap (internsFromMap . ssPoolParams) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherDelegations =
        interns (foldMap (internsFromMap . ssDelegations) otherSnapShots)
          . Keys.coerceKeyRole
  snapShotId <-
    selectFirst
      [SnapShotType ==. snapShotType, SnapShotEpochStateId ==. epochStateId]
      []
      <&> \case
        Nothing -> error $ "Missing a snapshot: " ++ show snapShotType
        Just (Entity snapShotId _) -> snapShotId
  stake <-
    selectMap [SnapShotStakeSnapShotId ==. snapShotId] $ \SnapShotStake {..} -> do
      Credential credential <- getJust snapShotStakeCredentialId
      pure (internOtherStakes credential, snapShotStakeCoin)
  poolParams <-
    selectMap [SnapShotPoolSnapShotId ==. snapShotId] $ \SnapShotPool {..} -> do
      KeyHash keyHash <- getJust snapShotPoolKeyHashId
      pure (internOtherPoolParams keyHash, snapShotPoolParams)
  let internPoolParams = interns (internsFromMap poolParams) . Keys.coerceKeyRole
  delegations <-
    selectMap [SnapShotDelegationSnapShotId ==. snapShotId] $ \SnapShotDelegation {..} -> do
      Credential credential <- getJust snapShotDelegationCredentialId
      KeyHash keyHash <- getJust snapShotDelegationKeyHash
      pure (internOtherDelegations credential, internPoolParams keyHash)
  pure
    SnapShotM
      { ssStake = stake,
        ssDelegations = delegations,
        ssPoolParams = poolParams
      }
{-# INLINEABLE getSnapShotWithSharingM #-}

getSnapShotsWithSharingM ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m (SnapShotsM C)
getSnapShotsWithSharingM (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotWithSharingM [] epochStateId SnapShotMark
  set <- getSnapShotWithSharingM [mark] epochStateId SnapShotSet
  go <- getSnapShotWithSharingM [mark, set] epochStateId SnapShotGo
  pure $
    SnapShotsM
      { ssPstakeMark = mark,
        ssPstakeSet = set,
        ssPstakeGo = go,
        ssFeeSS = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsWithSharingM #-}

-- Into a Map structure

selectMap ::
  ( MonadResource m,
    Ord k,
    PersistEntity record,
    PersistEntityBackend record ~ SqlBackend
  ) =>
  [Filter record] ->
  (record -> ReaderT SqlBackend m (k, a)) ->
  ReaderT SqlBackend m (Map.Map k a)
selectMap fs f = do
  runConduit $
    selectSource fs [] .| mapMC (\(Entity _ a) -> f a)
      .| foldlC (\m (k, v) -> Map.insert k v m) mempty
{-# INLINEABLE selectMap #-}

getSnapShotNoSharing ::
  MonadResource m =>
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m (EpochBoundary.SnapShot C)
getSnapShotNoSharing epochStateId snapShotType = do
  snapShotId <-
    selectFirst
      [SnapShotType ==. snapShotType, SnapShotEpochStateId ==. epochStateId]
      []
      <&> \case
        Nothing -> error $ "Missing a snapshot: " ++ show snapShotType
        Just (Entity snapShotId _) -> snapShotId
  stake <-
    selectVMap [SnapShotStakeSnapShotId ==. snapShotId] $ \SnapShotStake {..} -> do
      Credential credential <- getJust snapShotStakeCredentialId
      pure (Keys.coerceKeyRole credential, snapShotStakeCoin)
  delegations <-
    selectVMap [SnapShotDelegationSnapShotId ==. snapShotId] $ \SnapShotDelegation {..} -> do
      Credential credential <- getJust snapShotDelegationCredentialId
      KeyHash keyHash <- getJust snapShotDelegationKeyHash
      -- TODO ^ rename snapShotDelegationKeyHashId
      pure (Keys.coerceKeyRole credential, Keys.coerceKeyRole keyHash)
  poolParams <-
    selectVMap [SnapShotPoolSnapShotId ==. snapShotId] $ \SnapShotPool {..} -> do
      KeyHash keyHash <- getJust snapShotPoolKeyHashId
      pure (Keys.coerceKeyRole keyHash, snapShotPoolParams)
  pure
    EpochBoundary.SnapShot
      { _stake = EpochBoundary.Stake stake,
        _delegations = delegations,
        _poolParams = poolParams
      }
{-# INLINEABLE getSnapShotNoSharing #-}

getSnapShotsNoSharing ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m (EpochBoundary.SnapShots C)
getSnapShotsNoSharing (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotNoSharing epochStateId SnapShotMark
  set <- getSnapShotNoSharing epochStateId SnapShotSet
  go <- getSnapShotNoSharing epochStateId SnapShotGo
  pure $
    EpochBoundary.SnapShots
      { _pstakeMark = mark,
        _pstakeSet = set,
        _pstakeGo = go,
        _feeSS = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsNoSharing #-}

getSnapShotsNoSharingM ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m (SnapShotsM C)
getSnapShotsNoSharingM (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotNoSharingM epochStateId SnapShotMark
  set <- getSnapShotNoSharingM epochStateId SnapShotSet
  go <- getSnapShotNoSharingM epochStateId SnapShotGo
  pure $
    SnapShotsM
      { ssPstakeMark = mark,
        ssPstakeSet = set,
        ssPstakeGo = go,
        ssFeeSS = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsNoSharingM #-}

getSnapShotWithSharing ::
  MonadResource m =>
  [EpochBoundary.SnapShot C] ->
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m (EpochBoundary.SnapShot C)
getSnapShotWithSharing otherSnapShots epochStateId snapShotType = do
  let internOtherStakes =
        interns
          (foldMap (internsFromVMap . EpochBoundary.unStake . EpochBoundary._stake) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherPoolParams =
        interns (foldMap (internsFromVMap . EpochBoundary._poolParams) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherDelegations =
        interns (foldMap (internsFromVMap . EpochBoundary._delegations) otherSnapShots)
          . Keys.coerceKeyRole
  snapShotId <-
    selectFirst
      [SnapShotType ==. snapShotType, SnapShotEpochStateId ==. epochStateId]
      []
      <&> \case
        Nothing -> error $ "Missing a snapshot: " ++ show snapShotType
        Just (Entity snapShotId _) -> snapShotId
  stake <-
    selectVMap [SnapShotStakeSnapShotId ==. snapShotId] $ \SnapShotStake {..} -> do
      Credential credential <- getJust snapShotStakeCredentialId
      pure (internOtherStakes credential, snapShotStakeCoin)
  poolParams <-
    selectVMap [SnapShotPoolSnapShotId ==. snapShotId] $ \SnapShotPool {..} -> do
      KeyHash keyHash <- getJust snapShotPoolKeyHashId
      pure (internOtherPoolParams keyHash, snapShotPoolParams)
  let internPoolParams = interns (internsFromVMap poolParams) . Keys.coerceKeyRole
  delegations <-
    selectVMap [SnapShotDelegationSnapShotId ==. snapShotId] $ \SnapShotDelegation {..} -> do
      Credential credential <- getJust snapShotDelegationCredentialId
      KeyHash keyHash <- getJust snapShotDelegationKeyHash
      pure (internOtherDelegations credential, internPoolParams keyHash)
  pure
    EpochBoundary.SnapShot
      { _stake = EpochBoundary.Stake stake,
        _delegations = delegations,
        _poolParams = poolParams
      }
{-# INLINEABLE getSnapShotWithSharing #-}

getSnapShotsWithSharing ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m (EpochBoundary.SnapShots C)
getSnapShotsWithSharing (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotWithSharing [] epochStateId SnapShotMark
  set <- getSnapShotWithSharing [mark] epochStateId SnapShotSet
  go <- getSnapShotWithSharing [mark, set] epochStateId SnapShotGo
  pure $
    EpochBoundary.SnapShots
      { _pstakeMark = mark,
        _pstakeSet = set,
        _pstakeGo = go,
        _feeSS = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsWithSharing #-}

sourceUTxO ::
  MonadResource m =>
  ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceUTxO =
  selectSource [] []
    .| mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))

sourceWithSharingUTxO ::
  MonadResource m =>
  Map.Map (Credential.StakeCredential C) a ->
  ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceWithSharingUTxO stakeCredentials =
  sourceUTxO .| mapC (fmap internTxOut)
  where
    internTxOut = \case
      Alonzo.TxOut_AddrHash28_AdaOnly cred a b c d e ->
        Alonzo.TxOut_AddrHash28_AdaOnly (intern (Keys.coerceKeyRole cred) stakeCredentials) a b c d e
      Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 cred a b c d e o p q r ->
        Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32
          (intern (Keys.coerceKeyRole cred) stakeCredentials)
          a
          b
          c
          d
          e
          o
          p
          q
          r
      out -> out

foldDbUTxO ::
  MonadUnliftIO m =>
  -- | Folding function
  (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) ->
  -- | Empty acc
  a ->
  -- | Path to Sqlite db
  T.Text ->
  m a
foldDbUTxO f m fp = runSqlite fp (runConduit (sourceUTxO .| foldlC f m))

-- sourceUTxOr ::
--      MonadResource m
--   => Int64 -> Int64 -> ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
-- sourceUTxOr b t =
--   selectSource [TxId >. TxKey (SqlBackendKey b) , TxId <. TxKey (SqlBackendKey t)] [] .|
--   mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))

-- foldDbUTxOr ::
--      MonadUnliftIO m
--   => Int64
--   -> Int64
--   -> (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) -- ^ Folding function
--   -> a -- ^ Empty acc
--   -> T.Text -- ^ Path to Sqlite db
--   -> m a
-- foldDbUTxOr b t f m fp = runSqlite fp (runConduit (sourceUTxOr b t .| foldlC f m))

lsId :: Key LedgerState
lsId = LedgerStateKey (SqlBackendKey 1)

getLedgerState ::
  MonadIO m =>
  Shelley.UTxO CurrentEra ->
  LedgerState ->
  Shelley.DState C ->
  ReaderT SqlBackend m (Shelley.LedgerState CurrentEra)
getLedgerState utxo LedgerState {..} dstate = do
  UtxoState {..} <- getJust ledgerStateUtxoId
  pure
    Shelley.LedgerState
      { Shelley._utxoState =
          Shelley.smartUTxOState utxo utxoStateDeposited utxoStateFees utxoStatePpups, -- Maintain invariant
        Shelley._delegationState =
          Shelley.DPState
            { Shelley._dstate = dstate,
              Shelley._pstate = ledgerStatePstateBin
            }
      }

getDStateNoSharing ::
  MonadIO m => Key DState -> ReaderT SqlBackend m (Shelley.DState C)
getDStateNoSharing dstateId = do
  DState {..} <- getJust dstateId
  rewards <-
    Map.fromList <$> do
      rws <- selectList [RewardDstateId ==. dstateId] []
      forM rws $ \(Entity _ Reward {..}) -> do
        Credential credential <- getJust rewardCredentialId
        pure (Keys.coerceKeyRole credential, rewardCoin)
  delegations <-
    Map.fromList <$> do
      ds <- selectList [DelegationDstateId ==. dstateId] []
      forM ds $ \(Entity _ Delegation {..}) -> do
        Credential credential <- getJust delegationCredentialId
        KeyHash keyHash <- getJust delegationStakePoolId
        pure (Keys.coerceKeyRole credential, Keys.coerceKeyRole keyHash)
  ptrs <-
    Map.fromList <$> do
      ps <- selectList [PtrDstateId ==. dstateId] []
      forM ps $ \(Entity _ Ptr {..}) -> do
        Credential credential <- getJust ptrCredentialId
        pure (ptrPtr, Keys.coerceKeyRole credential)
  iRReserves <-
    Map.fromList <$> do
      ds <- selectList [IRReservesDstateId ==. dstateId] []
      forM ds $ \(Entity _ IRReserves {..}) -> do
        Credential credential <- getJust iRReservesCredentialId
        pure (Keys.coerceKeyRole credential, iRReservesCoin)
  iRTreasury <-
    Map.fromList <$> do
      ds <- selectList [IRTreasuryDstateId ==. dstateId] []
      forM ds $ \(Entity _ IRTreasury {..}) -> do
        Credential credential <- getJust iRTreasuryCredentialId
        pure (Keys.coerceKeyRole credential, iRTreasuryCoin)
  pure
    Shelley.DState
      { _unified = unify rewards delegations ptrs,
        _fGenDelegs = unEnc dStateFGenDelegs,
        _genDelegs = dStateGenDelegs,
        _irwd =
          Shelley.InstantaneousRewards
            { iRReserves = iRReserves,
              iRTreasury = iRTreasury,
              deltaReserves = dStateIrDeltaReserves,
              deltaTreasury = dStateIrDeltaTreasury
            }
      }

getDStateWithSharing ::
  MonadIO m => Key DState -> ReaderT SqlBackend m (Shelley.DState C)
getDStateWithSharing dstateId = do
  DState {..} <- getJust dstateId
  rewards <-
    Map.fromList <$> do
      rws <- selectList [RewardDstateId ==. dstateId] []
      forM rws $ \(Entity _ Reward {..}) -> do
        Credential credential <- getJust rewardCredentialId
        pure (Keys.coerceKeyRole credential, rewardCoin)
  delegations <-
    Map.fromList <$> do
      ds <- selectList [DelegationDstateId ==. dstateId] []
      forM ds $ \(Entity _ Delegation {..}) -> do
        Credential credential <- getJust delegationCredentialId
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        KeyHash keyHash <- getJust delegationStakePoolId
        pure (cred, Keys.coerceKeyRole keyHash)
  ptrs <-
    Map.fromList <$> do
      ps <- selectList [PtrDstateId ==. dstateId] []
      forM ps $ \(Entity _ Ptr {..}) -> do
        Credential credential <- getJust ptrCredentialId
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        pure (ptrPtr, cred)
  iRReserves <-
    Map.fromList <$> do
      ds <- selectList [IRReservesDstateId ==. dstateId] []
      forM ds $ \(Entity _ IRReserves {..}) -> do
        Credential credential <- getJust iRReservesCredentialId
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        pure (cred, iRReservesCoin)
  iRTreasury <-
    Map.fromList <$> do
      ds <- selectList [IRTreasuryDstateId ==. dstateId] []
      forM ds $ \(Entity _ IRTreasury {..}) -> do
        Credential credential <- getJust iRTreasuryCredentialId
        let !cred = intern (Keys.coerceKeyRole credential) rewards
        pure (cred, iRTreasuryCoin)
  pure
    Shelley.DState
      { _unified = unify rewards delegations ptrs,
        _fGenDelegs = unEnc dStateFGenDelegs,
        _genDelegs = dStateGenDelegs,
        _irwd =
          Shelley.InstantaneousRewards
            { iRReserves = iRReserves,
              iRTreasury = iRTreasury,
              deltaReserves = dStateIrDeltaReserves,
              deltaTreasury = dStateIrDeltaTreasury
            }
      }

loadDStateNoSharing :: MonadUnliftIO m => T.Text -> m (Shelley.DState C)
loadDStateNoSharing fp =
  runSqlite fp $ getDStateNoSharing (DStateKey (SqlBackendKey 1))

loadUTxONoSharing ::
  MonadUnliftIO m => T.Text -> m (Shelley.UTxO CurrentEra)
loadUTxONoSharing fp =
  runSqlite fp (Shelley.UTxO <$> runConduitFold sourceUTxO noSharing)

loadLedgerStateNoSharing ::
  MonadUnliftIO m => T.Text -> m (Shelley.LedgerState CurrentEra)
loadLedgerStateNoSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateNoSharing ledgerStateDstateId
    m <- runConduitFold sourceUTxO noSharing
    getLedgerState (Shelley.UTxO m) ledgerState dstate

loadLedgerStateDStateSharing ::
  MonadUnliftIO m => T.Text -> m (Shelley.LedgerState CurrentEra)
loadLedgerStateDStateSharing fp =
  runSqlite fp $ do
    ese <- getJustEntity esId
    getLedgerStateWithSharing ese

loadLedgerStateDStateTxIxSharing ::
  MonadUnliftIO m =>
  T.Text ->
  m
    ( Shelley.LedgerState CurrentEra,
      IntMap.IntMap (Map.Map (TxIn.TxId C) (Alonzo.TxOut CurrentEra))
    )
loadLedgerStateDStateTxIxSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateWithSharing ledgerStateDstateId
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold sourceUTxO txIxSharing
    pure (ls, m)

loadLedgerStateDStateTxIxSharingKeyMap ::
  MonadUnliftIO m =>
  T.Text ->
  m
    ( Shelley.LedgerState CurrentEra,
      IntMap.IntMap (KeyMap.KeyMap (Alonzo.TxOut CurrentEra))
    )
loadLedgerStateDStateTxIxSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateWithSharing ledgerStateDstateId
    m <- runConduitFold sourceUTxO txIxSharingKeyMap
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    pure (ls, m)

loadLedgerStateDStateTxIdSharingKeyMap ::
  MonadUnliftIO m =>
  T.Text ->
  m
    ( Shelley.LedgerState CurrentEra,
      KeyMap.KeyMap (IntMap.IntMap (Alonzo.TxOut CurrentEra))
    )
loadLedgerStateDStateTxIdSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateWithSharing ledgerStateDstateId
    m <- runConduitFold sourceUTxO txIdSharingKeyMap
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    pure (ls, m)

storeEpochState ::
  MonadUnliftIO m => T.Text -> Shelley.EpochState CurrentEra -> m ()
storeEpochState fp es =
  runSqlite fp $ do
    runMigration migrateAll
    insertEpochState es

loadDbUTxO :: UTxOFold a -> T.Text -> IO a
loadDbUTxO (Fold f e g) fp = runSqlite fp (g <$> runConduit (sourceUTxO .| foldlC f e))

esId :: Key EpochState
esId = EpochStateKey (SqlBackendKey 1)

loadEpochStateEntity :: MonadUnliftIO m => T.Text -> m (Entity EpochState)
loadEpochStateEntity fp = runSqlite fp (getJustEntity esId)

getLedgerStateWithSharing ::
  (MonadUnliftIO m, MonadResource m) =>
  Entity EpochState ->
  ReaderT SqlBackend m (Shelley.LedgerState CurrentEra)
getLedgerStateWithSharing ese = do
  ledgerState@LedgerState {..} <-
    maybe (error "Impossible") (pure . entityVal)
      =<< selectFirst [LedgerStateEpochStateId ==. entityKey ese] []
  dstate <- getDStateWithSharing ledgerStateDstateId
  m <- runConduitFold sourceUTxO noSharing
  getLedgerState (Shelley.UTxO m) ledgerState dstate

getLedgerStateNoSharing ::
  (MonadUnliftIO m, MonadResource m) =>
  Entity EpochState ->
  ReaderT SqlBackend m (Shelley.LedgerState CurrentEra)
getLedgerStateNoSharing ese = do
  ledgerState@LedgerState {..} <-
    maybe (error "Impossible") (pure . entityVal)
      =<< selectFirst [LedgerStateEpochStateId ==. entityKey ese] []
  dstate <- getDStateNoSharing ledgerStateDstateId
  m <- runConduitFold sourceUTxO noSharing
  getLedgerState (Shelley.UTxO m) ledgerState dstate

loadEpochState :: MonadUnliftIO m => T.Text -> m (Shelley.EpochState CurrentEra)
loadEpochState fp = runSqlite fp $ do
  ese@(Entity _ EpochState {..}) <- getJustEntity esId
  snapshots <- getSnapShotsNoSharing ese
  ledgerState <- getLedgerStateNoSharing ese
  pure
    Shelley.EpochState
      { esAccountState =
          Shelley.AccountState
            { _treasury = epochStateTreasury,
              _reserves = epochStateReserves
            },
        esLState = ledgerState,
        esSnapshots = snapshots,
        esPrevPp = epochStatePrevPp,
        esPp = epochStatePp,
        esNonMyopic = epochStateNonMyopic
      }

loadEpochStateWithSharing :: MonadUnliftIO m => T.Text -> m (Shelley.EpochState CurrentEra)
loadEpochStateWithSharing fp = runSqlite fp $ do
  ese@(Entity _ EpochState {..}) <- getJustEntity esId
  snapshots <- getSnapShotsWithSharing ese
  ledgerState <- getLedgerStateWithSharing ese
  pure
    Shelley.EpochState
      { esAccountState =
          Shelley.AccountState
            { _treasury = epochStateTreasury,
              _reserves = epochStateReserves
            },
        esLState = ledgerState,
        esSnapshots = snapshots,
        esPrevPp = epochStatePrevPp,
        esPp = epochStatePp,
        esNonMyopic = epochStateNonMyopic
      }

loadSnapShotsNoSharing ::
  MonadUnliftIO m => T.Text -> Entity EpochState -> m (EpochBoundary.SnapShots C)
loadSnapShotsNoSharing fp = runSqlite fp . getSnapShotsNoSharing
{-# INLINEABLE loadSnapShotsNoSharing #-}

loadSnapShotsWithSharing ::
  MonadUnliftIO m => T.Text -> Entity EpochState -> m (EpochBoundary.SnapShots C)
loadSnapShotsWithSharing fp = runSqlite fp . getSnapShotsWithSharing
{-# INLINEABLE loadSnapShotsWithSharing #-}

loadSnapShotsNoSharingM :: T.Text -> Entity EpochState -> IO (SnapShotsM C)
loadSnapShotsNoSharingM fp = runSqlite fp . getSnapShotsNoSharingM
{-# INLINEABLE loadSnapShotsNoSharingM #-}

loadSnapShotsWithSharingM :: T.Text -> Entity EpochState -> IO (SnapShotsM C)
loadSnapShotsWithSharingM fp = runSqlite fp . getSnapShotsWithSharingM
{-# INLINEABLE loadSnapShotsWithSharingM #-}

loadLedgerStateNoSharingKeyMap ::
  MonadUnliftIO m =>
  T.Text ->
  m
    ( Shelley.LedgerState CurrentEra,
      IntMap.IntMap (KeyMap.KeyMap (Alonzo.TxOut CurrentEra))
    )
loadLedgerStateNoSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateNoSharing ledgerStateDstateId
    m <- runConduitFold sourceUTxO txIxSharingKeyMap
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    pure (ls, m)

loadLedgerStateWithSharingKeyMap ::
  MonadUnliftIO m =>
  T.Text ->
  m
    ( Shelley.LedgerState CurrentEra,
      IntMap.IntMap (KeyMap.KeyMap (Alonzo.TxOut CurrentEra))
    )
loadLedgerStateWithSharingKeyMap fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateNoSharing ledgerStateDstateId
    let stakeCredentials = rewView (Shelley._unified dstate)
    m <- runConduitFold (sourceWithSharingUTxO stakeCredentials) txIxSharingKeyMap
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    pure (ls, m)
