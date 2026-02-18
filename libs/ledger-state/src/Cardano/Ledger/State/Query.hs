{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.State.Query where

import Cardano.Ledger.Babbage.TxOut (internBabbageTxOut)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core (TxOut, emptyPParams)
import qualified Cardano.Ledger.Credential as Credential
import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, prevPParamsEpochStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.State (EraCertState (..))
import qualified Cardano.Ledger.State as State
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
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List (sourceList)
import Data.Default (def)
import Data.Functor
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Database.Persist.Sqlite
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Conway.Era (EraTest, accountsFromAccountsMap, mkTestAccountState)

-- Populate database

insertGetKey ::
  ( MonadIO m
  , PersistUniqueWrite backend
  , PersistRecordBackend record backend
  , AtLeastOneUniqueKey record
  , SafeToInsert record
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
      { utxoStateDeposited = utxosDeposited
      , utxoStateFees = utxosFees
      , utxoStateGovState = utxosGovState
      , utxoStateDonation = utxosDonation
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
        insert $ Tx {txInIx = txIx, txInId = txId, txOut = out}
      txsKey <-
        insert $
          Txs
            { txsInIx = txIx
            , txsInId = txId
            , txsOut = out
            , txsStakeCredential = Nothing
            }
      insert_ $
        UtxoEntry
          { utxoEntryTxId = txKey
          , utxoEntryTxsId = txsKey
          , utxoEntryStateId = stateKey
          }

insertDState :: MonadIO m => Shelley.DState CurrentEra -> ReaderT SqlBackend m DStateId
insertDState Shelley.DState {..} = do
  let irDeltaReserves = Shelley.deltaReserves dsIRewards
  let irDeltaTreasury = Shelley.deltaTreasury dsIRewards
  dstateId <- insert $ DState (Enc dsFutureGenDelegs) dsGenDelegs irDeltaReserves irDeltaTreasury
  forM_ (Map.toList (dsAccounts ^. State.accountsMapL)) $ \(cred, accountState) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ $ -- TODO: fix insertion of DRep and StakePool delegation
      Account
        dstateId
        credId
        Nothing
        (accountState ^. State.balanceAccountStateL)
        (accountState ^. State.depositAccountStateL)
        Nothing
        DRepDelegationNone
        Nothing
  forM_ (Map.toList (Shelley.iRReserves dsIRewards)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (IRReserves dstateId credId c)
  forM_ (Map.toList (Shelley.iRTreasury dsIRewards)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (IRTreasury dstateId credId c)
  pure dstateId

insertLedgerState ::
  MonadIO m => EpochStateId -> Shelley.LedgerState CurrentEra -> ReaderT SqlBackend m ()
insertLedgerState epochStateKey Shelley.LedgerState {..} = do
  stateKey <- insertUTxOState lsUTxOState
  insertUTxO (Shelley.utxosUtxo lsUTxOState) stateKey
  dstateKey <- insertDState $ lsCertState ^. Shelley.certDStateL
  insert_
    LedgerState
      { ledgerStateUtxoId = stateKey
      , ledgerStateDstateId = dstateKey
      , ledgerStatePstateBin = lsCertState ^. Shelley.certPStateL
      , ledgerStateEpochStateId = epochStateKey
      }

insertSnapShot ::
  MonadIO m =>
  Key EpochState ->
  SnapShotType ->
  State.SnapShot ->
  ReaderT SqlBackend m ()
insertSnapShot snapShotEpochStateId snapShotType State.SnapShot {..} = do
  snapShotId <- insert $ SnapShot {snapShotType, snapShotEpochStateId}
  VG.forM_ (VMap.unVMap (State.unStake ssActiveStake)) $ \(cred, c) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    insert_ (SnapShotStake snapShotId credId c)
  VG.forM_ (VMap.unVMap ssDelegations) $ \(cred, spKeyHash) -> do
    credId <- insertGetKey (Credential (Keys.asWitness cred))
    keyHashId <- insertGetKey (KeyHash (Keys.asWitness spKeyHash))
    insert_ (SnapShotDelegation snapShotId credId keyHashId)
  VG.forM_ (VMap.unVMap ssStakePoolsSnapShot) $ \(keyHash, spss) -> do
    keyHashId <- insertGetKey (KeyHash (Keys.asWitness keyHash))
    insert_ (SnapShotStakePool snapShotId keyHashId spss)

insertSnapShots ::
  MonadIO m =>
  Key EpochState ->
  State.SnapShots ->
  ReaderT SqlBackend m ()
insertSnapShots epochStateKey State.SnapShots {..} = do
  mapM_
    (uncurry (insertSnapShot epochStateKey))
    [ (SnapShotMark, ssStakeMark)
    , (SnapShotSet, ssStakeSet)
    , (SnapShotGo, ssStakeGo)
    ]

insertEpochState ::
  MonadIO m => Shelley.EpochState CurrentEra -> ReaderT SqlBackend m ()
insertEpochState es@Shelley.EpochState {..} = do
  epochStateKey <-
    insert
      EpochState
        { epochStateTreasury = State.casTreasury esChainAccountState
        , epochStateReserves = State.casReserves esChainAccountState
        , epochStatePrevPp = es ^. prevPParamsEpochStateL
        , epochStatePp = es ^. curPParamsEpochStateL
        , epochStateNonMyopic = esNonMyopic
        , epochStateSnapShotsFee = State.ssFee esSnapshots
        }
  insertSnapShots epochStateKey esSnapshots
  insertLedgerState epochStateKey esLState

-- Query database

-- Into vector

selectVMap ::
  ( Ord k
  , PersistEntity record
  , PersistEntityBackend record ~ SqlBackend
  , VMap.Vector kv k
  , VMap.Vector vv v
  , MonadResource m
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
  ReaderT SqlBackend m SnapShotM
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
      -- TODO ^ rename snapShotDelegationKeyHashId
      pure (Keys.coerceKeyRole credential, Keys.coerceKeyRole keyHash)
  stakePoolsSnapShot <-
    selectMap [SnapShotStakePoolSnapShotId ==. snapShotId] $ \SnapShotStakePool {..} -> do
      KeyHash keyHash <- getJust snapShotStakePoolKeyHashId
      pure (Keys.coerceKeyRole keyHash, snapShotStakePoolSnapShot)
  pure
    SnapShotM
      { ssStake = stake
      , ssDelegations = delegations
      , ssStakePoolsSnapShot = stakePoolsSnapShot
      }
{-# INLINEABLE getSnapShotNoSharingM #-}

getSnapShotWithSharingM ::
  MonadResource m =>
  [SnapShotM] ->
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m SnapShotM
getSnapShotWithSharingM otherSnapShots epochStateId snapShotType = do
  let internOtherStakes =
        interns
          (foldMap (internsFromMap . ssStake) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherPoolParams =
        interns (foldMap (internsFromMap . ssStakePoolsSnapShot) otherSnapShots)
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
  stakePoolParams <-
    selectMap [SnapShotStakePoolSnapShotId ==. snapShotId] $ \SnapShotStakePool {..} -> do
      KeyHash keyHash <- getJust snapShotStakePoolKeyHashId
      pure (internOtherPoolParams keyHash, snapShotStakePoolSnapShot)
  let internPoolParams = interns (internsFromMap stakePoolParams) . Keys.coerceKeyRole
  delegations <-
    selectMap [SnapShotDelegationSnapShotId ==. snapShotId] $ \SnapShotDelegation {..} -> do
      Credential credential <- getJust snapShotDelegationCredentialId
      KeyHash keyHash <- getJust snapShotDelegationKeyHash
      pure (internOtherDelegations credential, internPoolParams keyHash)
  pure
    SnapShotM
      { ssStake = stake
      , ssDelegations = delegations
      , ssStakePoolsSnapShot = stakePoolParams
      }
{-# INLINEABLE getSnapShotWithSharingM #-}

getSnapShotsWithSharingM ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m SnapShotsM
getSnapShotsWithSharingM (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotWithSharingM [] epochStateId SnapShotMark
  set <- getSnapShotWithSharingM [mark] epochStateId SnapShotSet
  go <- getSnapShotWithSharingM [mark, set] epochStateId SnapShotGo
  pure $
    SnapShotsM
      { ssPstakeMark = mark
      , ssPstakeSet = set
      , ssPstakeGo = go
      , ssFeeSS = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsWithSharingM #-}

-- Into a Map structure

selectMap ::
  ( MonadResource m
  , Ord k
  , PersistEntity record
  , PersistEntityBackend record ~ SqlBackend
  ) =>
  [Filter record] ->
  (record -> ReaderT SqlBackend m (k, a)) ->
  ReaderT SqlBackend m (Map.Map k a)
selectMap fs f = do
  runConduit $
    selectSource fs []
      .| mapMC (\(Entity _ a) -> f a)
      .| foldlC (\m (k, v) -> Map.insert k v m) mempty
{-# INLINEABLE selectMap #-}

getSnapShotNoSharing ::
  MonadResource m =>
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m State.SnapShot
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
  stakePoolSnapShot <-
    selectVMap [SnapShotStakePoolSnapShotId ==. snapShotId] $ \SnapShotStakePool {..} -> do
      KeyHash keyHash <- getJust snapShotStakePoolKeyHashId
      pure (Keys.coerceKeyRole keyHash, snapShotStakePoolSnapShot)
  pure $ State.mkSnapShot (State.Stake stake) delegations stakePoolSnapShot
{-# INLINEABLE getSnapShotNoSharing #-}

getSnapShotsNoSharing ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m State.SnapShots
getSnapShotsNoSharing (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotNoSharing epochStateId SnapShotMark
  set <- getSnapShotNoSharing epochStateId SnapShotSet
  go <- getSnapShotNoSharing epochStateId SnapShotGo
  pure $
    State.SnapShots
      { ssStakeMark = mark
      , ssStakeMarkPoolDistr = State.calculatePoolDistr mark
      , ssStakeSet = set
      , ssStakeGo = go
      , ssFee = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsNoSharing #-}

getSnapShotsNoSharingM ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m SnapShotsM
getSnapShotsNoSharingM (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotNoSharingM epochStateId SnapShotMark
  set <- getSnapShotNoSharingM epochStateId SnapShotSet
  go <- getSnapShotNoSharingM epochStateId SnapShotGo
  pure $
    SnapShotsM
      { ssPstakeMark = mark
      , ssPstakeSet = set
      , ssPstakeGo = go
      , ssFeeSS = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsNoSharingM #-}

getSnapShotWithSharing ::
  MonadResource m =>
  [State.SnapShot] ->
  Key EpochState ->
  SnapShotType ->
  ReaderT SqlBackend m State.SnapShot
getSnapShotWithSharing otherSnapShots epochStateId snapShotType = do
  let internOtherStakes =
        interns
          (foldMap (internsFromVMap . State.unStake . State.ssActiveStake) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherPoolParams =
        interns (foldMap (internsFromVMap . State.ssStakePoolsSnapShot) otherSnapShots)
          . Keys.coerceKeyRole
  let internOtherDelegations =
        interns (foldMap (internsFromVMap . State.ssDelegations) otherSnapShots)
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
  stakePoolSnapShot <-
    selectVMap [SnapShotStakePoolSnapShotId ==. snapShotId] $ \SnapShotStakePool {..} -> do
      KeyHash keyHash <- getJust snapShotStakePoolKeyHashId
      pure (internOtherPoolParams keyHash, snapShotStakePoolSnapShot)
  let internStakePoolSnapShot = interns (internsFromVMap stakePoolSnapShot) . Keys.coerceKeyRole
  delegations <-
    selectVMap [SnapShotDelegationSnapShotId ==. snapShotId] $ \SnapShotDelegation {..} -> do
      Credential credential <- getJust snapShotDelegationCredentialId
      KeyHash keyHash <- getJust snapShotDelegationKeyHash
      pure (internOtherDelegations credential, internStakePoolSnapShot keyHash)
  pure $ State.mkSnapShot (State.Stake stake) delegations stakePoolSnapShot
{-# INLINEABLE getSnapShotWithSharing #-}

getSnapShotsWithSharing ::
  MonadResource m =>
  Entity EpochState ->
  ReaderT SqlBackend m State.SnapShots
getSnapShotsWithSharing (Entity epochStateId EpochState {epochStateSnapShotsFee}) = do
  mark <- getSnapShotWithSharing [] epochStateId SnapShotMark
  set <- getSnapShotWithSharing [mark] epochStateId SnapShotSet
  go <- getSnapShotWithSharing [mark, set] epochStateId SnapShotGo
  pure $
    State.SnapShots
      { ssStakeMark = mark
      , ssStakeMarkPoolDistr = State.calculatePoolDistr mark
      , ssStakeSet = set
      , ssStakeGo = go
      , ssFee = epochStateSnapShotsFee
      }
{-# INLINEABLE getSnapShotsWithSharing #-}

sourceUTxO ::
  MonadResource m =>
  ConduitM () (TxIn.TxIn, TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceUTxO =
  selectSource [] []
    .| mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId txInIx, txOut))

sourceWithSharingUTxO ::
  MonadResource m =>
  Map.Map (Credential.Credential Keys.Staking) a ->
  ConduitM () (TxIn.TxIn, TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceWithSharingUTxO stakeCredentials =
  sourceUTxO .| mapC (fmap (internBabbageTxOut (`intern` stakeCredentials)))

foldDbUTxO ::
  MonadUnliftIO m =>
  -- | Folding function
  (a -> (TxIn.TxIn, TxOut CurrentEra) -> a) ->
  -- | Empty acc
  a ->
  -- | Path to Sqlite db
  T.Text ->
  m a
foldDbUTxO f m fp = runSqlite fp (runConduit (sourceUTxO .| foldlC f m))

-- sourceUTxOr ::
--      MonadResource m
--   => Int64 -> Int64 -> ConduitM () (TxIn.TxIn C, TxOut CurrentEra) (ReaderT SqlBackend m) ()
-- sourceUTxOr b t =
--   selectSource [TxId >. TxKey (SqlBackendKey b) , TxId <. TxKey (SqlBackendKey t)] [] .|
--   mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))

-- foldDbUTxOr ::
--      MonadUnliftIO m
--   => Int64
--   -> Int64
--   -> (a -> (TxIn.TxIn C, TxOut CurrentEra) -> a) -- ^ Folding function
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
  Shelley.DState CurrentEra ->
  ReaderT SqlBackend m (Shelley.LedgerState CurrentEra)
getLedgerState utxo LedgerState {..} dstate = do
  UtxoState {..} <- getJust ledgerStateUtxoId
  pure
    Shelley.LedgerState
      { Shelley.lsUTxOState =
          Shelley.smartUTxOState
            emptyPParams
            utxo
            utxoStateDeposited
            utxoStateFees
            utxoStateGovState -- Maintain invariant
            utxoStateDonation
      , Shelley.lsCertState =
          def
            & certPStateL .~ ledgerStatePstateBin
            & certDStateL .~ dstate
      }

getDStateNoSharing ::
  MonadIO m => Key DState -> ReaderT SqlBackend m (Shelley.DState CurrentEra)
getDStateNoSharing dstateId = do
  DState {..} <- getJust dstateId
  accountsMap <- getAccountsMap dstateId
  -- Map.fromList <$> do
  --  ds <- selectList [DRepDstateId ==. dstateId] []
  --  forM ds $ \(Entity _ DRep {..}) -> do
  --    Credential credential <- getJust dRepCredentialId
  --    Credential dRepCredential <- getJust dRepDRepCredentialId
  --    pure (Keys.coerceKeyRole credential, Keys.coerceKeyRole dRepCredential)
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
      { dsAccounts = accountsFromAccountsMap accountsMap
      , dsFutureGenDelegs = unEnc dStateFGenDelegs
      , dsGenDelegs = dStateGenDelegs
      , dsIRewards =
          Shelley.InstantaneousRewards
            { iRReserves = iRReserves
            , iRTreasury = iRTreasury
            , deltaReserves = dStateIrDeltaReserves
            , deltaTreasury = dStateIrDeltaTreasury
            }
      }

getAccountsMap ::
  (MonadIO m, EraTest era) =>
  DStateId ->
  ReaderT SqlBackend m (Map.Map (Credential.Credential r') (State.AccountState era))
getAccountsMap dstateId =
  Map.fromList <$> do
    rws <- selectList [AccountDstateId ==. dstateId] []
    forM rws $ \(Entity _ Account {..}) -> do
      Credential credential <- getJust accountCredentialId
      mStakePool <- forM accountKeyHashStakePoolId (fmap (Keys.coerceKeyRole . keyHashWitness) . getJust)
      -- mDRep <- forM accountCredentialDRepId (fmap (Keys.coerceKeyRole . credentialWitness) . getJust)
      pure
        ( Keys.coerceKeyRole credential
        , mkTestAccountState accountPtr accountDeposit mStakePool Nothing
            & State.balanceAccountStateL .~ accountBalance
        )

getDStateWithSharing ::
  MonadIO m => Key DState -> ReaderT SqlBackend m (Shelley.DState CurrentEra)
getDStateWithSharing dstateId = do
  DState {..} <- getJust dstateId
  accountsMap <- getAccountsMap dstateId
  -- Map.fromList <$> do
  --  ds <- selectList [DRepDstateId ==. dstateId] []
  --  forM ds $ \(Entity _ DRep {..}) -> do
  --    Credential credential <- getJust dRepCredentialId
  --    let !cred = intern (Keys.coerceKeyRole credential) rewards
  --    Credential dRepCredential <- getJust dRepDRepCredentialId
  --    pure (cred, Keys.coerceKeyRole dRepCredential)
  iRReserves <-
    Map.fromList <$> do
      ds <- selectList [IRReservesDstateId ==. dstateId] []
      forM ds $ \(Entity _ IRReserves {..}) -> do
        Credential credential <- getJust iRReservesCredentialId
        let !cred = intern (Keys.coerceKeyRole credential) accountsMap
        pure (cred, iRReservesCoin)
  iRTreasury <-
    Map.fromList <$> do
      ds <- selectList [IRTreasuryDstateId ==. dstateId] []
      forM ds $ \(Entity _ IRTreasury {..}) -> do
        Credential credential <- getJust iRTreasuryCredentialId
        let !cred = intern (Keys.coerceKeyRole credential) accountsMap
        pure (cred, iRTreasuryCoin)
  pure
    Shelley.DState
      { dsAccounts = accountsFromAccountsMap accountsMap
      , dsFutureGenDelegs = unEnc dStateFGenDelegs
      , dsGenDelegs = dStateGenDelegs
      , dsIRewards =
          Shelley.InstantaneousRewards
            { iRReserves = iRReserves
            , iRTreasury = iRTreasury
            , deltaReserves = dStateIrDeltaReserves
            , deltaTreasury = dStateIrDeltaTreasury
            }
      }

loadDStateNoSharing :: MonadUnliftIO m => T.Text -> m (Shelley.DState CurrentEra)
loadDStateNoSharing fp =
  runSqlite fp $ getDStateNoSharing (DStateKey (SqlBackendKey 1))

loadUTxONoSharing ::
  MonadUnliftIO m => T.Text -> m (Shelley.UTxO CurrentEra)
loadUTxONoSharing fp =
  runSqlite fp (Shelley.UTxO <$> runConduitFold sourceUTxO noSharingMap)

loadLedgerStateNoSharing ::
  MonadUnliftIO m => T.Text -> m (Shelley.LedgerState CurrentEra)
loadLedgerStateNoSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateNoSharing ledgerStateDstateId
    m <- runConduitFold sourceUTxO noSharingMap
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
    ( Shelley.LedgerState CurrentEra
    , IntMap.IntMap (Map.Map TxIn.TxId (TxOut CurrentEra))
    )
loadLedgerStateDStateTxIxSharing fp =
  runSqlite fp $ do
    ledgerState@LedgerState {..} <- getJust lsId
    dstate <- getDStateWithSharing ledgerStateDstateId
    ls <- getLedgerState (Shelley.UTxO mempty) ledgerState dstate
    m <- runConduitFold sourceUTxO txIxSharing
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
  m <- runConduitFold sourceUTxO noSharingMap
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
  m <- runConduitFold sourceUTxO noSharingMap
  getLedgerState (Shelley.UTxO m) ledgerState dstate

loadEpochState :: MonadUnliftIO m => T.Text -> m (Shelley.EpochState CurrentEra)
loadEpochState fp = runSqlite fp $ do
  ese@(Entity _ EpochState {..}) <- getJustEntity esId
  snapshots <- getSnapShotsNoSharing ese
  ledgerState <- getLedgerStateNoSharing ese
  pure $
    Shelley.EpochState
      { esChainAccountState =
          State.ChainAccountState
            { casTreasury = epochStateTreasury
            , casReserves = epochStateReserves
            }
      , esLState = ledgerState
      , esSnapshots = snapshots
      , esNonMyopic = epochStateNonMyopic
      }
      & curPParamsEpochStateL .~ epochStatePp
      & prevPParamsEpochStateL .~ epochStatePrevPp

loadEpochStateWithSharing :: MonadUnliftIO m => T.Text -> m (Shelley.EpochState CurrentEra)
loadEpochStateWithSharing fp = runSqlite fp $ do
  ese@(Entity _ EpochState {..}) <- getJustEntity esId
  snapshots <- getSnapShotsWithSharing ese
  ledgerState <- getLedgerStateWithSharing ese
  pure $
    Shelley.EpochState
      { esChainAccountState =
          State.ChainAccountState
            { casTreasury = epochStateTreasury
            , casReserves = epochStateReserves
            }
      , esLState = ledgerState
      , esSnapshots = snapshots
      , esNonMyopic = epochStateNonMyopic
      }
      & prevPParamsEpochStateL .~ epochStatePrevPp
      & curPParamsEpochStateL .~ epochStatePp

loadSnapShotsNoSharing ::
  MonadUnliftIO m => T.Text -> Entity EpochState -> m State.SnapShots
loadSnapShotsNoSharing fp = runSqlite fp . getSnapShotsNoSharing
{-# INLINEABLE loadSnapShotsNoSharing #-}

loadSnapShotsWithSharing ::
  MonadUnliftIO m => T.Text -> Entity EpochState -> m State.SnapShots
loadSnapShotsWithSharing fp = runSqlite fp . getSnapShotsWithSharing
{-# INLINEABLE loadSnapShotsWithSharing #-}

loadSnapShotsNoSharingM :: T.Text -> Entity EpochState -> IO SnapShotsM
loadSnapShotsNoSharingM fp = runSqlite fp . getSnapShotsNoSharingM
{-# INLINEABLE loadSnapShotsNoSharingM #-}

loadSnapShotsWithSharingM :: T.Text -> Entity EpochState -> IO SnapShotsM
loadSnapShotsWithSharingM fp = runSqlite fp . getSnapShotsWithSharingM
{-# INLINEABLE loadSnapShotsWithSharingM #-}
