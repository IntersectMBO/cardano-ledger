{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}

module Cardano.Ledger.State.UTxO where

import Cardano.Binary
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Data hiding (scripts)
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Value
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.PoolDistr (individualPoolStakeVrf)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PoolRank
import Conduit
import Control.Exception (throwIO)
import Control.Foldl (Fold (..))
import Control.SetAlgebra (range)
import qualified Data.ByteString.Lazy as LBS
import Data.Compact.HashMap (toKey)
import Data.Compact.KeyMap as KeyMap
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Compact.VMap as VMap
import Data.Foldable as F
import Data.Functor
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable
import Data.UMap (delView, ptrView, rewView)
import Prettyprinter
import Text.Printf

type C = StandardCrypto

type CurrentEra = AlonzoEra C

--- Loading
readNewEpochState :: FilePath -> IO (NewEpochState CurrentEra)
readNewEpochState = readFromCBOR

readEpochState :: FilePath -> IO (EpochState CurrentEra)
readEpochState = readFromCBOR

readFromCBOR :: FromCBOR a => FilePath -> IO a
readFromCBOR fp =
  LBS.readFile fp <&> decodeFull >>= \case
    Left exc -> throwIO exc
    Right res -> pure res

writeEpochState :: FilePath -> EpochState CurrentEra -> IO ()
writeEpochState fp = LBS.writeFile fp . serialize

loadLedgerState :: FilePath -> IO (LedgerState CurrentEra)
loadLedgerState fp = esLState . nesEs <$> readNewEpochState fp

runConduitFold :: Monad m => ConduitT () a m () -> Fold a b -> m b
runConduitFold source (Fold f e g) = (g <$> runConduit (source .| foldlC f e))

type UTxOFold b = Fold (TxIn C, Alonzo.TxOut CurrentEra) b

noSharing :: Fold (TxIn C, a) (Map.Map (TxIn C) a)
noSharing = Fold (\ !m !(!k, !v) -> Map.insert k v m) mempty id

noSharing_ :: UTxOFold (Map.Map (TxIn C) ())
noSharing_ = Fold (\ !m !(!k, _) -> Map.insert k () m) mempty id

noSharingSplitMap :: Fold (TxIn C, a) (SplitMap.SplitMap (TxIn C) a)
noSharingSplitMap = Fold (\ !m !(!k, !v) -> SplitMap.insert k v m) mempty id

noSharingSplitMap_ :: UTxOFold (SplitMap.SplitMap (TxIn C) ())
noSharingSplitMap_ = Fold (\ !m !(!k, _) -> SplitMap.insert k () m) mempty id

txIdSharing ::
  UTxOFold (Map.Map (TxId C) (IntMap.IntMap (Alonzo.TxOut CurrentEra)))
txIdSharing = Fold txIdNestedInsert mempty id

txIdSharing_ :: UTxOFold (Map.Map (TxId C) (IntMap.IntMap ()))
txIdSharing_ = Fold (\a v -> txIdNestedInsert a (() <$ v)) mempty id

txIdNestedInsert ::
  Map.Map (TxId C) (IntMap.IntMap a) ->
  (TxIn C, a) ->
  Map.Map (TxId C) (IntMap.IntMap a)
txIdNestedInsert !m (TxIn !txId !txIx, !v) =
  let !e = IntMap.singleton (txIxToInt txIx) v
   in Map.insertWith (<>) txId e m

txIxSharing :: Fold (TxIn C, a) (IntMap.IntMap (Map.Map (TxId C) a))
txIxSharing = Fold txIxNestedInsert mempty id

txIxSharing_ :: UTxOFold (IntMap.IntMap (Map.Map (TxId C) ()))
txIxSharing_ = Fold (\a v -> txIxNestedInsert a (() <$ v)) mempty id

txIxNestedInsert ::
  IntMap.IntMap (Map.Map (TxId C) a) ->
  (TxIn C, a) ->
  IntMap.IntMap (Map.Map (TxId C) a)
txIxNestedInsert !im (TxIn !txId !txIx, !v) =
  let f =
        \case
          Nothing -> Just $! Map.singleton txId v
          Just !m -> Just $! Map.insert txId v m
   in IntMap.alter f (txIxToInt txIx) im

txIxSharingKeyMap :: Fold (TxIn C, a) (IntMap.IntMap (KeyMap.KeyMap a))
txIxSharingKeyMap = Fold txIxNestedInsertKeyMap mempty id

txIxSharingKeyMap_ :: UTxOFold (IntMap.IntMap (KeyMap.KeyMap ()))
txIxSharingKeyMap_ =
  Fold ((\m (k, _) -> txIxNestedInsertKeyMap m (k, ()))) mempty id

txIxNestedInsertKeyMap ::
  IntMap.IntMap (KeyMap.KeyMap a) ->
  (TxIn C, a) ->
  IntMap.IntMap (KeyMap.KeyMap a)
txIxNestedInsertKeyMap !m (TxIn txId txIx, !v) =
  let !key = toKey txId
      f =
        \case
          Nothing -> Just $! KeyMap.Leaf key v
          Just hm -> Just $! KeyMap.insert key v hm
   in IntMap.alter f (txIxToInt txIx) m

txIdSharingKeyMap :: Fold (TxIn C, a) (KeyMap.KeyMap (IntMap.IntMap a))
txIdSharingKeyMap = Fold txIdNestedInsertKeyMap KeyMap.Empty id

txIdSharingKeyMap_ :: UTxOFold (KeyMap.KeyMap (IntMap.IntMap ()))
txIdSharingKeyMap_ = Fold (\a (k, _) -> txIdNestedInsertKeyMap a (k, ())) KeyMap.Empty id

txIdNestedInsertKeyMap ::
  KeyMap.KeyMap (IntMap.IntMap a) ->
  (TxIn C, a) ->
  KeyMap.KeyMap (IntMap.IntMap a)
txIdNestedInsertKeyMap !m (TxIn txId txIx, !a) =
  let !key = toKey txId
      !v = IntMap.singleton (txIxToInt txIx) a
   in KeyMap.insertWith (<>) key v m

testKeyMap ::
  KeyMap.KeyMap (IntMap.IntMap (Alonzo.TxOut CurrentEra)) ->
  Map.Map (TxIn C) (Alonzo.TxOut CurrentEra) ->
  IO ()
testKeyMap km m =
  case Map.foldlWithKey' test km m of
    KeyMap.Empty -> putStrLn "Tested UTxO equality: Pass"
    _ -> error "Expected the KeyMap to be empty, but it's not."
  where
    test ::
      KeyMap.KeyMap (IntMap.IntMap (Alonzo.TxOut CurrentEra)) ->
      TxIn C ->
      Alonzo.TxOut CurrentEra ->
      KeyMap.KeyMap (IntMap.IntMap (Alonzo.TxOut CurrentEra))
    test acc txIn@(TxIn txId txIx) txOut =
      let !key = toKey txId
       in case KeyMap.lookup key acc of
            Nothing -> error $ "Can't find txId: " <> show txIn
            Just im ->
              let txIx' = txIxToInt txIx
                  im' = IntMap.delete txIx' im
               in case IntMap.lookup txIx' im of
                    Nothing -> error $ "Can't find txIx: " <> show txIn
                    Just txOut'
                      | txOut /= txOut' ->
                          error $ "Found mismatching TxOuts for " <> show txIn
                      | IntMap.null im' -> KeyMap.delete key acc
                      | otherwise -> KeyMap.insert key im' acc

totalADA :: Map.Map (TxIn C) (Alonzo.TxOut CurrentEra) -> Mary.Value C
totalADA = foldMap (\(Alonzo.TxOut _ v _) -> v)

readBinUTxO :: FilePath -> IO (UTxO CurrentEra)
readBinUTxO fp = do
  ls <- readNewEpochState fp
  pure $! _utxo $ lsUTxOState $ esLState $ nesEs ls

newtype Count = Count Int
  deriving (Eq, Ord, Enum, Real, Integral, Num, Pretty)

data Stat k = Stat
  { statUnique :: !(Set.Set k),
    statCount :: !Count
  }

instance Ord k => Semigroup (Stat k) where
  (<>) s1 s2 = Stat (statUnique s1 <> statUnique s2) (statCount s1 + statCount s2)

instance Ord k => Monoid (Stat k) where
  mempty = Stat mempty 0

instance Pretty (Stat k) where
  pretty Stat {..} =
    pretty n
      <+> "/"
      <+> pretty statCount
      <+> "(" <> pretty (intPercent n statCount) <> " unique)"
    where
      n = Set.size statUnique

data Percent = Percent Int Int

instance Pretty Percent where
  pretty (Percent x y) = pretty (printf "%d.%02d%%" x y :: String)

intPercent :: Integral i => Int -> i -> Percent
intPercent x y
  | y == 0 = Percent 0 0
  | otherwise = uncurry Percent (((10000 * x) `div` fromIntegral y) `quotRem` 100)

statSingleton :: a -> Stat a
statSingleton a = Stat (Set.singleton a) 1

statSet :: Set.Set a -> Stat a
statSet s = Stat s (Count (Set.size s))

statMapKeys :: Map.Map k v -> Stat k
statMapKeys = statSet . Map.keysSet

statFoldable :: (Ord a, Foldable t) => t a -> Stat a
statFoldable m = Stat (Set.fromList (F.toList m)) (Count (F.length m))

prettyRecord :: Doc ann -> [Doc ann] -> Doc ann
prettyRecord h content = h <> ":" <+> line <> indent 2 (vsep content)

(<:>) :: (Typeable a, Pretty a) => Doc ann -> a -> Doc ann
(<:>) x y =
  "[" <> x <> "]:" <+> pretty y <+> "<" <> pretty (showsTypeRep (typeOf y) ">")

infixr 6 <:>

data SnapShotStats = SnapShotStats
  { sssStake :: !(Stat (Credential 'Staking C)),
    sssDelegationCredential :: !(Stat (Credential 'Staking C)),
    sssDelegationStakePool :: !(Stat (KeyHash 'StakePool C)),
    sssPoolParams :: !(Stat (KeyHash 'StakePool C)),
    sssPoolParamsStats :: !PoolParamsStats
  }

instance Semigroup SnapShotStats where
  (<>) (SnapShotStats x1 x2 x3 x4 x5) (SnapShotStats y1 y2 y3 y4 y5) =
    SnapShotStats
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)
      (x4 <> y4)
      (x5 <> y5)

instance Monoid SnapShotStats where
  mempty = SnapShotStats mempty mempty mempty mempty mempty

instance Pretty SnapShotStats where
  pretty SnapShotStats {..} =
    prettyRecord
      "SnapShot"
      [ "Stake" <:> sssStake,
        "DelegationCredential" <:> sssDelegationCredential,
        "DelegationStakePool" <:> sssDelegationStakePool,
        "PoolParams" <:> sssPoolParams,
        pretty sssPoolParamsStats
      ]

instance AggregateStat SnapShotStats where
  aggregateStat SnapShotStats {..} =
    (aggregateStat sssPoolParamsStats)
      { gsCredentialStaking = sssStake <> sssDelegationCredential,
        gsKeyHashStakePool = sssDelegationStakePool <> sssPoolParams
      }

countSnapShotStat :: SnapShot C -> SnapShotStats
countSnapShotStat SnapShot {..} =
  SnapShotStats
    { sssStake = statMapKeys (VMap.toMap (unStake _stake)),
      sssDelegationCredential = statMapKeys (VMap.toMap _delegations),
      sssDelegationStakePool = statFoldable (VMap.toMap _delegations),
      sssPoolParams = statMapKeys (VMap.toMap _poolParams),
      sssPoolParamsStats = VMap.foldMap countPoolParamsStats _poolParams
    }

data PoolParamsStats = PoolParamsStats
  { ppsPoolId :: !(Stat (KeyHash 'StakePool C)),
    ppsRewardAcnt :: !(Stat (Credential 'Staking C)),
    ppsOwners :: !(Stat (KeyHash 'Staking C))
  }

instance Semigroup PoolParamsStats where
  (<>) (PoolParamsStats x1 x2 x3) (PoolParamsStats y1 y2 y3) =
    PoolParamsStats
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)

instance Monoid PoolParamsStats where
  mempty = PoolParamsStats mempty mempty mempty

instance Pretty PoolParamsStats where
  pretty PoolParamsStats {..} =
    prettyRecord
      "PoolParamsStats"
      [ "PoolId" <:> ppsPoolId,
        "RewardAcnt" <:> ppsRewardAcnt,
        "Owners" <:> ppsOwners
      ]

instance AggregateStat PoolParamsStats where
  aggregateStat PoolParamsStats {..} =
    mempty {gsCredentialStaking = ppsRewardAcnt, gsKeyHashStakePool = ppsPoolId}

countPoolParamsStats :: PoolParams C -> PoolParamsStats
countPoolParamsStats PoolParams {..} =
  PoolParamsStats
    { ppsPoolId = statSingleton _poolId,
      ppsRewardAcnt = statSingleton (getRwdCred _poolRAcnt),
      ppsOwners = statSet _poolOwners
    }

data RewardUpdateStats = RewardUpdateStats

instance Pretty RewardUpdateStats where
  pretty RewardUpdateStats {} =
    prettyRecord "RewardUpdateStats" []

instance AggregateStat RewardUpdateStats where
  aggregateStat RewardUpdateStats = mempty

data PoolDistrStats = PoolDistrStats
  { pdsStakePoolKeyHash :: !(Stat (KeyHash 'StakePool C)),
    pdsStakePoolStakeVrf :: !(Stat (Hash C (VerKeyVRF C)))
  }

instance Pretty PoolDistrStats where
  pretty PoolDistrStats {..} =
    prettyRecord
      "PoolDistrStats"
      [ "StakePoolKeyHash" <:> pdsStakePoolKeyHash,
        "StakePoolStakeVrf" <:> pdsStakePoolStakeVrf
      ]

instance AggregateStat PoolDistrStats where
  aggregateStat PoolDistrStats {..} =
    mempty
      { gsKeyHashStakePool = pdsStakePoolKeyHash,
        gsVerKeyVRF = pdsStakePoolStakeVrf
      }

calcPoolDistrStats :: PoolDistr C -> PoolDistrStats
calcPoolDistrStats (PoolDistr pd) =
  PoolDistrStats
    { pdsStakePoolKeyHash = statMapKeys pd,
      pdsStakePoolStakeVrf = statFoldable (individualPoolStakeVrf <$> Map.elems pd)
    }

data NewEpochStateStats = NewEpochStateStats
  { nessPrevBlocksMade :: !(Stat (KeyHash 'StakePool C)),
    nessCurBlocksMade :: !(Stat (KeyHash 'StakePool C)),
    nessBlocksMade :: !(Stat (KeyHash 'StakePool C)),
    nessEpochStateStats :: !EpochStateStats,
    nessRewardUpdate :: !RewardUpdateStats,
    nessPoolDistrStats :: !PoolDistrStats,
    nessAggregateStats :: !AggregateStats
  }

instance Pretty NewEpochStateStats where
  pretty NewEpochStateStats {..} =
    prettyRecord
      "NewEpochStateStats"
      [ "PrevBlocksMade" <:> statCount nessPrevBlocksMade,
        "CurBlocksMade" <:> statCount nessCurBlocksMade,
        "BlocksMade" <:> nessBlocksMade,
        pretty nessEpochStateStats,
        pretty nessRewardUpdate <> "TODO",
        pretty nessPoolDistrStats,
        pretty nessAggregateStats
      ]

countNewEpochStateStats :: NewEpochState CurrentEra -> NewEpochStateStats
countNewEpochStateStats NewEpochState {..} =
  let ness =
        NewEpochStateStats
          { nessPrevBlocksMade = statMapKeys (unBlocksMade nesBprev),
            nessCurBlocksMade = statMapKeys (unBlocksMade nesBcur),
            nessBlocksMade = mempty,
            nessEpochStateStats = countEpochStateStats nesEs,
            nessRewardUpdate = RewardUpdateStats,
            nessPoolDistrStats = calcPoolDistrStats nesPd,
            nessAggregateStats = mempty
          }
   in ness
        { nessBlocksMade = nessPrevBlocksMade ness <> nessCurBlocksMade ness,
          nessAggregateStats =
            mconcat
              [ aggregateStat (nessPrevBlocksMade ness),
                aggregateStat (nessCurBlocksMade ness),
                aggregateStat (nessRewardUpdate ness),
                essAggregateStats (nessEpochStateStats ness),
                aggregateStat (nessPoolDistrStats ness)
              ]
        }

printNewEpochStateStats :: NewEpochStateStats -> IO ()
printNewEpochStateStats = putStrLn . show . pretty

data EpochStateStats = EpochStateStats
  { essMarkSnapShotStats :: !SnapShotStats,
    essSetSnapShotStats :: !SnapShotStats,
    essGoSnapShotStats :: !SnapShotStats,
    essSnapShotsStats :: !SnapShotStats,
    essLedgerStateStats :: !LedgerStateStats,
    essNonMyopic :: !(Stat (KeyHash 'StakePool C)),
    essAggregateStats :: !AggregateStats
  }

instance Pretty EpochStateStats where
  pretty EpochStateStats {..} =
    prettyRecord
      "EpochStateStats"
      [ "mark" <:> statCount (sssStake essMarkSnapShotStats),
        "set" <:> statCount (sssStake essSetSnapShotStats),
        "go" <:> statCount (sssStake essGoSnapShotStats),
        "mark+set+go =" <+> pretty essSnapShotsStats,
        pretty essLedgerStateStats,
        "NonMyopic" <:> essNonMyopic,
        pretty essAggregateStats
      ]

countEpochStateStats :: EpochState CurrentEra -> EpochStateStats
countEpochStateStats EpochState {..} =
  let markSnap = countSnapShotStat (_pstakeMark esSnapshots)
      setSnap = countSnapShotStat (_pstakeSet esSnapshots)
      goSnap = countSnapShotStat (_pstakeGo esSnapshots)
      stats =
        EpochStateStats
          { essMarkSnapShotStats = markSnap,
            essSetSnapShotStats = setSnap,
            essGoSnapShotStats = goSnap,
            essSnapShotsStats = markSnap <> setSnap <> goSnap,
            essLedgerStateStats = countLedgerStateStats esLState,
            essNonMyopic = statMapKeys (likelihoodsNM esNonMyopic),
            essAggregateStats = mempty
          }
   in stats
        { essAggregateStats =
            mconcat
              [ aggregateStat (essSnapShotsStats stats),
                aggregateStat (essLedgerStateStats stats),
                aggregateStat (essNonMyopic stats)
              ]
        }

data DStateStats = DStateStats
  { dssCredentialStaking :: !(Stat (Credential 'Staking C)),
    dssDelegations :: !(Stat (KeyHash 'StakePool C)),
    dssKeyHashGenesis :: !(Stat (KeyHash 'Genesis C)),
    dssKeyHashGenesisDelegate :: !(Stat (KeyHash 'GenesisDelegate C)),
    dssHashVerKeyVRF :: !(Stat (Hash C (VerKeyVRF C)))
  }

instance Pretty DStateStats where
  pretty DStateStats {..} =
    prettyRecord
      "DStateStats"
      [ "CredentialStaking" <:> dssCredentialStaking,
        "Delegations" <:> dssDelegations,
        "KeyHashGenesis" <:> dssKeyHashGenesis,
        "KeyHashGenesisDelegate" <:> dssKeyHashGenesisDelegate,
        "HashVerKeyVRF" <:> dssHashVerKeyVRF
      ]

instance AggregateStat DStateStats where
  aggregateStat DStateStats {..} =
    mempty
      { gsCredentialStaking = dssCredentialStaking,
        gsKeyHashStakePool = dssDelegations,
        gsKeyHashGenesis = dssKeyHashGenesis,
        gsKeyHashGenesisDelegate = dssKeyHashGenesisDelegate,
        gsVerKeyVRF = dssHashVerKeyVRF
      }

countDStateStats :: DState C -> DStateStats
countDStateStats DState {..} =
  DStateStats
    { dssCredentialStaking =
        statMapKeys (rewView _unified)
          <> statMapKeys (delView _unified)
          <> statSet (range (ptrView _unified)),
      dssDelegations = statFoldable (delView _unified),
      dssKeyHashGenesis =
        statFoldable (fGenDelegGenKeyHash <$> Map.keys _fGenDelegs)
          <> statMapKeys (unGenDelegs _genDelegs),
      dssKeyHashGenesisDelegate =
        statFoldable (genDelegKeyHash <$> Map.elems _fGenDelegs)
          <> statFoldable
            (genDelegKeyHash <$> Map.elems (unGenDelegs _genDelegs)),
      dssHashVerKeyVRF =
        statFoldable (genDelegVrfHash <$> Map.elems _fGenDelegs)
          <> statFoldable
            (genDelegVrfHash <$> Map.elems (unGenDelegs _genDelegs))
    }

data PStateStats = PStateStats
  { pssKeyHashStakePool :: !(Stat (KeyHash 'StakePool C)),
    pssPoolParamsStats :: !PoolParamsStats
  }

instance Pretty PStateStats where
  pretty PStateStats {..} =
    prettyRecord
      "PStateStats"
      [ "KeyHashStakePool" <:> pssKeyHashStakePool,
        pretty pssPoolParamsStats
      ]

instance AggregateStat PStateStats where
  aggregateStat PStateStats {..} =
    (aggregateStat pssPoolParamsStats) {gsKeyHashStakePool = pssKeyHashStakePool}

countPStateStats :: PState C -> PStateStats
countPStateStats PState {..} =
  PStateStats
    { pssKeyHashStakePool =
        statMapKeys _pParams
          <> statMapKeys _fPParams
          <> statMapKeys _retiring,
      pssPoolParamsStats =
        foldMap countPoolParamsStats _pParams <> foldMap countPoolParamsStats _fPParams
    }

data LedgerStateStats = LedgerStateStats
  { lssUTxOStats :: !UTxOStats,
    lssDStateStats :: !DStateStats,
    lssPStateStats :: !PStateStats
  }

instance Pretty LedgerStateStats where
  pretty LedgerStateStats {..} =
    prettyRecord
      "LedgerStateStats"
      [ pretty lssUTxOStats,
        pretty lssDStateStats,
        pretty lssPStateStats
      ]

instance AggregateStat LedgerStateStats where
  aggregateStat LedgerStateStats {..} =
    mconcat
      [ aggregateStat lssUTxOStats,
        aggregateStat lssDStateStats,
        aggregateStat lssPStateStats
      ]

countLedgerStateStats :: LedgerState CurrentEra -> LedgerStateStats
countLedgerStateStats LedgerState {..} =
  LedgerStateStats
    { lssUTxOStats = countUTxOStats (_utxo lsUTxOState),
      lssDStateStats = countDStateStats (dpsDState lsDPState),
      lssPStateStats = countPStateStats (dpsPState lsDPState)
    }

data TxInStats = TxInStats
  { tisTxId :: !(Stat (TxId C)),
    tisTxIx :: !(Stat TxIx)
  }

instance Pretty TxInStats where
  pretty TxInStats {..} =
    prettyRecord "TxInStats" ["TxId" <:> tisTxId, "TxIx" <:> tisTxIx]

countTxInStats :: [TxIn C] -> TxInStats
countTxInStats txIns =
  case unzip (fmap (\(TxIn txId txIx) -> (txId, txIx)) txIns) of
    (txIds, txIxs) ->
      TxInStats
        { tisTxId = statFoldable txIds,
          tisTxIx = statFoldable txIxs
        }

data TxOutStats = TxOutStats
  { tosBootstrap :: !(Stat (BootstrapAddress C)),
    tosPaymentCredential :: !(Stat (Credential 'Payment C)),
    tosStakingCredential :: !(Stat (Credential 'Staking C)),
    tosStakingPtr :: !(Stat Ptr),
    tosNetwork :: !(Stat Network),
    tosValue :: !(Stat Integer),
    tosPolicyId :: !(Stat (PolicyID C)),
    tosAssetName :: !(Stat AssetName),
    tosAssetValue :: !(Stat Integer),
    tosDataHash :: !(Stat (DataHash C))
  }

instance Semigroup TxOutStats where
  (<>) (TxOutStats x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) (TxOutStats y0 y1 y2 y3 y4 y5 y6 y7 y8 y9) =
    TxOutStats
      (x0 <> y0)
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)
      (x4 <> y4)
      (x5 <> y5)
      (x6 <> y6)
      (x7 <> y7)
      (x8 <> y8)
      (x9 <> y9)

instance Monoid TxOutStats where
  mempty = TxOutStats mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance Pretty TxOutStats where
  pretty TxOutStats {..} =
    prettyRecord
      "TxOutStats"
      [ "Bootstrap" <:> tosBootstrap,
        "PaymentCredential" <:> tosPaymentCredential,
        "StakingCredential" <:> tosStakingCredential,
        "StakingPtr" <:> tosStakingPtr,
        "Network" <:> tosNetwork,
        "Value" <:> tosValue,
        "PolicyId" <:> tosPolicyId,
        "AssetName" <:> tosAssetName,
        "AssetValue" <:> tosAssetValue,
        "DataHash" <:> tosDataHash
      ]

instance AggregateStat TxOutStats where
  aggregateStat TxOutStats {..} = aggregateStat tosStakingCredential

countTxOutStats :: [Alonzo.TxOut CurrentEra] -> TxOutStats
countTxOutStats = foldMap countTxOutStat
  where
    countTxOutStat :: Alonzo.TxOut CurrentEra -> TxOutStats
    countTxOutStat (Alonzo.TxOut addr (Value v vm) mData) =
      let !dataStat =
            strictMaybe
              mempty
              (\d -> mempty {tosDataHash = statSingleton d})
              mData
          !vmElems = Map.elems vm
          !valueStat =
            dataStat
              { tosValue = statSingleton v,
                tosPolicyId = statMapKeys vm,
                tosAssetName = foldMap statMapKeys vmElems,
                tosAssetValue = foldMap statFoldable vmElems
              }
          !networkStat = valueStat {tosNetwork = statSingleton (getNetwork addr)}
       in case addr of
            AddrBootstrap addrBootstrap ->
              networkStat {tosBootstrap = statSingleton addrBootstrap}
            Addr _ pc sr ->
              let stakeStat =
                    case sr of
                      StakeRefNull -> networkStat
                      StakeRefPtr ptr ->
                        networkStat {tosStakingPtr = statSingleton ptr}
                      StakeRefBase cred ->
                        networkStat {tosStakingCredential = statSingleton cred}
               in stakeStat {tosPaymentCredential = statSingleton pc}

data UTxOStats = UTxOStats
  { usTxInStats :: !TxInStats,
    usTxOutStats :: !TxOutStats
  }

instance Pretty UTxOStats where
  pretty UTxOStats {..} =
    prettyRecord
      "UTxOStats"
      [pretty usTxInStats, pretty usTxOutStats]

instance AggregateStat UTxOStats where
  aggregateStat = aggregateStat . usTxOutStats

countUTxOStats :: UTxO (AlonzoEra StandardCrypto) -> UTxOStats
countUTxOStats (UTxO m) =
  UTxOStats
    { usTxInStats = countTxInStats (SplitMap.keys m),
      usTxOutStats = countTxOutStats (SplitMap.elems m)
    }

data AggregateStats = AggregateStats
  { gsCredentialStaking :: !(Stat (Credential 'Staking C)),
    gsKeyHashStakePool :: !(Stat (KeyHash 'StakePool C)),
    gsKeyHashGenesis :: !(Stat (KeyHash 'Genesis C)),
    gsKeyHashGenesisDelegate :: !(Stat (KeyHash 'GenesisDelegate C)),
    gsVerKeyVRF :: !(Stat (Hash C (VerKeyVRF C))),
    gsScriptHash :: !(Stat (ScriptHash C))
  }

instance Semigroup AggregateStats where
  (<>) (AggregateStats x1 x2 x3 x4 x5 x6) (AggregateStats y1 y2 y3 y4 y5 y6) =
    AggregateStats
      (x1 <> y1)
      (x2 <> y2)
      (x3 <> y3)
      (x4 <> y4)
      (x5 <> y5)
      (x6 <> y6)

instance Monoid AggregateStats where
  mempty = AggregateStats mempty mempty mempty mempty mempty mempty

instance Pretty AggregateStats where
  pretty AggregateStats {..} =
    prettyRecord
      "AggregateStats"
      [ "StakingCredential" <:> gsCredentialStaking,
        "KeyHashStakePool" <:> gsKeyHashStakePool,
        "ScriptHash" <:> gsScriptHash
      ]

class AggregateStat s where
  aggregateStat :: s -> AggregateStats

instance AggregateStat (Stat (Credential 'Staking C)) where
  aggregateStat s = mempty {gsCredentialStaking = s}

instance AggregateStat (Stat (KeyHash 'StakePool C)) where
  aggregateStat s = mempty {gsKeyHashStakePool = s}

instance AggregateStat (Stat (ScriptHash C)) where
  aggregateStat s = mempty {gsScriptHash = s}

-- Initial attempt at UTxO stats, which was mostly superseded by the above
-- approach that works for the whole state

data UTxOUniques = UTxOUniques
  { paymentKeys :: !(Set.Set (KeyHash 'Payment C)),
    paymentScripts :: !(Set.Set (ScriptHash C)),
    stakeKeys :: !(Set.Set (KeyHash 'Staking C)),
    stakeScripts :: !(Set.Set (ScriptHash C)),
    stakePtrs :: !(Set.Set Ptr),
    scripts :: !(Set.Set (ScriptHash C)),
    txIds :: !(Set.Set (TxId C)),
    txIxs :: !(Set.Set TxIx)
  }

emptyUniques :: UTxOUniques
emptyUniques = UTxOUniques mempty mempty mempty mempty mempty mempty mempty mempty

data UTxOStats' = UTxOStats'
  { statsTotalTxOuts :: !Int,
    statsByronTxOuts :: !Int,
    statsTotalPaymentKeys :: !Int,
    statsTotalPaymentScripts :: !Int,
    statsTotalStakeKeys :: !Int,
    statsTotalStakeScripts :: !Int,
    statsTotalStakePtrs :: !Int,
    stateTotalStakeNulls :: !Int
  }
  deriving (Show)

initStats :: UTxOStats'
initStats = UTxOStats' 0 0 0 0 0 0 0 0

collectStats :: ConduitT (TxIn C, Alonzo.TxOut CurrentEra) Void IO ()
collectStats = do
  (uniques, stats) <- foldlC collect (emptyUniques, initStats)
  lift $ reportStats uniques stats
  where
    collect ::
      (UTxOUniques, UTxOStats') ->
      (TxIn C, Alonzo.TxOut CurrentEra) ->
      (UTxOUniques, UTxOStats')
    collect (u@UTxOUniques {..}, s@UTxOStats' {..}) (TxIn txId txIx, Alonzo.TxOut addr _val _datum) =
      let u' = u {txIds = Set.insert txId txIds, txIxs = Set.insert txIx txIxs}
          s' = s {statsTotalTxOuts = statsTotalTxOuts + 1}
          updateStakingStats sr (su, ss) =
            case sr of
              StakeRefNull ->
                (su, ss {stateTotalStakeNulls = stateTotalStakeNulls + 1})
              StakeRefPtr ptr ->
                ( su {stakePtrs = Set.insert ptr stakePtrs},
                  ss {statsTotalStakePtrs = statsTotalStakePtrs + 1}
                )
              StakeRefBase a
                | KeyHashObj kh <- a ->
                    ( su {stakeKeys = Set.insert kh stakeKeys},
                      ss {statsTotalStakeKeys = statsTotalStakeKeys + 1}
                    )
                | ScriptHashObj sh <- a ->
                    ( su {stakeScripts = Set.insert sh stakeScripts},
                      ss {statsTotalStakeScripts = statsTotalStakeScripts + 1}
                    )
       in case addr of
            AddrBootstrap _ ->
              (u', s' {statsByronTxOuts = statsByronTxOuts + 1})
            Addr _ni pc sr
              | KeyHashObj kh <- pc ->
                  updateStakingStats
                    sr
                    ( u' {paymentKeys = Set.insert kh paymentKeys},
                      s' {statsTotalPaymentKeys = statsTotalPaymentKeys + 1}
                    )
              | ScriptHashObj kh <- pc ->
                  updateStakingStats
                    sr
                    ( u' {paymentScripts = Set.insert kh paymentScripts},
                      s' {statsTotalPaymentScripts = statsTotalPaymentScripts + 1}
                    )

reportStats :: UTxOUniques -> UTxOStats' -> IO ()
reportStats UTxOUniques {..} UTxOStats' {..} = do
  let showPercent x y
        | y == 0 = "0"
        | otherwise =
            case ((1000 * x) `div` y) `quotRem` 10 of
              (q, r) ->
                show x <> ", " <> show q <> "." <> show r <> "% of total"
  putStrLn $
    unlines
      [ "Total TxOuts = " <> show statsTotalTxOuts,
        "Byron TxOuts = " <> showPercent statsByronTxOuts statsTotalTxOuts,
        "Unique TxIds = " <> showPercent (Set.size txIds) statsTotalTxOuts,
        "Unique TxIxs = " <> showPercent (Set.size txIxs) statsTotalTxOuts,
        "Shelley Total Payment Keys = " <> show statsTotalPaymentKeys,
        "Shelley Unique Payment Keys = " <> showPercent (Set.size paymentKeys) statsTotalPaymentKeys,
        "Shelley Total Payment Scripts = " <> show statsTotalPaymentScripts,
        "Shelley Unique Payment Scripts = "
          <> showPercent (Set.size paymentScripts) statsTotalPaymentScripts,
        "Shelley Total Stake Keys = " <> show statsTotalStakeKeys,
        "Shelley Unique Stake Keys = " <> showPercent (Set.size stakeKeys) statsTotalStakeKeys,
        "Shelley Total Stake Scripts = " <> show statsTotalStakeScripts,
        "Shelley Unique Stake Scripts = "
          <> showPercent (Set.size stakeScripts) statsTotalStakeScripts,
        "Shelley Total Stake Ptrs = " <> show statsTotalStakePtrs,
        "Shelley Unique Stake Ptrs = " <> showPercent (Set.size stakePtrs) statsTotalStakePtrs
      ]
