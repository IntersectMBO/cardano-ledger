{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}

module Cardano.Ledger.State.UTxO where

import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys hiding (Hash)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.State
import Cardano.Ledger.TxIn
import Conduit
import Control.Exception (throwIO)
import Control.Foldl (Fold (..))
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable as F
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Typeable
import qualified Data.VMap as VMap
import Lens.Micro
import Prettyprinter
import Text.Printf

type CurrentEra = ConwayEra

--- Loading
readNewEpochState ::
  FilePath ->
  IO (NewEpochState CurrentEra)
readNewEpochState = readDecCBOR

readEpochState ::
  FilePath ->
  IO (EpochState CurrentEra)
readEpochState = readDecCBOR

readHexUTxO ::
  FilePath ->
  IO (UTxO CurrentEra)
readHexUTxO = readDecCBORHex

readDecCBOR :: FromCBOR a => FilePath -> IO a
readDecCBOR = either throwIO pure . Plain.decodeFull <=< LBS.readFile

readDecCBORHex :: FromCBOR a => FilePath -> IO a
readDecCBORHex = either throwIO pure . decodeFullHex <=< LBS.readFile
  where
    decodeFullHex =
      Plain.decodeFull
        <=< first (DecoderErrorCustom "Invalid Hex encoding:" . T.pack) . Base16.decode

writeEpochState :: FilePath -> EpochState CurrentEra -> IO ()
writeEpochState fp = LBS.writeFile fp . Plain.serialize

loadLedgerState ::
  FilePath ->
  IO (LedgerState CurrentEra)
loadLedgerState fp = esLState . nesEs <$> readNewEpochState fp

runConduitFold :: Monad m => ConduitT () a m () -> Fold a b -> m b
runConduitFold source (Fold f e g) = (g <$> runConduit (source .| foldlC f e))

type UTxOFold b = Fold (TxIn, TxOut CurrentEra) b

noSharing :: Fold (TxIn, a) (Map.Map TxIn a)
noSharing = Fold (\ !m !(!k, !v) -> Map.insert k v m) mempty id

noSharing_ :: UTxOFold (Map.Map TxIn ())
noSharing_ = Fold (\ !m !(!k, _) -> Map.insert k () m) mempty id

noSharingMap :: Fold (TxIn, a) (Map.Map TxIn a)
noSharingMap = Fold (\ !m !(!k, !v) -> Map.insert k v m) mempty id

noSharingMap_ :: UTxOFold (Map.Map TxIn ())
noSharingMap_ = Fold (\ !m !(!k, _) -> Map.insert k () m) mempty id

txIdSharing ::
  UTxOFold (Map.Map TxId (IntMap.IntMap (TxOut CurrentEra)))
txIdSharing = Fold txIdNestedInsert mempty id

txIdSharing_ :: UTxOFold (Map.Map TxId (IntMap.IntMap ()))
txIdSharing_ = Fold (\a v -> txIdNestedInsert a (() <$ v)) mempty id

txIdNestedInsert ::
  Map.Map TxId (IntMap.IntMap a) ->
  (TxIn, a) ->
  Map.Map TxId (IntMap.IntMap a)
txIdNestedInsert !m (TxIn !txId !txIx, !v) =
  let !e = IntMap.singleton (txIxToInt txIx) v
   in Map.insertWith (<>) txId e m

txIxSharing :: Fold (TxIn, a) (IntMap.IntMap (Map.Map TxId a))
txIxSharing = Fold txIxNestedInsert mempty id

txIxSharing_ :: UTxOFold (IntMap.IntMap (Map.Map TxId ()))
txIxSharing_ = Fold (\a v -> txIxNestedInsert a (() <$ v)) mempty id

txIxNestedInsert ::
  IntMap.IntMap (Map.Map TxId a) ->
  (TxIn, a) ->
  IntMap.IntMap (Map.Map TxId a)
txIxNestedInsert !im (TxIn !txId !txIx, !v) =
  let f =
        \case
          Nothing -> Just $! Map.singleton txId v
          Just !m -> Just $! Map.insert txId v m
   in IntMap.alter f (txIxToInt txIx) im

totalADA :: Map.Map TxIn (TxOut CurrentEra) -> MaryValue
totalADA = foldMap (^. valueTxOutL)

readBinUTxO ::
  FilePath ->
  IO (UTxO CurrentEra)
readBinUTxO fp = do
  ls <- readNewEpochState fp
  pure $! utxosUtxo $ lsUTxOState $ esLState $ nesEs ls

newtype Count = Count Int
  deriving (Eq, Ord, Enum, Real, Integral, Num, Pretty)

data Stat k = Stat
  { statUnique :: !(Set.Set k)
  , statCount :: !Count
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
      <+> "("
      <> pretty (intPercent n statCount)
      <> " unique)"
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

mapStat :: Ord b => (a -> b) -> Stat a -> Stat b
mapStat f s = Stat (Set.map f (statUnique s)) (statCount s)

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
  { sssStake :: !(Stat (Credential 'Staking))
  , sssDelegationCredential :: !(Stat (Credential 'Staking))
  , sssDelegationStakePool :: !(Stat (KeyHash 'StakePool))
  , sssPoolParams :: !(Stat (KeyHash 'StakePool))
  , sssPoolParamsStats :: !PoolParamsStats
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
      [ "Stake" <:> sssStake
      , "DelegationCredential" <:> sssDelegationCredential
      , "DelegationStakePool" <:> sssDelegationStakePool
      , "PoolParams" <:> sssPoolParams
      , pretty sssPoolParamsStats
      ]

instance AggregateStat SnapShotStats where
  aggregateStat SnapShotStats {..} =
    (aggregateStat sssPoolParamsStats)
      { gsCredentialStaking = sssStake <> sssDelegationCredential
      , gsKeyHashStakePool = sssDelegationStakePool <> sssPoolParams
      }

countSnapShotStat :: SnapShot -> SnapShotStats
countSnapShotStat SnapShot {..} =
  SnapShotStats
    { sssStake = statMapKeys (VMap.toMap (unStake ssStake))
    , sssDelegationCredential = statMapKeys (VMap.toMap ssDelegations)
    , sssDelegationStakePool = statFoldable (VMap.toMap ssDelegations)
    , sssPoolParams = statMapKeys (VMap.toMap ssPoolParams)
    , sssPoolParamsStats = VMap.foldMap countPoolParamsStats ssPoolParams
    }

data PoolParamsStats = PoolParamsStats
  { ppsPoolId :: !(Stat (KeyHash 'StakePool))
  , ppsRewardAccount :: !(Stat (Credential 'Staking))
  , ppsOwners :: !(Stat (KeyHash 'Staking))
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
      [ "PoolId" <:> ppsPoolId
      , "RewardAccount" <:> ppsRewardAccount
      , "Owners" <:> ppsOwners
      ]

instance AggregateStat PoolParamsStats where
  aggregateStat PoolParamsStats {..} =
    mempty {gsCredentialStaking = ppsRewardAccount, gsKeyHashStakePool = ppsPoolId}

countPoolParamsStats :: PoolParams -> PoolParamsStats
countPoolParamsStats PoolParams {..} =
  PoolParamsStats
    { ppsPoolId = statSingleton ppId
    , ppsRewardAccount = statSingleton (raCredential ppRewardAccount)
    , ppsOwners = statSet ppOwners
    }

data RewardUpdateStats = RewardUpdateStats

instance Pretty RewardUpdateStats where
  pretty RewardUpdateStats {} =
    prettyRecord "RewardUpdateStats" []

instance AggregateStat RewardUpdateStats where
  aggregateStat RewardUpdateStats = mempty

data PoolDistrStats = PoolDistrStats
  { pdsStakePoolKeyHash :: !(Stat (KeyHash 'StakePool))
  , pdsStakePoolStakeVrf :: !(Stat (VRFVerKeyHash 'StakePoolVRF))
  }

instance Pretty PoolDistrStats where
  pretty PoolDistrStats {..} =
    prettyRecord
      "PoolDistrStats"
      [ "StakePoolKeyHash" <:> pdsStakePoolKeyHash
      , "StakePoolStakeVrf" <:> pdsStakePoolStakeVrf
      ]

instance AggregateStat PoolDistrStats where
  aggregateStat PoolDistrStats {..} =
    mempty
      { gsKeyHashStakePool = pdsStakePoolKeyHash
      , gsVerKeyVRF = mapStat unVRFVerKeyHash pdsStakePoolStakeVrf
      }

calcPoolDistrStats :: PoolDistr -> PoolDistrStats
calcPoolDistrStats (PoolDistr pd _tot) =
  PoolDistrStats
    { pdsStakePoolKeyHash = statMapKeys pd
    , pdsStakePoolStakeVrf = statFoldable (individualPoolStakeVrf <$> Map.elems pd)
    }

data NewEpochStateStats = NewEpochStateStats
  { nessPrevBlocksMade :: !(Stat (KeyHash 'StakePool))
  , nessCurBlocksMade :: !(Stat (KeyHash 'StakePool))
  , nessBlocksMade :: !(Stat (KeyHash 'StakePool))
  , nessEpochStateStats :: !EpochStateStats
  , nessRewardUpdate :: !RewardUpdateStats
  , nessPoolDistrStats :: !PoolDistrStats
  , nessAggregateStats :: !AggregateStats
  }

instance Pretty NewEpochStateStats where
  pretty NewEpochStateStats {..} =
    prettyRecord
      "NewEpochStateStats"
      [ "PrevBlocksMade" <:> statCount nessPrevBlocksMade
      , "CurBlocksMade" <:> statCount nessCurBlocksMade
      , "BlocksMade" <:> nessBlocksMade
      , pretty nessEpochStateStats
      , pretty nessRewardUpdate <> "TODO"
      , pretty nessPoolDistrStats
      , pretty nessAggregateStats
      ]

countNewEpochStateStats :: NewEpochState CurrentEra -> NewEpochStateStats
countNewEpochStateStats NewEpochState {..} =
  let ness =
        NewEpochStateStats
          { nessPrevBlocksMade = statMapKeys (unBlocksMade nesBprev)
          , nessCurBlocksMade = statMapKeys (unBlocksMade nesBcur)
          , nessBlocksMade = mempty
          , nessEpochStateStats = countEpochStateStats nesEs
          , nessRewardUpdate = RewardUpdateStats
          , nessPoolDistrStats = calcPoolDistrStats nesPd
          , nessAggregateStats = mempty
          }
   in ness
        { nessBlocksMade = nessPrevBlocksMade ness <> nessCurBlocksMade ness
        , nessAggregateStats =
            mconcat
              [ aggregateStat (nessPrevBlocksMade ness)
              , aggregateStat (nessCurBlocksMade ness)
              , aggregateStat (nessRewardUpdate ness)
              , essAggregateStats (nessEpochStateStats ness)
              , aggregateStat (nessPoolDistrStats ness)
              ]
        }

printNewEpochStateStats :: NewEpochStateStats -> IO ()
printNewEpochStateStats = putStrLn . show . pretty

data EpochStateStats = EpochStateStats
  { essMarkSnapShotStats :: !SnapShotStats
  , essSetSnapShotStats :: !SnapShotStats
  , essGoSnapShotStats :: !SnapShotStats
  , essSnapShotsStats :: !SnapShotStats
  , essLedgerStateStats :: !LedgerStateStats
  , essNonMyopic :: !(Stat (KeyHash 'StakePool))
  , essAggregateStats :: !AggregateStats
  }

instance Pretty EpochStateStats where
  pretty EpochStateStats {..} =
    prettyRecord
      "EpochStateStats"
      [ "mark" <:> statCount (sssStake essMarkSnapShotStats)
      , "set" <:> statCount (sssStake essSetSnapShotStats)
      , "go" <:> statCount (sssStake essGoSnapShotStats)
      , "mark+set+go =" <+> pretty essSnapShotsStats
      , pretty essLedgerStateStats
      , "NonMyopic" <:> essNonMyopic
      , pretty essAggregateStats
      ]

countEpochStateStats :: EpochState CurrentEra -> EpochStateStats
countEpochStateStats EpochState {..} =
  let markSnap = countSnapShotStat (ssStakeMark esSnapshots)
      setSnap = countSnapShotStat (ssStakeSet esSnapshots)
      goSnap = countSnapShotStat (ssStakeGo esSnapshots)
      stats =
        EpochStateStats
          { essMarkSnapShotStats = markSnap
          , essSetSnapShotStats = setSnap
          , essGoSnapShotStats = goSnap
          , essSnapShotsStats = markSnap <> setSnap <> goSnap
          , essLedgerStateStats = countLedgerStateStats esLState
          , essNonMyopic = statMapKeys (likelihoodsNM esNonMyopic)
          , essAggregateStats = mempty
          }
   in stats
        { essAggregateStats =
            mconcat
              [ aggregateStat (essSnapShotsStats stats)
              , aggregateStat (essLedgerStateStats stats)
              , aggregateStat (essNonMyopic stats)
              ]
        }

data DStateStats = DStateStats
  { dssCredentialStaking :: !(Stat (Credential 'Staking))
  , dssDelegations :: !(Stat (KeyHash 'StakePool))
  , dssKeyHashGenesis :: !(Stat (KeyHash 'Genesis))
  , dssKeyHashGenesisDelegate :: !(Stat (KeyHash 'GenesisDelegate))
  , dssHashVerKeyVRF :: !(Stat (VRFVerKeyHash 'GenDelegVRF))
  }

instance Pretty DStateStats where
  pretty DStateStats {..} =
    prettyRecord
      "DStateStats"
      [ "CredentialStaking" <:> dssCredentialStaking
      , "SPoolUView" <:> dssDelegations
      , "KeyHashGenesis" <:> dssKeyHashGenesis
      , "KeyHashGenesisDelegate" <:> dssKeyHashGenesisDelegate
      , "HashVerKeyVRF" <:> dssHashVerKeyVRF
      ]

instance AggregateStat DStateStats where
  aggregateStat DStateStats {..} =
    mempty
      { gsCredentialStaking = dssCredentialStaking
      , gsKeyHashStakePool = dssDelegations
      , gsKeyHashGenesis = dssKeyHashGenesis
      , gsKeyHashGenesisDelegate = dssKeyHashGenesisDelegate
      , gsVerKeyVRF = mapStat unVRFVerKeyHash dssHashVerKeyVRF
      }

countDStateStats :: DState CurrentEra -> DStateStats
countDStateStats ds@DState {..} =
  let accountsMap = ds ^. accountsL . accountsMapL
   in DStateStats
        { dssCredentialStaking = statMapKeys accountsMap
        , dssDelegations = statFoldable (Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap)
        , dssKeyHashGenesis =
            statFoldable (fGenDelegGenKeyHash <$> Map.keys dsFutureGenDelegs)
              <> statMapKeys (unGenDelegs dsGenDelegs)
        , dssKeyHashGenesisDelegate =
            statFoldable (genDelegKeyHash <$> Map.elems dsFutureGenDelegs)
              <> statFoldable
                (genDelegKeyHash <$> Map.elems (unGenDelegs dsGenDelegs))
        , dssHashVerKeyVRF =
            statFoldable (genDelegVrfHash <$> Map.elems dsFutureGenDelegs)
              <> statFoldable
                (genDelegVrfHash <$> Map.elems (unGenDelegs dsGenDelegs))
        }

data PStateStats = PStateStats
  { pssKeyHashStakePool :: !(Stat (KeyHash 'StakePool))
  , pssPoolParamsStats :: !PoolParamsStats
  }

instance Pretty PStateStats where
  pretty PStateStats {..} =
    prettyRecord
      "PStateStats"
      [ "KeyHashStakePool" <:> pssKeyHashStakePool
      , pretty pssPoolParamsStats
      ]

instance AggregateStat PStateStats where
  aggregateStat PStateStats {..} =
    (aggregateStat pssPoolParamsStats) {gsKeyHashStakePool = pssKeyHashStakePool}

countPStateStats :: PState CurrentEra -> PStateStats
countPStateStats PState {..} =
  PStateStats
    { pssKeyHashStakePool =
        statMapKeys psStakePoolState
          <> statMapKeys psFutureStakePoolState
          <> statMapKeys psRetiring
    , pssPoolParamsStats =
        foldMap countPoolParamsStats (Map.mapWithKey stakePoolStateToPoolParams psStakePoolState)
          <> foldMap countPoolParamsStats (Map.mapWithKey stakePoolStateToPoolParams psFutureStakePoolState)
    }

data LedgerStateStats = LedgerStateStats
  { lssUTxOStats :: !UTxOStats
  , lssDStateStats :: !DStateStats
  , lssPStateStats :: !PStateStats
  }

instance Pretty LedgerStateStats where
  pretty LedgerStateStats {..} =
    prettyRecord
      "LedgerStateStats"
      [ pretty lssUTxOStats
      , pretty lssDStateStats
      , pretty lssPStateStats
      ]

instance AggregateStat LedgerStateStats where
  aggregateStat LedgerStateStats {..} =
    mconcat
      [ aggregateStat lssUTxOStats
      , aggregateStat lssDStateStats
      , aggregateStat lssPStateStats
      ]

countLedgerStateStats :: LedgerState CurrentEra -> LedgerStateStats
countLedgerStateStats LedgerState {..} =
  LedgerStateStats
    { lssUTxOStats = countUTxOStats (utxosUtxo lsUTxOState)
    , lssDStateStats = countDStateStats (lsCertState ^. certDStateL)
    , lssPStateStats = countPStateStats (lsCertState ^. certPStateL)
    }

data TxInStats = TxInStats
  { tisTxId :: !(Stat TxId)
  , tisTxIx :: !(Stat TxIx)
  }

instance Pretty TxInStats where
  pretty TxInStats {..} =
    prettyRecord "TxInStats" ["TxId" <:> tisTxId, "TxIx" <:> tisTxIx]

countTxInStats :: [TxIn] -> TxInStats
countTxInStats txIns =
  case unzip (fmap (\(TxIn txId txIx) -> (txId, txIx)) txIns) of
    (txIds, txIxs) ->
      TxInStats
        { tisTxId = statFoldable txIds
        , tisTxIx = statFoldable txIxs
        }

data TxOutStats = TxOutStats
  { tosBootstrap :: !(Stat BootstrapAddress)
  , tosPaymentCredential :: !(Stat (Credential 'Payment))
  , tosStakingCredential :: !(Stat (Credential 'Staking))
  , tosStakingPtr :: !(Stat Ptr)
  , tosNetwork :: !(Stat Network)
  , tosValue :: !(Stat Integer)
  , tosPolicyId :: !(Stat PolicyID)
  , tosAssetName :: !(Stat AssetName)
  , tosAssetValue :: !(Stat Integer)
  , tosDataHash :: !(Stat DataHash)
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
      [ "Bootstrap" <:> tosBootstrap
      , "PaymentCredential" <:> tosPaymentCredential
      , "StakingCredential" <:> tosStakingCredential
      , "StakingPtr" <:> tosStakingPtr
      , "Network" <:> tosNetwork
      , "Value" <:> tosValue
      , "PolicyId" <:> tosPolicyId
      , "AssetName" <:> tosAssetName
      , "AssetValue" <:> tosAssetValue
      , "DataHash" <:> tosDataHash
      ]

instance AggregateStat TxOutStats where
  aggregateStat TxOutStats {..} = aggregateStat tosStakingCredential

countTxOutStats :: [TxOut CurrentEra] -> TxOutStats
countTxOutStats = foldMap countTxOutStat
  where
    countTxOutStat :: TxOut CurrentEra -> TxOutStats
    countTxOutStat txOut =
      let addr = txOut ^. addrTxOutL
          MaryValue (Coin v) (MultiAsset m) = txOut ^. valueTxOutL
          !dataStat =
            strictMaybe
              mempty
              (\d -> mempty {tosDataHash = statSingleton d})
              (txOut ^. dataHashTxOutL)
          !vmElems = Map.elems m
          !valueStat =
            dataStat
              { tosValue = statSingleton v
              , tosPolicyId = statMapKeys m
              , tosAssetName = foldMap statMapKeys vmElems
              , tosAssetValue = foldMap statFoldable vmElems
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
  { usTxInStats :: !TxInStats
  , usTxOutStats :: !TxOutStats
  }

instance Pretty UTxOStats where
  pretty UTxOStats {..} =
    prettyRecord
      "UTxOStats"
      [pretty usTxInStats, pretty usTxOutStats]

instance AggregateStat UTxOStats where
  aggregateStat = aggregateStat . usTxOutStats

countUTxOStats :: UTxO CurrentEra -> UTxOStats
countUTxOStats (UTxO m) =
  UTxOStats
    { usTxInStats = countTxInStats (Map.keys m)
    , usTxOutStats = countTxOutStats (Map.elems m)
    }

data AggregateStats = AggregateStats
  { gsCredentialStaking :: !(Stat (Credential 'Staking))
  , gsKeyHashStakePool :: !(Stat (KeyHash 'StakePool))
  , gsKeyHashGenesis :: !(Stat (KeyHash 'Genesis))
  , gsKeyHashGenesisDelegate :: !(Stat (KeyHash 'GenesisDelegate))
  , gsVerKeyVRF :: !(Stat (Hash HASH KeyRoleVRF))
  , gsScriptHash :: !(Stat ScriptHash)
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
      [ "StakingCredential" <:> gsCredentialStaking
      , "KeyHashStakePool" <:> gsKeyHashStakePool
      , "ScriptHash" <:> gsScriptHash
      ]

class AggregateStat s where
  aggregateStat :: s -> AggregateStats

instance AggregateStat (Stat (Credential 'Staking)) where
  aggregateStat s = mempty {gsCredentialStaking = s}

instance AggregateStat (Stat (KeyHash 'StakePool)) where
  aggregateStat s = mempty {gsKeyHashStakePool = s}

instance AggregateStat (Stat ScriptHash) where
  aggregateStat s = mempty {gsScriptHash = s}

-- Initial attempt at UTxO stats, which was mostly superseded by the above
-- approach that works for the whole state

data UTxOUniques = UTxOUniques
  { paymentKeys :: !(Set.Set (KeyHash 'Payment))
  , paymentScripts :: !(Set.Set ScriptHash)
  , stakeKeys :: !(Set.Set (KeyHash 'Staking))
  , stakeScripts :: !(Set.Set ScriptHash)
  , stakePtrs :: !(Set.Set Ptr)
  , scripts :: !(Set.Set ScriptHash)
  , txIds :: !(Set.Set TxId)
  , txIxs :: !(Set.Set TxIx)
  }

emptyUniques :: UTxOUniques
emptyUniques = UTxOUniques mempty mempty mempty mempty mempty mempty mempty mempty

data UTxOStats' = UTxOStats'
  { statsTotalTxOuts :: !Int
  , statsByronTxOuts :: !Int
  , statsTotalPaymentKeys :: !Int
  , statsTotalPaymentScripts :: !Int
  , statsTotalStakeKeys :: !Int
  , statsTotalStakeScripts :: !Int
  , statsTotalStakePtrs :: !Int
  , stateTotalStakeNulls :: !Int
  }
  deriving (Show)

initStats :: UTxOStats'
initStats = UTxOStats' 0 0 0 0 0 0 0 0

collectStats :: ConduitT (TxIn, TxOut CurrentEra) Void IO ()
collectStats = do
  (uniques, stats) <- foldlC collect (emptyUniques, initStats)
  lift $ reportStats uniques stats
  where
    collect ::
      (UTxOUniques, UTxOStats') ->
      (TxIn, TxOut CurrentEra) ->
      (UTxOUniques, UTxOStats')
    collect (u@UTxOUniques {..}, s@UTxOStats' {..}) (TxIn txId txIx, txOut) =
      let u' = u {txIds = Set.insert txId txIds, txIxs = Set.insert txIx txIxs}
          s' = s {statsTotalTxOuts = statsTotalTxOuts + 1}
          addr = txOut ^. addrTxOutL
          updateStakingStats sr (su, ss) =
            case sr of
              StakeRefNull ->
                (su, ss {stateTotalStakeNulls = stateTotalStakeNulls + 1})
              StakeRefPtr ptr ->
                ( su {stakePtrs = Set.insert ptr stakePtrs}
                , ss {statsTotalStakePtrs = statsTotalStakePtrs + 1}
                )
              StakeRefBase a
                | KeyHashObj kh <- a ->
                    ( su {stakeKeys = Set.insert kh stakeKeys}
                    , ss {statsTotalStakeKeys = statsTotalStakeKeys + 1}
                    )
                | ScriptHashObj sh <- a ->
                    ( su {stakeScripts = Set.insert sh stakeScripts}
                    , ss {statsTotalStakeScripts = statsTotalStakeScripts + 1}
                    )
       in case addr of
            AddrBootstrap _ ->
              (u', s' {statsByronTxOuts = statsByronTxOuts + 1})
            Addr _ni pc sr
              | KeyHashObj kh <- pc ->
                  updateStakingStats
                    sr
                    ( u' {paymentKeys = Set.insert kh paymentKeys}
                    , s' {statsTotalPaymentKeys = statsTotalPaymentKeys + 1}
                    )
              | ScriptHashObj kh <- pc ->
                  updateStakingStats
                    sr
                    ( u' {paymentScripts = Set.insert kh paymentScripts}
                    , s' {statsTotalPaymentScripts = statsTotalPaymentScripts + 1}
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
      [ "Total TxOuts = " <> show statsTotalTxOuts
      , "Byron TxOuts = " <> showPercent statsByronTxOuts statsTotalTxOuts
      , "Unique TxIds = " <> showPercent (Set.size txIds) statsTotalTxOuts
      , "Unique TxIxs = " <> showPercent (Set.size txIxs) statsTotalTxOuts
      , "Shelley Total Payment Keys = " <> show statsTotalPaymentKeys
      , "Shelley Unique Payment Keys = " <> showPercent (Set.size paymentKeys) statsTotalPaymentKeys
      , "Shelley Total Payment Scripts = " <> show statsTotalPaymentScripts
      , "Shelley Unique Payment Scripts = "
          <> showPercent (Set.size paymentScripts) statsTotalPaymentScripts
      , "Shelley Total Stake Keys = " <> show statsTotalStakeKeys
      , "Shelley Unique Stake Keys = " <> showPercent (Set.size stakeKeys) statsTotalStakeKeys
      , "Shelley Total Stake Scripts = " <> show statsTotalStakeScripts
      , "Shelley Unique Stake Scripts = "
          <> showPercent (Set.size stakeScripts) statsTotalStakeScripts
      , "Shelley Total Stake Ptrs = " <> show statsTotalStakePtrs
      , "Shelley Unique Stake Ptrs = " <> showPercent (Set.size stakePtrs) statsTotalStakePtrs
      ]
