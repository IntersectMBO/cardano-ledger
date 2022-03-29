{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Model.LedgerState where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Slotting.Slot (EpochNo, SlotNo)
import Control.DeepSeq (NFData)
import Control.Lens
  ( Lens',
    foldOf,
    folded,
    ifoldMap,
    ix,
    lens,
    preview,
    to,
  )
import Data.Group (Group (..))
import Data.Group.GrpMap (GrpMap)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic, (:.:) (..))
import qualified PlutusTx (Data)
import Quiet (Quiet (..))
import Test.Cardano.Ledger.Model.Acnt
  ( ModelAcnt,
    ModelAcntF,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelBlocksMade,
    ModelPoolId,
    PreservedAda (..),
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSupport (..),
    ScriptFeature,
    ShelleyScriptFeatures,
  )
import Test.Cardano.Ledger.Model.PParams
  ( HasModelPParams (..),
    ModelPParams,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential (..),
    ModelPlutusScript,
    ModelScript (..),
    evalModelPlutusScript,
  )
import Test.Cardano.Ledger.Model.Snapshot
import Test.Cardano.Ledger.Model.Tx
  ( ModelPoolParams,
    ModelRedeemer,
    ModelTx (..),
    modelCWitness,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
  )
import Test.Cardano.Ledger.Model.UTxO (ModelUTxOMap)

data ModelRewardUpdate era = ModelRewardUpdate
  { _modelRewardUpdate_treasury :: Coin,
    _modelRewardUpdate_reserves :: Coin,
    _modelRewardUpdate_rewards :: GrpMap (ModelCredential 'Staking (ScriptFeature era)) Coin,
    _modelRewardUpdate_fees :: Coin
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelRewardUpdate era)

instance Semigroup (ModelRewardUpdate sf) where
  ModelRewardUpdate t r sf f <> ModelRewardUpdate t' r' sf' f' = ModelRewardUpdate (t <> t') (r <> r') (sf <> sf') (f <> f')

instance Monoid (ModelRewardUpdate sf) where
  mempty = ModelRewardUpdate mempty mempty mempty mempty

instance Group (ModelRewardUpdate sf) where
  ModelRewardUpdate t r sf f ~~ ModelRewardUpdate t' r' sf' f' = ModelRewardUpdate (t ~~ t') (r ~~ r') (sf ~~ sf') (f ~~ f')
  invert (ModelRewardUpdate t r sf f) = ModelRewardUpdate (invert t) (invert r) (invert sf) (invert f)
  pow (ModelRewardUpdate t r sf f) x = ModelRewardUpdate (t `pow` x) (r `pow` x) (sf `pow` x) (f `pow` x)

-- | fig 44
data ModelEpochState era = ModelEpochState
  { _modelEpochState_acnt :: !ModelAcnt,
    _modelEpochState_ss :: !(ModelSnapshots era),
    _modelEpochState_ls :: !(ModelLState era),
    _modelEpochState_prevPp :: !(ModelPParams era),
    _modelEpochState_pp :: !(ModelPParams era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelEpochState era)

instance PreservedAda (ModelEpochState era) where
  totalPreservedAda =
    totalPreservedAda . _modelEpochState_acnt
      <> totalPreservedAda . _modelEpochState_ls

modelEpochState_acnt :: Lens' (ModelEpochState era) ModelAcnt
modelEpochState_acnt a2fb s = (\b -> s {_modelEpochState_acnt = b}) <$> a2fb (_modelEpochState_acnt s)
{-# INLINE modelEpochState_acnt #-}

modelEpochState_ss :: Lens' (ModelEpochState era) (ModelSnapshots era)
modelEpochState_ss a2fb s = (\b -> s {_modelEpochState_ss = b}) <$> a2fb (_modelEpochState_ss s)
{-# INLINE modelEpochState_ss #-}

modelEpochState_ls :: Lens' (ModelEpochState era) (ModelLState era)
modelEpochState_ls a2fb s = (\b -> s {_modelEpochState_ls = b}) <$> a2fb (_modelEpochState_ls s)
{-# INLINE modelEpochState_ls #-}

modelEpochState_prevPp :: Lens' (ModelEpochState era) (ModelPParams era)
modelEpochState_prevPp a2fb s = (\b -> s {_modelEpochState_prevPp = b}) <$> a2fb (_modelEpochState_prevPp s)
{-# INLINE modelEpochState_prevPp #-}

modelEpochState_pp :: Lens' (ModelEpochState era) (ModelPParams era)
modelEpochState_pp a2fb s = (\b -> s {_modelEpochState_pp = b}) <$> a2fb (_modelEpochState_pp s)
{-# INLINE modelEpochState_pp #-}

instance HasModelPParams era (ModelEpochState era) where
  getModelPParams = _modelEpochState_pp

-- | fig 56
data ModelNewEpochState era = ModelNewEpochState
  { _modelNewEpochState_el :: !EpochNo,
    _modelNewEpochState_bPrev :: !ModelBlocksMade,
    _modelNewEpochState_bCur :: !ModelBlocksMade,
    _modelNewEpochState_es :: !(ModelEpochState era),
    _modelNewEpochState_ru :: !(Maybe (ModelRewardUpdate era))
    -- , _modelNewEpochState_pd :: !ModelPoolDistr
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelNewEpochState era)

instance PreservedAda (ModelNewEpochState era) where
  totalPreservedAda = totalPreservedAda . _modelNewEpochState_es

instance HasModelPParams era (ModelNewEpochState era) where
  getModelPParams = getModelPParams . _modelNewEpochState_es

modelNewEpochState_el :: Lens' (ModelNewEpochState era) EpochNo
modelNewEpochState_el a2fb s = (\b -> s {_modelNewEpochState_el = b}) <$> a2fb (_modelNewEpochState_el s)
{-# INLINE modelNewEpochState_el #-}

modelNewEpochState_bPrev :: Lens' (ModelNewEpochState era) ModelBlocksMade
modelNewEpochState_bPrev a2fb s = (\b -> s {_modelNewEpochState_bPrev = b}) <$> a2fb (_modelNewEpochState_bPrev s)
{-# INLINE modelNewEpochState_bPrev #-}

modelNewEpochState_bCur :: Lens' (ModelNewEpochState era) ModelBlocksMade
modelNewEpochState_bCur a2fb s = (\b -> s {_modelNewEpochState_bCur = b}) <$> a2fb (_modelNewEpochState_bCur s)
{-# INLINE modelNewEpochState_bCur #-}

modelNewEpochState_es :: Lens' (ModelNewEpochState era) (ModelEpochState era)
modelNewEpochState_es a2fb s = (\b -> s {_modelNewEpochState_es = b}) <$> a2fb (_modelNewEpochState_es s)
{-# INLINE modelNewEpochState_es #-}

modelNewEpochState_ru :: Lens' (ModelNewEpochState era) (Maybe (ModelRewardUpdate era))
modelNewEpochState_ru a2fb s = (\b -> s {_modelNewEpochState_ru = b}) <$> a2fb (_modelNewEpochState_ru s)
{-# INLINE modelNewEpochState_ru #-}

-- | fig 30
data ModelLState era = ModelLState
  { _modelLState_utxoSt :: !(ModelUTxOState era),
    _modelLState_dpstate :: !(ModelDPState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelLState era)

instance PreservedAda (ModelLState era) where
  totalPreservedAda =
    foldOf
      ( modelLState_dpstate . modelDPStatedpsDState . modelDState_rewards . folded
          <> modelLState_utxoSt
            . ( modelUTxOState_deposited
                  <> modelUTxOState_fees
                  <> modelUTxOState_utxo . to totalPreservedAda
              )
      )

modelLState_utxoSt :: Lens' (ModelLState era) (ModelUTxOState era)
modelLState_utxoSt a2fb s = (\b -> s {_modelLState_utxoSt = b}) <$> a2fb (_modelLState_utxoSt s)
{-# INLINE modelLState_utxoSt #-}

modelLState_dpstate :: Lens' (ModelLState era) (ModelDPState era)
modelLState_dpstate a2fb s = (\b -> s {_modelLState_dpstate = b}) <$> a2fb (_modelLState_dpstate s)
{-# INLINE modelLState_dpstate #-}

-- | fig 26
data ModelDPState era = ModelDPState
  { _modelDPStatedpsDState :: !(ModelDState era),
    _modelDPStatedpsPState :: !(ModelPState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelDPState era)

modelDPStatedpsDState :: Lens' (ModelDPState era) (ModelDState era)
modelDPStatedpsDState a2fb s = (\b -> s {_modelDPStatedpsDState = b}) <$> a2fb (_modelDPStatedpsDState s)
{-# INLINE modelDPStatedpsDState #-}

modelDPStatedpsPState :: Lens' (ModelDPState era) (ModelPState era)
modelDPStatedpsPState a2fb s = (\b -> s {_modelDPStatedpsPState = b}) <$> a2fb (_modelDPStatedpsPState s)
{-# INLINE modelDPStatedpsPState #-}

-- | fig 22
data ModelPState era = ModelPState
  { _modelPState_poolParams :: Map.Map ModelPoolId (ModelPoolParams era),
    _modelPState_fPoolParams :: Map.Map ModelPoolId (ModelPoolParams era),
    _modelPState_retiring :: Map.Map ModelPoolId EpochNo
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelPState era)

modelPState_poolParams :: Lens' (ModelPState era) (Map.Map ModelPoolId (ModelPoolParams era))
modelPState_poolParams a2fb s = (\b -> s {_modelPState_poolParams = b}) <$> a2fb (_modelPState_poolParams s)
{-# INLINE modelPState_poolParams #-}

modelPState_fPoolParams :: Lens' (ModelPState era) (Map.Map ModelPoolId (ModelPoolParams era))
modelPState_fPoolParams a2fb s = (\b -> s {_modelPState_fPoolParams = b}) <$> a2fb (_modelPState_fPoolParams s)
{-# INLINE modelPState_fPoolParams #-}

modelPState_retiring :: Lens' (ModelPState era) (Map.Map ModelPoolId EpochNo)
modelPState_retiring a2fb s = (\b -> s {_modelPState_retiring = b}) <$> a2fb (_modelPState_retiring s)
{-# INLINE modelPState_retiring #-}

type ModelInstantaneousRewards era = (:.:) ModelAcntF (ModelInstantaneousReward era) Coin

type ModelFutGenesisDelegation =
  Map.Map
    (SlotNo, ModelCredential 'Genesis ShelleyScriptFeatures)
    (ModelCredential 'GenesisDelegate ShelleyScriptFeatures)

type ModelGenesisDelegation =
  Map.Map
    (ModelCredential 'Genesis ShelleyScriptFeatures)
    (ModelCredential 'GenesisDelegate ShelleyScriptFeatures)

data ModelInstantaneousReward era a = ModelInstantaneousReward
  { -- | how much this "pot" changes
    _modelInstantaneousReward_delta :: !a,
    -- | rewards drawn from this pot.
    _modelInstantaneousReward_ir :: !(GrpMap (ModelCredential 'Staking (ScriptFeature era)) a)
  }
  deriving (Eq, Show, Generic, Foldable)

instance NFData a => NFData (ModelInstantaneousReward era a)

instance (Eq a, Monoid a) => Semigroup (ModelInstantaneousReward era a) where
  ModelInstantaneousReward d i <> ModelInstantaneousReward d' i' =
    ModelInstantaneousReward (d <> d') (i <> i')

instance (Eq a, Monoid a) => Monoid (ModelInstantaneousReward era a) where
  mempty = ModelInstantaneousReward mempty mempty

instance (Eq a, Group a) => Group (ModelInstantaneousReward era a) where
  invert (ModelInstantaneousReward a b) = ModelInstantaneousReward (invert a) (invert b)
  ModelInstantaneousReward a b ~~ ModelInstantaneousReward a' b' =
    ModelInstantaneousReward (a ~~ a') (b ~~ b')
  pow (ModelInstantaneousReward a b) x = ModelInstantaneousReward (pow a x) (pow b x)

modelInstantaneousReward_delta :: Lens' (ModelInstantaneousReward era a) a
modelInstantaneousReward_delta = lens _modelInstantaneousReward_delta (\s b -> s {_modelInstantaneousReward_delta = b})
{-# INLINE modelInstantaneousReward_delta #-}

modelInstantaneousReward_ir :: Lens' (ModelInstantaneousReward era a) (GrpMap (ModelCredential 'Staking (ScriptFeature era)) a)
modelInstantaneousReward_ir = lens _modelInstantaneousReward_ir (\s b -> s {_modelInstantaneousReward_ir = b})
{-# INLINE modelInstantaneousReward_ir #-}

-- | fig 22
data ModelDState era = ModelDState
  { _modelDState_rewards :: !(Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin),
    _modelDState_delegations :: !(Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelPoolId),
    -- , _modelDState_ptrs :: !(Map.Map ModelPtr (ModelCredential 'Staking (ScriptFeature era)))
    _modelDState_fGenDelegs :: !ModelFutGenesisDelegation,
    _modelDState_genDelegs :: !ModelGenesisDelegation,
    _modelDState_iRwd :: !(ModelInstantaneousRewards era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelDState era)

modelDState_rewards :: Lens' (ModelDState era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
modelDState_rewards a2fb s = (\b -> s {_modelDState_rewards = b}) <$> a2fb (_modelDState_rewards s)
{-# INLINE modelDState_rewards #-}

modelDState_delegations :: Lens' (ModelDState era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelPoolId)
modelDState_delegations a2fb s = (\b -> s {_modelDState_delegations = b}) <$> a2fb (_modelDState_delegations s)
{-# INLINE modelDState_delegations #-}

modelDState_fGenDelegs :: Lens' (ModelDState era) (Map.Map (SlotNo, ModelCredential 'Genesis ShelleyScriptFeatures) (ModelCredential 'GenesisDelegate ShelleyScriptFeatures))
modelDState_fGenDelegs a2fb s = (\b -> s {_modelDState_fGenDelegs = b}) <$> a2fb (_modelDState_fGenDelegs s)
{-# INLINE modelDState_fGenDelegs #-}

modelDState_genDelegs :: Lens' (ModelDState era) (Map.Map (ModelCredential 'Genesis ShelleyScriptFeatures) (ModelCredential 'GenesisDelegate ShelleyScriptFeatures))
modelDState_genDelegs a2fb s = (\b -> s {_modelDState_genDelegs = b}) <$> a2fb (_modelDState_genDelegs s)
{-# INLINE modelDState_genDelegs #-}

modelDState_iRwd :: Lens' (ModelDState era) (ModelInstantaneousRewards era)
modelDState_iRwd a2fb s = (\b -> s {_modelDState_iRwd = b}) <$> a2fb (_modelDState_iRwd s)
{-# INLINE modelDState_iRwd #-}

-- | fig 15
data ModelUTxOState era = ModelUTxOState
  { _modelUTxOState_utxo :: !(ModelUTxOMap era),
    _modelUTxOState_deposited :: !Coin,
    _modelUTxOState_fees :: !Coin
    -- , _modelUTxOState_ppup :: !(ModelPPupdateState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelUTxOState era)

modelUTxOState_utxo :: Lens' (ModelUTxOState era) (ModelUTxOMap era)
modelUTxOState_utxo a2fb s = (\b -> s {_modelUTxOState_utxo = b}) <$> a2fb (_modelUTxOState_utxo s)
{-# INLINE modelUTxOState_utxo #-}

modelUTxOState_deposited :: Lens' (ModelUTxOState era) Coin
modelUTxOState_deposited a2fb s = (\b -> s {_modelUTxOState_deposited = b}) <$> a2fb (_modelUTxOState_deposited s)
{-# INLINE modelUTxOState_deposited #-}

modelUTxOState_fees :: Lens' (ModelUTxOState era) Coin
modelUTxOState_fees a2fb s = (\b -> s {_modelUTxOState_fees = b}) <$> a2fb (_modelUTxOState_fees s)
{-# INLINE modelUTxOState_fees #-}

-- modelUTxOState_ppup :: Lens' (ModelUTxOState era) (ModelPPupdateState era)
-- modelUTxOState_ppup a2fb s = (\b -> s {_modelUTxOState_ppup = b}) <$> a2fb (_modelUTxOState_ppup s)
-- {-# INLINE modelUTxOState_ppup #-}

data ModelSnapshots era = ModelSnapshots
  { _modelSnapshots_pstake :: !(SnapshotQueue (ModelSnapshot era)),
    _modelSnapshots_feeSS :: !Coin
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelSnapshots era)

modelSnapshots_pstake :: Lens' (ModelSnapshots era) (SnapshotQueue (ModelSnapshot era))
modelSnapshots_pstake a2fb s = (\b -> s {_modelSnapshots_pstake = b}) <$> a2fb (_modelSnapshots_pstake s)
{-# INLINE modelSnapshots_pstake #-}

modelSnapshots_feeSS :: Lens' (ModelSnapshots era) Coin
modelSnapshots_feeSS a2fb s = (\b -> s {_modelSnapshots_feeSS = b}) <$> a2fb (_modelSnapshots_feeSS s)
{-# INLINE modelSnapshots_feeSS #-}

-- | fig 38
data ModelSnapshot era = ModelSnapshot
  { _modelSnapshot_stake :: !(Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake),
    _modelSnapshot_delegations :: !(Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelPoolId),
    _modelSnapshot_pools :: !(Map.Map ModelPoolId (ModelPoolParams era)),
    _modelSnapshot_utxos :: !(ModelUTxOMap era),
    _modelSnapshot_rewards :: !(Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
  }
  deriving (Eq, Generic)
  deriving (Show) via Quiet (ModelSnapshot era)

instance NFData (ModelSnapshot era)

emptyModelSnapshot :: ModelSnapshot era
emptyModelSnapshot = ModelSnapshot mempty Map.empty Map.empty mempty Map.empty

modelSnapshot_stake :: Lens' (ModelSnapshot era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake)
modelSnapshot_stake a2fb s = (\b -> s {_modelSnapshot_stake = b}) <$> a2fb (_modelSnapshot_stake s)
{-# INLINE modelSnapshot_stake #-}

modelSnapshot_delegations :: Lens' (ModelSnapshot era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelPoolId)
modelSnapshot_delegations a2fb s = (\b -> s {_modelSnapshot_delegations = b}) <$> a2fb (_modelSnapshot_delegations s)
{-# INLINE modelSnapshot_delegations #-}

modelSnapshot_pools :: Lens' (ModelSnapshot era) (Map.Map ModelPoolId (ModelPoolParams era))
modelSnapshot_pools a2fb s = (\b -> s {_modelSnapshot_pools = b}) <$> a2fb (_modelSnapshot_pools s)
{-# INLINE modelSnapshot_pools #-}

validateModelTx :: forall era. ModelUTxOMap era -> ModelTx era -> IsValid
validateModelTx
  utxos
  ( ModelTx
      { _mtxInputs = ins,
        _mtxDCert = dcerts,
        _mtxWdrl = wdrl,
        _mtxMint = mint
      }
    ) =
    ifoldMap
      ( \ui rdmr ->
          foldMap
            ( \(ModelTxOut addr _ dh) -> case _modelAddress_pmt addr of
                ModelKeyHashObj {} -> mempty
                ModelScriptHashObj c -> foldMapSupportsFeature (\dh' -> go dh' rdmr c) dh
            )
            (preview (ix ui) utxos)
      )
      ins
      <> foldMap
        ( \(dcert, rdmr) ->
            foldMap
              ( \case
                  ModelKeyHashObj {} -> mempty
                  ModelScriptHashObj c -> go Nothing rdmr c
              )
              $ modelCWitness dcert -- This is a little wrong, only concerning the presence of a redeemer, and ignoring if there *should* be a redeemer.
        )
        dcerts
      <> ifoldMap
        ( \case
            ModelKeyHashObj {} -> mempty
            ModelScriptHashObj c -> \(_, rdmr) -> go Nothing rdmr c
        )
        wdrl
      <> foldMapSupportsFeature
        ( ifoldMap $ \case
            ModelScript_Timelock {} -> mempty
            ModelScript_PlutusV1 policy -> \(_, rdmr) ->
              go Nothing rdmr policy
        )
        mint
    where
      go :: Maybe PlutusTx.Data -> ModelRedeemer (ScriptFeature era) -> ModelPlutusScript -> IsValid
      go dh rdmr script =
        (foldMapSupportsFeature . foldMap) (\(r, _) -> evalModelPlutusScript dh r script) rdmr

data ModelSnapshotStake = ModelSnapshotStake
  { _modelSnapshotStake_balance :: Coin, -- sum of utxos and rewards
    _modelSnapshotStake_utxos :: Set ModelUTxOId
  }
  deriving (Eq, Generic)
  deriving (Show) via Quiet ModelSnapshotStake

instance Semigroup ModelSnapshotStake where
  ModelSnapshotStake a b <> ModelSnapshotStake a' b' = ModelSnapshotStake (a <> a') (b <> b')

instance Monoid ModelSnapshotStake where
  mempty = ModelSnapshotStake mempty mempty

instance NFData ModelSnapshotStake
