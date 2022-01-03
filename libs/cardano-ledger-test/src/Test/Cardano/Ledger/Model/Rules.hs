{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | = Model implementation of the ledger semantics.
--
-- The implementation here follows the spec, and favors readability.
--
-- Unlike the real implementation, the model is impelented in a "closed world"
-- way, where all posssible hard forks are captured in a single implementation,
-- difference across versions are handled by matching on feature flags in the
-- 'era' parameter used throughout the model types.
--
-- other, relevant model implementations can be found in in these modules.
-- Where possible, namespaces and type names match the real implementation, with
-- Model prefixed as appropriate to avoid confusion.
-- * "Test.Cardano.Ledger.Model.BaseTypes"
-- * "Test.Cardano.Ledger.Model.LedgerState"
-- * "Test.Cardano.Ledger.Model.PParams"
-- * "Test.Cardano.Ledger.Model.Rewards"
-- * "Test.Cardano.Ledger.Model.Script"
-- * "Test.Cardano.Ledger.Model.Tx"
-- * "Test.Cardano.Ledger.Model.TxOut"
-- * "Test.Cardano.Ledger.Model.UTxO"
-- * "Test.Cardano.Ledger.Model.Value"
--
--
-- additionally, the base monad used throughout this module can be found in
-- "Test.Cardano.Ledger.Model.Prov".  That type is used mainly for quickcheck
-- shrinking and isn't critical to understand the main concepts here.
module Test.Cardano.Ledger.Model.Rules
  ( ModelRule (..),
    applyRule,
    ModelLEnv (..),
    ModelDPSEnv (..),
    ModelPredicateFailure (..),
  )
where

import Cardano.Ledger.BaseTypes
  ( Globals (..),
    epochInfo,
  )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API
  ( epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
  )
import Cardano.Slotting.Slot (EpochNo, SlotNo)
import Control.Lens
import Control.Monad (unless, when)
import qualified Control.Monad.RWS.CPS as RWS
import Control.Monad.Reader.Class (ask, asks)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable (fold, for_, traverse_)
import Data.Functor.Compose (Compose (..))
import Data.Group (Group (..))
import Data.Group.GrpMap (GrpMap (..), mkGrpMap, restrictKeysGrpMap)
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import GHC.Generics ((:.:) (Comp1))
import GHC.Natural (Natural)
import Test.Cardano.Ledger.Model.Acnt
  ( ModelAcnt,
    ModelAcntF (..),
    modelAcnt_treasury,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( HasGlobals (..),
    ModelPoolId,
    ModelValue,
    totalPreservedAda,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet,
    FeatureSupport (..),
    KnownRequiredFeatures,
    ValueFeature,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelDPState (..),
    ModelDState (..),
    ModelEpochState (..),
    ModelGenesisDelegation,
    ModelInstantaneousReward (..),
    ModelLState (..),
    ModelNewEpochState (..),
    ModelPState (..),
    ModelRewardUpdate,
    ModelSnapshot (..),
    ModelSnapshotStake (..),
    ModelSnapshots (..),
    ModelUTxOState (..),
    modelDPState_dstate,
    modelDPState_pstate,
    modelDState_delegations,
    modelDState_fGenDelegs,
    modelDState_iRwd,
    modelDState_rewards,
    modelEpochState_acnt,
    modelEpochState_ls,
    modelLState_dpstate,
    modelNewEpochState_bCur,
    modelNewEpochState_bPrev,
    modelNewEpochState_el,
    modelNewEpochState_es,
    modelNewEpochState_ru,
    modelPState_fPoolParams,
    modelPState_poolParams,
    modelPState_retiring,
    modelUTxOState_deposited,
    modelUTxOState_fees,
    modelUTxOState_utxo,
  )
import Test.Cardano.Ledger.Model.PParams
  ( HasModelPParams (..),
    ModelPParams,
    ModelPParamsF (..),
  )
import Test.Cardano.Ledger.Model.Prov
  ( MonadModelProvenance (..),
  )
import Test.Cardano.Ledger.Model.Rewards
  ( ModelRUpdEnv (..),
    applyRUpd,
    createRUpd,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential,
  )
import Test.Cardano.Ledger.Model.Snapshot (SnapshotQueue (..))
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelDelegCert (..),
    ModelDelegation (..),
    ModelGenesisDelegCert (..),
    ModelMIRCert (..),
    ModelMIRTarget (..),
    ModelPoolCert (..),
    ModelPoolParams (..),
    ModelTx (..),
    getModelTxId,
    modelDCerts,
    modelIsValid,
    modelKeyRefunds,
    modelTotalDeposits,
    modelTx_collateral,
    modelTx_fee,
    modelTx_redeemers,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
    spendModelUTxOs,
  )

-- | Universe of modeled ledger rules.
data ModelRule
  = ModelRule_LEDGERS
  | ModelRule_LEDGER
  | ModelRule_UTXOW
  | ModelRule_UTXO
  | ModelRule_UTXOS
  | ModelRule_DELEGS
  | ModelRule_DELPL
  | ModelRule_DELEG
  | ModelRule_POOL
  | ModelRule_TICK
  | ModelRule_RUPD
  | ModelRule_NEWEPOCH
  | ModelRule_MIR
  | ModelRule_EPOCH
  | ModelRule_SNAP
  | ModelRule_POOLREAP

-- | perform one model transition
-- this returns both the errors and the modified state.
applyRule ::
  ( ModelSTS (rule :: ModelRule),
    KnownRequiredFeatures era,
    MonadModelProvenance era provM
  ) =>
  proxy rule ->
  ModelSignal rule era ->
  Globals ->
  (ModelEnv rule era) ->
  ModelState rule era ->
  provM (ModelState rule era, Set (ModelFailure rule era))
applyRule p s globals env = applyRuleImpl p s (ModelGlobalsEnv globals env)

-- | One model rule transition.
class ModelSTS (rule :: ModelRule) where
  type ModelSignal rule :: FeatureSet -> Type
  type ModelState rule :: FeatureSet -> Type
  type ModelEnv rule :: FeatureSet -> Type
  type ModelFailure rule :: FeatureSet -> Type

  applyRuleImpl ::
    (KnownRequiredFeatures era, MonadModelProvenance era provM) =>
    proxy rule ->
    ModelSignal rule era ->
    ModelGlobalsEnv (ModelEnv rule era) ->
    ModelState rule era ->
    provM (ModelState rule era, Set (ModelFailure rule era))
  default applyRuleImpl ::
    (KnownRequiredFeatures era, ModelSignal rule ~ Const Void) =>
    proxy rule ->
    ModelSignal rule era ->
    ModelGlobalsEnv (ModelEnv rule era) ->
    ModelState rule era ->
    provM (ModelState rule era, Set (ModelFailure rule era))
  applyRuleImpl _ = absurd . getConst

class (ModelSTS rule, ModelSTS (ModelSuperRule rule)) => ModelSubRule (rule :: ModelRule) where
  type ModelSuperRule rule :: ModelRule
  type ModelSigIter rule :: Type -> Type

  getSubEnv ::
    proxy rule ->
    ModelSignal (ModelSuperRule rule) era ->
    ModelSigIter rule (ModelSignal rule era) ->
    ModelEnv (ModelSuperRule rule) era ->
    ModelState (ModelSuperRule rule) era ->
    ModelSigIter rule (ModelEnv rule era, ModelSignal rule era)
  default getSubEnv ::
    ( ModelEnv (ModelSuperRule rule) era ~ ModelEnv rule era,
      Functor (ModelSigIter rule)
    ) =>
    proxy rule ->
    ModelSignal (ModelSuperRule rule) era ->
    ModelSigIter rule (ModelSignal rule era) ->
    ModelEnv (ModelSuperRule rule) era ->
    ModelState (ModelSuperRule rule) era ->
    ModelSigIter rule (ModelEnv rule era, ModelSignal rule era)
  getSubEnv _ _ sigs env _ = (,) env <$> sigs

  getSubState ::
    proxy rule -> ModelState (ModelSuperRule rule) era -> ModelState rule era
  default getSubState ::
    (ModelState (ModelSuperRule rule) era ~ ModelState rule era) =>
    proxy rule ->
    ModelState (ModelSuperRule rule) era ->
    ModelState rule era
  getSubState _ = id

  putSubState :: proxy rule -> ModelState rule era -> ModelState (ModelSuperRule rule) era -> ModelState (ModelSuperRule rule) era
  default putSubState ::
    (ModelState (ModelSuperRule rule) era ~ ModelState rule era) =>
    proxy rule ->
    ModelState rule era ->
    ModelState (ModelSuperRule rule) era ->
    ModelState (ModelSuperRule rule) era
  putSubState _ st _ = st

  asSubFailure :: proxy rule -> ModelFailure rule era -> ModelFailure (ModelSuperRule rule) era
  default asSubFailure ::
    (Proxy era ~ ModelFailure (ModelSuperRule rule) era) =>
    proxy rule ->
    ModelFailure rule era ->
    ModelFailure (ModelSuperRule rule) era
  asSubFailure _ _ = Proxy

instance ModelSubRule 'ModelRule_LEDGER where
  type ModelSuperRule 'ModelRule_LEDGER = 'ModelRule_LEDGERS
  type ModelSigIter 'ModelRule_LEDGER = []

  getSubEnv _ _ sigs (ModelLEDGERSEnv slot pp) _ =
    imap
      ( \txIx sig ->
          ( ModelLEnv
              { _modelLEnv_slot = slot,
                _modelLEnv_txIx = txIx,
                _modelLEnv_pp = pp
                -- , _modelLEnv_acnt = acnt
              },
            sig
          )
      )
      sigs

instance ModelSubRule 'ModelRule_UTXOW where
  type ModelSuperRule 'ModelRule_UTXOW = 'ModelRule_LEDGER
  type ModelSigIter 'ModelRule_UTXOW = Identity

  getSubEnv _ _ (Identity tx) lenv st =
    Identity
      ( ModelUTxOEnv
          { _modelUTxOEnv_slot = _modelLEnv_slot lenv,
            _modelUTxOEnv_pp = _modelLEnv_pp lenv,
            _modelUTxOEnv_poolParams = _modelPState_poolParams $ _modelDPState_pstate $ _modelLState_dpstate st,
            _modelUTxOEnv_genDelegs = _modelDState_genDelegs $ _modelDPState_dstate $ _modelLState_dpstate st
          },
        tx
      )
  getSubState _ = _modelLState_utxoSt
  putSubState _ utxoSt st = st {_modelLState_utxoSt = utxoSt}

instance ModelSubRule 'ModelRule_DELEGS where
  type ModelSuperRule 'ModelRule_DELEGS = 'ModelRule_LEDGER
  type ModelSigIter 'ModelRule_DELEGS = Identity

  getSubEnv _ tx cs (ModelLEnv slot txIx pp) _ = flip fmap cs $ \c ->
    ( ModelDPSEnv
        { _modelDPSEnv_slot = slot,
          _modelDPSEnv_txIx = txIx,
          _modelDPSEnv_pp = pp,
          _modelDPSEnv_tx = tx
          -- , _modelDPSEnv_acnt = acnt
        },
      c
    )
  getSubState _ = _modelLState_dpstate
  putSubState _ dpst ls = ls {_modelLState_dpstate = dpst}

instance ModelSubRule 'ModelRule_UTXO where
  type ModelSuperRule 'ModelRule_UTXO = 'ModelRule_UTXOW
  type ModelSigIter 'ModelRule_UTXO = Identity

instance ModelSubRule 'ModelRule_UTXOS where
  type ModelSuperRule 'ModelRule_UTXOS = 'ModelRule_UTXO
  type ModelSigIter 'ModelRule_UTXOS = Identity

instance ModelSubRule 'ModelRule_DELPL where
  type ModelSuperRule 'ModelRule_DELPL = 'ModelRule_DELEGS
  type ModelSigIter 'ModelRule_DELPL = []

instance ModelSubRule 'ModelRule_DELEG where
  type ModelSuperRule 'ModelRule_DELEG = 'ModelRule_DELPL
  type ModelSigIter 'ModelRule_DELEG = Identity

  getSubEnv _ _ sig env _ = (,) (ModelDEnv (_modelDPSEnv_slot env)) <$> sig
  getSubState _ = _modelDPState_dstate
  putSubState _ dst dpst = dpst {_modelDPState_dstate = dst}

instance ModelSubRule 'ModelRule_POOL where
  type ModelSuperRule 'ModelRule_POOL = 'ModelRule_DELPL
  type ModelSigIter 'ModelRule_POOL = Identity

  getSubEnv _ _ sig env _ = (,) (ModelPEnv (_modelDPSEnv_slot env) (_modelDPSEnv_pp env)) <$> sig
  getSubState _ = _modelDPState_pstate
  putSubState _ pst dpst = dpst {_modelDPState_pstate = pst}

instance ModelSubRule 'ModelRule_RUPD where
  type ModelSuperRule 'ModelRule_RUPD = 'ModelRule_TICK
  type ModelSigIter 'ModelRule_RUPD = Identity

  getSubEnv _ _ (Identity sig) _ st = Identity (ModelRUpdEnv (_modelNewEpochState_bPrev st) (_modelNewEpochState_es st), sig)
  getSubState _ = Compose . _modelNewEpochState_ru
  putSubState _ (Compose rupd) st = st {_modelNewEpochState_ru = rupd}

instance ModelSubRule 'ModelRule_NEWEPOCH where
  type ModelSuperRule 'ModelRule_NEWEPOCH = 'ModelRule_TICK
  type ModelSigIter 'ModelRule_NEWEPOCH = Identity

instance ModelSubRule 'ModelRule_MIR where
  type ModelSuperRule 'ModelRule_MIR = 'ModelRule_NEWEPOCH
  type ModelSigIter 'ModelRule_MIR = Identity

  getSubState _ = _modelNewEpochState_es
  putSubState _ = set modelNewEpochState_es

instance ModelSubRule 'ModelRule_EPOCH where
  type ModelSuperRule 'ModelRule_EPOCH = 'ModelRule_NEWEPOCH
  type ModelSigIter 'ModelRule_EPOCH = Identity

  getSubState _ = _modelNewEpochState_es
  putSubState _ es nes = nes {_modelNewEpochState_es = es}

instance ModelSubRule 'ModelRule_SNAP where
  type ModelSuperRule 'ModelRule_SNAP = 'ModelRule_EPOCH
  type ModelSigIter 'ModelRule_SNAP = Identity

  getSubEnv _ _ (Identity sig) _ st = Identity (_modelEpochState_ls st, sig)
  getSubState _ = _modelEpochState_ss
  putSubState _ ss st = st {_modelEpochState_ss = ss}

data ModelPlReapState era = ModelPlReapState
  { _modelPlReapState_utxoSt :: !(ModelUTxOState era),
    _modelPlReapState_acnt :: !ModelAcnt,
    _modelPlReapState_dstate :: !(ModelDState era),
    _modelPlReapState_pstate :: !(ModelPState era)
  }

modelPlReapState_utxoSt :: Lens' (ModelPlReapState era) (ModelUTxOState era)
modelPlReapState_utxoSt = lens _modelPlReapState_utxoSt (\s b -> s {_modelPlReapState_utxoSt = b})
{-# INLINE modelPlReapState_utxoSt #-}

modelPlReapState_acnt :: Lens' (ModelPlReapState era) ModelAcnt
modelPlReapState_acnt = lens _modelPlReapState_acnt (\s b -> s {_modelPlReapState_acnt = b})
{-# INLINE modelPlReapState_acnt #-}

modelPlReapState_dstate :: Lens' (ModelPlReapState era) (ModelDState era)
modelPlReapState_dstate = lens _modelPlReapState_dstate (\s b -> s {_modelPlReapState_dstate = b})
{-# INLINE modelPlReapState_dstate #-}

modelPlReapState_pstate :: Lens' (ModelPlReapState era) (ModelPState era)
modelPlReapState_pstate = lens _modelPlReapState_pstate (\s b -> s {_modelPlReapState_pstate = b})
{-# INLINE modelPlReapState_pstate #-}

data ModelPOOLREAPEnv era = ModelPOOLREAPEnv (ModelPParams era)

instance ModelSubRule 'ModelRule_POOLREAP where
  type ModelSuperRule 'ModelRule_POOLREAP = 'ModelRule_EPOCH
  type ModelSigIter 'ModelRule_POOLREAP = Identity

  getSubEnv _ _ (Identity sig) _ st = Identity (ModelPOOLREAPEnv (getModelPParams st), sig)
  getSubState _ st =
    ModelPlReapState
      (_modelLState_utxoSt $ _modelEpochState_ls st)
      (_modelEpochState_acnt st)
      (_modelDPState_dstate $ _modelLState_dpstate $ _modelEpochState_ls st)
      (_modelDPState_pstate $ _modelLState_dpstate $ _modelEpochState_ls st)
  putSubState _ (ModelPlReapState utxoSt acnt dstate pstate) st =
    st
      { _modelEpochState_ls =
          ModelLState
            { _modelLState_utxoSt = utxoSt,
              _modelLState_dpstate = ModelDPState dstate pstate
            },
        _modelEpochState_acnt = acnt
      }

data ModelLEDGERSEnv env = ModelLEDGERSEnv SlotNo (ModelPParams env) -- (ModelAcnt)

-- FIG32[SL-D5]
instance ModelSTS 'ModelRule_LEDGERS where
  type ModelSignal 'ModelRule_LEDGERS = Compose [] ModelTx
  type ModelState 'ModelRule_LEDGERS = ModelLState
  type ModelEnv 'ModelRule_LEDGERS = ModelLEDGERSEnv
  type ModelFailure 'ModelRule_LEDGERS = Proxy

  applyRuleImpl _ txs = RWS.execRWST $ do
    liftApplyRules (Proxy @'ModelRule_LEDGER) (getCompose txs) txs

-- | See figure 4 [GL-D2]
modelFeesOK :: ModelPParams era -> ModelTx era -> ModelUTxOMap era -> Bool
modelFeesOK pp tx utxoMap =
  -- TODO: This is not fully completed at all
  has modelTx_redeemers tx
    || ( ( balance `pow` (100 :: Natural) >= (Val.coin $ tx ^. modelTx_fee)
             `pow` (bifoldMapSupportsFeature (const 100) id $ runIdentity $ _modelPParams_collateralPercent pp)
         )
           && bifoldMapSupportsFeature (const True) (not . null) (tx ^. modelTx_collateral)
       )
  where
    balance = bifoldMapSupportsFeature (const mempty) (foldMap (Val.coin . _mtxo_value) . Map.restrictKeys (_modelUTxOMap_utxos utxoMap)) (tx ^. modelTx_collateral)

-- | (fig 9)[GL-D2]
instance ModelSTS 'ModelRule_UTXOS where
  type ModelSignal 'ModelRule_UTXOS = ModelTx
  type ModelState 'ModelRule_UTXOS = ModelUTxOState
  type ModelEnv 'ModelRule_UTXOS = ModelUTxOEnv
  type ModelFailure 'ModelRule_UTXOS = Proxy

  applyRuleImpl _ tx
    | modelIsValid tx = RWS.execRWST $ do
      refunded <-
        modelKeyRefunds
          <$> asks (_modelUTxOEnv_pp . _modelEnv)
          <*> pure (toListOf modelDCerts tx)
      deposits <-
        modelTotalDeposits
          <$> asks (_modelUTxOEnv_pp . _modelEnv)
          <*> asks (_modelUTxOEnv_poolParams . _modelEnv)
          <*> pure (toListOf modelDCerts tx)

      let depositChange = deposits ~~ refunded

      -- pup' <- liftModelRule (Proxy @ModelRule_PPUP) tx

      modelUTxOState_utxo %= spendModelUTxOs (Map.keysSet $ _mtxInputs tx) (_mtxOutputs tx)
      modelUTxOState_deposited <>= depositChange
      modelUTxOState_fees <>= Val.coin (_mtxFee tx)
    -- modelUTxOState_ppup .= pup'

    | otherwise = RWS.execRWST $ do
      b <- uses modelUTxOState_utxo totalPreservedAda
      traverseSupportsFeature_
        (\collateral -> modelUTxOState_utxo %= spendModelUTxOs collateral [])
        (_mtxCollateral tx)
      b' <- uses modelUTxOState_utxo totalPreservedAda
      modelUTxOState_fees <>= b ~~ b'

-- | handle utxos on transaction
-- SEE: (fig 10)[GL-D2]
-- DEPRECATES: (fig 7)[GL-D1]
-- DEPRECATES: (fig 16)[SL-D5]
instance ModelSTS 'ModelRule_UTXO where
  type ModelSignal 'ModelRule_UTXO = ModelTx
  type ModelState 'ModelRule_UTXO = ModelUTxOState
  type ModelEnv 'ModelRule_UTXO = ModelUTxOEnv
  type ModelFailure 'ModelRule_UTXO = Proxy

  applyRuleImpl _ tx = RWS.execRWST $ do
    -- guardRule ModelUTXOFailure_InputSetEmtpy
    --   (not $ null $ _mtxInputs tx)

    -- ((<=) <$> pure (fromSupportsPlutus (const 0) length $ _mtxCollateral tx) <*> asks maxCollateralInputs)
    --   >>= guardRule ModelUTXOFailure_InputSetEmtpy
    utxoMap <- State.gets _modelUTxOState_utxo
    pp <- asks getModelPParams
    -- TODO: NoCollateralInputs error
    unless (modelFeesOK pp tx utxoMap) (tell $ Set.singleton Proxy)
    liftApplyRule (Proxy @'ModelRule_UTXOS) tx tx

data ModelLEnv env = ModelLEnv
  { _modelLEnv_slot :: !SlotNo,
    _modelLEnv_txIx :: !Int,
    _modelLEnv_pp :: !(ModelPParams env)
    -- , _modelLEnv_acnt :: !ModelAcnt
  }

-- fig 14(GL-D2)
-- DEPRECATED: FIG30[SL-D5]
instance ModelSTS 'ModelRule_LEDGER where
  type ModelSignal 'ModelRule_LEDGER = ModelTx
  type ModelState 'ModelRule_LEDGER = ModelLState
  type ModelEnv 'ModelRule_LEDGER = ModelLEnv
  type ModelFailure 'ModelRule_LEDGER = Proxy

  applyRuleImpl _ tx = RWS.execRWST $ do
    lift $ setProvenance (getModelTxId tx)
    liftApplyRule (Proxy @'ModelRule_UTXOW) tx tx -- order matters UTXOW gets its env from DPState, DELEGS updates DPState
    when (modelIsValid tx) $
      liftApplyRule (Proxy @'ModelRule_DELEGS) (Compose $ toListOf modelDCerts tx) tx
    lift $ clearProvenance

-- (fig13)[GL-D2]
instance ModelSTS 'ModelRule_UTXOW where
  type ModelSignal 'ModelRule_UTXOW = ModelTx
  type ModelState 'ModelRule_UTXOW = ModelUTxOState
  type ModelEnv 'ModelRule_UTXOW = ModelUTxOEnv
  type ModelFailure 'ModelRule_UTXOW = Proxy

  applyRuleImpl _ tx = RWS.execRWST $ do
    liftApplyRule (Proxy @'ModelRule_UTXO) tx tx

data ModelDPSEnv era = ModelDPSEnv
  { _modelDPSEnv_slot :: !SlotNo,
    _modelDPSEnv_txIx :: !Int,
    _modelDPSEnv_pp :: !(ModelPParams era),
    _modelDPSEnv_tx :: !(ModelTx era)
    -- , _modelDPSEnv_acnt :: !ModelAcnt
  }

-- fig28[SL-D5]
instance ModelSTS 'ModelRule_DELEGS where
  type ModelSignal 'ModelRule_DELEGS = Compose [] ModelDCert
  type ModelState 'ModelRule_DELEGS = ModelDPState
  type ModelEnv 'ModelRule_DELEGS = ModelDPSEnv
  type ModelFailure 'ModelRule_DELEGS = Proxy

  applyRuleImpl _ cs = RWS.execRWST $ do
    wdrls <- asks (_mtxWdrl . _modelDPSEnv_tx . _modelEnv)
    lift $ wdrlProvenance (Map.keysSet wdrls)
    badWdrls <-
      modelDPState_dstate . modelDState_rewards
        %%= Map.mergeA
          (Map.traverseMaybeMissing $ \_ (wdrl, _) -> (bool mempty (Set.singleton Proxy) (Val.zero == Val.coin wdrl), Nothing)) -- DelegateeNotRegistered
          (Map.preserveMissing)
          ( Map.zipWithAMatched $ \_ (mwdrl, _) rwd ->
              let wdrl = Val.coin mwdrl
               in (bool mempty (Set.singleton Proxy) (rwd == wdrl), Val.zero)
          ) -- WithdrawalsNotInRewards
          wdrls

    tell badWdrls

    liftApplyRules (Proxy @'ModelRule_DELPL) (getCompose cs) cs

-- Fig26 [SL-D5]
instance ModelSTS 'ModelRule_DELPL where
  type ModelSignal 'ModelRule_DELPL = ModelDCert
  type ModelState 'ModelRule_DELPL = ModelDPState
  type ModelEnv 'ModelRule_DELPL = ModelDPSEnv
  type ModelFailure 'ModelRule_DELPL = Proxy

  applyRuleImpl _ dcert = RWS.execRWST $ do
    lift $ delegProvenance dcert
    case dcert of
      c'@(ModelCertDeleg c) -> liftApplyRule (Proxy @'ModelRule_DELEG) c c'
      c'@(ModelCertPool c) -> liftApplyRule (Proxy @'ModelRule_POOL) c c'

data ModelDEnv (env :: FeatureSet) = ModelDEnv SlotNo -- ModelAcnt

-- fig22[SL-D5]
instance ModelSTS 'ModelRule_DELEG where
  type ModelSignal 'ModelRule_DELEG = ModelDelegCert
  type ModelState 'ModelRule_DELEG = ModelDState
  type ModelEnv 'ModelRule_DELEG = ModelDEnv
  type ModelFailure 'ModelRule_DELEG = Proxy

  applyRuleImpl _ = \case
    ModelRegKey hk ->
      RWS.execRWST $
        use (modelDState_rewards . at hk) >>= \case
          Nothing -> do
            modelDState_rewards . at hk .= Just mempty
          -- view ptrOptic >>= (\ptr -> modelDState_ptrs . at ptr .= hk)
          Just _ -> tell (Set.singleton Proxy) -- StakeKeyAlreadyRegistered
    ModelDeRegKey hk ->
      RWS.execRWST $
        use (modelDState_rewards . at hk) >>= \case
          Nothing -> tell (Set.singleton Proxy) -- StakeKeyNotRegistered
          Just reward
            | reward /= mempty -> tell (Set.singleton Proxy) -- StakeKeyNonZeroAccountBalance
            | otherwise -> do
              modelDState_rewards . at hk .= Nothing
              modelDState_delegations . at hk .= Nothing
    ModelDelegate (ModelDelegation hk dpool) ->
      RWS.execRWST $
        use (modelDState_rewards . at hk) >>= \case
          Nothing -> tell (Set.singleton Proxy) -- StakeDelegationImpossible
          Just _ -> modelDState_delegations . at hk .= Just dpool
    ModelDCertGenesis (ModelGenesisDelegCert gkh vkh) ->
      RWS.execRWST $ do
        ModelGlobalsEnv globals (ModelDEnv slot) <- ask
        let s' = slot + (fromIntegral $ stabilityWindow globals)
        modelDState_fGenDelegs . at (s', gkh) .= Just vkh
    ModelDCertMir (ModelMIRCert mirPot credCoinMap) -> RWS.execRWST $ do
      let irwds = case credCoinMap of
            ModelStakeAddressesMIR q ->
              let irwd =
                    ModelInstantaneousReward mempty $
                      mkGrpMap $
                        ( coerce ::
                            forall sf.
                            Map.Map (ModelCredential 'Staking sf) DeltaCoin ->
                            Map.Map (ModelCredential 'Staking sf) Coin
                        )
                          $ q
               in case mirPot of
                    TreasuryMIR -> ModelAcnt irwd mempty
                    ReservesMIR -> ModelAcnt mempty irwd
            ModelSendToOppositePotMIR q ->
              let irwd = ModelInstantaneousReward q mempty
               in case mirPot of
                    TreasuryMIR -> ModelAcnt (invert irwd) irwd
                    ReservesMIR -> ModelAcnt irwd (invert irwd)

      modelDState_iRwd <>= Comp1 irwds

data ModelPEnv env = ModelPEnv SlotNo (ModelPParams env)

-- fig22[SL-D5]
instance ModelSTS 'ModelRule_POOL where
  type ModelSignal 'ModelRule_POOL = ModelPoolCert
  type ModelState 'ModelRule_POOL = ModelPState
  type ModelEnv 'ModelRule_POOL = ModelPEnv
  type ModelFailure 'ModelRule_POOL = Proxy

  applyRuleImpl _ = \case
    ModelRegPool pool@ModelPoolParams {_mppId = hk} ->
      RWS.execRWST $
        use (modelPState_poolParams . at hk) >>= \case
          Nothing -> do
            modelPState_poolParams . at hk .= Just pool
          Just _ -> do
            modelPState_fPoolParams . at hk .= Just pool
            modelPState_retiring %= flip Map.withoutKeys (Set.singleton hk)
    ModelRetirePool hk e -> RWS.execRWST $ do
      modelPState_retiring . at hk .= Just e

-- fig63[SL-D5]
instance ModelSTS 'ModelRule_TICK where
  type ModelSignal 'ModelRule_TICK = Const SlotNo
  type ModelState 'ModelRule_TICK = ModelNewEpochState
  type ModelEnv 'ModelRule_TICK = Proxy
  type ModelFailure 'ModelRule_TICK = Proxy

  applyRuleImpl _ slot = RWS.execRWST $ do
    Identity epoch <-
      epochInfoEpoch
        <$> asks (epochInfo . getGlobals)
        <*> pure (getConst slot)

    lift $ setSlot epoch (getConst slot)
    liftApplyRule (Proxy @'ModelRule_NEWEPOCH) (Const epoch) slot
    liftApplyRule (Proxy @'ModelRule_RUPD) slot slot

-- Fig61[SL-D5]
instance ModelSTS 'ModelRule_RUPD where
  type ModelSignal 'ModelRule_RUPD = Const SlotNo
  type ModelState 'ModelRule_RUPD = Compose Maybe ModelRewardUpdate
  type ModelEnv 'ModelRule_RUPD = ModelRUpdEnv
  type ModelFailure 'ModelRule_RUPD = Proxy

  applyRuleImpl _ (Const s) =
    RWS.execRWST $
      State.get >>= \case
        Compose (Just _) -> pure ()
        Compose (Nothing) -> do
          ei <- asks (epochInfo . getGlobals)
          rsw <- asks (randomnessStabilisationWindow . getGlobals)
          let e = runIdentity $ epochInfoEpoch ei s
          when (s > runIdentity (epochInfoFirst ei e) + (fromIntegral rsw)) $ do
            createRUpd' <-
              createRUpd
                <$> asks _modelEnv
                <*> (pure $ runIdentity $ epochInfoSize ei e)
                <*> asks (Coin . toInteger . maxLovelaceSupply . getGlobals)
                <*> asks (activeSlotCoeff . getGlobals)
            ru' <- lift createRUpd'

            -- _Left (traceM . show) (validModelRewardUpdate ru')

            State.put (Compose $ Just ru')

-- Fig56[SL-D5]
instance ModelSTS 'ModelRule_NEWEPOCH where
  type ModelSignal 'ModelRule_NEWEPOCH = Const EpochNo
  type ModelState 'ModelRule_NEWEPOCH = ModelNewEpochState
  type ModelEnv 'ModelRule_NEWEPOCH = Proxy
  type ModelFailure 'ModelRule_NEWEPOCH = Proxy

  applyRuleImpl _ e = RWS.execRWST $ do
    el <- State.gets _modelNewEpochState_el
    case getConst e `compare` (el + 1) of
      LT -> pure ()
      GT -> error "skipped epochs" -- TODO, does the spec say to "do" anything here?
      EQ -> do
        use modelNewEpochState_ru
          >>= traverse_
            ( \ru -> do
                modelNewEpochState_es %= State.execState (applyRUpd ru)
            )

        liftApplyRule (Proxy @'ModelRule_MIR) Proxy e
        liftApplyRule (Proxy @'ModelRule_EPOCH) e e

        modelNewEpochState_el .= getConst e
        (modelNewEpochState_bPrev .=) =<< use modelNewEpochState_bCur
        modelNewEpochState_bCur .= mempty
        modelNewEpochState_ru .= Nothing

-- FIG54[SL-D5]
instance ModelSTS 'ModelRule_MIR where
  type ModelSignal 'ModelRule_MIR = Proxy
  type ModelState 'ModelRule_MIR = ModelEpochState
  type ModelEnv 'ModelRule_MIR = Proxy
  type ModelFailure 'ModelRule_MIR = Proxy

  applyRuleImpl _ Proxy = RWS.execRWST $ do
    ( Comp1
        ( ModelAcnt
            { _modelAcnt_treasury = ModelInstantaneousReward dTreasury irTreasury,
              _modelAcnt_reserves = ModelInstantaneousReward dReserves irReserves
            }
          )
      ) <-
      modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_iRwd
        <<.= mempty

    rewards <- use $ modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards
    ModelAcnt treasury reserves <- use modelEpochState_acnt

    let rewardAcnts = Map.keysSet rewards
        restrictIRToRewardAcnts = flip restrictKeysGrpMap rewardAcnts

        irwdR@(GrpMap irwdR') = restrictIRToRewardAcnts irReserves
        irwdT@(GrpMap irwdT') = restrictIRToRewardAcnts irTreasury
        availableReserves = reserves <> dReserves
        availableTreasury = treasury <> dTreasury
        totR = fold irwdR
        totT = fold irwdT

    if
        | totR <= availableReserves && totT <= availableTreasury -> do
          let update = Map.unionWith (<>) irwdR' irwdT'
              rewards' = Map.unionWith (<>) rewards update
          lift $ mirProvenance (Just $ Map.keysSet update)
          modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards .= rewards'
          modelEpochState_acnt <>= ModelAcnt dTreasury dReserves ~~ ModelAcnt totT totR
        | otherwise -> do
          lift $ mirProvenance Nothing

-- Fig44[SL-D5]
instance ModelSTS 'ModelRule_EPOCH where
  type ModelSignal 'ModelRule_EPOCH = Const EpochNo
  type ModelState 'ModelRule_EPOCH = ModelEpochState
  type ModelEnv 'ModelRule_EPOCH = Proxy
  type ModelFailure 'ModelRule_EPOCH = Proxy

  applyRuleImpl _ e = RWS.execRWST $ do
    liftApplyRule (Proxy @'ModelRule_SNAP) Proxy e

    modelEpochState_ls . modelLState_dpstate . modelDPState_pstate
      %= ( \(ModelPState poolParams fPoolParams retiring) ->
             ModelPState (Map.unionWith (\_ x -> x) poolParams fPoolParams) Map.empty retiring
         )

    liftApplyRule (Proxy @'ModelRule_POOLREAP) e e

-- liftApplyRule (Proxy @ModelRule_NEWPP) _ _

-- | fig 15
data ModelUTxOEnv era = ModelUTxOEnv
  { _modelUTxOEnv_slot :: !SlotNo,
    _modelUTxOEnv_pp :: !(ModelPParams era),
    _modelUTxOEnv_poolParams :: !(Map.Map ModelPoolId (ModelPoolParams era)),
    _modelUTxOEnv_genDelegs :: !ModelGenesisDelegation
  }

instance HasModelPParams era (ModelUTxOEnv era) where
  getModelPParams = _modelUTxOEnv_pp

data ModelGlobalsEnv a = ModelGlobalsEnv
  { _modelGlobals :: !Globals,
    _modelEnv :: !a
  }
  deriving (Show, Functor, Foldable, Traversable)

instance HasGlobals (ModelGlobalsEnv a) where
  getGlobals (ModelGlobalsEnv g _) = g

instance HasModelPParams era a => HasModelPParams era (ModelGlobalsEnv a) where
  getModelPParams = getModelPParams . _modelEnv

liftApplyRule ::
  ( ModelSubRule rule,
    MonadModelProvenance era provM,
    Ord (ModelFailure (ModelSuperRule rule) era),
    Ord (ModelFailure rule era),
    KnownRequiredFeatures (era),
    ModelSigIter rule ~ Identity
  ) =>
  proxy rule ->
  ModelSignal rule era ->
  ModelSignal (ModelSuperRule rule) era ->
  RWS.RWST
    (ModelGlobalsEnv (ModelEnv (ModelSuperRule rule) era))
    (Set (ModelFailure (ModelSuperRule rule) era))
    (ModelState (ModelSuperRule rule) era)
    provM
    ()
liftApplyRule proxy signal' = liftApplyRules proxy (Identity signal')

liftApplyRules ::
  ( ModelSubRule rule,
    MonadModelProvenance era provM,
    Ord (ModelFailure (ModelSuperRule rule) era),
    Ord (ModelFailure rule era),
    KnownRequiredFeatures (era),
    Foldable (ModelSigIter rule)
  ) =>
  proxy rule ->
  ModelSigIter rule (ModelSignal rule era) ->
  ModelSignal (ModelSuperRule rule) era ->
  RWS.RWST
    (ModelGlobalsEnv (ModelEnv (ModelSuperRule rule) era))
    (Set (ModelFailure (ModelSuperRule rule) era))
    (ModelState (ModelSuperRule rule) era)
    provM
    ()
liftApplyRules proxy signal' signal = RWS.rwsT $ \(ModelGlobalsEnv globals env) st -> do
  let envs' = getSubEnv proxy signal signal' env st

  (st''', w) <-
    RWS.execRWST
      ( for_ envs' $ \(env', signal'') -> RWS.rwsT $ \() st' -> do
          (st'', w') <- applyRuleImpl proxy signal'' (ModelGlobalsEnv globals env') st'
          pure ((), st'', w')
      )
      ()
      (getSubState proxy st)
  pure ((), putSubState proxy st''' st, Set.map (asSubFailure proxy) w)

-- FIG 37[SL-D5]
stakeDistr :: ModelUTxOMap era -> ModelDState era -> ModelPState era -> ModelSnapshot era
stakeDistr
  utxo
  ( ModelDState
      { _modelDState_rewards = rewards,
        _modelDState_delegations = delegations
      }
    )
  ( ModelPState
      { _modelPState_poolParams = poolParams
      }
    ) =
    ModelSnapshot
      { _modelSnapshot_stake = stake,
        _modelSnapshot_delegations = delegations,
        _modelSnapshot_pools = poolParams,
        _modelSnapshot_utxos = utxo,
        _modelSnapshot_rewards = rewards
      }
    where
      stake =
        Map.merge
          (Map.mapMissing $ \_ a -> ModelSnapshotStake a Set.empty)
          Map.dropMissing
          (Map.zipWithMatched $ \_ a (b, c) -> ModelSnapshotStake (a <> b) c)
          rewards
          (unGrpMap $ _modelUTxOMap_stake utxo)

-- FIG 38
instance ModelSTS 'ModelRule_SNAP where
  type ModelSignal 'ModelRule_SNAP = Proxy
  type ModelState 'ModelRule_SNAP = ModelSnapshots
  type ModelEnv 'ModelRule_SNAP = ModelLState
  type ModelFailure 'ModelRule_SNAP = Proxy

  applyRuleImpl _ Proxy = RWS.execRWST $ do
    ModelLState
      ( ModelUTxOState
          { _modelUTxOState_utxo = utxo,
            _modelUTxOState_fees = fees
          }
        )
      (ModelDPState dstate pstate) <-
      asks _modelEnv
    let stake = stakeDistr utxo dstate pstate
    SnapshotQueue mark setSS _go <- State.gets _modelSnapshots_pstake
    State.put $ ModelSnapshots (SnapshotQueue stake mark setSS) fees

instance HasModelPParams era (ModelPOOLREAPEnv era) where
  getModelPParams (ModelPOOLREAPEnv pp) = pp

-- FIG 40[SL-D5]
instance ModelSTS 'ModelRule_POOLREAP where
  type ModelSignal 'ModelRule_POOLREAP = Const EpochNo
  type ModelState 'ModelRule_POOLREAP = ModelPlReapState
  type ModelEnv 'ModelRule_POOLREAP = ModelPOOLREAPEnv
  type ModelFailure 'ModelRule_POOLREAP = Proxy

  applyRuleImpl _ (Const e) = RWS.execRWST $ do
    retired <-
      ifoldMap (\hk e' -> if e == e' then Set.singleton hk else Set.empty)
        <$> use (modelPlReapState_pstate . modelPState_retiring)
    pr <- asks (runIdentity . _modelPParams_poolDeposit . getModelPParams)

    rewards <- use $ modelPlReapState_dstate . modelDState_rewards
    poolParams <- use $ modelPlReapState_pstate . modelPState_poolParams

    let rewardAcnts =
          Map.fromList
            [ (hk, _mppRAcnt pool)
              | (hk, pool) <- Map.toList $ Map.restrictKeys poolParams retired
            ]
        rewardAcnts' =
          Map.fromListWith
            (<>)
            [ (a, pr)
              | (_hk, a) <- Map.toList rewardAcnts
            ]
        refunds = Map.intersection rewardAcnts' rewards
        mRefunds = Map.difference rewardAcnts' rewards
        refunded = fold refunds
        unclaimed = fold mRefunds

    modelPlReapState_utxoSt . modelUTxOState_deposited %= (~~ (unclaimed <> refunded))

    modelPlReapState_acnt . modelAcnt_treasury <>= unclaimed

    modelPlReapState_dstate . modelDState_rewards %= Map.unionWith (<>) refunds
    modelPlReapState_dstate . modelDState_delegations %= Map.filter (\hk -> not $ Set.member hk retired)

    modelPlReapState_pstate . modelPState_poolParams %= flip Map.withoutKeys retired
    modelPlReapState_pstate . modelPState_fPoolParams %= flip Map.withoutKeys retired
    modelPlReapState_pstate . modelPState_retiring %= flip Map.withoutKeys retired

data ModelPredicateFailure era
  = ModelValueNotConservedUTxO
      !(ModelValue (ValueFeature era) era)
      -- ^ the Coin consumed by this transaction
      !(ModelValue (ValueFeature era) era)
      -- ^ the Coin produced by this transaction
