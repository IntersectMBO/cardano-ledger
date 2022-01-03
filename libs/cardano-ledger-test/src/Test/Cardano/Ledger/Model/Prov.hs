{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Model.Prov where

import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Slotting.Slot (EpochNo, SlotNo)
import Control.Lens
import Control.Monad (ap)
import qualified Control.Monad.RWS.CPS as RWS
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (fold, for_)
import Data.Functor.PiecewiseConstant
  ( PiecewiseConstantMap,
    liftPiecewiseConstantMap,
    splicePiecewiseConstantMap,
  )
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelPoolId,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( ScriptFeature,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential,
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelDelegCert (..),
    ModelDelegation (..),
    ModelGenesisDelegCert (..),
    ModelMIRCert (..),
    ModelPoolCert (..),
    ModelPoolParams (..),
    ModelTxId,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelUTxOId,
  )

class Monad m => MonadModelProvenance era m | m -> era where
  setProvenance :: ModelTxId -> m ()
  clearProvenance :: m ()
  delegProvenance :: ModelDCert era -> m ()

  setSlot :: EpochNo -> SlotNo -> m ()

  rewardOperatorProvenance,
    rewardMemberProvenance ::
      ModelPoolId ->
      Map.Map (ModelCredential 'Staking (ScriptFeature era)) (Set ModelUTxOId) ->
      m ()

  wdrlProvenance :: Set (ModelCredential 'Staking (ScriptFeature era)) -> m ()

  mirProvenance :: Maybe (Set (ModelCredential 'Staking (ScriptFeature era))) -> m ()

type EpochMap = PiecewiseConstantMap EpochNo

-- TODO: temporalize all this.
data ModelProvenanceState era = ModelProvenanceState
  { _modelProvenanceState_currentTxId :: !(Maybe (ModelTxId)),
    _modelProvenanceState_currentSlot :: !(EpochNo, SlotNo),
    _modelProvenanceState_regStake ::
      !( Map.Map
           (ModelCredential 'Staking (ScriptFeature era))
           (EpochMap (Maybe ModelTxId))
       ),
    _modelProvenanceState_regPool ::
      !( Map.Map
           ModelPoolId
           (EpochMap (Maybe ModelTxId))
       ),
    _modelProvenanceState_deleg ::
      !( Map.Map
           (ModelCredential 'Staking (ScriptFeature era))
           (EpochMap (Maybe ModelTxId))
       ),
    _modelProvenanceState_reward ::
      !( Map.Map
           EpochNo
           ( Map.Map
               (ModelCredential 'Staking (ScriptFeature era))
               (Set ModelUTxOId, Set ModelTxId)
           )
       ),
    _modelProvenanceState_wdrl ::
      !( Map.Map
           (ModelCredential 'Staking (ScriptFeature era))
           EpochNo
       ),
    _modelProvenanceState_wdrlRewards ::
      !( Map.Map
           (ModelCredential 'Staking (ScriptFeature era))
           ( Map.Map
               ModelTxId
               (EpochNo, EpochNo)
           )
       ),
    _modelProvenanceState_poolPerformance :: !(Map.Map EpochNo (Set ModelPoolId)),
    _modelProvenanceState_ir :: !(Map.Map EpochNo (Set ModelTxId))
  }
  deriving (Show)

emptyModelProvenanceState :: ModelProvenanceState era
emptyModelProvenanceState =
  ModelProvenanceState
    { _modelProvenanceState_currentTxId = Nothing,
      _modelProvenanceState_currentSlot = (-1, -1),
      _modelProvenanceState_regStake = Map.empty,
      _modelProvenanceState_regPool = Map.empty,
      _modelProvenanceState_deleg = Map.empty,
      _modelProvenanceState_reward = Map.empty,
      _modelProvenanceState_wdrl = Map.empty,
      _modelProvenanceState_poolPerformance = Map.empty,
      _modelProvenanceState_wdrlRewards = Map.empty,
      _modelProvenanceState_ir = Map.empty
    }

modelProvenanceState_currentSlot :: Lens' (ModelProvenanceState era) (EpochNo, SlotNo)
modelProvenanceState_currentSlot a2fb s = (\b -> s {_modelProvenanceState_currentSlot = b}) <$> a2fb (_modelProvenanceState_currentSlot s)
{-# INLINE modelProvenanceState_currentSlot #-}

modelProvenanceState_currentTxId :: Lens' (ModelProvenanceState era) (Maybe (ModelTxId))
modelProvenanceState_currentTxId a2fb s = (\b -> s {_modelProvenanceState_currentTxId = b}) <$> a2fb (_modelProvenanceState_currentTxId s)
{-# INLINE modelProvenanceState_currentTxId #-}

modelProvenanceState_regStake :: Lens' (ModelProvenanceState era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) (EpochMap (Maybe ModelTxId)))
modelProvenanceState_regStake a2fb s = (\b -> s {_modelProvenanceState_regStake = b}) <$> a2fb (_modelProvenanceState_regStake s)
{-# INLINE modelProvenanceState_regStake #-}

modelProvenanceState_regPool :: Lens' (ModelProvenanceState era) (Map.Map ModelPoolId (EpochMap (Maybe ModelTxId)))
modelProvenanceState_regPool a2fb s = (\b -> s {_modelProvenanceState_regPool = b}) <$> a2fb (_modelProvenanceState_regPool s)
{-# INLINE modelProvenanceState_regPool #-}

modelProvenanceState_deleg :: Lens' (ModelProvenanceState era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) (EpochMap (Maybe ModelTxId)))
modelProvenanceState_deleg a2fb s = (\b -> s {_modelProvenanceState_deleg = b}) <$> a2fb (_modelProvenanceState_deleg s)
{-# INLINE modelProvenanceState_deleg #-}

modelProvenanceState_reward :: Lens' (ModelProvenanceState era) (Map.Map EpochNo (Map.Map (ModelCredential 'Staking (ScriptFeature era)) (Set ModelUTxOId, Set ModelTxId)))
modelProvenanceState_reward a2fb s = (\b -> s {_modelProvenanceState_reward = b}) <$> a2fb (_modelProvenanceState_reward s)
{-# INLINE modelProvenanceState_reward #-}

modelProvenanceState_wdrl :: Lens' (ModelProvenanceState era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) EpochNo)
modelProvenanceState_wdrl a2fb s = (\b -> s {_modelProvenanceState_wdrl = b}) <$> a2fb (_modelProvenanceState_wdrl s)
{-# INLINE modelProvenanceState_wdrl #-}

modelProvenanceState_wdrlRewards :: Lens' (ModelProvenanceState era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) (Map.Map ModelTxId (EpochNo, EpochNo)))
modelProvenanceState_wdrlRewards a2fb s = (\b -> s {_modelProvenanceState_wdrlRewards = b}) <$> a2fb (_modelProvenanceState_wdrlRewards s)
{-# INLINE modelProvenanceState_wdrlRewards #-}

modelProvenanceState_poolPerformance :: Lens' (ModelProvenanceState era) (Map.Map EpochNo (Set ModelPoolId))
modelProvenanceState_poolPerformance a2fb s = (\b -> s {_modelProvenanceState_poolPerformance = b}) <$> a2fb (_modelProvenanceState_poolPerformance s)
{-# INLINE modelProvenanceState_poolPerformance #-}

modelProvenanceState_ir :: Lens' (ModelProvenanceState era) (Map.Map EpochNo (Set ModelTxId))
modelProvenanceState_ir a2fb s = (\b -> s {_modelProvenanceState_ir = b}) <$> a2fb (_modelProvenanceState_ir s)
{-# INLINE modelProvenanceState_ir #-}

class HasModelProvenanceState era st | st -> era where
  modelProvenanceState :: Lens' st (ModelProvenanceState era)

instance HasModelProvenanceState era (ModelProvenanceState era) where
  modelProvenanceState = id

type SomeMonadTrans :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
newtype SomeMonadTrans t m a = SomeMonadTrans {getSomeMonadTrans :: t m a}
  deriving (Functor)

instance Monad (t m) => Applicative (SomeMonadTrans t m) where
  pure = SomeMonadTrans . pure
  (<*>) = ap

instance Monad (t m) => Monad (SomeMonadTrans t m) where
  SomeMonadTrans xs >>= k = SomeMonadTrans $ xs >>= (getSomeMonadTrans <$> k)

instance MonadTrans t => MonadTrans (SomeMonadTrans t) where
  lift = SomeMonadTrans . lift

instance
  ( MonadModelProvenance era m,
    MonadTrans t,
    Monad (t m)
  ) =>
  MonadModelProvenance era (SomeMonadTrans t m)
  where
  setProvenance = lift . setProvenance
  setSlot e s = lift $ setSlot e s
  clearProvenance = lift clearProvenance
  delegProvenance = lift . delegProvenance
  rewardOperatorProvenance p = lift . rewardOperatorProvenance p
  rewardMemberProvenance p = lift . rewardMemberProvenance p
  wdrlProvenance = lift . wdrlProvenance
  mirProvenance = lift . mirProvenance

instance MonadModelProvenance era (Tagged era) where
  setProvenance _ = pure ()
  setSlot _ _ = pure ()
  clearProvenance = pure ()
  delegProvenance _ = pure ()
  rewardOperatorProvenance _ _ = pure ()
  rewardMemberProvenance _ _ = pure ()
  wdrlProvenance _ = pure ()
  mirProvenance _ = pure ()

newtype SomeMonadState s m a = SomeMonadState (m a)
  deriving (Functor, Applicative, Monad)

deriving newtype instance MonadState s m => MonadState s (SomeMonadState s m)

instance
  (MonadState s m, HasModelProvenanceState era s) =>
  MonadModelProvenance era (SomeMonadState s m)
  where
  setProvenance = State.modify . set (modelProvenanceState . modelProvenanceState_currentTxId) . Just
  setSlot epoch slot = modelProvenanceState . modelProvenanceState_currentSlot .= (epoch, slot)
  clearProvenance = State.modify $ set (modelProvenanceState . modelProvenanceState_currentTxId) Nothing

  delegProvenance dcert = do
    provs <- use $ modelProvenanceState . modelProvenanceState_currentTxId
    epoch <- use $ modelProvenanceState . modelProvenanceState_currentSlot . _1

    let mergeProv ::
          Ord k =>
          k ->
          Maybe v ->
          Maybe (PiecewiseConstantMap k (Maybe v)) ->
          PiecewiseConstantMap k (Maybe v)
        mergeProv k v = \l ->
          let l' = maybe (pure Nothing) id l
              u = pure v
           in splicePiecewiseConstantMap k l' u
        {-# INLINE mergeProv #-}

    for_ provs $ \prov -> case dcert of
      ModelCertDeleg cert -> case cert of
        ModelRegKey cred ->
          modelProvenanceState . modelProvenanceState_regStake . at cred
            %= Just . mergeProv epoch (Just prov)
        ModelDeRegKey cred ->
          modelProvenanceState . modelProvenanceState_regStake . at cred
            %= Just . mergeProv epoch Nothing
        ModelDelegate (ModelDelegation stk _) ->
          modelProvenanceState . modelProvenanceState_deleg . at stk
            %= Just . mergeProv epoch (Just prov)
        ModelDCertGenesis (ModelGenesisDelegCert {}) -> pure ()
        ModelDCertMir (ModelMIRCert {}) ->
          modelProvenanceState . modelProvenanceState_ir
            %= Map.unionWith (<>) (Map.singleton epoch $ Set.singleton prov)
      ModelCertPool cert -> case cert of
        ModelRegPool (ModelPoolParams {_mppId = pool}) -> do
          -- if the pool is already registered, then the registration takes
          -- effect next epoch, otherwise it's valid this epoch
          oldReg <- use $ modelProvenanceState . modelProvenanceState_regPool . at pool
          let oldReg' = maybe epoch (\_ -> epoch + 1) $ oldReg >>= (flip liftPiecewiseConstantMap epoch)
          modelProvenanceState . modelProvenanceState_regPool . at pool
            %= Just . mergeProv oldReg' (Just prov)
        ModelRetirePool {} ->
          -- TODO
          pure ()

  rewardMemberProvenance pool stakeProv = do
    -- stakeProv only tells about which UTxO's contributed to stake.  we need to
    -- work out the registrations.
    --
    -- This gets called in the RUPD step, which determines the rewards earned
    -- for pool performance in epoch-1; so the relevnat registrations are
    -- in epoch-3.  we need to know when the stake was registered, when the pool
    -- was registered, and when the delegation occured.

    epoch <- use $ modelProvenanceState . modelProvenanceState_currentSlot . _1

    regState <- use $ modelProvenanceState . modelProvenanceState_regStake
    poolReg <- use $ modelProvenanceState . modelProvenanceState_regPool . at pool
    delegs <- use $ modelProvenanceState . modelProvenanceState_deleg

    let lookupProv = foldMap Set.singleton . flip liftPiecewiseConstantMap (epoch - 3)

        poolReg' = foldMap lookupProv poolReg
        stakeProv' =
          Map.intersectionWith
            (\stk reg -> (stk, poolReg' <> lookupProv reg))
            stakeProv
            regState

        stakeProv'' =
          Map.intersectionWith
            (\(stk, regs) deleg -> (stk, regs <> lookupProv deleg))
            stakeProv'
            delegs

    modelProvenanceState . modelProvenanceState_reward . at epoch %= Just . Map.unionWith (<>) stakeProv'' . fold

  rewardOperatorProvenance pool stakeProv = do
    epoch <- use $ modelProvenanceState . modelProvenanceState_currentSlot . _1
    poolReg <- use $ modelProvenanceState . modelProvenanceState_regPool . at pool

    let lookupProv = foldMap Set.singleton . flip liftPiecewiseConstantMap (epoch - 3)

        poolReg' = foldMap lookupProv poolReg
        stakeProv' = (\stk -> (stk, poolReg')) <$> stakeProv

    modelProvenanceState . modelProvenanceState_reward . at epoch
      %= Just . Map.unionWith (<>) stakeProv' . fold
    modelProvenanceState . modelProvenanceState_poolPerformance
      %= Map.unionWith (<>) (Map.singleton epoch $ Set.singleton pool)

  wdrlProvenance stk = do
    provs <- use $ modelProvenanceState . modelProvenanceState_currentTxId
    for_ provs $ \prov -> do
      currentEpoch <- use $ modelProvenanceState . modelProvenanceState_currentSlot . _1
      let availableRewards = currentEpoch - 2
      oldWdrls <- use $ modelProvenanceState . modelProvenanceState_wdrl
      let wdrlRewards =
            Map.merge
              Map.dropMissing
              (Map.mapMissing $ \_ () -> Map.singleton prov (0, currentEpoch))
              ( Map.zipWithMaybeMatched $ \_ oldWdrl () ->
                  if (currentEpoch > oldWdrl)
                    then Just $ Map.singleton prov (oldWdrl + 1, currentEpoch)
                    else Nothing
              )
              oldWdrls
              $ Map.fromSet (const ()) stk
      modelProvenanceState . modelProvenanceState_wdrlRewards %= Map.unionWith Map.union wdrlRewards
      modelProvenanceState . modelProvenanceState_wdrl %= Map.unionWith max (Map.fromSet (const availableRewards) stk)

  mirProvenance = \case
    Nothing -> pure ()
    Just ir -> do
      epoch <- use $ modelProvenanceState . modelProvenanceState_currentSlot . _1
      prov <- uses (modelProvenanceState . modelProvenanceState_ir . at epoch) fold
      regState <- use $ modelProvenanceState . modelProvenanceState_regStake

      let lookupProv = foldMap Set.singleton . flip liftPiecewiseConstantMap epoch

          ir' =
            (\reg -> (,) mempty $ prov <> lookupProv reg)
              <$> Map.restrictKeys
                regState
                ir

      modelProvenanceState . modelProvenanceState_reward . at epoch
        %= Just . Map.unionWith (<>) ir' . fold

deriving via (SomeMonadState s (State.StateT s m)) instance (Monad m, HasModelProvenanceState era s) => MonadModelProvenance era (State.StateT s m)

deriving via (SomeMonadState s (RWS.RWST r w s m)) instance (Monad m, HasModelProvenanceState era s) => MonadModelProvenance era (RWS.RWST r w s m)
