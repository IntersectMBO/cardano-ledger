{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.Tx where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), txscriptfee)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (EpochNo)
import Control.DeepSeq (NFData (..))
import Control.Lens
  ( Lens',
    Prism',
    Traversal',
    foldMapOf,
    foldOf,
    folded,
    lengthOf,
    lens,
    prism,
    toListOf,
    _1,
    _2,
  )
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..))
import Data.Group (Group (..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import GHC.Generics (Generic)
import qualified GHC.Records as GHC
import qualified PlutusTx (Data)
import Quiet (Quiet (..))
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelPoolId (..),
    ModelValue,
    liftModelValue,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet,
    FeatureTag (..),
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    KnownRequiredFeatures,
    RequiredFeatures (..),
    ScriptFeature,
    ShelleyScriptFeatures,
    TyScriptFeature (..),
    TyValueExpected (..),
    ValueFeature,
    ValueFeatureTag (..),
    bitraverseSupportsPlutus,
    filterSupportsPlutus,
    hasKnownScriptFeature,
    ifSupportsPlutus,
    mapSupportsPlutus,
    reifyRequiredFeatures,
    reifySupportsPlutus,
    traverseSupportsMint,
  )
import Test.Cardano.Ledger.Model.PParams
  ( ModelPParams,
    ModelPParamsF (..),
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential,
    ModelScript,
    coerceKeyRole',
    filterModelCredential,
    filterModelScript,
    liftModelCredential,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
    modelTxOut_value,
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
  )
import Test.Cardano.Ledger.Orphans ()

-- a (valid) transaction can be summarised by the UTxOs it spends.  we use this
-- as a proxy for txid in the model.
newtype ModelTxId = ModelTxId (Set ModelUTxOId)
  deriving (Eq, NFData, Ord, Show)

getModelTxId :: ModelTx era -> ModelTxId
getModelTxId mtx = ModelTxId $ case _mtxValidity mtx of
  NoPlutusSupport () -> _mtxInputs mtx
  SupportsPlutus (IsValid True) -> _mtxInputs mtx
  SupportsPlutus (IsValid False) -> case _mtxCollateral mtx of
    SupportsPlutus xs -> xs

instance NFData (ModelTx era)

class HasModelTx era a | a -> era where
  modelTxs :: Traversal' a (ModelTx era)

instance HasModelTx era (ModelTx era) where
  modelTxs = id
  {-# INLINE modelTxs #-}

instance HasModelDCert era (ModelTx era) where
  modelDCerts = modelTx_dCert . traverse
  {-# INLINE modelDCerts #-}

data ModelTx (era :: FeatureSet) = ModelTx
  { _mtxInputs :: !(Set ModelUTxOId),
    _mtxOutputs :: ![(ModelUTxOId, ModelTxOut era)],
    _mtxFee :: !(ModelValue 'ExpectAdaOnly era),
    _mtxDCert :: ![ModelDCert era],
    _mtxWdrl :: !(Map.Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era)),
    _mtxMint :: !(IfSupportsMint () (ModelValue (ValueFeature era) era) (ValueFeature era)),
    _mtxCollateral :: !(IfSupportsPlutus () (Set ModelUTxOId) (ScriptFeature era)),
    _mtxValidity :: !(IfSupportsPlutus () IsValid (ScriptFeature era)),
    _mtxRedeemers :: !(Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)),
    _mtxWitnessSigs :: !(Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)))
  }
  deriving (Show, Generic, Eq)

modelTx_inputs :: Lens' (ModelTx era) (Set ModelUTxOId)
modelTx_inputs = lens _mtxInputs (\s b -> s {_mtxInputs = b})
{-# INLINE modelTx_inputs #-}

modelTx_outputs :: Lens' (ModelTx era) [(ModelUTxOId, ModelTxOut era)]
modelTx_outputs = lens _mtxOutputs (\s b -> s {_mtxOutputs = b})
{-# INLINE modelTx_outputs #-}

-- -- focus on a specified output with the given id;
-- modelTx_outputAt :: ModelUTxOId -> Lens' (ModelTx era) (Maybe (ModelTxOut era))
-- modelTx_outputAt k = modelTx_outputs . lens (List.lookup k) (flip f)
--   where
--     f :: forall a. Maybe a -> [(ModelUTxOId, a)] -> [(ModelUTxOId, a)]
--     f = \case
--       Nothing ->
--         let g [] = []
--             g ((k', v) : rest) = if k == k' then rest else (k', v) : g rest
--          in g
--       Just v' ->
--         let h [] = [(k, v')]
--             h ((k', v) : rest) = if k == k' then (k, v') : rest else (k', v) : h rest
--          in h
-- {-# INLINE modelTx_outputAt #-}

modelTx_fee :: Lens' (ModelTx era) (ModelValue 'ExpectAdaOnly era)
modelTx_fee = lens _mtxFee (\s b -> s {_mtxFee = b})
{-# INLINE modelTx_fee #-}

modelTx_dCert :: Lens' (ModelTx era) [ModelDCert era]
modelTx_dCert = lens _mtxDCert (\s b -> s {_mtxDCert = b})
{-# INLINE modelTx_dCert #-}

modelTx_wdrl :: Lens' (ModelTx era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era))
modelTx_wdrl = lens _mtxWdrl (\s b -> s {_mtxWdrl = b})
{-# INLINE modelTx_wdrl #-}

modelTx_mint :: Lens' (ModelTx era) (IfSupportsMint () (ModelValue (ValueFeature era) era) (ValueFeature era))
modelTx_mint = lens _mtxMint (\s b -> s {_mtxMint = b})
{-# INLINE modelTx_mint #-}

modelTx_collateral :: Lens' (ModelTx era) (IfSupportsPlutus () (Set ModelUTxOId) (ScriptFeature era))
modelTx_collateral = lens _mtxCollateral (\s b -> s {_mtxCollateral = b})
{-# INLINE modelTx_collateral #-}

modelTx_validity :: Lens' (ModelTx era) (IfSupportsPlutus () IsValid (ScriptFeature era))
modelTx_validity = lens _mtxValidity (\s b -> s {_mtxValidity = b})
{-# INLINE modelTx_validity #-}

modelTx_redeemers :: Lens' (ModelTx era) (Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits))
modelTx_redeemers = lens _mtxRedeemers (\s b -> s {_mtxRedeemers = b})
{-# INLINE modelTx_redeemers #-}

modelTx_witnessSigs :: Lens' (ModelTx era) (Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)))
modelTx_witnessSigs = lens _mtxWitnessSigs (\s b -> s {_mtxWitnessSigs = b})
{-# INLINE modelTx_witnessSigs #-}

modelIsValid :: ModelTx era -> Bool
modelIsValid tx = case _mtxValidity tx of
  NoPlutusSupport () -> True
  SupportsPlutus (IsValid isValid) -> isValid

-- | helper to produce a "blank" ModelTx with most fields set to a reasonable
-- "default"
modelTx :: forall (era :: FeatureSet). KnownRequiredFeatures era => ModelTx era
modelTx =
  ModelTx
    { _mtxInputs = Set.empty,
      _mtxOutputs = [],
      _mtxFee = mempty,
      _mtxDCert = [],
      _mtxWdrl = Map.empty,
      _mtxMint = case reifyRequiredFeatures (Proxy :: Proxy era) of
        FeatureTag v _ -> case v of
          ValueFeatureTag_AdaOnly -> NoMintSupport ()
          ValueFeatureTag_AnyOutput -> SupportsMint mempty,
      _mtxCollateral = mapSupportsPlutus (const Set.empty) $ reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)),
      _mtxValidity = mapSupportsPlutus (const $ IsValid True) $ reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)),
      _mtxRedeemers = Map.empty,
      _mtxWitnessSigs = Set.empty
    }

instance RequiredFeatures ModelTx where
  filterFeatures :: forall a b. KnownRequiredFeatures a => FeatureTag b -> ModelTx a -> Maybe (ModelTx b)
  filterFeatures tag@(FeatureTag _ sf) (ModelTx ins outs fee dcert wdrl g cins valid rdmr wits) =
    ModelTx ins
      <$> (traverse . traverse) (filterFeatures tag) outs
      <*> (filterFeatures tag fee)
      <*> traverse (filterFeatures tag) dcert
      <*> fmap Map.fromList (for (Map.toList wdrl) $ \(k, v) -> (,) <$> filterModelCredential tag k <*> filterFeatures tag v)
      <*> ( case g of
              NoMintSupport () -> case tag of
                FeatureTag ValueFeatureTag_AdaOnly _ -> pure $ NoMintSupport ()
                FeatureTag ValueFeatureTag_AnyOutput _ -> pure (SupportsMint mempty)
              SupportsMint g' -> case tag of
                FeatureTag ValueFeatureTag_AdaOnly _ | g' == mempty -> pure $ NoMintSupport ()
                FeatureTag ValueFeatureTag_AdaOnly _ -> Nothing
                FeatureTag ValueFeatureTag_AnyOutput _ -> SupportsMint <$> filterFeatures tag g'
          )
      <*> ( let setNotEmpty :: Set x -> Maybe (Set x)
                setNotEmpty x
                  | Set.null x = Nothing
                  | otherwise = Just x
             in (fmap (mapSupportsPlutus fold) $ filterSupportsPlutus tag $ mapSupportsPlutus setNotEmpty cins)
          )
      <*> filterModelValidity valid
      <*> fmap Map.fromList ((traverse . _1) (filterFeatures tag) $ Map.toList rdmr)
      <*> pure wits
    where
      filterModelValidity = hasKnownScriptFeature sf $ \case
        NoPlutusSupport () -> Just $ ifSupportsPlutus sf () (IsValid True)
        SupportsPlutus v@(IsValid True) -> Just $ ifSupportsPlutus sf () v
        SupportsPlutus v@(IsValid False) -> bitraverseSupportsPlutus id id $ ifSupportsPlutus sf Nothing (Just v)

data ModelPoolParams era = ModelPoolParams
  { _mppId :: !ModelPoolId,
    _mppVrm :: !(ModelCredential 'StakePool ('TyScriptFeature 'False 'False)),
    _mppPledge :: !Coin,
    _mppCost :: !Coin,
    _mppMargin :: !UnitInterval,
    _mppRAcnt :: !(ModelCredential 'Staking (ScriptFeature era)),
    _mppOwners :: ![ModelCredential 'Staking ('TyScriptFeature 'False 'False)]
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData (ModelPoolParams era)

instance RequiredFeatures ModelPoolParams where
  filterFeatures tag (ModelPoolParams poolId poolVrf pledge cost margin rAcnt owners) =
    ModelPoolParams poolId poolVrf pledge cost margin
      <$> filterModelCredential tag rAcnt
      <*> pure owners

modelPoolParams_id :: Lens' (ModelPoolParams era) ModelPoolId
modelPoolParams_id a2fb s = (\b -> s {_mppId = b}) <$> a2fb (_mppId s)
{-# INLINE modelPoolParams_id #-}

modelPoolParams_vrm :: Lens' (ModelPoolParams era) (ModelCredential 'StakePool ('TyScriptFeature 'False 'False))
modelPoolParams_vrm a2fb s = (\b -> s {_mppVrm = b}) <$> a2fb (_mppVrm s)
{-# INLINE modelPoolParams_vrm #-}

modelPoolParams_pledge :: Lens' (ModelPoolParams era) Coin
modelPoolParams_pledge a2fb s = (\b -> s {_mppPledge = b}) <$> a2fb (_mppPledge s)
{-# INLINE modelPoolParams_pledge #-}

modelPoolParams_cost :: Lens' (ModelPoolParams era) Coin
modelPoolParams_cost a2fb s = (\b -> s {_mppCost = b}) <$> a2fb (_mppCost s)
{-# INLINE modelPoolParams_cost #-}

modelPoolParams_margin :: Lens' (ModelPoolParams era) UnitInterval
modelPoolParams_margin a2fb s = (\b -> s {_mppMargin = b}) <$> a2fb (_mppMargin s)
{-# INLINE modelPoolParams_margin #-}

modelPoolParams_rAcnt :: Lens' (ModelPoolParams era) (ModelCredential 'Staking (ScriptFeature era))
modelPoolParams_rAcnt a2fb s = (\b -> s {_mppRAcnt = b}) <$> a2fb (_mppRAcnt s)
{-# INLINE modelPoolParams_rAcnt #-}

modelPoolParams_owners :: Lens' (ModelPoolParams era) [ModelCredential 'Staking ('TyScriptFeature 'False 'False)]
modelPoolParams_owners a2fb s = (\b -> s {_mppOwners = b}) <$> a2fb (_mppOwners s)
{-# INLINE modelPoolParams_owners #-}

data ModelScriptPurpose era where
  ModelScriptPurpose_Minting :: ModelScript (ScriptFeature era) -> ModelScriptPurpose era
  ModelScriptPurpose_Spending :: ModelUTxOId -> ModelScriptPurpose era
  ModelScriptPurpose_Rewarding :: ModelCredential 'Staking (ScriptFeature era) -> ModelScriptPurpose era
  ModelScriptPurpose_Certifying :: ModelDCert era -> ModelScriptPurpose era

deriving instance Eq (ModelScriptPurpose era)

deriving instance Ord (ModelScriptPurpose era)

deriving instance Show (ModelScriptPurpose era)

instance NFData (ModelScriptPurpose era) where
  rnf = \case
    ModelScriptPurpose_Minting x -> rnf x
    ModelScriptPurpose_Spending x -> rnf x
    ModelScriptPurpose_Rewarding x -> rnf x
    ModelScriptPurpose_Certifying x -> rnf x

instance RequiredFeatures ModelScriptPurpose where
  filterFeatures tag@(FeatureTag _ sf) = \case
    ModelScriptPurpose_Minting policy ->
      ModelScriptPurpose_Minting
        <$> hasKnownScriptFeature sf (filterModelScript policy)
    ModelScriptPurpose_Spending uid -> ModelScriptPurpose_Spending <$> pure uid
    ModelScriptPurpose_Rewarding ra -> ModelScriptPurpose_Rewarding <$> filterModelCredential tag ra
    ModelScriptPurpose_Certifying mdc -> ModelScriptPurpose_Certifying <$> filterFeatures tag mdc

-- ignores genesis delegation details.
-- deviates slightly from main implementation to better agree with the spec STS
-- rules that handle them, case for rule.
data ModelDCert era
  = ModelCertDeleg (ModelDelegCert era)
  | ModelCertPool (ModelPoolCert era)
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelDCert era)

_ModelRegisterStake :: Prism' (ModelDCert era) (ModelCredential 'Staking (ScriptFeature era))
_ModelRegisterStake = prism (ModelCertDeleg . ModelRegKey) $ \case
  ModelCertDeleg (ModelRegKey x) -> Right x
  x -> Left x
{-# INLINE _ModelRegisterStake #-}

_ModelDeRegisterStake :: Prism' (ModelDCert era) (ModelCredential 'Staking (ScriptFeature era))
_ModelDeRegisterStake = prism (ModelCertDeleg . ModelRegKey) $ \case
  ModelCertDeleg (ModelRegKey x) -> Right x
  x -> Left x
{-# INLINE _ModelDeRegisterStake #-}

_ModelDelegate :: Prism' (ModelDCert era) (ModelDelegation era)
_ModelDelegate = prism (ModelCertDeleg . ModelDelegate) $ \case
  ModelCertDeleg (ModelDelegate x) -> Right x
  x -> Left x
{-# INLINE _ModelDelegate #-}

_ModelRegisterPool :: Prism' (ModelDCert era) (ModelPoolParams era)
_ModelRegisterPool = prism (ModelCertPool . ModelRegPool) $ \case
  ModelCertPool (ModelRegPool x) -> Right x
  x -> Left x
{-# INLINE _ModelRegisterPool #-}

_ModelRetirePool :: Prism' (ModelDCert era) (ModelPoolId, EpochNo)
_ModelRetirePool = prism (ModelCertPool . uncurry ModelRetirePool) $ \case
  ModelCertPool (ModelRetirePool x y) -> Right (x, y)
  x -> Left x
{-# INLINE _ModelRetirePool #-}

_ModelMIR :: Prism' (ModelDCert era) (ModelMIRCert era)
_ModelMIR = prism (ModelCertDeleg . ModelDCertMir) $ \case
  ModelCertDeleg (ModelDCertMir c) -> Right c
  x -> Left x
{-# INLINE _ModelMIR #-}

class HasModelDCert era a | a -> era where
  modelDCerts :: Traversal' a (ModelDCert era)

instance HasModelDCert era (ModelDCert era) where
  modelDCerts = id
  {-# INLINE modelDCerts #-}

instance RequiredFeatures ModelDCert where
  filterFeatures tag = \case
    ModelCertDeleg cert ->
      ModelCertDeleg <$> case cert of
        ModelRegKey a -> ModelRegKey <$> filterModelCredential tag a
        ModelDeRegKey a -> ModelDeRegKey <$> filterModelCredential tag a
        ModelDelegate a -> ModelDelegate <$> filterFeatures tag a
        ModelDCertGenesis a -> pure $ ModelDCertGenesis a
        ModelDCertMir a -> ModelDCertMir <$> filterFeatures tag a
    ModelCertPool cert ->
      ModelCertPool <$> case cert of
        ModelRegPool a -> ModelRegPool <$> filterFeatures tag a
        ModelRetirePool a b -> pure $ ModelRetirePool a b

data ModelPoolCert era
  = ModelRegPool (ModelPoolParams era)
  | ModelRetirePool ModelPoolId EpochNo
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelPoolCert era)

data ModelDelegCert era
  = ModelRegKey (ModelCredential 'Staking (ScriptFeature era))
  | ModelDeRegKey (ModelCredential 'Staking (ScriptFeature era))
  | ModelDelegate (ModelDelegation era)
  | ModelDCertGenesis ModelGenesisDelegCert
  | ModelDCertMir (ModelMIRCert era)
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelDelegCert era)

data ModelDelegation era = ModelDelegation
  { _mdDelegator :: !(ModelCredential 'Staking (ScriptFeature era)),
    _mdDelegatee :: !ModelPoolId
  }
  deriving (Generic, Eq, Ord)
  deriving (Show) via Quiet (ModelDelegation era)

modelDelegation_delegator :: Lens' (ModelDelegation era) (ModelCredential 'Staking (ScriptFeature era))
modelDelegation_delegator a2fb s = (\b -> s {_mdDelegator = b}) <$> a2fb (_mdDelegator s)
{-# INLINE modelDelegation_delegator #-}

modelDelegation_delegatee :: Lens' (ModelDelegation era) ModelPoolId
modelDelegation_delegatee a2fb s = (\b -> s {_mdDelegatee = b}) <$> a2fb (_mdDelegatee s)
{-# INLINE modelDelegation_delegatee #-}

instance NFData (ModelDelegation era)

instance RequiredFeatures ModelDelegation where
  filterFeatures tag (ModelDelegation a b) =
    ModelDelegation
      <$> filterModelCredential tag a
      <*> pure b

data ModelGenesisDelegCert
  = ModelGenesisDelegCert
      (ModelCredential 'Genesis ShelleyScriptFeatures)
      (ModelCredential 'GenesisDelegate ShelleyScriptFeatures)
  deriving (Show, Generic, Eq, Ord)

instance NFData ModelGenesisDelegCert

data ModelMIRCert era = ModelMIRCert
  { _modelMIRCert_pot :: !MIRPot,
    _modelMIRCert_rewards :: !(ModelMIRTarget era)
  }
  deriving (Generic, Eq, Ord)
  deriving (Show) via Quiet (ModelMIRCert era)

instance NFData (ModelMIRCert era)

instance RequiredFeatures ModelMIRCert where
  filterFeatures tag (ModelMIRCert pot rewards) = ModelMIRCert pot <$> filterFeatures tag rewards

data ModelMIRTarget era
  = ModelStakeAddressesMIR (Map.Map (ModelCredential 'Staking (ScriptFeature era)) DeltaCoin)
  | ModelSendToOppositePotMIR Coin
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelMIRTarget era)

instance RequiredFeatures ModelMIRTarget where
  filterFeatures tag = \case
    ModelStakeAddressesMIR rewards ->
      ModelStakeAddressesMIR . Map.fromAscList
        <$> (traverse . _1) (filterModelCredential tag) (Map.toAscList rewards)
    ModelSendToOppositePotMIR x -> pure $ ModelSendToOppositePotMIR x

-- SEE: Fig4[GL-D2]
-- DEPRECATES fig9[SL-D5]
getModelMinfee ::
  ModelPParams era ->
  ModelTx era ->
  Coin
getModelMinfee pp tx =
  naturalToCoin (GHC.getField @"_minfeeA" pp) `pow` getModelTxSize tx
    <> naturalToCoin (GHC.getField @"_minfeeA" pp)
    <> txscriptfee (GHC.getField @"_prices" pp) (modelTotExunits tx)
  where
    naturalToCoin = Coin . toInteger

-- | (fig 17)[SL-D5]
modelTotalDeposits ::
  Foldable t =>
  ModelPParams era ->
  Map.Map ModelPoolId (ModelPoolParams era) ->
  t (ModelDCert era) ->
  Coin
modelTotalDeposits
  ( ModelPParams
      { _modelPParams_keyDeposit = Identity keyDeposit,
        _modelPParams_poolDeposit = Identity poolDeposit
      }
    )
  poolParams
  certs =
    keyDeposit `pow` (lengthOf (folded . _ModelRegisterStake) certs)
      <> poolDeposit `pow` Set.size newPools
    where
      newPools =
        Set.fromList
          [ c
            | c <- toListOf (folded . _ModelRegisterPool) certs,
              not (Map.member (_mppId c) poolParams)
          ]

-- Sec 9.1 [SL-D5]
modelCWitness :: ModelDCert era -> Maybe (ModelCredential 'Witness (ScriptFeature era))
modelCWitness = \case
  ModelCertDeleg c -> case c of
    ModelRegKey _ -> Nothing
    ModelDeRegKey addr -> Just $ coerceKeyRole' addr
    ModelDelegate (ModelDelegation cred _) -> Just $ coerceKeyRole' cred
    ModelDCertGenesis (ModelGenesisDelegCert gkh _) -> Just $ liftModelCredential $ coerceKeyRole' gkh
    ModelDCertMir (ModelMIRCert {}) -> Nothing
  ModelCertPool c -> case c of
    ModelRegPool mpp -> Just $ liftModelCredential $ coerceKeyRole' $ unModelPoolId $ _mppId mpp
    ModelRetirePool mpid _ -> Just $ liftModelCredential $ coerceKeyRole' $ unModelPoolId mpid

getModelConsumed ::
  ModelPParams era ->
  ModelUTxOMap era ->
  ModelTx era ->
  ModelValue (ValueFeature era) era
getModelConsumed pp (ModelUTxOMap {_modelUTxOMap_utxos = utxo}) tx =
  foldMapOf modelTx_inputs (foldMap (_mtxo_value . snd) . Map.restrictKeys utxo) tx
    <> foldOf (modelTx_mint . traverseSupportsMint) tx
    <> foldMapOf (modelTx_wdrl . traverse) liftModelValue tx
    <> Val.inject (modelKeyRefunds pp $ _mtxDCert tx)

getModelProduced ::
  ModelPParams era ->
  Map.Map ModelPoolId (ModelPoolParams era) ->
  ModelTx era ->
  ModelValue (ValueFeature era) era
getModelProduced pp poolParams tx =
  foldOf (modelTx_outputs . traverse . _2 . modelTxOut_value) tx
    <> foldMapOf modelTx_fee liftModelValue tx
    <> Val.inject (modelTotalDeposits pp poolParams $ _mtxDCert tx)

-- TODO: this is not at all correct, just a placeholder
getModelTxSize ::
  ModelTx era ->
  Int
getModelTxSize _ = 2000

-- SEE Fig 4 [GL-D2]
modelTotExunits :: ModelTx era -> ExUnits
modelTotExunits = foldOf (modelTx_redeemers . traverse . _2)

-- | (fig 17)[SL-D5]
modelKeyRefunds :: ModelPParams era -> [ModelDCert era] -> Coin
modelKeyRefunds (ModelPParams {_modelPParams_keyDeposit = Identity keyDeposit}) certs =
  keyDeposit `pow` (lengthOf (traverse . _ModelDeRegisterStake) certs)
