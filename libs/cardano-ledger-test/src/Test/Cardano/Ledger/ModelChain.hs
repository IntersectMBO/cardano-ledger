{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.ModelChain where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..), txscriptfee)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName, PolicyID (..))
import qualified Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.Rules.EraMapping ()
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Slot hiding (at)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS.CPS as RWS
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable (fold, for_, toList, traverse_)
import Data.Functor.Compose
import Data.Functor.PiecewiseConstant
import Data.Group
import Data.Group.GrpMap
import Data.HKD
import Data.Kind (Constraint, Type)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import Data.Traversable
import Data.Typeable
import Data.Void
import qualified GHC.Exts as GHC
import GHC.Generics (Generic, (:*:) (..), (:.:) (..))
import GHC.Natural
import qualified GHC.Records as GHC
import qualified PlutusTx
import Quiet (Quiet (..))
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.FeatureSet
import Test.Cardano.Ledger.ModelChain.Script
import Test.Cardano.Ledger.ModelChain.Value
import Test.Cardano.Ledger.Orphans ()

class Val.Val val => ValueFromList val crypto | val -> crypto where
  valueFromList :: Integer -> [(PolicyID crypto, AssetName, Integer)] -> val

  insert :: (Integer -> Integer -> Integer) -> PolicyID crypto -> AssetName -> Integer -> val -> val

  gettriples :: val -> (Integer, [(PolicyID crypto, AssetName, Integer)])

instance C.Crypto crypto => ValueFromList (Cardano.Ledger.Mary.Value.Value crypto) crypto where
  valueFromList = Cardano.Ledger.Mary.Value.valueFromList

  insert = Cardano.Ledger.Mary.Value.insert

  gettriples (Cardano.Ledger.Mary.Value.Value c m1) = (c, triples)
    where
      triples =
        [ (policyId, aname, amount)
          | (policyId, m2) <- Map.toList m1,
            (aname, amount) <- Map.toList m2
        ]

-- TODO: this is a bit overconstrained, we probably want a more constraints
-- based approach using ValueFromList
type family ElaborateValueType (valF :: TyValueExpected) crypto :: Type where
  ElaborateValueType 'ExpectAdaOnly _ = Coin
  ElaborateValueType 'ExpectAnyOutput crypto = Cardano.Ledger.Mary.Value.Value crypto

instance RequiredFeatures ModelTxOut where
  filterFeatures tag@(FeatureTag v _) (ModelTxOut addr qty dat) =
    hasKnownValueFeature v $
      ModelTxOut
        <$> filterModelAddress tag addr
        <*> (filterFeatures tag =<< filterModelValue qty)
        <*> (filterSupportsPlutus tag dat)

filterModelValueVars ::
  forall a b c d.
  (KnownRequiredFeatures c, KnownValueFeature d) =>
  ModelValueVars a b ->
  Maybe (ModelValueVars c d)
filterModelValueVars (ModelValue_MA ys) = do
  Refl <- reifyExpectAnyOutput (reifyValueFeature (Proxy @d))
  Refl <- reifyExpectAnyOutput (reifyValueFeature (Proxy @(ValueFeature c)))

  ModelValue_MA <$> _1 filterModelScript ys

liftModelValueVars :: ModelValueVars era 'ExpectAdaOnly -> ModelValueVars era k
liftModelValueVars = \case

filterModelScript ::
  forall b a.
  KnownScriptFeature b =>
  ModelScript a ->
  Maybe (ModelScript b)
filterModelScript = \case
  ModelScript_Timelock t -> case reifyScriptFeature (Proxy @b) of
    ScriptFeatureTag_None -> Nothing
    ScriptFeatureTag_Simple -> Just $ ModelScript_Timelock t
    ScriptFeatureTag_PlutusV1 -> Just $ ModelScript_Timelock t
  ModelScript_PlutusV1 t -> case reifyScriptFeature (Proxy @b) of
    ScriptFeatureTag_None -> Nothing
    ScriptFeatureTag_Simple -> Nothing
    ScriptFeatureTag_PlutusV1 -> Just $ ModelScript_PlutusV1 t

-- change the "expected return type" of a ModelValue
filterModelValue ::
  forall a b c.
  (KnownValueFeature b, KnownRequiredFeatures c) =>
  ModelValue a c ->
  Maybe (ModelValue b c)
filterModelValue = \case
  ModelValue x -> ModelValue <$> traverse filterModelValueVars x

instance KnownValueFeature v => RequiredFeatures (ModelValue v) where
  filterFeatures tag (ModelValue val) = ModelValue <$> traverse (hasKnownRequiredFeatures tag filterModelValueVars) val

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
                FeatureTag ValueFeatureTag_AnyOutput _ -> pure (SupportsMint . ModelValue . ModelValue_Inject $ Coin 0)
              SupportsMint g' -> case tag of
                FeatureTag ValueFeatureTag_AdaOnly _ | g' == ModelValue (ModelValue_Inject $ Val.zero) -> pure $ NoMintSupport ()
                FeatureTag ValueFeatureTag_AdaOnly _ -> Nothing
                FeatureTag ValueFeatureTag_AnyOutput _ -> SupportsMint <$> filterFeatures tag g'
          )
      <*> ( let setNotEmpty :: Set x -> Maybe (Set x)
                setNotEmpty x
                  | Set.null x = Nothing
                  | otherwise = Just x
             in (fmap (mapSupportsPlutus fold) $ filterSupportsPlutus tag $ mapSupportsPlutus setNotEmpty cins)
          )
      <*> filterModelValidity sf valid
      <*> fmap Map.fromList ((traverse . _1) (filterFeatures tag) $ Map.toList rdmr)
      <*> pure wits

filterModelAddress ::
  FeatureTag b ->
  ModelAddress a ->
  Maybe (ModelAddress (ScriptFeature b))
filterModelAddress tag (ModelAddress pmt stk) =
  ModelAddress
    <$> filterModelCredential tag pmt
    <*> filterModelCredential tag stk

filterModelCredential ::
  FeatureTag b ->
  ModelCredential r a ->
  Maybe (ModelCredential r' (ScriptFeature b))
filterModelCredential (FeatureTag _ s) = \case
  ModelKeyHashObj a -> Just (ModelKeyHashObj a)
  ModelScriptHashObj a -> case s of
    ScriptFeatureTag_None -> Nothing
    ScriptFeatureTag_Simple -> Nothing
    ScriptFeatureTag_PlutusV1 -> Just (ModelScriptHashObj a)

filterModelValidity ::
  forall a b.
  ScriptFeatureTag b ->
  IfSupportsPlutus () IsValid a ->
  Maybe (IfSupportsPlutus () IsValid b)
filterModelValidity sf = hasKnownScriptFeature sf $ \case
  NoPlutusSupport () -> Just $ ifSupportsPlutus sf () (IsValid True)
  SupportsPlutus v@(IsValid True) -> Just $ ifSupportsPlutus sf () v
  SupportsPlutus v@(IsValid False) -> bitraverseSupportsPlutus id id $ ifSupportsPlutus sf Nothing (Just v)

instance RequiredFeatures ModelBlock where
  filterFeatures tag (ModelBlock slotNo txns) =
    ModelBlock slotNo
      <$> traverse (filterFeatures tag) txns

instance RequiredFeatures ModelEpoch where
  filterFeatures tag (ModelEpoch blocks x) =
    ModelEpoch
      <$> traverse (filterFeatures tag) blocks
      <*> pure x

type ModelMA era = (ModelScript era, AssetName)

data ModelValueVars era (k :: TyValueExpected) where
  ModelValue_MA ::
    ('ExpectAnyOutput ~ ValueFeature era) =>
    ModelMA (ScriptFeature era) ->
    ModelValueVars era 'ExpectAnyOutput

instance NFData (ModelValueVars era k) where
  rnf = \case
    ModelValue_MA a -> rnf a

deriving instance Show (ModelValueVars era valF)

deriving instance Eq (ModelValueVars era valF)

deriving instance Ord (ModelValueVars era valF)

type ModelValue' k era = ModelValueSimple (ModelValueVars era k)

newtype ModelValue k era = ModelValue {unModelValue :: ModelValueF (ModelValueVars era k)}
  deriving (Eq, Ord, Generic, NFData)
  deriving (Show) via Quiet (ModelValue k era)

instance Semigroup (ModelValue k era) where
  ModelValue x <> ModelValue y = ModelValue (x `ModelValue_Add` y)

instance Monoid (ModelValue k era) where
  mempty = modelValueInject $ Coin 0

data ModelTxOut era = ModelTxOut
  { _mtxo_address :: !(ModelAddress (ScriptFeature era)),
    _mtxo_value :: !(ModelValue (ValueFeature era) era),
    _mtxo_data :: !(IfSupportsPlutus () (Maybe PlutusTx.Data) (ScriptFeature era))
  }
  deriving (Eq, Ord, Generic)
  deriving (Show) via Quiet (ModelTxOut era)

instance NFData (ModelTxOut era)

liftModelValue :: ModelValue 'ExpectAdaOnly era -> ModelValue k era
liftModelValue = ModelValue . fmap liftModelValueVars . unModelValue

modelValueInject :: Coin -> ModelValue k era
modelValueInject = ModelValue . ModelValue_Inject

mkModelValue :: ModelValue' k era -> ModelValue k era
mkModelValue = ModelValue . mkModelValueF

-- | Convenience function to create a spendable ModelTxOut
modelTxOut :: forall era. KnownScriptFeature (ScriptFeature era) => ModelAddress (ScriptFeature era) -> ModelValue (ValueFeature era) era -> ModelTxOut era
modelTxOut a v = ModelTxOut a v dh
  where
    dh = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () $
      case _modelAddress_pmt a of
        ModelKeyHashObj _ -> Nothing
        ModelScriptHashObj _ -> Just $ PlutusTx.I 42

modelTxOut_address :: forall era. Lens' (ModelTxOut era) (ModelAddress (ScriptFeature era))
modelTxOut_address = lens _mtxo_address (\s b -> s {_mtxo_address = b})
{-# INLINE modelTxOut_address #-}

modelTxOut_value :: Lens' (ModelTxOut era) (ModelValue (ValueFeature era) era)
modelTxOut_value = lens _mtxo_value (\s b -> s {_mtxo_value = b})
{-# INLINE modelTxOut_value #-}

modelTxOut_data :: Lens' (ModelTxOut era) (IfSupportsPlutus () (Maybe PlutusTx.Data) (ScriptFeature era))
modelTxOut_data = lens _mtxo_data (\s b -> s {_mtxo_data = b})
{-# INLINE modelTxOut_data #-}

newtype ModelUTxOId = ModelUTxOId {unModelUTxOId :: Integer}
  deriving (Eq, Ord, Num, Enum, Generic, NFData)

deriving newtype instance Show ModelUTxOId

data ModelScriptPurpose era where
  ModelScriptPurpose_Minting :: ModelScript (ScriptFeature era) -> ModelScriptPurpose era
  ModelScriptPurpose_Spending :: ModelUTxOId -> ModelScriptPurpose era
  ModelScriptPurpose_Rewarding :: ModelCredential 'Staking (ScriptFeature era) -> ModelScriptPurpose era
  ModelScriptPurpose_Certifying :: ModelDCert era -> ModelScriptPurpose era

deriving instance Eq (ModelScriptPurpose era)

deriving instance Ord (ModelScriptPurpose era)

deriving instance Show (ModelScriptPurpose era)

instance NFData (ModelScriptPurpose era) where
  rnf = rwhnf

instance RequiredFeatures ModelScriptPurpose where
  filterFeatures tag@(FeatureTag _ sf) = \case
    ModelScriptPurpose_Minting policy ->
      ModelScriptPurpose_Minting
        <$> hasKnownScriptFeature sf (filterModelScript policy)
    ModelScriptPurpose_Spending uid -> ModelScriptPurpose_Spending <$> pure uid
    ModelScriptPurpose_Rewarding ra -> ModelScriptPurpose_Rewarding <$> filterModelCredential tag ra
    ModelScriptPurpose_Certifying mdc -> ModelScriptPurpose_Certifying <$> filterFeatures tag mdc

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

modelTx_inputs :: Lens' (ModelTx era) (Set ModelUTxOId)
modelTx_inputs = lens _mtxInputs (\s b -> s {_mtxInputs = b})
{-# INLINE modelTx_inputs #-}

modelTx_outputs :: Lens' (ModelTx era) [(ModelUTxOId, ModelTxOut era)]
modelTx_outputs = lens _mtxOutputs (\s b -> s {_mtxOutputs = b})
{-# INLINE modelTx_outputs #-}

-- focus on a specified output with the given id;
modelTx_outputAt :: ModelUTxOId -> Lens' (ModelTx era) (Maybe (ModelTxOut era))
modelTx_outputAt k = modelTx_outputs . lens (List.lookup k) (flip f)
  where
    f :: forall a. Maybe a -> [(ModelUTxOId, a)] -> [(ModelUTxOId, a)]
    f = \case
      Nothing ->
        let g [] = []
            g ((k', v) : rest) = if k == k' then rest else (k', v) : g rest
         in g
      Just v' ->
        let h [] = [(k, v')]
            h ((k', v) : rest) = if k == k' then (k, v') : rest else (k', v) : h rest
         in h
{-# INLINE modelTx_outputAt #-}

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
      _mtxFee = ModelValue $ ModelValue_Inject $ Coin 0,
      _mtxDCert = [],
      _mtxWdrl = Map.empty,
      _mtxMint = case reifyRequiredFeatures (Proxy :: Proxy era) of
        FeatureTag v _ -> case v of
          ValueFeatureTag_AdaOnly -> NoMintSupport ()
          ValueFeatureTag_AnyOutput -> SupportsMint $ ModelValue $ ModelValue_Inject $ Coin 0,
      _mtxCollateral = mapSupportsPlutus (const Set.empty) $ reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)),
      _mtxValidity = mapSupportsPlutus (const $ IsValid True) $ reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)),
      _mtxRedeemers = Map.empty,
      _mtxWitnessSigs = Set.empty
    }

witnessModelTx ::
  forall (era :: FeatureSet). ModelTx era -> ModelLedger era -> ModelTx era
witnessModelTx mtx ml =
  let (witnessSigs, redeemers) = witnessModelTxImpl mtx ml
   in mtx
        { _mtxWitnessSigs = witnessSigs,
          _mtxRedeemers = redeemers
        }

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

-- TODO: there's some extra degrees of freedom hidden in this function, they
-- should be exposed
-- - redeemer value/exunits
-- - how timelocks are signed (which n of m)
witnessModelTxImpl ::
  forall (era :: FeatureSet).
  ModelTx era ->
  ModelLedger era ->
  ( Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)),
    Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)
  )
witnessModelTxImpl mtx ml =
  let mkRdmr :: ModelScript (ScriptFeature era) -> ModelScriptPurpose era -> (PlutusTx.Data, ExUnits)
      mkRdmr _ _ = (PlutusTx.I 10, ExUnits 1 1)

      lookupOutput :: ModelUTxOId -> Maybe (ModelUTxOId, ModelCredential 'Payment (ScriptFeature era))
      lookupOutput ui = (,) ui <$> preview (to getModelLedger_utxos . at ui . _Just . modelTxOut_address @era . modelAddress_pmt) ml

      matchDCert :: ModelDCert era -> Maybe (ModelScriptPurpose era, ModelCredential 'Witness (ScriptFeature era))
      matchDCert cert = (,) (ModelScriptPurpose_Certifying cert) <$> modelCWitness cert

      matchPoolCert :: ModelDCert era -> [ModelCredential 'Witness ('TyScriptFeature 'False 'False)]
      matchPoolCert = \case
        ModelCertPool (ModelRegPool mpp) -> [coerceKeyRole' $ unModelPoolId $ _mppId mpp] <> fmap coerceKeyRole' (_mppOwners mpp)
        _ -> []

      witnessMint :: ModelValueVars era (ValueFeature era) -> (Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)), Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits))
      witnessMint = \case
        ModelValue_MA (modelPolicy, _) -> case modelPolicy of
          ModelScript_Timelock tl -> foldMap (\wit -> (Set.singleton wit, Map.empty)) (modelScriptNeededSigs tl)
          ModelScript_PlutusV1 _s1 ->
            let sp = ModelScriptPurpose_Minting modelPolicy
             in (Set.empty, Map.singleton sp (mkRdmr modelPolicy sp))

      witnessCredential ::
        ModelScriptPurpose era ->
        ModelCredential k (ScriptFeature era) ->
        ( Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)),
          Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)
        )
      witnessCredential msp = \case
        ModelKeyHashObj k -> (Set.singleton (ModelKeyHashObj k), Map.empty)
        ModelScriptHashObj s -> (Set.empty, Map.singleton msp (mkRdmr (ModelScript_PlutusV1 s) msp))

      witnessSigs :: Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False))
      redeemers :: Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)
      (witnessSigs, redeemers) =
        foldMap (uncurry witnessCredential . first ModelScriptPurpose_Spending) (mapMaybe lookupOutput $ Set.toList $ _mtxInputs mtx)
          <> foldMap (uncurry witnessCredential) (mapMaybe matchDCert $ _mtxDCert mtx)
          <> foldMap (\c -> (Set.singleton c, Map.empty)) (matchPoolCert =<< _mtxDCert mtx)
          <> foldMap (uncurry witnessCredential . (ModelScriptPurpose_Rewarding &&& id)) (Map.keys $ _mtxWdrl mtx)
          <> fromSupportsMint mempty (foldMap witnessMint . unModelValue) (_mtxMint mtx)
          <> fromSupportsPlutus
            mempty
            (foldMap (uncurry witnessCredential . first ModelScriptPurpose_Spending) . mapMaybe lookupOutput . Set.toList)
            (_mtxCollateral mtx)
   in (witnessSigs, redeemers)

getModelConsumed ::
  ModelPParams era ->
  ModelUTxOMap era ->
  ModelTx era ->
  ModelValue (ValueFeature era) era
getModelConsumed pp (ModelUTxOMap {_modelUTxOMap_utxos = utxo}) tx =
  foldMapOf modelTx_inputs (foldMap (_mtxo_value . snd) . Map.restrictKeys utxo) tx
    <> foldOf (modelTx_mint . traverseSupportsMint) tx
    <> foldMapOf (modelTx_wdrl . traverse) liftModelValue tx
    <> modelValueInject (modelKeyRefunds pp $ _mtxDCert tx)

getModelProduced ::
  ModelPParams era ->
  Map.Map ModelPoolId (ModelPoolParams era) ->
  ModelTx era ->
  ModelValue (ValueFeature era) era
getModelProduced pp poolParams tx =
  foldOf (modelTx_outputs . traverse . _2 . modelTxOut_value) tx
    <> foldMapOf modelTx_fee liftModelValue tx
    <> modelValueInject (modelTotalDeposits pp poolParams $ _mtxDCert tx)

-- TODO: this is not at all correct, just a placeholder
getModelTxSize ::
  ModelTx era ->
  Int
getModelTxSize _ = 300

-- SEE Fig 4 [GL-D2]
modelTotExunits :: ModelTx era -> ExUnits
modelTotExunits = foldOf (modelTx_redeemers . traverse . _2)

-- SEE Fig 17 [GL-D2]
-- TODO: this is not at all correct, just a placeholder
modelUTxOEntrySize :: ModelTxOut era -> Integer
modelUTxOEntrySize (ModelTxOut _a v d) =
  utxoEntrySizeWithoutVal + Val.size v' + fromSupportsPlutus (\() -> 0) modelDataHashSize d
  where
    v' = either (error . (<>) "modelDataHashSize:" . show) id $ evalModelValueSimple $ unModelValue v
    utxoEntrySizeWithoutVal = 29 -- according to spec, anways.
    modelDataHashSize = maybe 0 (const 10)

modelMinUTxOCoins :: ModelPParams era -> ModelTxOut era -> Coin
modelMinUTxOCoins pp txout = case runIdentity $ _modelPParams_coinsPerUTxOWord pp of
  NoMintSupport x -> x
  SupportsMint coinsPerUTxOWord -> coinsPerUTxOWord `pow` modelUTxOEntrySize txout

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

-- getModel

data ModelBlock era = ModelBlock
  { _modelBlock_slot :: SlotNo,
    _modelBlock_txSeq :: [ModelTx era]
  }
  deriving (Show, Generic, Eq)

instance NFData (ModelBlock era)

instance HasModelDCert era (ModelBlock era) where
  modelDCerts = modelBlock_txSeq . traverse . modelDCerts
  {-# INLINE modelDCerts #-}

instance HasModelTx era (ModelBlock era) where
  modelTxs = modelBlock_txSeq . traverse
  {-# INLINE modelTxs #-}

modelBlock_slot :: Lens' (ModelBlock era) SlotNo
modelBlock_slot = lens _modelBlock_slot (\s b -> s {_modelBlock_slot = b})
{-# INLINE modelBlock_slot #-}

modelBlock_txSeq :: Lens' (ModelBlock era) [ModelTx era]
modelBlock_txSeq = lens _modelBlock_txSeq (\s b -> s {_modelBlock_txSeq = b})
{-# INLINE modelBlock_txSeq #-}

newtype ModelPoolId = ModelPoolId {unModelPoolId :: ModelCredential 'StakePool ('TyScriptFeature 'False 'False)}
  deriving (Eq, Ord, GHC.IsString, NFData)

-- always works because the wrapped credential is always KeyHashObj, and
-- consequently always like a string.
deriving newtype instance Show ModelPoolId

newtype ModelBlocksMade = ModelBlocksMade {unModelBlocksMade :: Map.Map ModelPoolId Natural}
  deriving (Generic, NFData, Eq)
  deriving (Show) via Quiet ModelBlocksMade
  deriving (Semigroup) via GrpMap ModelPoolId (Sum Natural)
  deriving (Monoid) via GrpMap ModelPoolId (Sum Natural)

_ModelBlocksMade :: Iso' ModelBlocksMade (Map.Map ModelPoolId Natural)
_ModelBlocksMade = iso unModelBlocksMade ModelBlocksMade

repartition :: forall a b t. (Traversable t, Integral a, RealFrac b) => a -> t b -> t a
repartition total weights = State.evalState (traverse step weights) (0 :: b)
  where
    step weight = do
      err <- State.get
      let fracValue :: b
          fracValue = err + fromIntegral total * weight
          value :: a
          value = round fracValue
      State.put (fracValue - fromIntegral value)
      pure value

-- TODO: explicit Epoch.
data ModelEpoch era = ModelEpoch
  { _modelEpoch_blocks :: [ModelBlock era],
    _modelEpoch_blocksMade :: ModelBlocksMade
  }
  deriving (Show, Generic, Eq)

instance NFData (ModelEpoch era)

instance HasModelDCert era (ModelEpoch era) where
  modelDCerts = modelEpoch_blocks . traverse . modelDCerts
  {-# INLINE modelDCerts #-}

instance HasModelTx era (ModelEpoch era) where
  modelTxs = modelEpoch_blocks . traverse . modelTxs
  {-# INLINE modelTxs #-}

modelEpoch_blocks :: Lens' (ModelEpoch era) [ModelBlock era]
modelEpoch_blocks = lens _modelEpoch_blocks (\s b -> s {_modelEpoch_blocks = b})
{-# INLINE modelEpoch_blocks #-}

modelEpoch_blocksMade :: Lens' (ModelEpoch era) ModelBlocksMade
modelEpoch_blocksMade = lens _modelEpoch_blocksMade (\s b -> s {_modelEpoch_blocksMade = b})
{-# INLINE modelEpoch_blocksMade #-}

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

data ModelGenesisDelegCert
  = ModelGenesisDelegCert
      (ModelCredential 'Genesis ShelleyScriptFeatures)
      (ModelCredential 'GenesisDelegate ShelleyScriptFeatures)
  deriving (Show, Generic, Eq, Ord)

instance NFData ModelGenesisDelegCert

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

data ModelMIRCert era = ModelMIRCert
  { _modelMIRCert_pot :: !MIRPot,
    _modelMIRCert_rewards :: !(ModelMIRTarget era)
  }
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelMIRCert era)

instance RequiredFeatures ModelMIRCert where
  filterFeatures tag (ModelMIRCert pot rewards) = ModelMIRCert pot <$> filterFeatures tag rewards

data ModelDelegCert era
  = ModelRegKey (ModelCredential 'Staking (ScriptFeature era))
  | ModelDeRegKey (ModelCredential 'Staking (ScriptFeature era))
  | ModelDelegate (ModelDelegation era)
  | ModelDCertGenesis ModelGenesisDelegCert
  | ModelDCertMir (ModelMIRCert era)
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelDelegCert era)

data ModelPoolCert era
  = ModelRegPool (ModelPoolParams era)
  | ModelRetirePool ModelPoolId EpochNo
  deriving (Show, Generic, Eq, Ord)

instance NFData (ModelPoolCert era)

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

data ModelPredicateFailure era
  = ModelValueNotConservedUTxO
      !(ModelValue (ValueFeature era) era)
      -- ^ the Coin consumed by this transaction
      !(ModelValue (ValueFeature era) era)
      -- ^ the Coin produced by this transaction

type ModelLedgerInputs era =
  ( [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)],
    [ModelEpoch era]
  )

type ModelLedgerError era = ()

newtype ModelM era m a = ModelM {unModelM :: RWS.RWST Globals (ModelLedgerError era) (ModelLedger era) m a}
  deriving newtype (Functor, Applicative, Monad)

runModelM ::
  MonadModelProvenance era provM =>
  ModelM era provM a ->
  Globals ->
  ModelLedger era ->
  provM (Either (ModelLedgerError era) a, ModelLedger era)
runModelM (ModelM k) r s = do
  (a, s', w) <- RWS.runRWST k r s
  pure $
    if (w == mempty)
      then (Right a, s')
      else (Left w, s)

deriving newtype instance Monad m => MonadReader Globals (ModelM era m)

deriving newtype instance Monad m => MonadWriter (ModelLedgerError era) (ModelM era m)

deriving newtype instance Monad m => MonadState (ModelLedger era) (ModelM era m)

deriving via
  (SomeMonadTrans (RWS.RWST Globals (ModelLedgerError era) (ModelLedger era)) m)
  instance
    MonadModelProvenance era m => MonadModelProvenance era (ModelM era m)

execModelM ::
  forall era.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m ()) ->
  Globals ->
  ModelLedger era ->
  ModelLedger era
execModelM k r s = snd $ modelM k r s

modelM ::
  forall era a.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m a) ->
  Globals ->
  ModelLedger era ->
  (a, ModelLedger era)
modelM k r s =
  let k' :: ModelM era (Tagged era) a
      k' = k
      Tagged (x, s') = runModelM k' r s
   in (either (error . (<> "modelM:") . show) id x, s')

execModelMWithProv ::
  forall era.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m ()) ->
  Globals ->
  (ModelProvenanceState era, ModelLedger era) ->
  (ModelProvenanceState era, ModelLedger era)
execModelMWithProv k r s = snd $ modelMWithProv k r s

modelMWithProv ::
  forall era a.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m a) ->
  Globals ->
  (ModelProvenanceState era, ModelLedger era) ->
  (a, (ModelProvenanceState era, ModelLedger era))
modelMWithProv k r (p, s) =
  let k' :: ModelM era (State.State (ModelProvenanceState era)) a
      k' = k
      ((x, s'), p') = State.runState (runModelM k' r s) p
   in (either (error . (<> "modelM:") . show) id x, (p', s'))

type HasModelM era st r m =
  ( MonadReader r m,
    HasGlobals r,
    MonadState st m,
    HasModelLedger era st,
    KnownRequiredFeatures era,
    MonadModelProvenance era m
  )

-- TODO: split history/provenance fully from state
data ModelSnapshotProvenance era = ModelSnapshotProvenance
  { _modelSnapshotProvenance_stake :: Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelTxId,
    _modelSnapshotProvenance_delegations :: Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelTxId,
    _modelSnapshotProvenance_pools :: Map.Map ModelPoolId ModelTxId
  }
  deriving (Eq, Generic)
  deriving (Show) via Quiet (ModelSnapshotProvenance era)

instance NFData (ModelSnapshotProvenance era)

emptyModelSnapshotProv :: ModelSnapshotProvenance era
emptyModelSnapshotProv = ModelSnapshotProvenance mempty Map.empty Map.empty

modelSnapshotProvenance_stake :: Lens' (ModelSnapshotProvenance era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelTxId)
modelSnapshotProvenance_stake a2fb s = (\b -> s {_modelSnapshotProvenance_stake = b}) <$> a2fb (_modelSnapshotProvenance_stake s)
{-# INLINE modelSnapshotProvenance_stake #-}

modelSnapshotProvenance_delegations :: Lens' (ModelSnapshotProvenance era) (Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelTxId)
modelSnapshotProvenance_delegations a2fb s = (\b -> s {_modelSnapshotProvenance_delegations = b}) <$> a2fb (_modelSnapshotProvenance_delegations s)
{-# INLINE modelSnapshotProvenance_delegations #-}

modelSnapshotProvenance_pools :: Lens' (ModelSnapshotProvenance era) (Map.Map ModelPoolId ModelTxId)
modelSnapshotProvenance_pools a2fb s = (\b -> s {_modelSnapshotProvenance_pools = b}) <$> a2fb (_modelSnapshotProvenance_pools s)
{-# INLINE modelSnapshotProvenance_pools #-}

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

data ModelUTxOMap era = ModelUTxOMap
  { _modelUTxOMap_utxos :: !(Map.Map ModelUTxOId (Coin, ModelTxOut era)),
    _modelUTxOMap_stake :: !(GrpMap (ModelCredential 'Staking (ScriptFeature era)) (Coin, Set ModelUTxOId)),
    _modelUTxOMap_collateralUtxos :: !(Set ModelUTxOId),
    _modelUTxOMap_balance :: !Coin -- TODO: ModelValueSimple
  }
  deriving (Eq, Show, Generic)

validModelUTxOMap :: ModelUTxOMap era -> Bool
validModelUTxOMap m = m == toModelUTxOMap (fmap snd $ _modelUTxOMap_utxos m)

instance Semigroup (ModelUTxOMap era) where
  -- TODO: this instance is partial. fix uses to be better?
  xs <> ys =
    let (conflicts, zs) = mergeModelUTxOMapWithConflicts xs ys
     in if null conflicts
          then zs
          else error $ unwords ["unmergable ModelUTxOMap:", show conflicts]

instance Monoid (ModelUTxOMap era) where
  mempty = emptyUTxOMap

mergeModelUTxOMapWithConflicts ::
  ModelUTxOMap era ->
  ModelUTxOMap era ->
  (Map.Map ModelUTxOId ((Coin, ModelTxOut era), (Coin, ModelTxOut era)), (ModelUTxOMap era))
mergeModelUTxOMapWithConflicts
  (ModelUTxOMap utxos stake collateral bal)
  (ModelUTxOMap utxos' stake' collateral' bal') =
    ModelUTxOMap
      <$> Map.mergeA
        Map.preserveMissing
        Map.preserveMissing
        (Map.zipWithMaybeAMatched $ \ui x y -> (Map.singleton ui (x, y), Nothing))
        utxos
        utxos'
      <*> pure (stake <> stake')
      <*> pure (Set.union collateral collateral')
      <*> pure (bal <> bal')

emptyUTxOMap :: ModelUTxOMap era
emptyUTxOMap = ModelUTxOMap Map.empty mempty Set.empty mempty

toModelUTxOMap :: Map.Map ModelUTxOId (ModelTxOut era) -> ModelUTxOMap era
toModelUTxOMap =
  ifoldMap $ \ui txo@(ModelTxOut ma mval _) ->
    let val = getModelValueCoin mval
     in ModelUTxOMap
          (Map.singleton ui (val, txo))
          (grpMapSingleton (_modelAddress_stk ma) (val, Set.singleton ui))
          (bool Set.empty (Set.singleton ui) $ has (modelAddress_pmt . _ModelKeyHashObj) ma)
          val

mkModelUTxOMap ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)] ->
  ModelUTxOMap era
mkModelUTxOMap =
  foldMap $ \(ui, ma, val) ->
    ModelUTxOMap
      (Map.singleton ui (val, ModelTxOut ma (modelValueInject val) dh))
      (grpMapSingleton (_modelAddress_stk ma) (val, Set.singleton ui))
      (bool Set.empty (Set.singleton ui) $ has (modelAddress_pmt . _ModelKeyHashObj) ma)
      val
  where
    dh = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Nothing

getModelUTxOMapTotalAda :: ModelUTxOMap era -> Coin
getModelUTxOMapTotalAda = _modelUTxOMap_balance

getModelValueCoin :: ModelValue a c -> Coin
getModelValueCoin = foldMap Val.coin . evalModelValueSimple . unModelValue

spendModelUTxOs ::
  Set ModelUTxOId ->
  [(ModelUTxOId, ModelTxOut era)] ->
  ModelUTxOMap era ->
  ModelUTxOMap era
spendModelUTxOs ins outs xs =
  let ins' = Map.restrictKeys (_modelUTxOMap_utxos xs) ins
      outs' = Map.fromList $ (fmap . fmap) (getModelValueCoin . _mtxo_value &&& id) outs
      newCollateral = foldMap (\(ui, txo) -> bool Set.empty (Set.singleton ui) $ has (modelTxOut_address . modelAddress_pmt . _ModelKeyHashObj) txo) outs
      getStake :: Map.Map ModelUTxOId (Coin, ModelTxOut era) -> GrpMap (ModelCredential 'Staking (ScriptFeature era)) (Coin, Set ModelUTxOId)
      getStake = ifoldMap (\ui (val, txo) -> grpMapSingleton (_modelAddress_stk $ _mtxo_address txo) (val, Set.singleton ui))

      mergeStake :: (Coin, Set ModelUTxOId) -> (Coin, Set ModelUTxOId) -> (Coin, Set ModelUTxOId)
      mergeStake (c, ui) (c', ui') = (c' ~~ c, Set.difference ui' ui)
   in ModelUTxOMap
        { _modelUTxOMap_utxos = Map.withoutKeys (_modelUTxOMap_utxos xs `Map.union` outs') ins,
          _modelUTxOMap_stake = flip State.execState (_modelUTxOMap_stake xs) $ do
            id <>= getStake outs'
            State.modify $ zipWithGrpMap mergeStake $ getStake ins',
          _modelUTxOMap_balance = _modelUTxOMap_balance xs <> foldMap fst outs' ~~ foldMap fst ins',
          _modelUTxOMap_collateralUtxos =
            Set.difference (_modelUTxOMap_collateralUtxos xs) ins
              `Set.union` newCollateral
        }

instance NFData (ModelUTxOMap era)

type instance Index (ModelUTxOMap era) = ModelUTxOId

type instance IxValue (ModelUTxOMap era) = ModelTxOut era

instance Ixed (ModelUTxOMap era)

instance At (ModelUTxOMap era) where
  at :: ModelUTxOId -> Lens' (ModelUTxOMap era) (Maybe (ModelTxOut era))
  at k = \a2fb s ->
    let a = Map.lookup k $ _modelUTxOMap_utxos s
        b2t :: Maybe (ModelTxOut era) -> ModelUTxOMap era
        b2t b =
          let val' = foldMap fst $ Map.lookup k $ _modelUTxOMap_utxos s
              val = foldMap (getModelValueCoin . _mtxo_value) b
              hodler' = fmap (_modelAddress_stk . _mtxo_address . snd) $ Map.lookup k $ _modelUTxOMap_utxos s
              hodler = _modelAddress_stk . _mtxo_address <$> b
           in ModelUTxOMap
                { _modelUTxOMap_utxos = set (at k) (fmap ((,) val) b) (_modelUTxOMap_utxos s),
                  _modelUTxOMap_collateralUtxos =
                    set
                      (at k)
                      (() <$ preview (_Just . modelTxOut_address . modelAddress_pmt . _ModelKeyHashObj) b)
                      (_modelUTxOMap_collateralUtxos s),
                  _modelUTxOMap_stake = flip State.execState (_modelUTxOMap_stake s) $ do
                    for_ hodler' $ \h -> do
                      grpMap h . _1 <>= invert val'
                      grpMap h . _2 %= Set.delete k
                    for_ hodler $ \h -> do
                      grpMap h . _1 <>= val'
                      grpMap h . _2 %= Set.insert k,
                  _modelUTxOMap_balance = _modelUTxOMap_balance s <> val ~~ val'
                }
     in b2t <$> (a2fb $ fmap snd a)

data ModelRewardProvenanceDelegate = ModelRewardProvenanceDelegate
  { _modelRewardProvenanceDelegate_stakeRegistration :: ModelTxId,
    _modelRewardProvenanceDelegate_poolRegistration :: ModelTxId,
    _modelRewardProvenanceDelegate_delegation :: ModelTxId
  }
  deriving (Eq, Generic, Show)

instance NFData ModelRewardProvenanceDelegate

data ModelRewardProvenancePool = ModelRewardProvenancePool
  { _modelRewardProvenancePool_poolRegistration :: ModelTxId
  }
  deriving (Eq, Generic, Show)

instance NFData ModelRewardProvenancePool

data ModelRewardProvenance
  = ModelRewardProvenance_Delegate ModelRewardProvenanceDelegate
  | ModelRewardProvenance_Pool ModelRewardProvenancePool
  deriving (Eq, Generic, Show)

instance NFData ModelRewardProvenance

data ModelPParamsF (era :: FeatureSet) f = ModelPParams
  { _modelPParams_minfeeA :: !(f Natural),
    _modelPParams_minfeeB :: !(f Natural),
    _modelPParams_collateralPercent :: !(f (IfSupportsPlutus () Natural (ScriptFeature era))),
    _modelPParams_d :: !(f UnitInterval),
    _modelPParams_tau :: !(f UnitInterval),
    _modelPParams_a0 :: !(f NonNegativeInterval),
    _modelPParams_rho :: !(f UnitInterval),
    _modelPParams_nOpt :: !(f Natural),
    _modelPParams_protocolVersion :: !(f ProtVer),
    _modelPParams_keyDeposit :: !(f Coin),
    _modelPParams_poolDeposit :: !(f Coin),
    _modelPParams_maxCollateralInputs :: !(f (IfSupportsPlutus () Natural (ScriptFeature era))),
    _modelPParams_prices :: !(f (IfSupportsPlutus () Prices (ScriptFeature era))),
    -- | mixes shelley era minUTxOValue with alonzo coinsPerUTxOWord.
    -- intermediate (shelley-ma : fig7[GL-D1]) is not handled.
    _modelPParams_coinsPerUTxOWord :: !(f (IfSupportsMint Coin Coin (ValueFeature era))),
    _modelPParams_maxTxSize :: !(f Natural)
  }
  deriving (Generic)

type TransModelPParams :: forall k. (k -> Constraint) -> (Type -> k) -> FeatureSet -> Constraint

type TransModelPParams c f era =
  ( c (f (IfSupportsPlutus () Natural (ScriptFeature era))),
    c (f (IfSupportsPlutus () Prices (ScriptFeature era))),
    c (f (IfSupportsMint Coin Coin (ValueFeature era))),
    c (f UnitInterval),
    c (f NonNegativeInterval),
    c (f Natural),
    c (f ProtVer),
    c (f Coin),
    c (f (IfSupportsPlutus () Natural (ScriptFeature era)))
  )

deriving instance TransModelPParams Eq f era => Eq (ModelPParamsF era f)

deriving instance TransModelPParams Show f era => Show (ModelPParamsF era f)

instance TransModelPParams NFData f era => NFData (ModelPParamsF era f)

instance FFunctor (ModelPParamsF era) where ffmap = ffmapDefault

instance FZip (ModelPParamsF era) where fzipWith = gfzipWith

instance FRepeat (ModelPParamsF era) where frepeat = gfrepeat

instance FFoldable (ModelPParamsF era) where ffoldMap = ffoldMapDefault

instance FTraversable (ModelPParamsF era) where ftraverse = gftraverse

type ModelPParams era = ModelPParamsF era Identity

type ModelPParamsUpdate era = ModelPParamsF era Maybe

instance GHC.HasField "_minfeeA" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_minfeeA

instance GHC.HasField "_minfeeB" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_minfeeB

instance GHC.HasField "_d" (ModelPParams era) UnitInterval where
  getField = runIdentity . _modelPParams_d

instance GHC.HasField "_tau" (ModelPParams era) UnitInterval where
  getField = runIdentity . _modelPParams_tau

instance GHC.HasField "_a0" (ModelPParams era) NonNegativeInterval where
  getField = runIdentity . _modelPParams_a0

instance GHC.HasField "_rho" (ModelPParams era) UnitInterval where
  getField = runIdentity . _modelPParams_rho

instance GHC.HasField "_nOpt" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_nOpt

instance GHC.HasField "_protocolVersion" (ModelPParams era) ProtVer where
  getField = runIdentity . _modelPParams_protocolVersion

instance GHC.HasField "_keyDeposit" (ModelPParams era) Coin where
  getField = runIdentity . _modelPParams_keyDeposit

instance GHC.HasField "_poolDeposit" (ModelPParams era) Coin where
  getField = runIdentity . _modelPParams_poolDeposit

instance GHC.HasField "_maxCollateralInputs" (ModelPParams era) Natural where
  getField = fromSupportsPlutus (\() -> 0) id . runIdentity . _modelPParams_maxCollateralInputs

instance GHC.HasField "_prices" (ModelPParams era) Prices where
  getField = fromSupportsPlutus (\() -> Prices minBound minBound) id . runIdentity . _modelPParams_prices

instance GHC.HasField "_maxTxSize" (ModelPParams era) Natural where
  getField = runIdentity . _modelPParams_maxTxSize

instance MintSupported (ValueFeature era) => GHC.HasField "_coinsPerUTxOWord" (ModelPParams era) Coin where
  getField = supportsMint . runIdentity . _modelPParams_coinsPerUTxOWord

class HasModelPParams era a | a -> era where
  getModelPParams :: a -> ModelPParams era

dM :: HasModelPParams era r => r -> UnitInterval
dM = runIdentity . _modelPParams_d . getModelPParams

maxCollateralInputs :: HasModelPParams era r => r -> Natural
maxCollateralInputs = fromSupportsPlutus (const 0) id . runIdentity . _modelPParams_maxCollateralInputs . getModelPParams

-- | TODO: fig 12
type ModelPPupdateState era = ()

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

-- | fig 15
data ModelUTxOEnv era = ModelUTxOEnv
  { _modelUTxOEnv_slot :: !SlotNo,
    _modelUTxOEnv_pp :: !(ModelPParams era),
    _modelUTxOEnv_poolParams :: !(Map.Map ModelPoolId (ModelPoolParams era)),
    _modelUTxOEnv_genDelegs :: !ModelGenesisDelegation
  }

instance HasModelPParams era (ModelUTxOEnv era) where
  getModelPParams = _modelUTxOEnv_pp

data ModelUTxOError
  = BadInput
  | Expired
  | MaxTxSize
  | InputSetEmpty
  | FeeTooSmall
  | ValueNotConserved
  | WrongNetwork
  | WrongNetworkWihtdrawal
  | OutputTooSmall
  | OutputBootAddrAttrsTooBig

-- | (fig 17)[SL-D5]
modelTotalDeposits ::
  ModelPParams era ->
  Map.Map ModelPoolId (ModelPoolParams era) ->
  [ModelDCert era] ->
  Coin
modelTotalDeposits
  ( ModelPParams
      { _modelPParams_keyDeposit = Identity keyDeposit,
        _modelPParams_poolDeposit = Identity poolDeposit
      }
    )
  poolParams
  certs =
    keyDeposit `pow` (lengthOf (traverse . _ModelRegisterStake) certs)
      <> poolDeposit `pow` Set.size newPools
    where
      newPools =
        Set.fromList
          [ c
            | c <- toListOf (traverse . _ModelRegisterPool) certs,
              not (Map.member (_mppId c) poolParams)
          ]

-- | (fig 17)[SL-D5]
modelKeyRefunds :: ModelPParams era -> [ModelDCert era] -> Coin
modelKeyRefunds (ModelPParams {_modelPParams_keyDeposit = Identity keyDeposit}) certs =
  keyDeposit `pow` (lengthOf (traverse . _ModelDeRegisterStake) certs)

-- | (fig 9)[GL-D2]
instance ModelSTS 'ModelRule_UTXOS where
  type ModelSignal 'ModelRule_UTXOS = ModelTx
  type ModelState 'ModelRule_UTXOS = ModelUTxOState
  type ModelEnv 'ModelRule_UTXOS = ModelUTxOEnv
  type ModelFailure 'ModelRule_UTXOS = Proxy

  applyRule _ tx
    | modelIsValid tx = RWS.execRWST $ do
      refunded <-
        modelKeyRefunds
          <$> asks (_modelUTxOEnv_pp . _modelEnv)
          <*> pure (_mtxDCert tx)
      deposits <-
        modelTotalDeposits
          <$> asks (_modelUTxOEnv_pp . _modelEnv)
          <*> asks (_modelUTxOEnv_poolParams . _modelEnv)
          <*> pure (_mtxDCert tx)

      let depositChange = deposits ~~ refunded

      -- pup' <- liftModelRule (Proxy @ModelRule_PPUP) tx

      modelUTxOState_utxo %= spendModelUTxOs (_mtxInputs tx) (_mtxOutputs tx)
      modelUTxOState_deposited <>= depositChange
      modelUTxOState_fees <>= getModelValueCoin (_mtxFee tx)
    -- modelUTxOState_ppup .= pup'

    | otherwise = RWS.execRWST $ do
      b <- uses modelUTxOState_utxo _modelUTxOMap_balance
      traverseSupportsPlutus_
        (\collateral -> modelUTxOState_utxo %= spendModelUTxOs collateral [])
        (_mtxCollateral tx)
      b' <- uses modelUTxOState_utxo _modelUTxOMap_balance
      modelUTxOState_fees <>= b' ~~ b

-- | handle utxos on transaction
-- SEE: (fig 10)[GL-D2]
-- DEPRECATES: (fig 7)[GL-D1]
-- DEPRECATES: (fig 16)[SL-D5]
instance ModelSTS 'ModelRule_UTXO where
  type ModelSignal 'ModelRule_UTXO = ModelTx
  type ModelState 'ModelRule_UTXO = ModelUTxOState
  type ModelEnv 'ModelRule_UTXO = ModelUTxOEnv
  type ModelFailure 'ModelRule_UTXO = Proxy

  applyRule _ tx = RWS.execRWST $ do
    -- guardRule ModelUTXOFailure_InputSetEmtpy
    --   (not $ null $ _mtxInputs tx)

    -- ((<=) <$> pure (fromSupportsPlutus (const 0) length $ _mtxCollateral tx) <*> asks maxCollateralInputs)
    --   >>= guardRule ModelUTXOFailure_InputSetEmtpy
    utxoMap <- State.gets _modelUTxOState_utxo
    pp <- asks getModelPParams
    -- TODO: NoCollateralInputs error
    unless (modelFeesOK pp tx utxoMap) (tell $ Set.singleton Proxy)
    liftApplyRule (Proxy @'ModelRule_UTXOS) tx tx

-- | See figure 4 [GL-D2]
modelFeesOK :: ModelPParams era -> ModelTx era -> ModelUTxOMap era -> Bool
modelFeesOK pp tx utxoMap =
  -- TODO: This is not fully completed at all
  (null $ tx ^. modelTx_redeemers)
    || ( ( balance `pow` (100 :: Natural) >= (getModelValueCoin $ tx ^. modelTx_fee)
             `pow` (fromSupportsPlutus (const 100) id $ runIdentity $ _modelPParams_collateralPercent pp)
         )
           && fromSupportsPlutus (const True) (not . null) (tx ^. modelTx_collateral)
       )
  where
    balance = fromSupportsPlutus (const mempty) (foldMap fst . Map.restrictKeys (_modelUTxOMap_utxos utxoMap)) (tx ^. modelTx_collateral)

-- modelUTxOState_ppup .= pup'

data ModelGlobalsEnv a = ModelGlobalsEnv
  { _modelGlobals :: !Globals,
    _modelEnv :: !a
  }
  deriving (Show, Functor, Foldable, Traversable)

modelEnv :: Lens (ModelGlobalsEnv a) (ModelGlobalsEnv b) a b
modelEnv a2fb s = (\b -> s {_modelEnv = b}) <$> a2fb (_modelEnv s)

instance HasGlobals (ModelGlobalsEnv a) where
  getGlobals (ModelGlobalsEnv g _) = g

instance HasModelPParams era a => HasModelPParams era (ModelGlobalsEnv a) where
  getModelPParams = getModelPParams . _modelEnv

type SlotMap = PiecewiseConstantMap SlotNo

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
       )
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
      _modelProvenanceState_wdrl = Map.empty
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

class HasModelProvenanceState era st | st -> era where
  modelProvenanceState :: Lens' st (ModelProvenanceState era)

instance HasModelProvenanceState era (ModelProvenanceState era) where
  modelProvenanceState = id

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
          pure () -- TODO: this can *totally* affect rewards
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

    modelProvenanceState . modelProvenanceState_reward . at epoch %= Just . Map.unionWith (<>) stakeProv' . fold

  wdrlProvenance stk = do
    currentEpoch <- use $ modelProvenanceState . modelProvenanceState_currentSlot . _1
    let availableRewards = currentEpoch - 2
    modelProvenanceState . modelProvenanceState_wdrl %= Map.unionWith max (Map.fromSet (const availableRewards) stk)

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

instance MonadModelProvenance era (Tagged era) where
  setProvenance _ = pure ()
  setSlot _ _ = pure ()
  clearProvenance = pure ()
  delegProvenance _ = pure ()
  rewardOperatorProvenance _ _ = pure ()
  rewardMemberProvenance _ _ = pure ()
  wdrlProvenance _ = pure ()

deriving via (SomeMonadState s (State.StateT s m)) instance (Monad m, HasModelProvenanceState era s) => MonadModelProvenance era (State.StateT s m)

deriving via (SomeMonadState s (RWS.RWST r w s m)) instance (Monad m, HasModelProvenanceState era s) => MonadModelProvenance era (RWS.RWST r w s m)

class ModelSTS (rule :: ModelRule) where
  type ModelSignal rule :: FeatureSet -> Type
  type ModelState rule :: FeatureSet -> Type
  type ModelEnv rule :: FeatureSet -> Type
  type ModelFailure rule :: FeatureSet -> Type

  applyRule ::
    (KnownRequiredFeatures era, MonadModelProvenance era provM) =>
    proxy rule ->
    ModelSignal rule era ->
    ModelGlobalsEnv (ModelEnv rule era) ->
    ModelState rule era ->
    provM (ModelState rule era, Set (ModelFailure rule era))
  default applyRule ::
    (KnownRequiredFeatures era, ModelSignal rule ~ Const Void) =>
    proxy rule ->
    ModelSignal rule era ->
    ModelGlobalsEnv (ModelEnv rule era) ->
    ModelState rule era ->
    provM (ModelState rule era, Set (ModelFailure rule era))
  applyRule _ = absurd . getConst

-- TODO: ModelRule_MIR ModelRule_NEWPP ModelRule_PPUP
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

  applyRule _ txs = RWS.execRWST $ do
    liftApplyRules (Proxy @'ModelRule_LEDGER) (getCompose txs) txs

data ModelLEnv env = ModelLEnv
  { _modelLEnv_slot :: !SlotNo,
    _modelLEnv_txIx :: !Int,
    _modelLEnv_pp :: !(ModelPParams env)
    -- , _modelLEnv_acnt :: !ModelAcnt
  }

-- FIG30[SL-D5]
instance ModelSTS 'ModelRule_LEDGER where
  type ModelSignal 'ModelRule_LEDGER = ModelTx
  type ModelState 'ModelRule_LEDGER = ModelLState
  type ModelEnv 'ModelRule_LEDGER = ModelLEnv
  type ModelFailure 'ModelRule_LEDGER = Proxy

  applyRule _ tx = RWS.execRWST $ do
    lift $ setProvenance (getModelTxId tx)
    liftApplyRule (Proxy @'ModelRule_DELEGS) (Compose $ _mtxDCert tx) tx
    liftApplyRule (Proxy @'ModelRule_UTXOW) tx tx
    lift $ clearProvenance

-- (fig13)[GL-D2]
instance ModelSTS 'ModelRule_UTXOW where
  type ModelSignal 'ModelRule_UTXOW = ModelTx
  type ModelState 'ModelRule_UTXOW = ModelUTxOState
  type ModelEnv 'ModelRule_UTXOW = ModelUTxOEnv
  type ModelFailure 'ModelRule_UTXOW = Proxy

  applyRule _ tx = RWS.execRWST $ do
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

  applyRule _ cs = RWS.execRWST $ do
    wdrls <- asks (_mtxWdrl . _modelDPSEnv_tx . _modelEnv)
    lift $ wdrlProvenance (Map.keysSet wdrls)
    badWdrls <-
      modelDPState_dstate . modelDState_rewards
        %%= Map.mergeA
          (Map.traverseMaybeMissing $ \_ wdrl -> (bool mempty (Set.singleton Proxy) (Val.zero == getModelValueCoin wdrl), Nothing)) -- DelegateeNotRegistered
          (Map.preserveMissing)
          ( Map.zipWithAMatched $ \_ mwdrl rwd ->
              let wdrl = getModelValueCoin mwdrl
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

  applyRule _ dcert = RWS.execRWST $ do
    lift $ delegProvenance dcert
    case dcert of
      c'@(ModelCertDeleg c) -> liftApplyRule (Proxy @'ModelRule_DELEG) c c'
      c'@(ModelCertPool c) -> liftApplyRule (Proxy @'ModelRule_POOL) c c'

data ModelDEnv env = ModelDEnv SlotNo -- ModelAcnt

-- fig22[SL-D5]
instance ModelSTS 'ModelRule_DELEG where
  type ModelSignal 'ModelRule_DELEG = ModelDelegCert
  type ModelState 'ModelRule_DELEG = ModelDState
  type ModelEnv 'ModelRule_DELEG = ModelDEnv
  type ModelFailure 'ModelRule_DELEG = Proxy

  applyRule _ = \case
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
      let irwd = case credCoinMap of
            ModelStakeAddressesMIR q ->
              (:*:) mempty $
                mkGrpMap $
                  ( coerce ::
                      forall sf.
                      Map.Map (ModelCredential 'Staking sf) DeltaCoin ->
                      Map.Map (ModelCredential 'Staking sf) Coin
                  )
                    $ q
            ModelSendToOppositePotMIR q -> Identity q :*: mempty
          irwds = case mirPot of
            TreasuryMIR -> ModelAcnt irwd mempty
            ReservesMIR -> ModelAcnt mempty irwd

      modelDState_iRwd <>= Comp1 irwds

data ModelPEnv env = ModelPEnv SlotNo (ModelPParams env)

-- fig22[SL-D5]
instance ModelSTS 'ModelRule_POOL where
  type ModelSignal 'ModelRule_POOL = ModelPoolCert
  type ModelState 'ModelRule_POOL = ModelPState
  type ModelEnv 'ModelRule_POOL = ModelPEnv
  type ModelFailure 'ModelRule_POOL = Proxy

  applyRule _ = \case
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

  applyRule _ slot = RWS.execRWST $ do
    Identity epoch <-
      epochInfoEpoch
        <$> asks (epochInfo . getGlobals)
        <*> pure (getConst slot)

    lift $ setSlot epoch (getConst slot)
    liftApplyRule (Proxy @'ModelRule_NEWEPOCH) (Const epoch) slot
    liftApplyRule (Proxy @'ModelRule_RUPD) slot slot

data ModelRUpdEnv era = ModelRUpdEnv
  { _modelRUpdEnv_b :: !ModelBlocksMade,
    _modelRUpdEnv_es :: !(ModelEpochState era)
  }
  deriving (Show)

-- Fig61[SL-D5]
instance ModelSTS 'ModelRule_RUPD where
  type ModelSignal 'ModelRule_RUPD = Const SlotNo
  type ModelState 'ModelRule_RUPD = Compose Maybe ModelRewardUpdate
  type ModelEnv 'ModelRule_RUPD = ModelRUpdEnv
  type ModelFailure 'ModelRule_RUPD = Proxy

  applyRule _ (Const s) =
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

  applyRule _ e = RWS.execRWST $ do
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

  applyRule _ Proxy = RWS.execRWST $ do
    ( Comp1
        ( ModelAcnt
            { _modelAcnt_treasury = irTreasury,
              _modelAcnt_reserves = irReserves
            }
          )
      ) <-
      modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_iRwd
        <<.= mempty

    rewards <- use $ modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards
    let rewardAcnts = Map.keysSet rewards
        restrictIRToRewardAcnts = over _2 (flip restrictKeysGrpMap rewardAcnts)

        -- We swap around the pot moves because the fields on iRwd are the pots
        -- the funds are drawn from.  pot moves should go *to* the opposite pot.
        irwdR@(Identity newT :*: GrpMap irwdR') = restrictIRToRewardAcnts irReserves
        totR = fold irwdR

        irwdT@(Identity newR :*: GrpMap irwdT') = restrictIRToRewardAcnts irTreasury
        totT = fold irwdT

    ModelAcnt treasury reserves <- use modelEpochState_acnt

    when (totR <= reserves && totT <= treasury) $ do
      let rewards' = Map.unionsWith (<>) [rewards, irwdR', irwdT']
      modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards .= rewards'

      modelEpochState_acnt %= (~~ ModelAcnt totT totR) . (<> ModelAcnt newT newR)

-- Fig44[SL-D5]
instance ModelSTS 'ModelRule_EPOCH where
  type ModelSignal 'ModelRule_EPOCH = Const EpochNo
  type ModelState 'ModelRule_EPOCH = ModelEpochState
  type ModelEnv 'ModelRule_EPOCH = Proxy
  type ModelFailure 'ModelRule_EPOCH = Proxy

  applyRule _ e = RWS.execRWST $ do
    liftApplyRule (Proxy @'ModelRule_SNAP) Proxy e

    modelEpochState_ls . modelLState_dpstate . modelDPState_pstate
      %= ( \(ModelPState poolParams fPoolParams retiring) ->
             ModelPState (Map.unionWith (\_ x -> x) poolParams fPoolParams) Map.empty retiring
         )

    liftApplyRule (Proxy @'ModelRule_POOLREAP) e e

-- liftApplyRule (Proxy @ModelRule_NEWPP) _ _

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

  applyRule _ Proxy = RWS.execRWST $ do
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

instance HasModelPParams era (ModelPOOLREAPEnv era) where
  getModelPParams (ModelPOOLREAPEnv pp) = pp

-- FIG 40[SL-D5]
instance ModelSTS 'ModelRule_POOLREAP where
  type ModelSignal 'ModelRule_POOLREAP = Const EpochNo
  type ModelState 'ModelRule_POOLREAP = ModelPlReapState
  type ModelEnv 'ModelRule_POOLREAP = ModelPOOLREAPEnv
  type ModelFailure 'ModelRule_POOLREAP = Proxy

  applyRule _ (Const e) = RWS.execRWST $ do
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
          (st'', w') <- applyRule proxy signal'' (ModelGlobalsEnv globals env') st'
          pure ((), st'', w')
      )
      ()
      (getSubState proxy st)
  pure ((), putSubState proxy st''' st, Set.map (asSubFailure proxy) w)

type ModelFutGenesisDelegation =
  Map.Map
    (SlotNo, ModelCredential 'Genesis ShelleyScriptFeatures)
    (ModelCredential 'GenesisDelegate ShelleyScriptFeatures)

type ModelGenesisDelegation =
  Map.Map
    (ModelCredential 'Genesis ShelleyScriptFeatures)
    (ModelCredential 'GenesisDelegate ShelleyScriptFeatures)

type ModelInstantaneousRewardF era = Identity :*: GrpMap (ModelCredential 'Staking (ScriptFeature era))

type ModelInstantaneousRewards era = (:.:) ModelAcntF (ModelInstantaneousRewardF era) Coin

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

-- | fig 26
data ModelDPState era = ModelDPState
  { _modelDPState_dstate :: !(ModelDState era),
    _modelDPState_pstate :: !(ModelPState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelDPState era)

modelDPState_dstate :: Lens' (ModelDPState era) (ModelDState era)
modelDPState_dstate a2fb s = (\b -> s {_modelDPState_dstate = b}) <$> a2fb (_modelDPState_dstate s)
{-# INLINE modelDPState_dstate #-}

modelDPState_pstate :: Lens' (ModelDPState era) (ModelPState era)
modelDPState_pstate a2fb s = (\b -> s {_modelDPState_pstate = b}) <$> a2fb (_modelDPState_pstate s)
{-# INLINE modelDPState_pstate #-}

-- | fig 30
data ModelLState era = ModelLState
  { _modelLState_utxoSt :: !(ModelUTxOState era),
    _modelLState_dpstate :: !(ModelDPState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelLState era)

modelLState_utxoSt :: Lens' (ModelLState era) (ModelUTxOState era)
modelLState_utxoSt a2fb s = (\b -> s {_modelLState_utxoSt = b}) <$> a2fb (_modelLState_utxoSt s)
{-# INLINE modelLState_utxoSt #-}

modelLState_dpstate :: Lens' (ModelLState era) (ModelDPState era)
modelLState_dpstate a2fb s = (\b -> s {_modelLState_dpstate = b}) <$> a2fb (_modelLState_dpstate s)
{-# INLINE modelLState_dpstate #-}

-- | fig 35
-- TODO: swap out for LedgerState.AccountState
data ModelAcntF a = ModelAcnt
  { _modelAcnt_treasury :: !a,
    _modelAcnt_reserves :: !a
  }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance Applicative ModelAcntF where
  pure x = ModelAcnt x x
  ModelAcnt f g <*> ModelAcnt x y = ModelAcnt (f x) (g y)

instance Monad ModelAcntF where
  ModelAcnt t r >>= k = ModelAcnt (_modelAcnt_treasury $ k t) (_modelAcnt_reserves $ k r)

instance MonadReader MIRPot ModelAcntF where
  ask = ModelAcnt TreasuryMIR ReservesMIR
  local f xs = ModelAcnt (g TreasuryMIR) (g ReservesMIR)
    where
      g = lookupModelAcnt xs . f
  reader f = ModelAcnt (f TreasuryMIR) (f ReservesMIR)

-- ModelAcntF is representable, with representing object MIRPot
lookupModelAcnt :: ModelAcntF a -> MIRPot -> a
lookupModelAcnt (ModelAcnt t r) = \case
  TreasuryMIR -> t
  ReservesMIR -> r

type ModelAcnt = ModelAcntF Coin

instance NFData a => NFData (ModelAcntF a)

instance Semigroup w => Semigroup (ModelAcntF w) where
  (<>) = liftA2 (<>)

instance Monoid w => Monoid (ModelAcntF w) where
  mempty = pure mempty

instance Group g => Group (ModelAcntF g) where
  invert = fmap invert
  (~~) = liftA2 (~~)
  pow xs n = fmap (`pow` n) xs

instance Abelian g => Abelian (ModelAcntF g)

modelAcnt_treasury :: Lens' ModelAcnt Coin
modelAcnt_treasury a2fb s = (\b -> s {_modelAcnt_treasury = b}) <$> a2fb (_modelAcnt_treasury s)
{-# INLINE modelAcnt_treasury #-}

modelAcnt_reserves :: Lens' ModelAcnt Coin
modelAcnt_reserves a2fb s = (\b -> s {_modelAcnt_reserves = b}) <$> a2fb (_modelAcnt_reserves s)
{-# INLINE modelAcnt_reserves #-}

-- | fig 38
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

data ModelLedgerLog era
  = ModelLedgerLog_Rewards EpochNo (ModelRewardUpdate era)
  deriving (Eq, Show, Generic)

instance NFData (ModelLedgerLog era)

data ModelLedger era = ModelLedger
  { -- | offset from first slot in epoch
    _modelLedger_slotOffset :: !SlotNo,
    _modelLedger_nes :: !(ModelNewEpochState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelLedger era)

-- The spec stipulates that certain values particularly, the sigma parameter
-- used in rewards calculations) to be within [0,1], but which never-the-less
-- can be above 1 due to the
newtype PositiveRational = PositiveRational {unboundPositiveRational :: Rational}
  deriving (Eq, Ord, Show)
  deriving (Semigroup) via (Sum Rational)

boundPositiveRational :: Rational -> Maybe PositiveRational
boundPositiveRational x
  | x > 0 = Just $ PositiveRational x
  | otherwise = Nothing

-- | convenient initial pparams
modelPParams :: forall era. KnownRequiredFeatures era => ModelPParams era
modelPParams =
  ModelPParams
    { _modelPParams_protocolVersion = pure $ ProtVer 5 0,
      _modelPParams_minfeeA = pure 0,
      _modelPParams_minfeeB = pure 0,
      _modelPParams_collateralPercent = pure $ sp () 150,
      _modelPParams_d = pure $ maxBound,
      _modelPParams_maxTxSize = pure $ 1_000_000,
      _modelPParams_nOpt = pure $ 10,
      _modelPParams_rho = pure $ unsafeFromRational "modelPParams::rho" 0.02,
      _modelPParams_tau = pure minBound,
      _modelPParams_a0 = pure minBound,
      _modelPParams_keyDeposit = pure (Coin 0),
      _modelPParams_poolDeposit = pure (Coin 0),
      _modelPParams_prices = pure $ sp () (Prices minBound minBound),
      _modelPParams_coinsPerUTxOWord = pure $ sm (Coin 0) (Coin 10000), -- TODO: because bad Default AlonzoGenesis
      _modelPParams_maxCollateralInputs = pure $ sp () 5
    }
  where
    sp :: forall a b. a -> b -> IfSupportsPlutus a b (ScriptFeature era)
    sp = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era))

    sm :: forall a b. a -> b -> IfSupportsMint a b (ValueFeature era)
    sm = ifSupportsMint (Proxy :: Proxy (ValueFeature era))

data ModelGenesis era = ModelGenesis
  { _modelGenesis_pp :: !(ModelPParams era),
    _modelGenesis_genDelegs :: !ModelGenesisDelegation,
    _modelGenesis_utxos :: ![(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)]
  }
  deriving (Show, Generic)

modelGenesis_pp :: Lens' (ModelGenesis era) (ModelPParams era)
modelGenesis_pp = lens _modelGenesis_pp $ \s b -> s {_modelGenesis_pp = b}

modelGenesis_utxos :: Lens' (ModelGenesis era) [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)]
modelGenesis_utxos = lens _modelGenesis_utxos $ \s b -> s {_modelGenesis_utxos = b}

instance NFData (ModelGenesis era)

mkModelLedger ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Globals ->
  ModelGenesis era ->
  ModelLedger era
mkModelLedger globals (ModelGenesis pp genDelegs utxos) =
  ModelLedger
    { _modelLedger_nes = nes,
      _modelLedger_slotOffset = 0
    }
  where
    utxos' = mkModelUTxOMap utxos
    nes =
      ModelNewEpochState
        { _modelNewEpochState_el = 0,
          _modelNewEpochState_bPrev = mempty,
          _modelNewEpochState_bCur = mempty,
          _modelNewEpochState_es =
            ModelEpochState
              { _modelEpochState_acnt =
                  ModelAcnt
                    { _modelAcnt_treasury = Val.zero,
                      _modelAcnt_reserves = reserves
                    },
                _modelEpochState_ss =
                  ModelSnapshots
                    { _modelSnapshots_pstake =
                        pure
                          ModelSnapshot
                            { _modelSnapshot_stake = Map.empty,
                              _modelSnapshot_delegations = Map.empty,
                              _modelSnapshot_pools = Map.empty,
                              _modelSnapshot_utxos = emptyUTxOMap,
                              _modelSnapshot_rewards = Map.empty
                            },
                      _modelSnapshots_feeSS = mempty
                    },
                _modelEpochState_ls =
                  ModelLState
                    { _modelLState_utxoSt =
                        ModelUTxOState
                          { _modelUTxOState_utxo = utxos',
                            _modelUTxOState_deposited = mempty,
                            _modelUTxOState_fees = mempty
                          },
                      _modelLState_dpstate =
                        ModelDPState
                          { _modelDPState_dstate =
                              ModelDState
                                { _modelDState_rewards = Map.empty,
                                  _modelDState_delegations = Map.empty,
                                  _modelDState_fGenDelegs = Map.empty,
                                  _modelDState_genDelegs = genDelegs,
                                  _modelDState_iRwd = mempty
                                },
                            _modelDPState_pstate =
                              ModelPState
                                { _modelPState_poolParams = Map.empty,
                                  _modelPState_fPoolParams = Map.empty,
                                  _modelPState_retiring = Map.empty
                                }
                          }
                    },
                _modelEpochState_prevPp = pp,
                _modelEpochState_pp = pp
              },
          _modelNewEpochState_ru = Nothing
        }

    reserves =
      word64ToCoin (maxLovelaceSupply globals)
        ~~ foldOf (traverse . _3) utxos

class HasModelLedger era a | a -> era where
  modelLedger :: Lens' a (ModelLedger era)

instance HasModelLedger era (ModelLedger era) where
  modelLedger = id

class HasGlobals a where
  getGlobals :: a -> Globals

instance HasGlobals Globals where
  getGlobals = id

-- a couple of optics are defined for convenience of the generators, but as
-- Getters instead of lenses because they should not be used to modify the state
-- (instead, go through applyRule/applyModelTx or similar)
getModelLedger_utxos :: ModelLedger era -> ModelUTxOMap era
getModelLedger_utxos = view $ modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo
{-# INLINE getModelLedger_utxos #-}

getModelLedger_epoch :: ModelLedger era -> EpochNo
getModelLedger_epoch = view $ modelLedger_nes . modelNewEpochState_el
{-# INLINE getModelLedger_epoch #-}

modelLedger_slotOffset :: Lens' (ModelLedger era) SlotNo
modelLedger_slotOffset a2fb s = (\b -> s {_modelLedger_slotOffset = b}) <$> a2fb (_modelLedger_slotOffset s)
{-# INLINE modelLedger_slotOffset #-}

getModelLedger_rewards :: ModelLedger era -> Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin
getModelLedger_rewards = view $ modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards
{-# INLINE getModelLedger_rewards #-}

modelLedger_nes :: Lens' (ModelLedger era) (ModelNewEpochState era)
modelLedger_nes a2fb s = (\b -> s {_modelLedger_nes = b}) <$> a2fb (_modelLedger_nes s)
{-# INLINE modelLedger_nes #-}

getSubUTxOMapForStake :: ModelCredential 'Staking (ScriptFeature era) -> ModelUTxOMap era -> ModelUTxOMap era
getSubUTxOMapForStake maddr utxos =
  ModelUTxOMap
    (Map.restrictKeys (_modelUTxOMap_utxos utxos) uis)
    (grpMapSingleton maddr stake)
    (Set.intersection uis $ _modelUTxOMap_collateralUtxos utxos)
    bal
  where
    stake@(bal, uis) = view (grpMap maddr) (_modelUTxOMap_stake utxos)

applyModelDCert ::
  HasModelM era st r m =>
  ModelDCert era ->
  m ()
applyModelDCert dCert = do
  globals <- asks getGlobals

  pp <- uses (modelLedger . modelLedger_nes) getModelPParams
  st <- use $ modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate
  (st', _errs) <-
    applyRule
      (Proxy @'ModelRule_DELPL)
      dCert
      ( ModelGlobalsEnv globals $
          ModelDPSEnv
            { _modelDPSEnv_slot = 999,
              _modelDPSEnv_txIx = 999,
              _modelDPSEnv_pp = pp,
              _modelDPSEnv_tx = modelTx
            }
      )
      st
  -- TODO: unless (null errs) sulk
  modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate
    .= st'

applyModelTick :: HasModelM era st r m => SlotNo -> m ()
applyModelTick mslot = do
  globals <- asks getGlobals
  let ei = epochInfo globals
  Identity firstSlot <- epochInfoFirst ei <$> use (modelLedger . to getModelLedger_epoch)
  let slot = firstSlot + mslot

  modelLedger . modelLedger_slotOffset %= max mslot

  nes <- use $ modelLedger . modelLedger_nes
  (nes', _errs') <-
    applyRule
      (Proxy @'ModelRule_TICK)
      (Const slot)
      (ModelGlobalsEnv globals Proxy)
      nes
  -- TODO: unless (null $ errs') sulk
  modelLedger . modelLedger_nes .= nes'

applyModelBlock :: HasModelM era st r m => ModelBlock era -> m ()
applyModelBlock (ModelBlock mslot txs) = do
  applyModelTick mslot
  ifor_ txs (applyModelTx mslot)

-- TODO: take _mtxValidity into account
applyModelTx :: HasModelM era st r m => SlotNo -> Int -> ModelTx era -> m ()
applyModelTx slot txIx tx = do
  globals <- asks getGlobals

  es <- use $ modelLedger . modelLedger_nes . modelNewEpochState_es
  (ls', _errs) <-
    applyRule
      (Proxy @'ModelRule_LEDGER)
      tx
      (ModelGlobalsEnv globals $ ModelLEnv slot txIx $ getModelPParams es)
      (_modelEpochState_ls es)
  -- TODO: unless (null errs) sulk
  let es' = set modelEpochState_ls ls' es
  modelLedger . modelLedger_nes . modelNewEpochState_es .= es'

minStakeForRewards :: Coin
minStakeForRewards = Coin 200

getCoinUTxOsfromUTxoMap :: ModelUTxOMap era -> (Coin, Set ModelUTxOId)
getCoinUTxOsfromUTxoMap (ModelUTxOMap utxos _ _ balances) = (balances, Map.keysSet utxos)

getTotalDeposits :: ModelUTxOMap era -> Coin
getTotalDeposits (ModelUTxOMap _ _ _ balances) = balances

applyModelBlocksMade :: forall era m st r. HasModelM era st r m => ModelBlocksMade -> m ()
applyModelBlocksMade blocksMade = do
  epochNo <- use $ modelLedger . to getModelLedger_epoch

  -- we "emulate" blocks made
  modelLedger . modelLedger_nes . modelNewEpochState_bCur <>= blocksMade

  currentSlotOffset <- modelLedger . modelLedger_slotOffset <<.= 0

  globals <- asks getGlobals
  let ei = epochInfo globals
      prevEpoch = epochNo
      epoch = succ epochNo
      firstOfOld = runIdentity $ epochInfoFirst ei prevEpoch
      firstOfNew = runIdentity $ epochInfoFirst ei epoch

      -- make sure we do a reward update every epoch, even if the model hasn't
      -- any slots.
      neededSlot = SlotNo (randomnessStabilisationWindow globals) + firstOfOld
      currentSlot = firstOfOld + currentSlotOffset

  nes <- use $ modelLedger . modelLedger_nes

  (nes', errs') <-
    if currentSlot > neededSlot
      then pure (nes, mempty)
      else
        applyRule
          (Proxy @'ModelRule_TICK)
          (Const neededSlot + 1)
          (ModelGlobalsEnv globals Proxy)
          nes
  (nes'', errs'') <-
    applyRule
      (Proxy @'ModelRule_TICK)
      (Const firstOfNew)
      (ModelGlobalsEnv globals Proxy)
      nes'
  let errs = errs' <> errs''

  if null errs
    then modelLedger . modelLedger_nes .= nes''
    else error $ show errs

applyModelEpoch :: HasModelM era st r m => ModelEpoch era -> m ()
applyModelEpoch epoch = do
  forOf_ (modelEpoch_blocks . traverse) epoch applyModelBlock
  applyModelBlocksMade (_modelEpoch_blocksMade epoch)

data SnapshotQueue a = SnapshotQueue
  { _snapshotQueue_mark :: a,
    _snapshotQueue_set :: a,
    _snapshotQueue_go :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (SnapshotQueue a)

snapshotQueue_mark :: Lens' (SnapshotQueue a) a
snapshotQueue_mark = lens _snapshotQueue_mark (\s b -> s {_snapshotQueue_mark = b})
{-# INLINE snapshotQueue_mark #-}

snapshotQueue_set :: Lens' (SnapshotQueue a) a
snapshotQueue_set = lens _snapshotQueue_set (\s b -> s {_snapshotQueue_set = b})
{-# INLINE snapshotQueue_set #-}

snapshotQueue_go :: Lens' (SnapshotQueue a) a
snapshotQueue_go = lens _snapshotQueue_go (\s b -> s {_snapshotQueue_go = b})
{-# INLINE snapshotQueue_go #-}

shiftSnapshotQueue :: SnapshotQueue a -> (a, SnapshotQueue a)
shiftSnapshotQueue (SnapshotQueue markX setX goX) = (goX, SnapshotQueue markX markX setX)

instance Applicative SnapshotQueue where
  pure x = SnapshotQueue x x x
  SnapshotQueue markF setF goF <*> SnapshotQueue markX setX goX =
    SnapshotQueue (markF markX) (setF setX) (goF goX)

instance Monad SnapshotQueue where
  SnapshotQueue markX setX goX >>= cont =
    SnapshotQueue
      (_snapshotQueue_mark $ cont markX)
      (_snapshotQueue_set $ cont setX)
      (_snapshotQueue_go $ cont goX)

instance Semigroup a => Semigroup (SnapshotQueue a) where (<>) = liftA2 (<>)

instance Monoid a => Monoid (SnapshotQueue a) where mempty = pure mempty

-- | fig 50
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

-- | Reward Update Application
-- [SL-D5] Figure 52
applyRUpd :: (MonadState (ModelEpochState era) m) => ModelRewardUpdate era -> m ()
applyRUpd (ModelRewardUpdate deltaT deltaR (GrpMap rs) deltaF) = do
  rewards <- use $ modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards
  let regRU = Map.intersection rs rewards
      unregRU = Map.difference rs rewards
      unregRU' = fold unregRU
  modelEpochState_acnt . modelAcnt_treasury <>= deltaT <> unregRU'
  modelEpochState_acnt . modelAcnt_reserves <>= deltaR
  modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards
    %= Map.unionWith (<>) regRU
  modelEpochState_ls . modelLState_utxoSt . modelUTxOState_fees <>= deltaF

class PreservedAda a where
  totalPreservedAda :: a -> Coin

instance PreservedAda (ModelLedger era) where
  totalPreservedAda = totalPreservedAda . _modelLedger_nes

instance PreservedAda (ModelNewEpochState era) where
  totalPreservedAda = totalPreservedAda . _modelNewEpochState_es

instance PreservedAda (ModelEpochState era) where
  totalPreservedAda =
    totalPreservedAda . _modelEpochState_acnt
      <> totalPreservedAda . _modelEpochState_ls

instance PreservedAda ModelAcnt where
  totalPreservedAda = _modelAcnt_treasury <> _modelAcnt_reserves

instance PreservedAda (ModelLState era) where
  totalPreservedAda =
    foldOf
      ( modelLState_dpstate . modelDPState_dstate . modelDState_rewards . folded
          <> modelLState_utxoSt
            . ( modelUTxOState_deposited
                  <> modelUTxOState_fees
                  <> modelUTxOState_utxo . to getModelUTxOMapTotalAda
              )
      )

modelTotalAda :: ModelLedger era -> Coin
modelTotalAda = totalPreservedAda

modelTotalAdaES :: ModelEpochState era -> Coin
modelTotalAdaES = totalPreservedAda

modelTotalAdaLState :: ModelLState era -> Coin
modelTotalAdaLState = totalPreservedAda

validModelRewardUpdate ::
  ModelRewardUpdate era ->
  Either (NonEmpty.NonEmpty String) (ModelRewardUpdate era)
validModelRewardUpdate ru@(ModelRewardUpdate dTreasury dReserves dRewards dFees) =
  case NonEmpty.nonEmpty $
    concat
      [ ["unbalanced" | dTreasury <> dReserves <> fold dRewards <> dFees /= mempty],
        ["treasury<0" | dTreasury < mempty],
        ["reserves>0" | dReserves > mempty],
        ["fees>0" | dFees > mempty],
        ["negative rewards" | not $ Map.null $ Map.filter (< mempty) $ unGrpMap dRewards]
      ] of
    Nothing -> Right ru
    Just errors -> Left errors

-- getModelLedgerDeposits :: ModelLedger era -> Coin
-- getModelLedgerDeposits l
--   = runIdentity (_modelPParams_keyDeposit $ _modelLedger_prevPp l) `pow` lengthOf modelLedger_rewards l
--   <> runIdentity (_modelPParams_poolDeposit $ _modelLedger_prevPp l) `pow` lengthOf (modelLedger_stake . snapshotQueue_mark . modelSnapshotProvenance_pools) l

-- | calculation to create a reward update
-- [SL-D5] Figure 51
createRUpd :: MonadModelProvenance era provM => ModelRUpdEnv era -> EpochSize -> Coin -> ActiveSlotCoeff -> provM (ModelRewardUpdate era)
createRUpd
  ( ModelRUpdEnv
      { _modelRUpdEnv_b = b,
        _modelRUpdEnv_es =
          ModelEpochState
            { _modelEpochState_acnt = ModelAcnt {_modelAcnt_reserves = reserves},
              _modelEpochState_ss =
                ModelSnapshots
                  { _modelSnapshots_pstake =
                      SnapshotQueue
                        { _snapshotQueue_go =
                            ModelSnapshot
                              { _modelSnapshot_stake = stake,
                                _modelSnapshot_delegations = delegs,
                                _modelSnapshot_pools = poolSS
                              }
                        },
                    _modelSnapshots_feeSS = feeSS
                  },
              _modelEpochState_ls =
                ModelLState
                  { _modelLState_dpstate =
                      ModelDPState
                        { _modelDPState_dstate =
                            ModelDState
                              { _modelDState_rewards = rewards
                              }
                        }
                  },
              _modelEpochState_prevPp = prevPp
            }
      }
    )
  (toRational -> slotsPerEpoch)
  total
  (unboundRational . activeSlotVal -> activeSlotCoeff') = do
    let d = unboundRational $ GHC.getField @"_d" prevPp
        tau = unboundRational $ GHC.getField @"_tau" prevPp
        rho = unboundRational $ GHC.getField @"_rho" prevPp

        deltaR1 = rationalToCoin (min 1 eta * rho * coinToRational reserves)
        eta
          | d >= 0.8 = 1
          | otherwise = blocksMade / toRational (floor $ (1 - d) * slotsPerEpoch * activeSlotCoeff' :: Integer)
        rewardPot = feeSS <> deltaR1
        deltaT1 = rationalToCoin (tau * coinToRational rewardPot)
        r = rewardPot ~~ deltaT1
        circulation = total ~~ reserves
        blocksMade = toRational $ sum (unModelBlocksMade b)
    rs <- rewardModel prevPp b r (Map.keysSet rewards) poolSS stake delegs circulation
    let deltaR2 = r ~~ fold rs
    pure $
      ModelRewardUpdate
        { _modelRewardUpdate_treasury = deltaT1,
          _modelRewardUpdate_reserves = deltaR2 ~~ deltaR1,
          _modelRewardUpdate_rewards = mkGrpMap rs,
          _modelRewardUpdate_fees = invert feeSS
        }

rationalToCoin :: Rational -> Coin
rationalToCoin = Coin . floor

-- |
-- [SL-D5] Figure 46
-- [SL-D1] 5.5.2 f(s, σ)
maxPoolModel :: UsesModelPP pparams => pparams -> Coin -> PositiveRational -> UnitInterval -> Coin
maxPoolModel
  pp
  (coinToRational -> r)
  (unboundPositiveRational -> sigma)
  (unboundRational -> pr) =
    rationalToCoin (r / (1 + a0) * (sigma' + p' * a0 * (sigma' - p' * (z0 - sigma') / z0) / z0))
    where
      a0 = unboundRational $ GHC.getField @"_a0" pp
      nOpt = toRational $ GHC.getField @"_nOpt" pp
      z0 = 1 / nOpt
      sigma' = min sigma z0
      p' = min pr z0

-- |
-- [SL-D5] Figure 46
-- [SL-D1] 5.5.2 (p-hat)
mkApparentPerformance ::
  UnitInterval ->
  UnitInterval ->
  Natural ->
  Natural ->
  Rational
mkApparentPerformance (unboundRational -> d) (unboundRational -> sigma) (toRational -> n) (toRational -> nbar)
  | d < 0.8 = beta / sigma
  | otherwise = 1
  where
    beta = n / max 1 nbar

-- |
-- [SL-D5] Figure 47
-- [SL-D1] 5.5.3
r_operator :: MonadModelProvenance era provM => ModelSnapshotStake -> Coin -> ModelPoolParams era -> UnitInterval -> PositiveRational -> provM Coin
r_operator stk f_hat (ModelPoolParams {_mppId = poolId, _mppCost = c, _mppMargin = (unboundRational -> m), _mppRAcnt = ra}) (unboundRational -> s) (unboundPositiveRational -> sigma)
  | f_hat <= c = pure f_hat
  | otherwise = do
    rewardOperatorProvenance poolId $ Map.singleton ra $ _modelSnapshotStake_utxos stk
    pure $ c <> rationalToCoin ((coinToRational $ f_hat ~~ c) * (m + (1 - m) * s / sigma))

-- |
-- [SL-D5] Figure 47
-- [SL-D1] 5.5.3
r_member :: MonadModelProvenance era provM => (ModelCredential 'Staking (ScriptFeature era), ModelSnapshotStake) -> Coin -> ModelPoolParams era -> Rational -> PositiveRational -> provM Coin
r_member (hk, stk) f_hat (ModelPoolParams {_mppId = poolId, _mppCost = c, _mppMargin = (unboundRational -> m)}) t (unboundPositiveRational -> sigma)
  | f_hat <= c = pure $ Val.zero
  | otherwise = do
    rewardMemberProvenance poolId $ Map.singleton hk $ _modelSnapshotStake_utxos stk
    pure $ rationalToCoin ((coinToRational $ f_hat ~~ c) * (1 - m) * t / sigma)

unsafeFromRational :: forall r. (Typeable r, BoundedRational r) => String -> Rational -> r
unsafeFromRational clue x = maybe (error $ "unsafeFromRational@" <> show (typeRep (Proxy :: Proxy r)) <> " " <> clue <> " out of bounds:" <> show x) id $ boundRational x

-- Fig48 [SL-D5]
rewardOnePoolModel ::
  ( UsesModelPP pparams,
    MonadModelProvenance era provM
  ) =>
  pparams ->
  Coin ->
  Natural ->
  Natural ->
  ModelPoolParams era ->
  Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake ->
  PositiveRational ->
  UnitInterval ->
  Coin ->
  Set (ModelCredential 'Staking (ScriptFeature era)) ->
  provM (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
rewardOnePoolModel
  pp
  r
  n
  nbar
  pool@(ModelPoolParams {_mppPledge = (coinToRational -> pledge)})
  stake
  sigma
  sigma_a
  (coinToRational -> tot)
  addrs_rew = do
    let poolOwners = Set.fromList $ fmap liftModelCredential $ _mppOwners pool
        ostake' = fold $ Map.restrictKeys stake poolOwners
        ostake = coinToRational $ _modelSnapshotStake_balance ostake'
        p_r = unsafeFromRational "rewardOnePoolModel::p_r" $ pledge / tot

        maxP
          | pledge <= ostake = maxPoolModel pp r sigma p_r
          | otherwise = Val.zero

        appPerf = mkApparentPerformance (GHC.getField @"_d" pp) sigma_a n nbar
        poolR = rationalToCoin (appPerf * coinToRational maxP)

    mRewards <-
      fmap Map.fromList $
        sequence
          [ (,) hk <$> r_member (hk, stk) poolR pool c_over_tot sigma
            | (hk, stk) <- Map.toList stake,
              let c = coinToRational $ _modelSnapshotStake_balance stk,
              let c_over_tot = c / tot,
              not (Set.member hk poolOwners)
          ]
    iReward <-
      r_operator
        ostake'
        poolR
        pool
        (unsafeFromRational "rewardOnePoolModel::ostake/tot" $ ostake / tot)
        sigma
    let potentialRewards = Map.unionWith (<>) mRewards (Map.singleton (_mppRAcnt pool) iReward)
        rewards = Map.restrictKeys potentialRewards addrs_rew
    pure rewards

-- |
-- [SL-D5] Figure 34
poolStakeModel ::
  ModelPoolId ->
  Map.Map (ModelCredential 'Staking sf) ModelPoolId ->
  Map.Map (ModelCredential 'Staking sf) a ->
  Map.Map (ModelCredential 'Staking sf) a
poolStakeModel hk delegs stake = Map.intersection stake (Map.filter (== hk) delegs)

type UsesModelPP pparams =
  ( GHC.HasField "_d" pparams UnitInterval,
    GHC.HasField "_tau" pparams UnitInterval,
    GHC.HasField "_a0" pparams NonNegativeInterval,
    GHC.HasField "_rho" pparams UnitInterval,
    GHC.HasField "_nOpt" pparams Natural,
    GHC.HasField "_protocolVersion" pparams ProtVer
  )

data ClippedRational a = Underflow | InBounds a | Overflow

boundRational' :: forall r. (Typeable r, BoundedRational r) => Rational -> ClippedRational r
boundRational' x
  | x < unboundRational (minBound :: r) = Underflow
  | x > unboundRational (maxBound :: r) = Overflow
  | otherwise = InBounds $ unsafeFromRational "boundRational'" x

-- Fig48[SL-D5]
rewardModel ::
  forall era pparams provM.
  ( UsesModelPP pparams,
    MonadModelProvenance era provM
  ) =>
  pparams ->
  ModelBlocksMade ->
  Coin ->
  Set (ModelCredential 'Staking (ScriptFeature era)) ->
  Map.Map ModelPoolId (ModelPoolParams era) ->
  Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake ->
  Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelPoolId ->
  Coin ->
  provM (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
rewardModel
  pp
  (ModelBlocksMade blocks)
  r
  addrs_rew
  poolParams
  stake
  delegs
  total = do
    let total_a = coinToRational $ foldMap _modelSnapshotStake_balance stake
        nbar = sum blocks
        pdata :: Map.Map ModelPoolId (ModelPoolParams era, Natural, Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake)
        pdata =
          Map.intersectionWithKey
            (\hk p n -> (p, n, poolStakeModel hk delegs stake))
            poolParams
            blocks

    results ::
      Map.Map ModelPoolId (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin) <-
      Map.fromList
        <$> sequence
          [ (,) hk <$> rewardOnePoolModel pp r n nbar p s sigma sigma_a total addrs_rew
            | (hk, (p, n, s)) <- Map.toList pdata,
              let sbar = coinToRational (foldMap _modelSnapshotStake_balance s),
              total_a > 0,
              sigma_a <- case boundRational' $ sbar / total_a of
                Underflow -> []
                InBounds x -> [x]
                Overflow -> error $ "sigma_a overflow: " <> show (sbar / total_a),
              sigma <- toList $ boundPositiveRational (sbar / coinToRational total)
          ]
    let rewards = Map.unionsWith (<>) results
    pure rewards
