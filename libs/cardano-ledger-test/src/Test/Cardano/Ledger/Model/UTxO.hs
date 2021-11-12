{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Model.UTxO where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Control.Lens
import qualified Control.Monad.State.Strict as State
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Group (Group (..))
import Data.Group.GrpMap
  ( GrpMap,
    grpMap,
    grpMapSingleton,
    zipWithGrpMap,
  )
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Model.BaseTypes (PreservedAda (..))
import Test.Cardano.Ledger.Model.FeatureSet
  ( KnownScriptFeature,
    ScriptFeature,
    ifSupportsPlutus,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential,
    modelAddress_pmt,
    _ModelKeyHashObj,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
    modelTxOut_address,
  )

data ModelUTxOMap era = ModelUTxOMap
  { _modelUTxOMap_utxos :: !(Map.Map ModelUTxOId (Coin, ModelTxOut era)),
    _modelUTxOMap_stake :: !(GrpMap (ModelCredential 'Staking (ScriptFeature era)) (Coin, Set ModelUTxOId)),
    _modelUTxOMap_collateralUtxos :: !(Set ModelUTxOId),
    _modelUTxOMap_balance :: !Coin -- TODO: ModelValueF
  }
  deriving (Eq, Show, Generic)

instance PreservedAda (ModelUTxOMap era) where
  totalPreservedAda = _modelUTxOMap_balance

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
    let val = Val.coin mval
     in ModelUTxOMap
          (Map.singleton ui (val, txo))
          (grpMapSingleton (_modelAddress_stk ma) (val, Set.singleton ui))
          (bool Set.empty (Set.singleton ui) $ has (modelAddress_pmt . _ModelKeyHashObj) ma)
          val

mkModelUTxOMap ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Map.Map ModelUTxOId (ModelAddress (ScriptFeature era), Coin) ->
  ModelUTxOMap era
mkModelUTxOMap =
  ifoldMap $ \ui (ma, val) ->
    ModelUTxOMap
      (Map.singleton ui (val, ModelTxOut ma (Val.inject val) dh))
      (grpMapSingleton (_modelAddress_stk ma) (val, Set.singleton ui))
      (bool Set.empty (Set.singleton ui) $ has (modelAddress_pmt . _ModelKeyHashObj) ma)
      val
  where
    dh = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Nothing

getModelUTxOMapTotalAda :: ModelUTxOMap era -> Coin
getModelUTxOMapTotalAda = _modelUTxOMap_balance

spendModelUTxOs ::
  Set ModelUTxOId ->
  [(ModelUTxOId, ModelTxOut era)] ->
  ModelUTxOMap era ->
  ModelUTxOMap era
spendModelUTxOs ins outs xs =
  let ins' = Map.restrictKeys (_modelUTxOMap_utxos xs) ins
      outs' = Map.fromList $ (fmap . fmap) (Val.coin . _mtxo_value &&& id) outs
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
              val = foldMap (Val.coin . _mtxo_value) b
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

getCoinUTxOsfromUTxoMap :: ModelUTxOMap era -> (Coin, Set ModelUTxOId)
getCoinUTxOsfromUTxoMap (ModelUTxOMap utxos _ _ balances) = (balances, Map.keysSet utxos)
