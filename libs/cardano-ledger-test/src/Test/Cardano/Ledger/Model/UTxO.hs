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
import Control.DeepSeq (NFData)
import Control.Lens
  ( At (..),
    Index,
    IxValue,
    Ixed,
    Lens',
    ifoldMap,
    set,
    (%=),
    (<>=),
    _1,
    _2,
  )
import Control.Monad (guard)
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
import Test.Cardano.Ledger.Model.BaseTypes (ModelValue, PreservedAda (..))
import Test.Cardano.Ledger.Model.FeatureSet
  ( KnownScriptFeature,
    ScriptFeature,
    ValueFeature,
    ifSupportsPlutus,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
    canBeUsedAsCollateral,
  )

data ModelUTxOMap era = ModelUTxOMap
  { _modelUTxOMap_utxos :: !(Map.Map ModelUTxOId (ModelTxOut era)),
    _modelUTxOMap_stake :: !(GrpMap (ModelCredential 'Staking (ScriptFeature era)) (Coin, Set ModelUTxOId)),
    _modelUTxOMap_collateralUtxos :: !(Set ModelUTxOId),
    _modelUTxOMap_balance :: !(ModelValue (ValueFeature era) era)
  }
  deriving (Eq, Show, Generic)

instance PreservedAda (ModelUTxOMap era) where
  totalPreservedAda = Val.coin . _modelUTxOMap_balance

validModelUTxOMap :: ModelUTxOMap era -> Bool
validModelUTxOMap m = m == toModelUTxOMap (_modelUTxOMap_utxos m)

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
  (Map.Map ModelUTxOId (ModelTxOut era, ModelTxOut era), (ModelUTxOMap era))
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
          (Map.singleton ui txo)
          (grpMapSingleton (_modelAddress_stk ma) (val, Set.singleton ui))
          (bool Set.empty (Set.singleton ui) $ canBeUsedAsCollateral txo)
          (Val.inject val)

mkModelUTxOMap ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Map.Map ModelUTxOId (ModelAddress (ScriptFeature era), Coin) ->
  ModelUTxOMap era
mkModelUTxOMap =
  ifoldMap $ \ui (ma, val) ->
    let txo = ModelTxOut ma (Val.inject val) dh
     in ModelUTxOMap
          (Map.singleton ui txo)
          (grpMapSingleton (_modelAddress_stk ma) (val, Set.singleton ui))
          (bool Set.empty (Set.singleton ui) $ canBeUsedAsCollateral txo)
          (Val.inject val)
  where
    dh = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Nothing

spendModelUTxOs ::
  Set ModelUTxOId ->
  [(ModelUTxOId, ModelTxOut era)] ->
  ModelUTxOMap era ->
  ModelUTxOMap era
spendModelUTxOs ins outs xs =
  let ins' = Map.restrictKeys (_modelUTxOMap_utxos xs) ins
      outs' = Map.fromList outs
      newCollateral =
        foldMap
          ( \(ui, txo) ->
              bool Set.empty (Set.singleton ui) $
                canBeUsedAsCollateral txo
          )
          outs
      getStake :: Map.Map ModelUTxOId (ModelTxOut era) -> GrpMap (ModelCredential 'Staking (ScriptFeature era)) (Coin, Set ModelUTxOId)
      getStake = ifoldMap (\ui txo -> grpMapSingleton (_modelAddress_stk $ _mtxo_address txo) (Val.coin $ _mtxo_value txo, Set.singleton ui))

      mergeStake :: (Coin, Set ModelUTxOId) -> (Coin, Set ModelUTxOId) -> (Coin, Set ModelUTxOId)
      mergeStake (c, ui) (c', ui') = (c' ~~ c, Set.difference ui' ui)
   in ModelUTxOMap
        { _modelUTxOMap_utxos = Map.withoutKeys (_modelUTxOMap_utxos xs `Map.union` outs') ins,
          _modelUTxOMap_stake = flip State.execState (_modelUTxOMap_stake xs) $ do
            id <>= getStake outs'
            State.modify $ zipWithGrpMap mergeStake $ getStake ins',
          _modelUTxOMap_balance = _modelUTxOMap_balance xs <> foldMap _mtxo_value outs' ~~ foldMap _mtxo_value ins',
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
          let val' = foldMap _mtxo_value $ Map.lookup k $ _modelUTxOMap_utxos s
              val = foldMap _mtxo_value b
              hodler' = fmap (_modelAddress_stk . _mtxo_address) $ Map.lookup k $ _modelUTxOMap_utxos s
              hodler = _modelAddress_stk . _mtxo_address <$> b
           in ModelUTxOMap
                { _modelUTxOMap_utxos = set (at k) b (_modelUTxOMap_utxos s),
                  _modelUTxOMap_collateralUtxos =
                    set
                      (at k)
                      (guard =<< fmap canBeUsedAsCollateral b)
                      (_modelUTxOMap_collateralUtxos s),
                  _modelUTxOMap_stake = flip State.execState (_modelUTxOMap_stake s) $ do
                    for_ hodler' $ \h -> do
                      grpMap h . _1 <>= invert (Val.coin val')
                      grpMap h . _2 %= Set.delete k
                    for_ hodler $ \h -> do
                      grpMap h . _1 <>= Val.coin val
                      grpMap h . _2 %= Set.insert k,
                  _modelUTxOMap_balance = _modelUTxOMap_balance s <> val ~~ val'
                }
     in b2t <$> (a2fb a)
  {-# INLINE at #-}

getCoinUTxOsfromUTxoMap :: ModelUTxOMap era -> (Coin, Set ModelUTxOId)
getCoinUTxOsfromUTxoMap (ModelUTxOMap utxos _ _ balances) = (Val.coin balances, Map.keysSet utxos)
