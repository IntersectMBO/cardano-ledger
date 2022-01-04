{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Model.Generators.Shrinking where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Lens
  ( Lens',
    foldMapOf,
    has,
    ifoldMap,
    ifoldl',
    imap,
    itraverse,
    ix,
    lastOf,
    lens,
    over,
    preview,
    use,
    (%=),
    (<>=),
    (^.),
    _1,
    _2,
    _Just,
  )
import Control.Monad (ap)
import Control.Monad.Reader.Class (MonadReader (..), asks)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Foldable (fold, foldl', for_, toList, traverse_)
import Data.Functor.PiecewiseConstant (liftPiecewiseConstantMap)
import Data.Functor.Reverse (Reverse (..))
import Data.Group.GrpMap (GrpMap, mkGrpMap)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Any (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Model.API
  ( ModelBlock (..),
    ModelEpoch (..),
    ModelGenesis (..),
    applyModelEpoch,
    execModelMWithProv,
    mkModelLedger,
    modelGenesis_utxos,
  )
import Test.Cardano.Ledger.Model.BaseTypes (ModelPoolId (..))
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSupport (..),
    KnownRequiredFeatures,
    ScriptFeature,
    traverseSupportsFeature,
  )
import Test.Cardano.Ledger.Model.Fixup (fixupValues)
import Test.Cardano.Ledger.Model.Generators ()
import Test.Cardano.Ledger.Model.Generators.Chain ()
import Test.Cardano.Ledger.Model.LedgerState
  ( validateModelTx,
  )
import Test.Cardano.Ledger.Model.Prov
  ( EpochMap,
    ModelProvenanceState (..),
    emptyModelProvenanceState,
    modelProvenanceState_wdrlRewards,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential (..),
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelDelegCert (..),
    ModelDelegation (..),
    ModelPoolCert (..),
    ModelPoolParams (..),
    ModelTx (..),
    ModelTxId,
    getModelTxId,
    mkMintValue,
    modelIsValid,
    modelRedeemerPresent,
    modelTx_collateral,
    modelTx_dCert,
    modelTx_inputs,
    modelTx_mint,
    modelTx_outputs,
    modelTx_wdrl,
    modelTxs,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
    modelTxOut_data,
  )
import Test.Cardano.Ledger.Model.UTxO (ModelUTxOMap (..), mkModelUTxOMap, toModelUTxOMap)

-- | Shrink the epochs down to txns and each element will have its own prefix of txns, including all txns that came before.
-- This function also includes a prefix where there is a block with no transactions.
shrinkModelSimple ::
  (a, [ModelEpoch era]) ->
  [(a, [ModelEpoch era])]
shrinkModelSimple (genesis, epochs) = (,) genesis <$> (List.init $ shrinkModelEpochs epochs)
  where
    getBlockPrefixes ::
      ModelBlock era ->
      [ModelBlock era]
    getBlockPrefixes (ModelBlock slotNo txns) = (ModelBlock slotNo) <$> (List.inits txns)

    getEpochPrefixes ::
      ModelEpoch era ->
      [ModelEpoch era]
    getEpochPrefixes (ModelEpoch blocks blocksMade) =
      let blockPrefixes = snd $ foldl' (appendPrefixes getBlockPrefixes) ([], []) blocks
       in (flip ModelEpoch blocksMade) <$> blockPrefixes

    -- This function produces a tuple
    -- The first value is to keep track of any previous blocks/epochs
    -- The second value is the previous blocks/epochs plus the prefixes of each block/epoch
    appendPrefixes ::
      (b -> [b]) ->
      ([b], [[b]]) ->
      b ->
      ([b], [[b]])
    appendPrefixes getPrefixsFn (accumulatedXs, prefixList) x =
      let addPrevXsFn = (<>) accumulatedXs
          newPrefixes = addPrevXsFn <$> (pure <$> (getPrefixsFn x))
       in (accumulatedXs <> [x], prefixList <> newPrefixes)

    shrinkModelEpochs ::
      [ModelEpoch era] ->
      [[ModelEpoch era]]
    shrinkModelEpochs es = snd $ foldl' (appendPrefixes getEpochPrefixes) ([], []) es

data TxFieldChecks era = TxFieldChecks
  { txFieldChecks_UTxOIds :: Set.Set ModelUTxOId,
    txFieldChecks_Delegators :: Set.Set (ModelCredential 'Staking (ScriptFeature era)),
    txFieldChecks_Delegatees :: Set.Set ModelPoolId,
    txFieldChecks_ModelTxNo :: Set.Set ModelTxId
  }
  deriving (Show)

data TrackDeps era = TrackDeps
  { trackDeps_genesis :: ModelGenesis era,
    trackDeps_txFieldChecks :: TxFieldChecks era,
    trackDeps_modelLedger :: [ModelProvenanceState era]
  }
  deriving (Show)

-- gets the "provenance" of unwithdrawan rewards, by stakeholder.
getModelLedger_rewardsProv :: forall era. ModelProvenanceState era -> GrpMap (ModelCredential 'Staking (ScriptFeature era)) (Set.Set ModelUTxOId, Set.Set ModelTxId)
getModelLedger_rewardsProv prov =
  let getOneStk hk e = foldMap (fold . Map.lookup hk) $ snd $ Map.split e (_modelProvenanceState_reward prov)
      stk = imap getOneStk (_modelProvenanceState_wdrl prov)
      stk' = mkGrpMap stk
   in stk'

traverseReversedWithEpochNoMaybeTxs ::
  forall m era.
  Applicative m =>
  (EpochNo -> ModelTx era -> m (Maybe (ModelTx era))) ->
  [ModelEpoch era] ->
  m [ModelEpoch era]
traverseReversedWithEpochNoMaybeTxs f epochs =
  fmap getReverse $ itraverse (handleEpoch . toEnum) $ Reverse epochs
  where
    handleEpoch i (ModelEpoch blocks blocksMade) = ModelEpoch <$> (fmap getReverse $ traverse (handleBlock i) $ Reverse blocks) <*> pure blocksMade
    handleBlock i (ModelBlock slot txs) = ModelBlock slot <$> (fmap (catMaybes . getReverse) $ traverse (f i) $ Reverse txs)

type TxDependencies = (Set ModelUTxOId, Set ModelTxId)

newtype DiscardTransactionM era a = DiscardTransactionM
  {runDiscardTransactionM :: (ModelProvenanceState era, ModelUTxOMap era) -> TxDependencies -> (TxDependencies, Any, a)}
  deriving (Functor)

instance Monad (DiscardTransactionM era) where
  DiscardTransactionM xs >>= f = DiscardTransactionM $ \prov txDeps ->
    let (DiscardTransactionM ys) = f x
        (txDeps', discarded, x) = xs prov txDeps
        (txDeps'', discarded', y) = ys prov txDeps'
     in (txDeps'', discarded' <> discarded, y)

instance Applicative (DiscardTransactionM era) where
  pure x = DiscardTransactionM $ \_ txDeps -> (txDeps, mempty, x)
  (<*>) = ap

instance MonadReader (ModelProvenanceState era, ModelUTxOMap era) (DiscardTransactionM era) where
  ask = DiscardTransactionM (\prov s -> (s, mempty, prov))
  local f (DiscardTransactionM xs) = DiscardTransactionM (\prov s -> xs (f prov) s)

instance MonadWriter Any (DiscardTransactionM era) where
  tell a = DiscardTransactionM (\_ s -> (s, a, ()))
  listen (DiscardTransactionM xs) = DiscardTransactionM $ \prov s ->
    let (s', a, x) = xs prov s
     in (s', a, (x, a))
  pass (DiscardTransactionM xs) = DiscardTransactionM $ \prov s ->
    let (s', a, (x, f)) = xs prov s
     in (s', f a, x)

instance State.MonadState TxDependencies (DiscardTransactionM era) where
  get = DiscardTransactionM (\_ s -> (s, mempty, s))
  put s = DiscardTransactionM (\_ _ -> (s, mempty, ()))

discardOneTx ::
  (MonadWriter Any m, State.MonadState TxDependencies m, MonadReader (ModelProvenanceState era, ModelUTxOMap era) m) =>
  EpochNo ->
  ModelTx era ->
  m (Maybe (ModelTx era))
discardOneTx epochNo tx = do
  (uTxOIds, txIds) <- State.get
  if (Set.member (getModelTxId tx) txIds || not (null $ Set.intersection (Set.fromList $ fmap fst $ _mtxOutputs tx) uTxOIds))
    then do
      _1 <>= Map.keysSet (_mtxInputs tx)
      _1 <>= foldSupportsFeature (_mtxCollateral tx)
      dcerts' <- fmap catMaybes $ traverse (discardDCert epochNo $ getModelTxId tx) $ _mtxDCert tx
      wdrls' <- fmap (Map.mapMaybe id) $ itraverse (discardWdrls (getModelTxId tx)) $ _mtxWdrl tx
      let tx' = tx {_mtxDCert = dcerts', _mtxWdrl = wdrls'}
          unIsValid (IsValid x) = x
      utxos <- asks snd
      pure $
        Just $ case (unIsValid $ foldSupportsFeature (_mtxValidity tx'), unIsValid $ validateModelTx utxos tx') of
          (True, True) -> tx'
          (True, False) -> error "discardOneTx rendered tx invalid"
          (False, True) -> tx
          (False, False) -> tx'
    else do
      tell $ Any True
      pure Nothing

discardWdrls ::
  (MonadReader (ModelProvenanceState era, ModelUTxOMap era) m, State.MonadState TxDependencies m) =>
  ModelTxId ->
  ModelCredential 'Staking (ScriptFeature era) ->
  a ->
  m (Maybe a)
discardWdrls tx cred x = do
  ranges <- preview $ _1 . modelProvenanceState_wdrlRewards . ix cred . ix tx
  case ranges of
    Nothing -> pure Nothing
    Just (start, end) -> do
      allRewards <- asks $ _modelProvenanceState_reward . fst
      let (_, startRewards, mostRewards) = Map.splitLookup start allRewards
          (rewards, endRewards, _) = Map.splitLookup end mostRewards
          deps = fold $ foldMap (Map.lookup cred) $ (toList startRewards) <> (toList rewards) <> (toList endRewards)
      if (deps == mempty)
        then pure Nothing
        else do
          id <>= deps
          pure $ Just x

lookupEpochProv :: Ord k => k -> EpochNo -> Map.Map k (EpochMap (Maybe a)) -> Maybe a
lookupEpochProv k epochNo prov =
  let registrations = fromMaybe (pure Nothing) $ Map.lookup k prov
      dep = liftPiecewiseConstantMap registrations epochNo
   in dep

discardDCert :: forall era m a. (MonadReader (ModelProvenanceState era, ModelUTxOMap era) m, State.MonadState TxDependencies m) => EpochNo -> ModelTxId -> (ModelDCert era, a) -> m (Maybe (ModelDCert era, a))
discardDCert epochNo txNo (dcert, rdmr) =
  fmap (,rdmr) <$> case dcert of
    ModelCertDeleg cert ->
      case cert of
        ModelRegKey a -> do
          provReg <- asks $ _modelProvenanceState_regStake . fst
          pure $
            let dep = lookupEpochProv a epochNo provReg
             in if (Just txNo == dep)
                  then Just dcert
                  else Nothing
        ModelDeRegKey _ ->
          -- TODO: This depends on rewards being zero, not currently tracked
          pure $ Just dcert
        ModelDelegate (ModelDelegation stake pool) -> do
          provDeleg <- asks $ _modelProvenanceState_deleg . fst
          let dep = lookupEpochProv stake epochNo provDeleg
          if (Just txNo == dep)
            then do
              provStake <- asks $ _modelProvenanceState_regStake . fst
              provPool <- asks $ _modelProvenanceState_regPool . fst
              for_ (lookupEpochProv stake epochNo provStake) $ \txId' -> _2 %= Set.insert txId'
              for_ (lookupEpochProv pool epochNo provPool) $ \txId' -> _2 %= Set.insert txId'
              pure $ Just dcert
            else pure Nothing
        ModelDCertGenesis _ -> pure $ Just dcert
        ModelDCertMir _ -> pure $ Just dcert
    ModelCertPool cert ->
      case cert of
        ModelRegPool a -> do
          provReg <- asks $ _modelProvenanceState_regPool . fst
          pure $
            if (Just txNo == lookupEpochProv (_mppId a) epochNo provReg)
              || (Just txNo == lookupEpochProv (_mppId a) (epochNo + 1) provReg)
              then Just dcert
              else Nothing
        ModelRetirePool _ _ -> pure $ Just dcert

discardUnnecessaryTxns ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (ModelGenesis era, [ModelEpoch era])
discardUnnecessaryTxns _ (_, []) = Nothing
discardUnnecessaryTxns globals (genesis, epochs) =
  fmap (fixupValues "discard" globals) $ discardUnnecessaryTxnsImpl globals (genesis, epochs)

discardUnnecessaryTxnsImpl ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (ModelGenesis era, [ModelEpoch era])
discardUnnecessaryTxnsImpl globals (genesis, epochs) =
  let DiscardTransactionM f = traverseReversedWithEpochNoMaybeTxs discardOneTx epochs
      (prov, _) = execModelMWithProv (traverse_ applyModelEpoch epochs) globals (emptyModelProvenanceState, mkModelLedger globals genesis)
      utxoMap =
        foldMapOf
          (traverse . modelTxs)
          ( \tx ->
              if foldSupportsFeature (_mtxValidity tx) == IsValid True
                then toModelUTxOMap $ Map.fromList $ _mtxOutputs tx
                else mempty
          )
          epochs
          <> mkModelUTxOMap (_modelGenesis_utxos genesis)
      initialTxDeps =
        foldMap (Set.singleton . getModelTxId) (lastOf (traverse . modelTxs) epochs)
          <> fold (_modelProvenanceState_ir prov) -- TODO: this just keeps every MIR unconditionally; but we can probably do better than that.
      ((utxoDeps, _), Any didShrink, epochs') = f (prov, utxoMap) (Set.empty, initialTxDeps)
      genesis' = genesis {_modelGenesis_utxos = Map.restrictKeys (_modelGenesis_utxos genesis) utxoDeps}
   in if didShrink
        then Just (genesis', epochs')
        else Nothing

traverseMaybeTxs :: forall m era. Applicative m => (ModelTx era -> m (Maybe (ModelTx era))) -> [ModelEpoch era] -> m [ModelEpoch era]
traverseMaybeTxs f = traverse f'
  where
    f' :: ModelEpoch era -> m (ModelEpoch era)
    f' (ModelEpoch bs bsMade) = ModelEpoch <$> (traverse f'' bs) <*> pure bsMade
    f'' :: ModelBlock era -> m (ModelBlock era)
    f'' (ModelBlock slot txs) = ModelBlock slot <$> (catMaybes <$> traverse f txs)

data CollapseTransactionsState era = CollapseTransactionsState
  { _cts_Gen :: ModelGenesis era,
    _cts_RequiredUTxOs :: Set.Set ModelUTxOId
  }

cts_Gen :: Lens' (CollapseTransactionsState era) (ModelGenesis era)
cts_Gen = lens _cts_Gen (\s b -> s {_cts_Gen = b})
{-# INLINE cts_Gen #-}

cts_RequiredUTxOs :: Lens' (CollapseTransactionsState era) (Set.Set ModelUTxOId)
cts_RequiredUTxOs = lens _cts_RequiredUTxOs (\s b -> s {_cts_RequiredUTxOs = b})
{-# INLINE cts_RequiredUTxOs #-}

newtype CollapseTransactionsM era a = CollapseTransactionsM
  {runCollapseTransactionsM :: CollapseTransactionsState era -> (Any, CollapseTransactionsState era, a)}
  deriving (Functor)

reportCollapse :: Bool -> CollapseTransactionsM era ()
reportCollapse collapsed = CollapseTransactionsM (\s -> (Any collapsed, s, ()))

instance State.MonadState (CollapseTransactionsState era) (CollapseTransactionsM era) where
  get = CollapseTransactionsM (\s -> (mempty, s, s))
  put s = CollapseTransactionsM (\_ -> (mempty, s, ()))

instance Monad (CollapseTransactionsM era) where
  CollapseTransactionsM xs >>= f =
    CollapseTransactionsM
      ( \mGen ->
          let (anyBools, mGen', x) = xs mGen
              (anyBools', mGen'', y) = runCollapseTransactionsM (f x) mGen'
           in (anyBools' <> anyBools, mGen'', y)
      )

instance Applicative (CollapseTransactionsM era) where
  pure x = CollapseTransactionsM (\mGen -> (mempty, mGen, x))
  (<*>) = ap

txOutputToGenesis ::
  (ModelUTxOId, ModelTxOut era') ->
  Map.Map ModelUTxOId (ModelAddress (ScriptFeature era'), Coin)
txOutputToGenesis (uTxOId, ModelTxOut (ModelAddress pmt stk) value _) =
  let newPmtKey = fixPmtCred pmt uTxOId
   in Map.singleton uTxOId (ModelAddress newPmtKey stk, Val.coin value)

fixPmtCred ::
  ModelCredential 'Payment k ->
  ModelUTxOId ->
  ModelCredential 'Payment k
fixPmtCred (ModelScriptHashObj _) uid = modifyPmtCred uid
fixPmtCred cred@(ModelKeyHashObj _) _ = cred

modifyPmtCred ::
  ModelUTxOId ->
  ModelCredential 'Payment k
modifyPmtCred uid = ModelKeyHashObj $ "modifiedPmt:" <> show uid

txPossibleOutputs ::
  forall era'.
  ModelTx era' ->
  (Set.Set ModelUTxOId, Set.Set ModelUTxOId, [(ModelUTxOId, ModelTxOut era')])
txPossibleOutputs tx =
  let collateral = foldSupportsFeature $ tx ^. modelTx_collateral
      txIns = Map.keysSet $ tx ^. modelTx_inputs
   in if modelIsValid tx
        then (txIns, collateral, tx ^. modelTx_outputs)
        else (collateral, txIns, [])

checkForDuplicateGenesis ::
  forall sf.
  ModelUTxOId ->
  (Set (ModelAddress sf), Map.Map ModelUTxOId (ModelAddress sf, Coin)) ->
  (ModelAddress sf, Coin) ->
  (Set (ModelAddress sf), Map.Map ModelUTxOId (ModelAddress sf, Coin))
checkForDuplicateGenesis uTxOId (allPmts, prevGenesis) originalGen@(addr@(ModelAddress _ stk), coin) =
  let modifiedGenesis =
        case Set.member addr allPmts of
          False -> originalGen
          True -> (ModelAddress (modifyPmtCred uTxOId) stk, coin)
   in ( Set.insert (fst modifiedGenesis) allPmts,
        Map.insert uTxOId modifiedGenesis prevGenesis
      )

fixAddrCollisions ::
  forall sf.
  Map.Map ModelUTxOId (ModelAddress sf, Coin) ->
  Map.Map ModelUTxOId (ModelAddress sf, Coin)
fixAddrCollisions genesisToCheck = snd $ ifoldl' checkForDuplicateGenesis (Set.empty, Map.empty) genesisToCheck

spendGenesis ::
  forall sf.
  Set.Set ModelUTxOId ->
  Map.Map ModelUTxOId (ModelAddress sf, Coin) ->
  Map.Map ModelUTxOId (ModelAddress sf, Coin)
spendGenesis txInputs allGenesis = Map.withoutKeys allGenesis txInputs

collapseOneTx ::
  forall era.
  ModelTx era ->
  ModelTx era ->
  CollapseTransactionsM era (Maybe (ModelTx era))
collapseOneTx lastTx tx = do
  allGenesis <- use $ cts_Gen . modelGenesis_utxos
  reqUTxOs <- use cts_RequiredUTxOs
  let genesisUTxOIds = Map.keysSet allGenesis
      txInputs =
        Set.union
          (Map.keysSet $ tx ^. modelTx_inputs)
          (foldSupportsFeature $ tx ^. modelTx_collateral)
      intersectingUTxOIds = Set.difference txInputs genesisUTxOIds

      mTxOutsData =
        has
          (modelTx_outputs . traverse . _2 . modelTxOut_data . traverseSupportsFeature . _Just)
          tx

      isLastTx = getModelTxId lastTx == getModelTxId tx

      lastTxScriptInputs =
        ifoldMap
          (\utxoId r -> if modelRedeemerPresent r then Set.singleton utxoId else mempty)
          (_mtxInputs lastTx)
      lastTxScriptIntersection =
        Set.intersection lastTxScriptInputs $
          Set.fromList (fst <$> tx ^. modelTx_outputs)

      isMinting = (/= mempty) $ mkMintValue @era $ tx ^. modelTx_mint

      collapseConditions =
        null intersectingUTxOIds
          && not isLastTx
          && not mTxOutsData
          && not isMinting
          && null lastTxScriptIntersection
          && (null $ tx ^. modelTx_dCert)
          && (null $ tx ^. modelTx_wdrl)
          && (null $ Set.intersection reqUTxOs spentIds)

      (spentIds, unspentIds, txOuts) = txPossibleOutputs tx

  cts_RequiredUTxOs %= flip Set.difference spentIds
  reportCollapse collapseConditions
  case collapseConditions of
    False -> do
      cts_RequiredUTxOs %= Set.union unspentIds
      pure $ Just tx
    True -> do
      let txOutputsGenesis = foldMap txOutputToGenesis txOuts
      cts_Gen . modelGenesis_utxos %= (<> txOutputsGenesis) . spendGenesis spentIds
      pure Nothing

collapseTransactions ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (ModelGenesis era, [ModelEpoch era])
collapseTransactions _ (_, []) = Nothing
collapseTransactions globals (genesis, epochs) =
  -- better way to do mLastTx business?
  let mLastTx = lastOf (traverse . modelTxs) epochs
      (Any collapsed, CollapseTransactionsState newGenesis _, newEpochs) =
        case mLastTx of
          Nothing -> (mempty, CollapseTransactionsState genesis Set.empty, epochs)
          Just lastTx ->
            runCollapseTransactionsM (traverseMaybeTxs (collapseOneTx lastTx) epochs) $
              CollapseTransactionsState genesis Set.empty
   in if collapsed
        then
          let newGenesis' = over modelGenesis_utxos fixAddrCollisions newGenesis
              result = fixupValues "collapse" globals (newGenesis', newEpochs)
           in Just result
        else Nothing

data ModelShrinkingPhase
  = NotShrunk
  | DiscardUnusedTxs
  | CollapsedGenesisTxs
  deriving (Show)

instance NFData ModelShrinkingPhase where
  rnf = rwhnf

shrinkModel ::
  KnownRequiredFeatures era =>
  Globals ->
  ( ModelShrinkingPhase,
    ( ModelGenesis era,
      [ModelEpoch era]
    )
  ) ->
  [ ( ModelShrinkingPhase,
      ( ModelGenesis era,
        [ModelEpoch era]
      )
    )
  ]
shrinkModel globals =
  shrinkPhases
    shrinkModelSimple
    (toList . discardUnnecessaryTxns globals)
    (toList . collapseTransactions globals)
  where
    shrinkPhases f _ _ (NotShrunk, x) = (,) DiscardUnusedTxs <$> (f x)
    shrinkPhases _ g _ (DiscardUnusedTxs, x) = (,) CollapsedGenesisTxs <$> (g x)
    shrinkPhases _ _ h (CollapsedGenesisTxs, x) = (,) CollapsedGenesisTxs <$> (h x)
