{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ModelChain.Shrinking where

import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Keys (KeyRole (..))
import Control.DeepSeq
import Control.Lens
import Control.Monad (ap)
import qualified Control.Monad.State.Strict as State
import Data.Foldable
import Data.Group.GrpMap
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Monoid (Any (..))
import qualified Data.Set as Set
import Test.Cardano.Ledger.DependGraph (FixupValuesErrors, FixupValuesFlags (..), fixupValues)
import Test.Cardano.Ledger.Elaborators.Alonzo ()
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.FeatureSet
import Test.Cardano.Ledger.ModelChain.Script
import Test.Cardano.Ledger.ModelChain.Value

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

discardUnnecessaryTxns ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (ModelGenesis era, [ModelEpoch era])
discardUnnecessaryTxns a b = snd <$> discardUnnecessaryTxnsWithErrors a b

discardUnnecessaryTxnsWithErrors ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (Maybe (FixupValuesErrors era), (ModelGenesis era, [ModelEpoch era]))
discardUnnecessaryTxnsWithErrors _ (_, []) = Nothing
discardUnnecessaryTxnsWithErrors globals (genesis, epochs) =
  let lastEpochsTxns = toListOf modelTxs $ last epochs
      prevModelLedgers = snd $ foldl' createLedgers ((emptyModelProvenanceState, emptyLedger genesis), []) $ init' epochs
      (initialTxFieldChecks, initialTxn) = getInitialInfo genesis prevModelLedgers lastEpochsTxns

      (_, shrunkenEpochs) =
        foldr
          checkEpoch
          (TrackDeps genesis initialTxFieldChecks prevModelLedgers, [])
          epochs
      lastOfShrunkenEpochs = last shrunkenEpochs
      lastEpoch = lastOfShrunkenEpochs {_modelEpoch_blocks = addInitialTxn (_modelEpoch_blocks lastOfShrunkenEpochs) initialTxn}

      newEpochs = (init shrunkenEpochs) <> [lastEpoch]
   in if (epochs == newEpochs)
        then Nothing
        else Just $ fixupValues (FixupValuesFlags False) globals (genesis, newEpochs)
  where
    init' [] = []
    init' xs = init xs
    last' [] = Nothing
    last' xs = Just $ last xs

    createLedgers :: forall era'. KnownRequiredFeatures era' => ((ModelProvenanceState era', ModelLedger era'), [ModelProvenanceState era']) -> ModelEpoch era' -> ((ModelProvenanceState era', ModelLedger era'), [ModelProvenanceState era'])
    createLedgers (currentLedger, ledgers) e =
      let newLedger = execModelMWithProv (applyModelEpoch e) globals currentLedger
       in (newLedger, ledgers <> [fst newLedger])

    emptyLedger ::
      forall era'.
      KnownScriptFeature (ScriptFeature era') =>
      ModelGenesis era' ->
      ModelLedger era'
    emptyLedger = mkModelLedger globals

    emptyTxFieldChecks :: forall era'. TxFieldChecks era'
    emptyTxFieldChecks = TxFieldChecks Set.empty Set.empty Set.empty Set.empty

    -- This function is to get the counter-example's TxFieldChecks and the offending transaction
    getInitialInfo ::
      forall era'.
      ModelGenesis era' ->
      [ModelProvenanceState era'] ->
      [ModelTx era'] ->
      (TxFieldChecks era', [ModelTx era'])
    getInitialInfo _ _ [] = (emptyTxFieldChecks, [])
    getInitialInfo genesis' ls ts =
      let lastTx = last ts
          inputs = lastTx ^. modelTx_inputs
          delegates = toListOf (modelTx_dCert . traverse . _ModelDelegate) lastTx
          txDelegators = Set.fromList $ _mdDelegator <$> delegates
          txDelegatees = Set.fromList $ _mdDelegatee <$> delegates

          -- here we are checking whether the delegations deps are within the same tx
          -- already keeping the initial tx
          (_, (TxFieldChecks _ delegatorsLeft delegateesLeft _)) =
            checkTxDelegationDeps
              (TxFieldChecks [] txDelegators txDelegatees [])
              (lastTx ^. modelTx_dCert)

          txWdrls = lastTx ^. modelTx_wdrl
          updatedFieldChecks =
            trackWdrlDeps txWdrls (TxFieldChecks inputs delegatorsLeft delegateesLeft []) $
              TrackDeps genesis' emptyTxFieldChecks ls

          updatedFieldChecks' = trackCollateralDeps lastTx updatedFieldChecks
       in (updatedFieldChecks', [lastTx])

    -- This function adds the orignal counter-example transaction into the shrunken Epochs
    addInitialTxn [] _ = []
    addInitialTxn bs t =
      let lastBlock = last bs
          updatedLastBlock = lastBlock {_modelBlock_txSeq = (_modelBlock_txSeq lastBlock) <> t}
       in (init bs) <> [updatedLastBlock]

    checkTxNo ::
      forall era'.
      ModelTxId ->
      TxFieldChecks era' ->
      (Bool, TxFieldChecks era')
    checkTxNo currentTxNo (TxFieldChecks utxos dors dees txNos) =
      let bMatchingTxNo = elem currentTxNo txNos
          newTxNos = Set.filter ((/=) currentTxNo) txNos
       in (bMatchingTxNo, TxFieldChecks utxos dors dees newTxNos)

    -- This function will return a bool to indicate whether to keep the tx and filter out any matching fields
    checkTxDelegationDeps ::
      forall era'.
      TxFieldChecks era' ->
      [ModelDCert era'] ->
      (Bool, TxFieldChecks era')
    checkTxDelegationDeps (TxFieldChecks uTxOIds prevDelegators prevDelegatees modelTxNos) dCerts =
      let delegateeIds = (fmap _mppId . toListOf (traverse . _ModelRegisterPool)) dCerts
          delegators = toListOf (traverse . _ModelRegisterStake) dCerts

          bMatchingDelegateeIds = not . null $ Set.intersection (Set.fromList delegateeIds) prevDelegatees
          bMatchingDelegators = not . null $ Set.intersection (Set.fromList delegators) prevDelegators
          updatedDelegateeIds = Set.difference prevDelegatees (Set.fromList delegateeIds)
          updatedDelegators = Set.difference prevDelegators (Set.fromList delegators)
       in ( bMatchingDelegateeIds || bMatchingDelegators,
            TxFieldChecks uTxOIds updatedDelegators updatedDelegateeIds modelTxNos
          )

    trackCollateralDeps ::
      forall era'.
      ModelTx era' ->
      TxFieldChecks era' ->
      TxFieldChecks era'
    trackCollateralDeps tx txFieldChecks =
      let oldUTxOs = txFieldChecks_UTxOIds txFieldChecks
          newUTxOs =
            case (tx ^. modelTx_collateral) of
              NoPlutusSupport _ -> mempty
              SupportsPlutus uTxOIds -> uTxOIds
       in txFieldChecks {txFieldChecks_UTxOIds = oldUTxOs <> newUTxOs}

    trackWdrlDeps ::
      forall era'.
      Map.Map (ModelCredential 'Staking (ScriptFeature era')) (ModelValue 'ExpectAdaOnly era') ->
      TxFieldChecks era' ->
      TrackDeps era' ->
      TxFieldChecks era'
    trackWdrlDeps wdrls txFieldChecks@(TxFieldChecks utxos delegators delegatees txNos) trackDeps
      | wdrls == Map.empty = txFieldChecks
      | otherwise =
        let maybeLedger = last' $ trackDeps_modelLedger trackDeps
            (newUtxos, newTxNos) =
              case maybeLedger of
                Nothing -> (utxos, txNos)
                Just currentMLedger ->
                  let wdrlAddrSet = Map.keysSet wdrls
                      currentMLedgerRewards = getModelLedger_rewardsProv currentMLedger
                      addrRewards = restrictKeysGrpMap currentMLedgerRewards wdrlAddrSet
                      (rewardModelUTxOIds', rewardTxNos') = fold addrRewards
                   in (utxos <> rewardModelUTxOIds', txNos <> rewardTxNos')
         in TxFieldChecks newUtxos delegators delegatees newTxNos

    -- This function checks whether to keep a tx. Appends it to list of txs to keep and tracks it's dependencies
    checkTx ::
      forall era'.
      ModelTx era' ->
      (TrackDeps era', [ModelTx era']) ->
      (TrackDeps era', [ModelTx era'])
    checkTx txToCheck ogTuple@(TrackDeps genesis' txFieldChecks ledgers, txsToKeep) =
      -- Check if current tx outputs any tx of Interests
      let txOutputsUTxOIds = Set.fromList $ fst <$> txToCheck ^. modelTx_outputs
          trackedUTxOIds = txFieldChecks_UTxOIds txFieldChecks
          bMatchingUTxOIds = not . null $ Set.intersection txOutputsUTxOIds trackedUTxOIds

          txDCerts = toList $ txToCheck ^. modelTx_dCert
          (keepTx', updatedDelegFieldChecks) = checkTxDelegationDeps txFieldChecks txDCerts

          (keepTx, updatedDelegWdrlFieldChecks') =
            checkTxNo (getModelTxId txToCheck) updatedDelegFieldChecks
       in case (bMatchingUTxOIds || keepTx || keepTx') of
            False -> ((fst ogTuple), txsToKeep)
            _ ->
              -- tx is tx of interest
              let updatedTrackedUTxOIds = Set.difference trackedUTxOIds txOutputsUTxOIds
                  txInputs = txToCheck ^. modelTx_inputs
                  -- track inputs of tx
                  updatedUTxOIds = txInputs <> updatedTrackedUTxOIds

                  allUpdatedFieldChecks =
                    trackWdrlDeps (txToCheck ^. modelTx_wdrl) updatedDelegWdrlFieldChecks' $ fst ogTuple
                  (TxFieldChecks wdrlUTxOIds prevDelegators prevDelegatees txNos) =
                    trackCollateralDeps txToCheck allUpdatedFieldChecks

                  txDelegations = toListOf (traverse . _ModelDelegate) txDCerts
                  txDelegators = Set.fromList $ _mdDelegator <$> txDelegations
                  txDelegatees = Set.fromList $ _mdDelegatee <$> txDelegations
                  -- We want to check whether Delegation Deps are within the same tx's _mtxDCerts
                  (alreadyFound, (TxFieldChecks _ delegatorsLeft delegateesLeft _)) =
                    checkTxDelegationDeps
                      (TxFieldChecks [] txDelegators txDelegatees [])
                      txDCerts
                  -- Only add if not already found within same txn
                  (updatedDelegators, updatedDelegatees) = case alreadyFound of
                    True -> (prevDelegators <> delegatorsLeft, prevDelegatees <> delegateesLeft)
                    False -> (prevDelegators <> txDelegators, prevDelegatees <> txDelegatees)

                  newTxFieldChecks =
                    TxFieldChecks (updatedUTxOIds <> wdrlUTxOIds) updatedDelegators updatedDelegatees txNos
               in (TrackDeps genesis' newTxFieldChecks ledgers, txToCheck : txsToKeep)

    checkBlock ::
      forall era'.
      ModelBlock era' ->
      (TrackDeps era', [ModelBlock era']) ->
      (TrackDeps era', [ModelBlock era'])
    checkBlock (ModelBlock slotNo txs) (trackDeps, trackedBlocks) =
      let (newTrackDeps, newTxs) = foldr checkTx (trackDeps, []) txs
       in (newTrackDeps, (ModelBlock slotNo newTxs) : trackedBlocks)

    checkEpoch ::
      forall era'.
      ModelEpoch era' ->
      (TrackDeps era', [ModelEpoch era']) ->
      (TrackDeps era', [ModelEpoch era'])
    checkEpoch (ModelEpoch blocks blocksMade) (trackDeps, trackedEpochs) =
      let (TrackDeps genesis' newUTxOIds ledgers, newBlocks) = foldr checkBlock (trackDeps, []) blocks
       in (TrackDeps genesis' newUTxOIds $ init' ledgers, (ModelEpoch newBlocks blocksMade) : trackedEpochs)

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
  (ModelUTxOId, ModelAddress (ScriptFeature era'), Coin)
txOutputToGenesis (uTxOId, ModelTxOut (ModelAddress pmt stk) value _) =
  let newPmtKey = fixPmtCred pmt uTxOId
   in (uTxOId, ModelAddress newPmtKey stk, getModelValueCoin value)

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
  let collateral = fromSupportsPlutus (\() -> Set.empty) id $ tx ^. modelTx_collateral
      txIns = tx ^. modelTx_inputs
   in if modelIsValid tx
        then (txIns, collateral, tx ^. modelTx_outputs)
        else (collateral, txIns, [])

checkForDuplicateGenesis ::
  forall sf.
  [(ModelUTxOId, ModelAddress sf, Coin)] ->
  (ModelUTxOId, ModelAddress sf, Coin) ->
  [(ModelUTxOId, ModelAddress sf, Coin)]
checkForDuplicateGenesis prevGenesis originalGen@(uTxOId, ModelAddress pmt stk, coin) =
  let allPmts = (\(_, addr, _) -> view modelAddress_pmt addr) <$> prevGenesis
      modifiedGenesis =
        case (elem pmt allPmts) of
          False -> originalGen
          True -> (uTxOId, ModelAddress (modifyPmtCred uTxOId) stk, coin)
   in prevGenesis <> [modifiedGenesis]

fixAddrCollisions ::
  forall sf.
  [(ModelUTxOId, ModelAddress sf, Coin)] ->
  [(ModelUTxOId, ModelAddress sf, Coin)]
fixAddrCollisions genesisToCheck = foldl' (checkForDuplicateGenesis) [] genesisToCheck

spendGenesis ::
  forall sf.
  Set.Set ModelUTxOId ->
  [(ModelUTxOId, ModelAddress sf, Coin)] ->
  [(ModelUTxOId, ModelAddress sf, Coin)]
spendGenesis txInputs allGenesis = filter (\(x, _, _) -> not $ elem x txInputs) allGenesis

collapseOneTx ::
  ModelTx era ->
  ModelTx era ->
  CollapseTransactionsM era (Maybe (ModelTx era))
collapseOneTx lastTx tx = do
  allGenesis <- use $ cts_Gen . modelGenesis_utxos
  reqUTxOs <- use cts_RequiredUTxOs
  let genesisUTxOIds = Set.fromList $ (\(x, _, _) -> x) <$> allGenesis
      txInputs = Set.union (tx ^. modelTx_inputs) (fromSupportsPlutus (\() -> Set.empty) id $ tx ^. modelTx_collateral)
      intersectingUTxOIds = Set.difference txInputs genesisUTxOIds

      txOutsData = (\(_, x) -> view modelTxOut_data x) <$> (tx ^. modelTx_outputs)
      mTxOutsData = traverse (fromSupportsPlutus (const Nothing) id) txOutsData

      isLastTx = (_mtxInputs lastTx) == (_mtxInputs tx)
      lastTxScriptInputs =
        Set.fromList $
          mapMaybe
            ( \r -> case r of
                ModelScriptPurpose_Spending utxoId -> Just utxoId
                _ -> Nothing
            )
            $ Map.keys $ lastTx ^. modelTx_redeemers
      lastTxScriptIntersection =
        Set.intersection lastTxScriptInputs $
          Set.fromList (fst <$> tx ^. modelTx_outputs)

      isMinting =
        fromSupportsMint
          (const False)
          ( \x ->
              let eitherMValue = evalModelValueSimple $ unModelValue x
               in -- if it fails, do we throw an error?
                  either (const False) (\(ModelValueSimple (_, mGrpMap)) -> (not . null) mGrpMap) eitherMValue
          )
          $ tx ^. modelTx_mint

      collapseConditions =
        null intersectingUTxOIds
          && not isLastTx
          && isNothing mTxOutsData
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
      let txOutputsGenesis = txOutputToGenesis <$> txOuts
      cts_Gen . modelGenesis_utxos %= (<> txOutputsGenesis) . spendGenesis spentIds
      pure Nothing

collapseTransactions ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (ModelGenesis era, [ModelEpoch era])
collapseTransactions a b = snd <$> collapseTransactionsWithErrors a b

collapseTransactionsWithErrors ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Maybe (Maybe (FixupValuesErrors era), (ModelGenesis era, [ModelEpoch era]))
collapseTransactionsWithErrors _ (_, []) = Nothing
collapseTransactionsWithErrors globals (genesis, epochs) =
  -- better way to do mLastTx business?
  let mLastTx = last' $ fold $ (toListOf modelTxs) <$> epochs
      (Any collapsed, CollapseTransactionsState newGenesis _, newEpochs) =
        case mLastTx of
          Nothing -> (mempty, CollapseTransactionsState genesis Set.empty, epochs)
          Just lastTx ->
            runCollapseTransactionsM (traverseMaybeTxs (collapseOneTx lastTx) epochs) $
              CollapseTransactionsState genesis Set.empty
   in if collapsed
        then
          let newGenesis' = over modelGenesis_utxos fixAddrCollisions newGenesis
              result = fixupValues (FixupValuesFlags True) globals (newGenesis', newEpochs)
           in Just result
        else Nothing
  where
    last' [] = Nothing
    last' xs = Just $ last xs

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
