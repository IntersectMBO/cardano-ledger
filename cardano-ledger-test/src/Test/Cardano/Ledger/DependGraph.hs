{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.DependGraph where

import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Arrow ((&&&))
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.State
import Control.Monad.Supply
import Data.Either
import Data.Foldable
import Data.Functor.Compose
import qualified Data.Graph.Inductive as FGL
import Data.Group
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import QuickCheck.GenT
import qualified System.Random
import Test.Cardano.Ledger.Elaborators
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Control.Monad.Reader
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.FeatureSet
import Test.Cardano.Ledger.ModelChain.Value
import Test.QuickCheck hiding (choose, elements, frequency, oneof, resize, shuffle, sized, variant)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)

-- TODO: Handle anti-dependencies. e.g. retiring a pool precludes delegating to it
-- or receiving rewards from it. likewise deregistering a staking cert precludes
-- delegating or receiving rewards. Delegating the same staking cert again
-- nullifies the previous delegation.

-- TODO: Generate withdrawal actions that occur in different epochs but are part of the same
-- stake delegation, i.e. they withdraw into the same rewards account.

-- TODO: Model scenarios where pools make varying amounts of blocks (e.g. 0 or many)

-- | Nodes in an abstract graph of actions recorded in a ledger.
-- This is finer grained than transactions since a transaction is a set
-- of coherent actions.
data Action
  = Action_Withdraw
  | Action_RegisterStake
  | Action_RegisterPool
  | Action_Delegate
  | Action_Spend
  deriving (Eq, Ord, Show)

-- | Directed edges in an abstract graph of action recorded in a ledger.
-- Edges record how an action enables a later action to be taken.
-- For example, you cannot expect to withdraw a reward unless you're
-- staked to an active pool.
data ActionEnables
  = ActionEnables_Spending
  | ActionEnables_StakeForDelegating
  | ActionEnables_PoolForDelegating
  | ActionEnables_Withdrawing
  deriving (Eq, Ord, Show)

-- A node in the graph is an action in an epoch. We need the epoch to ensure consistency of
-- dependencies between rewards and delegation.
newtype ActionGraph = ActionGraph {unActionGraph :: FGL.Gr (EpochNo, Action) ActionEnables}

data GenActionContextF f = GenActionContext
  { _genActionContexts_epochs :: f EpochNo,
    _genActionContexts_actionsPerEpoch :: f Int,
    _genActionContexts_inputs :: f Int
  }

type GenActionContext = GenActionContextF Gen

defaultGenActionContext :: GenActionContext
defaultGenActionContext = GenActionContext
  (choose (1, 5))
  (choose (2, 8)) -- 128))
  (pure 1) -- (choose (1, 5))

data ActionState = ActionState
  { -- | Lazy supply of Ints
    _actionState_freshNames :: Int,
    -- | Node ID to label and in-edges
    _actionState_withDeps :: Map ActionId (EpochNo, Action, Map ActionId ActionEnables),
    -- | Epoch number to set of node ids
    _actionState_needsDelegate :: Map EpochNo (Set ActionId),
    -- | Node IDs
    _actionState_needsStake :: Set ActionId,
    -- | Node IDs
    _actionState_needsPool :: Set ActionId,
    -- | Node ID to number of inputs needed
    _actionState_needsInputs :: Set ActionId
  }
  deriving (Eq, Ord, Show)

actionState_freshNames :: Lens' ActionState (Int)
actionState_freshNames a2fb s = (\b -> s {_actionState_freshNames = b}) <$> a2fb (_actionState_freshNames s)
{-# INLINE actionState_freshNames #-}

actionState_withDeps :: Lens' ActionState (Map ActionId (EpochNo, Action, Map ActionId ActionEnables))
actionState_withDeps a2fb s = (\b -> s {_actionState_withDeps = b}) <$> a2fb (_actionState_withDeps s)
{-# INLINE actionState_withDeps #-}

actionState_needsDelegate :: Lens' ActionState (Map EpochNo (Set ActionId))
actionState_needsDelegate a2fb s = (\b -> s {_actionState_needsDelegate = b}) <$> a2fb (_actionState_needsDelegate s)
{-# INLINE actionState_needsDelegate #-}

actionState_needsStake :: Lens' ActionState (Set ActionId)
actionState_needsStake a2fb s = (\b -> s {_actionState_needsStake = b}) <$> a2fb (_actionState_needsStake s)
{-# INLINE actionState_needsStake #-}

actionState_needsPool :: Lens' ActionState (Set ActionId)
actionState_needsPool a2fb s = (\b -> s {_actionState_needsPool = b}) <$> a2fb (_actionState_needsPool s)
{-# INLINE actionState_needsPool #-}

actionState_needsInputs :: Lens' ActionState (Set ActionId)
actionState_needsInputs a2fb s = (\b -> s {_actionState_needsInputs = b}) <$> a2fb (_actionState_needsInputs s)
{-# INLINE actionState_needsInputs #-}

initialActionState :: ActionState
initialActionState = ActionState 1 mempty mempty mempty mempty mempty

testGenActions :: IO (Set ActionId, ActionGraph)
testGenActions = do
  s <- generate (execStateT (runReaderT genActions defaultGenActionContext) initialActionState)
  pure $ (Map.keysSet (_actionState_withDeps s), materializeActionGraph s)

materializeActionGraph :: ActionState -> ActionGraph
materializeActionGraph s = ActionGraph $
  FGL.buildGr $
    flip fmap (Map.toAscList (_actionState_withDeps s)) $
      \(ActionId aid, (epoch, alab, deps)) ->
        let aIn = flip fmap (Map.toList deps) $ \(ActionId inId, inLab) -> (inLab, inId)
         in (aIn, aid, (epoch, alab), [])

-- TODO: this generation method doesn't produce long-lived UTxOs which are common in a realistic ledger
outputTo :: (MonadReader GenActionContext m, MonadGen m, MonadState ActionState m) => ActionId -> Int -> Int -> Set ActionId -> m ()
outputTo aId minOutputs maxOutputs spenders = do
  -- First decide how many and which actions to enable
  numOutputs <- choose (min minOutputs 0, max maxOutputs (Set.size spenders))
  chosenSpenders <- take numOutputs <$> shuffle (Set.toList spenders)
  forM_ chosenSpenders $ \i -> do
    actionState_withDeps . at i . _Just . _3 . at aId .= Just ActionEnables_Spending
    mdeps <- fmap (view _3) <$> use (actionState_withDeps . at i)
    -- Decide whether or not this dependent is satisfied
    case mdeps of
      Nothing -> error "outputTo: impossible empty deps"
      Just deps -> do
        let numInputs = Map.size $ Map.filter (== ActionEnables_Spending) deps
        r <- join $ asks (liftGen . _genActionContexts_inputs)
        case r <= numInputs of
          True -> actionState_needsInputs %= (Set.delete i)
          False -> pure ()

makeWithdrawal :: (MonadGen m, MonadState ActionState m) => ActionId -> EpochNo -> Set ActionId -> m ()
makeWithdrawal aId epoch withdrawals = do
  case Set.toList withdrawals of
    [] -> pure ()
    _ : _ -> do
      wId <- elements (Set.toList withdrawals)
      actionState_withDeps . at wId . _Just . _3 . at aId .= Just ActionEnables_Withdrawing
      actionState_needsDelegate . at epoch . _Just %= Set.delete wId

-- TODO: Some of these actions should not be passed more than one dependent.
chooseEnablement :: (MonadReader GenActionContext m, MonadState ActionState m, MonadGen m) => Set ActionId -> a -> ActionId -> Action -> m ()
chooseEnablement targets _ i = \case
  Action_Withdraw -> outputTo i 1 1 targets
  Action_Delegate -> do
    forM_ (Set.toList targets) $ \target ->
      fmap (view _1) <$> (use (actionState_withDeps . at target)) >>= \case
        Nothing -> pure ()
        Just targetEpoch -> makeWithdrawal i targetEpoch (Set.singleton target)
  Action_Spend -> outputTo i (Set.size targets) (Set.size targets) targets
  Action_RegisterPool -> do
    forM_ (Set.toList targets) $ \target ->
      actionState_withDeps . at target . _Just . _3 . at i .= Just ActionEnables_PoolForDelegating
    actionState_needsPool %= (`Set.difference` targets)
  Action_RegisterStake -> do
    forM_ (Set.toList targets) $ \target ->
      actionState_withDeps . at target . _Just . _3 . at i .= Just ActionEnables_StakeForDelegating
    actionState_needsStake %= (`Set.difference` targets)

randomEnablement :: (MonadReader GenActionContext m, MonadState ActionState m, MonadGen m) => EpochNo -> ActionId -> Action -> m ()
randomEnablement epoch i = \case
  Action_Withdraw -> do
    -- Withdrawal produces outputs that can be spent
    spenders <- use actionState_needsInputs
    outputTo i 0 4 spenders
  Action_Delegate -> do
    -- Delegation can enable withdrawal of staking rewards
    -- A particular delegation can only enable a withdrawal once per epoch
    eligibleEpochs <- uses actionState_needsDelegate $ Map.filterWithKey (\e _ -> e >= epoch + 2)
    iforM_ eligibleEpochs $ \e withdrawals ->
      frequency
        [ (1, pure ()),
          (3, makeWithdrawal i e withdrawals)
        ]
  Action_Spend -> do
    -- Spends create outputs which can then be used in later spends
    spenders <- use actionState_needsInputs
    outputTo i 0 4 spenders
  Action_RegisterPool -> do
    -- Delegations depend on pools
    delegations <- use actionState_needsPool
    numDelegations <- choose (0, 8)
    poolDelegs <- take numDelegations <$> shuffle (Set.toList delegations)
    forM_ poolDelegs $ \dId -> do
      actionState_withDeps . at dId . _Just . _3 . at i .= Just ActionEnables_PoolForDelegating
    actionState_needsPool %= (`Set.difference` Set.fromList poolDelegs)
  Action_RegisterStake -> do
    -- Delegations depend on a staking certificate, which can only justify one
    -- delegation.
    delegations <- use actionState_needsStake
    numDelegations <-
      frequency
        [ (1, pure 0),
          (1 + 2 * Set.size delegations, pure 1)
        ]
    stakeDelegs <- take numDelegations <$> shuffle (Set.toList delegations)
    forM_ stakeDelegs $ \dId -> do
      actionState_withDeps . at dId . _Just . _3 . at i .= Just ActionEnables_StakeForDelegating
    actionState_needsStake %= (`Set.difference` Set.fromList stakeDelegs)

addAction :: MonadState ActionState m => (EpochNo -> ActionId -> Action -> m ()) -> EpochNo -> ActionId -> Action -> m ()
addAction enablement epoch i a = do
  actionState_withDeps . at i .= Just (epoch, a, mempty)
  -- Action specific logic. An action reports the dependencies it needs fulfilled
  -- and also chooses to fulfill some dependencies that have been emitted by actions
  -- that happen after it.
  enablement epoch i a
  actionState_needsInputs %= Set.insert i
  case a of
    Action_Withdraw -> do
      -- Withdrawal can only occur if a delegation was made in the past
      actionState_needsDelegate . at epoch %= Just . maybe (Set.singleton i) (Set.insert i)
    Action_Delegate -> do
      -- Delegation needs a staking certificate and registered pool
      actionState_needsStake %= Set.insert i
      actionState_needsPool %= Set.insert i
    Action_Spend -> pure ()
      -- Spending requires inputs, which potentially could come from the genesis block, but that's the only thing it requires.
    Action_RegisterPool -> pure ()
    Action_RegisterStake -> pure ()

fillDependencies :: (MonadReader GenActionContext m, MonadState ActionState m, MonadSupply Int m, MonadGen m) => m ()
fillDependencies = do
  -- Delegations first
  ds <- uses actionState_needsDelegate fold
  forM_ (Set.toList ds) $ \i -> do
    n <- ActionId <$> supply
    addAction (chooseEnablement (Set.singleton i)) 1 n Action_Delegate
  -- Use one pool for all the hanging threads
  do
    n <- ActionId <$> supply
    ps <- use actionState_needsPool
    addAction (chooseEnablement ps) 1 n Action_RegisterPool
  -- One staking cert per delegation
  ss <- use actionState_needsStake
  forM_ (Set.toList ss) $ \i -> do
    n <- ActionId <$> supply
    addAction (chooseEnablement (Set.singleton i)) 1 n Action_RegisterStake

genEpoch :: (MonadReader GenActionContext m, MonadGen m, MonadSupply Int m, MonadState ActionState m) => EpochNo -> m ()
genEpoch epoch = do
  numActions <- join $ asks $ liftGen . _genActionContexts_actionsPerEpoch
  replicateM_ numActions $ do
    -- We assign IDs to actions so that the lower IDs depend on
    -- higher IDs.
    i <- ActionId <$> supply
    a <-
      frequency
        -- Can't ever withdraw rewards before rewards
        -- could have been generated for a staking pool.
        [ (if epoch > 2 then 1 else 0, pure Action_Withdraw),
          (2, pure Action_RegisterStake),
          (2, pure Action_RegisterPool),
          (1, pure Action_Delegate),
          (12, pure Action_Spend)
        ]
    addAction randomEnablement epoch i a
  pure ()

genActions :: (MonadReader GenActionContext m, MonadGen m, MonadSupply Int m, MonadState ActionState m) => m ()
genActions = do
  -- First figure out how many epochs we're aiming for.
  -- We need to do this here because delegation to a pool
  -- has to happen some number of epochs (2) before reward
  -- withdrawal.
  numEpochs <- join $ asks $ liftGen . _genActionContexts_epochs
  forM_ [numEpochs, numEpochs -1 .. 1] genEpoch
  -- After random generation there might still be actions in the graph
  -- that don't have their dependencies met. Fill those in now in the first epoch.
  fillDependencies
  pure ()

minFee :: Integer
minFee = 100000

maxFee :: Integer
maxFee = 10 * minFee

-- When we materialize actions into concrete transactions, we gain more information about
-- how one action depends on another.
--
-- In particular, we know which reward address is used for withdrawals, delegation and staking
-- TODO: Enrich the enablement relation with more information, like UTxOIds
data TransactionEnables era
  = TransactionEnables_Spending
  | TransactionEnables_StakeForDelegating (ModelAddress (ScriptFeature era))
  | TransactionEnables_PoolForDelegating
  | TransactionEnables_Withdrawing (ModelAddress (ScriptFeature era))
  deriving (Eq, Ord, Show)

newtype ActionId = ActionId {unActionId :: FGL.Node}
  deriving (Eq, Ord, Show)

data TransactionState era = TransactionState
  { _transactionState_freshNames :: Integer,
    _transactionState_livePaymentAddresses :: Set (ModelAddress (ScriptFeature era)),
    _transactionState_liveRewardAddresses :: Set (ModelAddress (ScriptFeature era)),
    _transactionState_utxos :: Map ActionId (Map ModelUTxOId (ModelTxOut era)),
    _transactionState_txDependents :: Map ActionId (Map ModelTxId (NESet (TransactionEnables era))),
    _transactionState_genesisUtxo :: [(ModelUTxOId, (ModelAddress (ScriptFeature era)), Coin)],
    -- NB: In the action graph we recorded actions with their in-edges.
    -- Here we record transactions with their out-edges.
    -- It's a little more convenient for merging transactions together.
    _transactionState_withDepsInEpoch :: Map EpochNo (Map ModelTxId (ModelTx era, Map ModelTxId (NESet (TransactionEnables era))))
  }

transactionState_freshNames :: Lens' (TransactionState era) (Integer)
transactionState_freshNames a2fb s = (\b -> s {_transactionState_freshNames = b}) <$> a2fb (_transactionState_freshNames s)
{-# INLINE transactionState_freshNames #-}

transactionState_livePaymentAddresses :: Lens' (TransactionState era) (Set (ModelAddress (ScriptFeature era)))
transactionState_livePaymentAddresses a2fb s = (\b -> s {_transactionState_livePaymentAddresses = b}) <$> a2fb (_transactionState_livePaymentAddresses s)
{-# INLINE transactionState_livePaymentAddresses #-}

transactionState_liveRewardAddresses :: Lens' (TransactionState era) (Set (ModelAddress (ScriptFeature era)))
transactionState_liveRewardAddresses a2fb s = (\b -> s {_transactionState_liveRewardAddresses = b}) <$> a2fb (_transactionState_liveRewardAddresses s)
{-# INLINE transactionState_liveRewardAddresses #-}

transactionState_utxos :: Lens' (TransactionState era) (Map ActionId (Map ModelUTxOId (ModelTxOut era)))
transactionState_utxos a2fb s = (\b -> s {_transactionState_utxos = b}) <$> a2fb (_transactionState_utxos s)
{-# INLINE transactionState_utxos #-}

transactionState_txDependents :: Lens' (TransactionState era) (Map ActionId (Map ModelTxId (NESet (TransactionEnables era))))
transactionState_txDependents a2fb s = (\b -> s {_transactionState_txDependents = b}) <$> a2fb (_transactionState_txDependents s)
{-# INLINE transactionState_txDependents #-}

transactionState_genesisUtxo :: Lens' (TransactionState era) ([(ModelUTxOId, (ModelAddress (ScriptFeature era)), Coin)])
transactionState_genesisUtxo a2fb s = (\b -> s {_transactionState_genesisUtxo = b}) <$> a2fb (_transactionState_genesisUtxo s)
{-# INLINE transactionState_genesisUtxo #-}

transactionState_withDepsInEpoch :: Lens' (TransactionState era) (Map EpochNo (Map ModelTxId (ModelTx era, Map ModelTxId (NESet (TransactionEnables era)))))
transactionState_withDepsInEpoch a2fb s = (\b -> s {_transactionState_withDepsInEpoch = b}) <$> a2fb (_transactionState_withDepsInEpoch s)
{-# INLINE transactionState_withDepsInEpoch #-}

initialTransactionState :: TransactionState era
initialTransactionState =
  TransactionState
    { _transactionState_freshNames = 1,
      _transactionState_livePaymentAddresses = mempty,
      _transactionState_liveRewardAddresses = mempty,
      _transactionState_utxos = mempty,
      _transactionState_txDependents = mempty,
      _transactionState_genesisUtxo = mempty,
      _transactionState_withDepsInEpoch = mempty
    }

-- TODO: monoidal-containers
newtype MonMap k v = MonMap {unMonMap :: Map k v}
  deriving (Functor)

instance (Ord k, Semigroup v) => Semigroup (MonMap k v) where
  MonMap xs <> MonMap ys = MonMap $ Map.unionWith (<>) xs ys

instance (Ord k, Semigroup v) => Monoid (MonMap k v) where
  mempty = MonMap Map.empty

-- Postprocessing pass to fix annoying correctness issues.  If possible, each of
-- these fixes should be somehow incorporated into the generator itself.
--
-- 1. generator can produce too-small outputs.  fixed by adding some extra coin
-- to small outputs, spending the excess into fees, and propogating inputs
-- backwards using numerically lowest input.
-- 2. generator can produce multiple genesis accounts with same owner.  owners
-- are perturbed with model utxo id.
fixupDust ::
  forall era.
  Coin ->
  -- | min Utxo value
  ( [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)],
    [ModelEpoch era]
  ) ->
  ( [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)],
    [ModelEpoch era]
  )
fixupDust minOutput (genesis, epochs) =
  let txns :: Map ModelTxId (ModelTx era)
      txns =
        Map.fromList
          [ (_mtxId tx, tx)
            | tx <-
                toListOf
                  ( traverse . modelEpoch_blocks
                      . traverse
                      . modelBlock_txSeq
                      . traverse
                  )
                  epochs
          ]

      -- absence of a utxo means it's from the genesis account.
      outUtxos :: Map ModelUTxOId ModelTxId
      outUtxos =
        Map.fromList $
          [ (ui, _mtxId tx)
            | (_, tx) <- Map.toList txns,
              ui <- toListOf (modelTx_outputs . traverse . _1) tx
          ]

      spendUtxos :: Map ModelUTxOId ModelTxId
      spendUtxos =
        Map.fromList $
          [ (ui, _mtxId tx)
            | (_, tx) <- Map.toList txns,
              ui <- toListOf (modelTx_inputs . folded) tx
          ]

      -- how much needs to be added to the UTXO to meet the minimum
      tooSmall :: Map ModelUTxOId Coin
      tooSmall =
        Map.fromList $
          [ (ui, delta)
            | (_, tx) <- Map.toList txns,
              (ui, txo) <- toListOf (modelTx_outputs . traverse) tx,
              delta <- case runIdentity $ evalModelValue (pure $ pure $ Coin 0) (unModelValue $ _mtxo_value txo) of
                Left _ -> []
                Right qty
                  | qty >= minOutput -> []
                  | otherwise -> [minOutput ~~ qty]
          ]

      genesisMap = Map.fromList $ (view _1 &&& view _3) <$> genesis

      txns', txns'' :: Map ModelTxId (ModelTx era)
      txns' = Map.merge Map.preserveMissing Map.dropMissing (Map.zipWithMatched $ \_ mtx val -> over modelTx_fee (offsetModelValue val) mtx) txns $ unMonMap $ foldMap (MonMap . uncurry Map.singleton) $ Map.intersectionWith (,) spendUtxos tooSmall

      offsetModelValue c (ModelValue x) = ModelValue (x `ModelValue_Add` ModelValue_Inject c)

      (txns'', genesis', _) =
        let popItem = do
              x <- use _3
              case Map.lookupMin x of
                Nothing -> pure Nothing
                Just (k, v) -> do
                  _3 . at k .= Nothing
                  pure $ Just (k, v)
            go =
              popItem >>= \case
                Nothing -> pure ()
                Just (k, v) -> do
                  -- fix the source of this UTxO to have enough available to create
                  -- it.
                  case Map.lookup k outUtxos of
                    Nothing -> do
                      -- utxo is a genesis account, it can be adjusted without any further work
                      _2 . at k <>= Just v
                    Just txid -> do
                      -- add the offset amount to the output
                      _1 . at txid . _Just . modelTx_outputAt k . _Just . modelTxOut_value %= offsetModelValue v
                      -- pick an input to add the balance to. queue it up.
                      preuse (_1 . ix txid . modelTx_inputs . folded) >>= \case
                        Nothing -> error "fixupDust: txn has no inputs"
                        Just k' -> _3 . at k' <>= Just v
                  go
         in execState go (txns', genesisMap, tooSmall)

      relabelGenesisAddr (ModelUTxOId ui) = \case
        ModelAddress x -> ModelAddress (x <> ":GEN#" <> show ui)
        x -> x
   in ( (\(ui, ma, val) -> (ui, relabelGenesisAddr ui ma, maybe val id $ Map.lookup ui genesis')) <$> genesis, -- genesis'
        over
          (traverse . modelEpoch_blocks . traverse . modelBlock_txSeq . traverse)
          (\mtx -> maybe mtx id $ Map.lookup (_mtxId mtx) txns'')
          epochs
      )

-- | convert the graph useable models
transactionStateToModel ::
  forall era.
  TransactionState era ->
  ( [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)],
    [ModelEpoch era]
  )
transactionStateToModel st =
  (,) (_transactionState_genesisUtxo st) $
    let -- alias Node with ModelTxId.
        txIdToNode :: ModelTxId -> FGL.Node
        txIdToNode (ModelTxId mTxId) = fromInteger mTxId

        txDeps :: FGL.Gr ModelTxId ()
        txDeps =
          FGL.mkGraph
            ( fmap (txIdToNode &&& id) $
                Set.toList $
                  foldMap Map.keysSet $
                    _transactionState_withDepsInEpoch st
            )
            ( fmap (\(vStart, vEnd) -> (txIdToNode vStart, txIdToNode vEnd, ())) $
                Set.toList $
                  ifoldMap (\(_epochNo, mTxId) (_modelTx, deps) -> Set.fromList $ fmap ((,) mTxId) $ Map.keys deps) $
                    Compose $ _transactionState_withDepsInEpoch st
            )

        getFreeTxns :: FGL.Gr ModelTxId () -> ([ModelTxId], FGL.Gr ModelTxId ())
        getFreeTxns txns =
          let f ctx acc = case ctx of
                ([], _, txid, _) -> txid : acc
                ((_ : _), _, _, _) -> acc
              result = FGL.ufold f [] txns
              txns' = FGL.delNodes (fmap txIdToNode result) txns
           in if null result && not (FGL.isEmpty txns') then error "getFreeTxns:got stuck" else (result, txns')

        allTxs :: Map ModelTxId (EpochNo, ModelTx era)
        allTxs = flip ifoldMap (Compose $ _transactionState_withDepsInEpoch st) $
          \(epoch, mtxId) (mtx, _) -> Map.singleton mtxId (epoch, mtx)

        getTx :: ModelTxId -> MonMap EpochNo [ModelTx era]
        getTx tx = case Map.lookup tx allTxs of
          Nothing -> error "missing tx"
          Just (epoch, mtx) -> MonMap $ Map.singleton epoch [mtx]

        txGraphToEpochMap :: FGL.Gr ModelTxId () -> MonMap EpochNo [[ModelTx era]]
        txGraphToEpochMap =
          let go :: MonMap EpochNo [[ModelTx era]] -> FGL.Gr ModelTxId () -> MonMap EpochNo [[ModelTx era]]
              go acc ftxs
                | FGL.isEmpty ftxs = acc
                | otherwise =
                  let (a, ftxs') = getFreeTxns ftxs
                   in go (acc <> (pure <$> foldMap getTx a)) ftxs'
           in go (MonMap Map.empty)

        unrollEpochMap :: MonMap EpochNo [[ModelTx era]] -> [ModelEpoch era]
        unrollEpochMap (MonMap xs) = case Map.lookupMax xs of
          Nothing -> []
          Just (lastEpoch, _) ->
            [0 .. lastEpoch] <&> \epoch ->
              ModelEpoch
                (zipWith ModelBlock [0 ..] $ fold (Map.lookup epoch xs))
                (ModelBlocksMade Map.empty) -- TODO
     in unrollEpochMap $ txGraphToEpochMap txDeps

-- Nodes are labeled with an epoch and transaction body (which includes the tx id). Edges are labeled with the set of
-- ways in which one transaction's actions enable the actions in another transaction.
newtype TransactionGraph (era :: FeatureSet) = TransactionGraph {unTransactionGraph :: FGL.Gr (EpochNo {-, ModelTx era -}) (NESet (TransactionEnables era))}

materializeTransactionGraph :: TransactionState era -> TransactionGraph era
materializeTransactionGraph s = TransactionGraph $
  FGL.buildGr $ do
    (epoch, txs) <- Map.toDescList $ _transactionState_withDepsInEpoch s
    (ModelTxId txId, (_txBody, deps)) <- Map.toAscList txs
    --TODO: Better than this. FGL uses Int for node IDs but our IDs are naturally integers
    let tOut = flip fmap (Map.toList deps) $ \(ModelTxId outId, outLab) -> (outLab, fromIntegral outId)
    pure $ ([], fromIntegral txId, (epoch {-, txBody-}), tOut)

testGenTransactions :: IO ()
testGenTransactions = do
  actions <- testGenActions
  s <- generate (execStateT (uncurry (genTransactions @(EraFeatureSet (ShelleyEra C_Crypto))) actions) initialTransactionState)
  FGL.prettyPrint $ unTransactionGraph $ materializeTransactionGraph s
  putStrLn "---"
  print $ transactionStateToModel s
  putStrLn "==="
  pure ()

genModel ::
  forall era m.
  ( KnownValueFeature (ValueFeature era),
    KnownScriptFeature (ScriptFeature era),
    MonadGen m
  ) =>
  GenActionContext ->
  m
    ( [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)],
      [ModelEpoch era]
    )
genModel ctx = do
  s1 <- (execStateT (runReaderT genActions ctx) initialActionState)
  let actions = (Map.keysSet (_actionState_withDeps s1), materializeActionGraph s1)
  s2 <- (execStateT (uncurry (genTransactions @era) actions) initialTransactionState)
  pure $ fixupDust (Coin 500_000) $ transactionStateToModel s2

freshTxId :: MonadSupply Integer m => m ModelTxId
freshTxId = ModelTxId <$> supply

freshUtxoId :: MonadSupply Integer m => m ModelUTxOId
freshUtxoId = ModelUTxOId <$> supply

freshPaymentAddress :: MonadSupply Integer m => String -> m (ModelAddress era)
freshPaymentAddress clue = ModelAddress . (("pay:" <> clue <> "_") <>) . show <$> supply

freshRewardAddress :: MonadSupply Integer m => m (ModelAddress era)
freshRewardAddress = ModelAddress . ("reward_" <>) . show <$> supply

freshPoolAddress :: MonadSupply Integer m => m ModelPoolId
freshPoolAddress = ModelPoolId . ("pool_" <>) . show <$> supply

genTransactions ::
  forall era m.
  ( MonadGen m,
    MonadState (TransactionState era) m,
    MonadSupply Integer m,
    KnownValueFeature (ValueFeature era),
    KnownScriptFeature (ScriptFeature era)
  ) =>
  Set ActionId ->
  ActionGraph ->
  m ()
genTransactions actionIds actionGraph = do
  -- Here we exploit the fact that we generated our action graph in such a way that [1..]
  -- is an antitone topological sort of the actions.
  _ :: FGL.Gr (EpochNo, Action) ActionEnables <- (\x y f -> foldlM f x y) (unActionGraph actionGraph) (Set.toAscList actionIds) $ \gr i -> do
    let (mctx, gr') = FGL.match (unActionId i) gr
        mintValue = ModelValue $ ModelValue_Inject $ Coin 0 -- TODO genMint
    case mctx of
      Nothing -> error "genTransactions: Passed invalid actionId"
      Just (eIn, _, (epoch, action), eOut) -> do
        case eOut of
          [] -> pure ()
          _ : _ -> error "genTransactions: actionIds were not topsorted"
        case action of
          -- Spends are the simplest. Since they represent an arbitrary exchange of inputs to outputs
          -- each spend action is its own transaction. Other actions must be merged into
          -- transactions with spends or require a genesis UTxO for fees
          Action_Spend -> do
            -- Get the UTxOs my dependents spend and also generate some that don't ever get spent
            depOutputs <- uses (transactionState_utxos . at i) (maybe mempty id)
            inertOutputs <-
              mconcat <$> do
                numInert <- choose (0, 4)
                replicateM numInert $ do
                  val <- choose (minFee, maxFee)
                  uncurry Map.singleton <$> generateUtxo "inert" val
            let myOutputs = Map.union depOutputs inertOutputs
                Sum sumMyOutputs = flip foldMap myOutputs $ \(ModelTxOut _ (ModelValue mv) _) -> case mv of
                  ModelValue_Inject (Coin v) -> Sum v
                  _ -> error "genTransactions: utxo has abstract value"
            txId <- freshTxId
            -- Generate my fee and then ask my dependencies to cover my total outputs + fee
            myFee <- choose (minFee, maxFee)
            let ins = [(TransactionEnables_Spending, ActionId aId) | (ActionEnables_Spending, aId) <- eIn]
            -- Assert the action dependencies are well-formed
            when (length ins /= length eIn) $ error "genTransactions: Spend action had non-spending dependencies"
            myInputs <- generateUtxosFor (sumMyOutputs + myFee) (fmap snd ins)
            -- Tell my dependencies my transaction id
            dependents <- uses (transactionState_txDependents . at i) (maybe mempty id)
            addDependent i txId ins
            -- Finally record the body of my transaction along with my dependents
            let newTx =
                  ModelTx
                    { _mtxId = txId,
                      _mtxCollateral = ifSupportsPlutus (Proxy @(ScriptFeature era)) () Set.empty,
                      _mtxInputs = myInputs,
                      _mtxOutputs = Map.toList myOutputs,
                      _mtxFee = ModelValue (ModelValue_Inject (Coin myFee)),
                      _mtxDCert = [],
                      _mtxWdrl = mempty,
                      _mtxMint = ifSupportsMint (Proxy @(ValueFeature era)) () mintValue
                    }
            transactionState_withDepsInEpoch . at epoch
              %= Just . Map.insert txId (newTx, dependents)
                . maybe mempty id
            pure ()
          -- Withdrawals are materialized in a similar way to spends. A withdrawal can live in its
          -- own transaction, covering its own fees and UTxOs, but can also be part of a larger
          -- spend that involves input UTxOs as well. TODO: Sometimes merge withdrawals into
          -- other transactions instead of creating a new one.
          Action_Withdraw -> do
            depOutputs <- uses (transactionState_utxos . at i) (maybe mempty id)
            myFee <- choose (minFee, maxFee)
            -- We are going to generate an inert output with the abstract "leftover" reward value
            let Sum sumMyOutputs = flip foldMap depOutputs $ \(ModelTxOut _ (ModelValue mv) _) -> case mv of
                  ModelValue_Inject (Coin v) -> Sum v
                  _ -> error "genTransactions: utxo has abstract value"
            let (fromRewards, r) = divMod (sumMyOutputs + myFee) 2
                fromInputs = fromRewards + r
            txId <- freshTxId
            raddr <- generateRewardAddress
            rutxo <- generateRewardsUtxo raddr (sumMyOutputs + myFee)
            let (spendInputs, delegInputs) = partitionEithers $
                  flip fmap eIn $ \(enable, aId) ->
                    case enable of
                      ActionEnables_Withdrawing -> Right (TransactionEnables_Withdrawing raddr, ActionId aId)
                      ActionEnables_Spending -> Left (TransactionEnables_Spending, ActionId aId)
                      _ -> error "genTransactions: withdrawal had a dependency that was neither an input or a delegation"

            myInputs <- generateUtxosFor fromInputs (fmap snd spendInputs)
            -- Tell my dependencies my transaction id
            dependents <- uses (transactionState_txDependents . at i) (maybe mempty id)
            addDependent i txId (spendInputs ++ delegInputs)
            let newTx =
                  ModelTx
                    { _mtxId = txId,
                      _mtxCollateral = ifSupportsPlutus (Proxy @(ScriptFeature era)) () Set.empty,
                      _mtxInputs = myInputs,
                      _mtxOutputs = rutxo : Map.toList depOutputs,
                      _mtxFee = ModelValue (ModelValue_Inject (Coin myFee)),
                      _mtxDCert = [],
                      _mtxWdrl = Map.singleton raddr (ModelValue (ModelValue_Var (ModelValue_Reward raddr))),
                      _mtxMint = ifSupportsMint (Proxy @(ValueFeature era)) () mintValue
                    }
            transactionState_withDepsInEpoch . at epoch %= Just . Map.insert txId (newTx, dependents) . maybe mempty id
            pure ()
          Action_Delegate -> pure ()
          Action_RegisterStake -> pure ()
          Action_RegisterPool -> pure ()
        pure gr'
  pure ()

-- TODO: Reuse reward addresses to simulate multiple epochs of rewards for the same stake
generateRewardAddress ::
  (MonadState (TransactionState era) m, MonadSupply Integer m) =>
  m (ModelAddress (ScriptFeature era))
generateRewardAddress = do
  raddr <- freshRewardAddress
  transactionState_liveRewardAddresses %= (Set.insert raddr)
  pure raddr

generatePaymentAddress ::
  (MonadState (TransactionState era) m, MonadSupply Integer m, MonadGen m) =>
  String ->
  m (ModelAddress (ScriptFeature era))
generatePaymentAddress clue = do
  paddrs <- use transactionState_livePaymentAddresses
  frequency $
    [ (1, freshPaymentAddress clue >>= \paddr -> transactionState_livePaymentAddresses %= (Set.insert paddr) >> pure paddr),
      (Set.size paddrs, elements (Set.toList paddrs))
    ]

-- TODO: handle this more elegantly
modelTxOut ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  (ModelAddress (ScriptFeature era)) ->
  (ModelValue (ValueFeature era) era) ->
  ModelTxOut era
modelTxOut x y =
  ModelTxOut x y $
    ifSupportsPlutus (Proxy @(ScriptFeature era)) () Nothing

generateUtxosFor ::
  ( MonadState (TransactionState era) m,
    MonadSupply Integer m,
    MonadGen m,
    KnownScriptFeature (ScriptFeature era)
  ) =>
  Integer ->
  [ActionId] ->
  m (Set ModelUTxOId)
generateUtxosFor totalValue = \case
  -- No input actions were specified so we need to generate genesis UTxOs
  [] -> do
    utxoId <- freshUtxoId
    paddr <- generatePaymentAddress "genesis"
    transactionState_genesisUtxo %= ((utxoId, paddr, Coin totalValue) :)
    pure $ Set.singleton utxoId
  aId0 : aIds -> do
    (leftoverValue, newUtxos) <- (\x y f -> foldlM f x y) (totalValue, mempty) aIds $ \(valueLeft, acc) aId -> do
      utxoId <- freshUtxoId
      amount <- choose (1, max 1 (valueLeft `div` 4)) -- TODO: Split up the value in a more sensible and robust way
      paddr <- generatePaymentAddress "act"
      let newUtxo = Map.singleton aId (Map.singleton utxoId (modelTxOut paddr (ModelValue (ModelValue_Inject (Coin amount)))))
      pure (valueLeft - amount, Map.union newUtxo acc)
    xUtxos <- do
      utxoId <- freshUtxoId
      paddr <- generatePaymentAddress "leftover"
      pure $
        Map.singleton aId0 $
          Map.singleton utxoId $
            modelTxOut paddr (ModelValue (ModelValue_Inject (Coin leftoverValue)))
    let allUtxos = Map.union newUtxos xUtxos
    transactionState_utxos %= Map.unionWith Map.union allUtxos
    pure $ mconcat $ fmap Map.keysSet $ Map.elems allUtxos

generateUtxo ::
  ( MonadSupply Integer m,
    MonadState (TransactionState era) m,
    MonadGen m,
    KnownScriptFeature (ScriptFeature era)
  ) =>
  String ->
  Integer ->
  m (ModelUTxOId, ModelTxOut era)
generateUtxo clue value = do
  utxoId <- freshUtxoId
  paddr <- generatePaymentAddress clue
  pure $ (utxoId, modelTxOut paddr $ ModelValue (ModelValue_Inject (Coin value)))

generateRewardsUtxo ::
  ( MonadSupply Integer m,
    MonadState (TransactionState era) m,
    MonadGen m,
    KnownScriptFeature (ScriptFeature era)
  ) =>
  (ModelAddress (ScriptFeature era)) ->
  Integer ->
  m (ModelUTxOId, ModelTxOut era)
generateRewardsUtxo rewardAddr deficit = do
  utxoId <- freshUtxoId
  paddr <- generatePaymentAddress "reward"
  pure $
    ( utxoId,
      modelTxOut paddr $
        ModelValue $
          ModelValue_Var (ModelValue_Reward rewardAddr) `ModelValue_Sub` ModelValue_Inject (Coin deficit)
    )

-- When associating an action to a transaction id, add that transaction id to the set of dependents of all its enabling
-- dependencies
addDependent :: MonadState (TransactionState era) m => ActionId -> ModelTxId -> [(TransactionEnables era, ActionId)] -> m ()
addDependent aId txId eIn = do
  let newDeps = Map.fromList $ flip fmap eIn $ \(enable, depId) -> (depId, Map.singleton txId (NES.singleton enable))
  transactionState_txDependents %= Map.unionWith (Map.unionWith (<>)) newDeps . Map.delete aId

-- Orphans
deriving newtype instance System.Random.Random EpochNo -- TODO: this can be moved closer to the package that defines EpochNo

instance MonadGen g => MonadGen (StateT s g) where
  liftGen = lift . liftGen
  variant n a = StateT $ \s -> variant n (runStateT a s)
  sized f = StateT $ \s -> sized (\i -> runStateT (f i) s)
  resize n a = StateT $ \s -> resize n (runStateT a s)
  choose = lift . choose

instance MonadGen g => MonadGen (ReaderT r g) where
  liftGen = lift . liftGen
  variant n a = ReaderT $ \s -> variant n (runReaderT a s)
  sized f = ReaderT $ \s -> sized (\i -> runReaderT (f i) s)
  resize n a = ReaderT $ \s -> resize n (runReaderT a s)
  choose = lift . choose

instance {-# OVERLAPPING #-} (Monad m) => MonadSupply Int (StateT ActionState m) where
  supply = actionState_freshNames <<+= 1
  peek = use actionState_freshNames
  exhausted = uses actionState_freshNames (>= maxBound)

instance {-# OVERLAPPING #-} (Monad m) => MonadSupply Integer (StateT (TransactionState era) m) where
  supply = transactionState_freshNames <<+= 1
  peek = use transactionState_freshNames
  exhausted = pure False
