{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.DependGraph where

import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.Lens hiding (elements)
import Control.Monad
import Data.Proxy
import Control.Monad.State
import Control.Monad.Supply
import Data.Foldable
import qualified Data.Graph.Inductive as FGL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import QuickCheck.GenT
import Test.Cardano.Ledger.Elaborators
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.FeatureSet
import Test.Cardano.Ledger.ModelChain.Value
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.QuickCheck hiding (choose, elements, frequency, oneof, resize, shuffle, sized, variant)

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
newtype ActionGraph = ActionGraph {unActionGraph :: FGL.Gr (Int, Action) ActionEnables}

minEpochs :: Int
minEpochs = 3

maxEpochs :: Int
maxEpochs = 3

minActionsPerEpoch :: Int
minActionsPerEpoch = 64

maxActionsPerEpoch :: Int
maxActionsPerEpoch = 2 * minActionsPerEpoch

minInputs :: Int
minInputs = 1

maxInputs :: Int
maxInputs = 5

data ActionState = ActionState
  { -- | Lazy supply of Ints
    _actionState_freshNames :: [Int],
    -- | Node ID to label and in-edges
    _actionState_withDeps :: Map Int (Int, Action, Map Int ActionEnables),
    -- | Epoch number to set of node ids
    _actionState_needsDelegate :: Map Int (Set Int),
    -- | Node IDs
    _actionState_needsStake :: Set Int,
    -- | Node IDs
    _actionState_needsPool :: Set Int,
    -- | Node ID to number of inputs needed
    _actionState_needsInputs :: Set Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''ActionState

initialActionState :: ActionState
initialActionState = ActionState [1 ..] mempty mempty mempty mempty mempty

testGenActions :: IO (Set Int, ActionGraph)
testGenActions = do
  s <- generate (execStateT genActions initialActionState)
  pure $ (Map.keysSet (_actionState_withDeps s), materializeActionGraph s)

materializeActionGraph :: ActionState -> ActionGraph
materializeActionGraph s = ActionGraph $ FGL.buildGr $ flip fmap (Map.toAscList (_actionState_withDeps s)) $
  \(aid, (epoch, alab, deps)) ->
    let aIn = flip fmap (Map.toList deps) $ \(inId, inLab) -> (inLab, inId)
     in (aIn, aid, (epoch, alab), [])

-- TODO: this generation method doesn't produce long-lived UTxOs which are common in a realistic ledger
outputTo :: (MonadGen m, MonadState ActionState m) => Int -> Int -> Int -> Set Int -> m ()
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
        r <- choose (minInputs, maxInputs)
        case r <= numInputs of
          True -> actionState_needsInputs %= (Set.delete i)
          False -> pure ()

makeWithdrawal :: (MonadGen m, MonadState ActionState m) => Int -> Int -> Set Int -> m ()
makeWithdrawal aId epoch withdrawals = do
  case Set.toList withdrawals of
    [] -> pure ()
    _ : _ -> do
      wId <- elements (Set.toList withdrawals)
      actionState_withDeps . at wId . _Just . _3 . at aId .= Just ActionEnables_Withdrawing
      actionState_needsDelegate . at epoch . _Just %= Set.delete wId

-- TODO: Some of these actions should not be passed more than one dependent.
chooseEnablement :: (MonadState ActionState m, MonadGen m) => Set Int -> Int -> Int -> Action -> m ()
chooseEnablement targets _ i = \case
  Action_Withdraw -> outputTo i 1 1 targets
  Action_Delegate -> do
    forM_ (Set.toList targets) $ \target -> fmap (view _1) <$> (use (actionState_withDeps . at target)) >>= \case
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

randomEnablement :: (MonadState ActionState m, MonadGen m) => Int -> Int -> Action -> m ()
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

addAction :: MonadState ActionState m => (Int -> Int -> Action -> m ()) -> Int -> Int -> Action -> m ()
addAction enablement epoch i a = do
  actionState_withDeps . at i .= Just (epoch, a, mempty)
  -- Action specific logic. An action reports the dependencies it needs fulfilled
  -- and also chooses to fulfill some dependencies that have been emitted by actions
  -- that happen after it.
  enablement epoch i a
  case a of
    Action_Withdraw -> do
      -- Withdrawal can only occur if a delegation was made in the past
      actionState_needsDelegate . at epoch %= Just . maybe (Set.singleton i) (Set.insert i)
    Action_Delegate -> do
      -- Delegation needs a staking certificate and registered pool
      actionState_needsStake %= Set.insert i
      actionState_needsPool %= Set.insert i
    Action_Spend -> do
      -- Spending requires inputs, which potentially could come from the genesis block
      actionState_needsInputs %= Set.insert i
    Action_RegisterPool -> pure ()
    Action_RegisterStake -> pure ()

fillDependencies :: (MonadState ActionState m, MonadSupply Int m, MonadGen m) => m ()
fillDependencies = do
  -- Delegations first
  ds <- uses actionState_needsDelegate fold
  forM_ (Set.toList ds) $ \i -> do
    n <- supply
    addAction (chooseEnablement (Set.singleton i)) 1 n Action_Delegate
  -- Use one pool for all the hanging threads
  do
    n <- supply
    ps <- use actionState_needsPool
    addAction (chooseEnablement ps) 1 n Action_RegisterPool
  -- One staking cert per delegation
  ss <- use actionState_needsStake
  forM_ (Set.toList ss) $ \i -> do
    n <- supply
    addAction (chooseEnablement (Set.singleton i)) 1 n Action_RegisterStake


genEpoch :: (MonadGen m, MonadSupply Int m, MonadState ActionState m) => Int -> m ()
genEpoch epoch = do
  numActions <- choose (minActionsPerEpoch, maxActionsPerEpoch)
  replicateM_ numActions $ do
    -- We assign IDs to actions so that the lower IDs depend on
    -- higher IDs.
    i <- supply
    a <-
      frequency
        -- Can't ever withdraw rewards before rewards
        -- could have been generated for a staking pool.
        [ (if epoch > 2 then 1 else 0, pure Action_Withdraw),
          (2, pure Action_RegisterStake),
          (2, pure Action_RegisterPool),
          (if i > minActionsPerEpoch then 1 else 0, pure Action_Delegate),
          (12, pure Action_Spend)
        ]
    addAction randomEnablement epoch i a
  pure ()

genActions :: (MonadGen m, MonadSupply Int m, MonadState ActionState m) => m ()
genActions = do
  -- First figure out how many epochs we're aiming for.
  -- We need to do this here because delegation to a pool
  -- has to happen some number of epochs (2) before reward
  -- withdrawal.
  numEpochs <- choose (minEpochs, maxEpochs)
  forM_ [numEpochs, numEpochs-1 .. 1] genEpoch
  -- After random generation there might still be actions in the graph
  -- that don't have their dependencies met. Fill those in now in the first epoch.
  fillDependencies
  pure ()

minActions :: Int
minActions = 1

maxActions :: Int
maxActions = 4

minTxs :: Int
minTxs = 1

maxTxs :: Int
maxTxs = 4

minFee :: Integer
minFee = 1000

maxFee :: Integer
maxFee = 100000

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

data TransactionState era = TransactionState
  { _transactionState_freshNames :: [Integer]
  , _transactionState_livePaymentAddresses :: Set (ModelAddress (ScriptFeature era))
  , _transactionState_liveRewardAddresses :: Set (ModelAddress (ScriptFeature era))
  , _transactionState_utxos :: Map Int {- action id -} (Map ModelUTxOId (ModelTxOut era))
  , _transactionState_txDependents :: Map {- action id -} Int (Map ModelTxId (NESet (TransactionEnables era)))
  , _transactionState_genesisUtxo :: [(ModelUTxOId, (ModelAddress (ScriptFeature era)), Coin)]
  -- NB: In the action graph we recorded actions with their in-edges.
  -- Here we record transactions with their out-edges.
  -- It's a little more convenient for merging transactions together.
  , _transactionState_withDepsInEpoch :: Map {- epoch -} Int (Map ModelTxId (ModelTx era, Map ModelTxId (NESet (TransactionEnables era))))
  }

makeLenses ''TransactionState

initialTransactionState :: TransactionState era
initialTransactionState = TransactionState
  { _transactionState_freshNames = [1..]
  , _transactionState_livePaymentAddresses = mempty
  , _transactionState_liveRewardAddresses = mempty
  , _transactionState_utxos = mempty
  , _transactionState_txDependents = mempty
  , _transactionState_genesisUtxo = mempty
  , _transactionState_withDepsInEpoch = mempty
  }

-- Nodes are labeled with an epoch and transaction body (which includes the tx id). Edges are labeled with the set of
-- ways in which one transaction's actions enable the actions in another transaction.
newtype TransactionGraph (era :: FeatureSet) = TransactionGraph { unTransactionGraph :: FGL.Gr (Int {-, ModelTx era -}) (NESet (TransactionEnables era)) }

materializeTransactionGraph :: TransactionState era -> TransactionGraph era
materializeTransactionGraph s = TransactionGraph $ FGL.buildGr $ do
  (epoch, txs) <- Map.toDescList $ _transactionState_withDepsInEpoch s
  (ModelTxId txId, (txBody, deps)) <- Map.toAscList txs
  --TODO: Better than this. FGL uses Int for node IDs but our IDs are naturally integers
  let tOut = flip fmap (Map.toList deps) $ \(ModelTxId outId, outLab) -> (outLab, fromIntegral outId)
  pure $ ([], fromIntegral txId, (epoch{-, txBody-}), tOut)

testGenTransactions :: IO ()
testGenTransactions = do
  actions <- testGenActions
  s <- generate (execStateT (uncurry (genTransactions @(EraFeatureSet (ShelleyEra C_Crypto))) actions) initialTransactionState)
  FGL.prettyPrint $ unTransactionGraph $ materializeTransactionGraph s
  pure ()

freshTxId :: MonadSupply Integer m => m ModelTxId
freshTxId = ModelTxId <$> supply

freshUtxoId :: MonadSupply Integer m => m ModelUTxOId
freshUtxoId = ModelUTxOId <$> supply

freshPaymentAddress :: MonadSupply Integer m => m (ModelAddress era)
freshPaymentAddress = ModelAddress . ("pay_" <>) . show <$> supply

freshRewardAddress :: MonadSupply Integer m => m (ModelAddress era)
freshRewardAddress = ModelAddress . ("reward_" <>) . show <$> supply

freshPoolAddress :: MonadSupply Integer m => m (ModelAddress era)
freshPoolAddress = ModelAddress . ("pool_" <>) . show <$> supply

genTransactions
  :: forall era m.
     ( MonadGen m
     , MonadState (TransactionState era) m
     , MonadSupply Integer m
     , ValueFeature era ~ 'ExpectAdaOnly
     , KnownScriptFeature (ScriptFeature era)
     )
  => Set Int -> ActionGraph -> m ()
genTransactions actionIds actionGraph = do
  -- Here we exploit the fact that we generated our action graph in such a way that [1..]
  -- is an antitone topological sort of the actions.
  gr <- (\x y f -> foldlM f x y) (unActionGraph actionGraph) (Set.toAscList actionIds) $ \gr i -> do
    let (mctx, gr') = FGL.match i gr
    case mctx of
      Nothing -> error "genTransactions: Passed invalid actionId"
      Just (eIn, _, (epoch, action), eOut) -> do
        case eOut of
          [] -> pure ()
          _:_ -> error "genTransactions: actionIds were not topsorted"
        case action of
          -- Spends are the simplest. Since they represent an arbitrary exchange of inputs to outputs
          -- each spend action is its own transaction. Other actions must be merged into
          -- transactions with spends or require a genesis UTxO for fees
          Action_Spend -> do
            -- Get the UTxOs my dependents spend and also generate some that don't ever get spent
            depOutputs <- uses (transactionState_utxos . at i) (maybe mempty id)
            inertOutputs <- mconcat <$> do
              numInert <- choose (0,4)
              replicateM numInert $ do
                val <- choose (minFee, maxFee)
                uncurry Map.singleton <$> generateUtxo val
            let myOutputs = Map.union depOutputs inertOutputs
                Sum sumMyOutputs = flip foldMap myOutputs $ \(ModelTxOut _ (ModelValue mv) _) -> case mv of
                  ModelValue_Inject (Coin v) -> Sum v
                  _ -> error "genTransactions: utxo has abstract value"
            txId <- freshTxId
            -- Generate my fee and then ask my dependencies to cover my total outputs + fee
            myFee <- choose (minFee, maxFee)
            let ins = [ (TransactionEnables_Spending, aId) | (ActionEnables_Spending, aId) <- eIn ]
            -- Assert the action dependencies are well-formed
            when (length ins /= length eIn) $ error "genTransactions: Spend action had non-spending dependencies"
            myInputs <- generateUtxosFor (sumMyOutputs + myFee) (fmap snd ins)
            -- Tell my dependencies my transaction id
            dependents <- uses (transactionState_txDependents . at i) (maybe mempty id)
            addDependent i txId ins
            -- Finally record the body of my transaction along with my dependents
            let newTx = ModelTx
                  { _mtxId = txId
                  , _mtxCollateral = ifSupportsPlutus (Proxy @(ScriptFeature era)) () Set.empty
                  , _mtxInputs = myInputs
                  , _mtxOutputs = Map.toList myOutputs
                  , _mtxFee = ModelValue (ModelValue_Inject (Coin myFee))
                  , _mtxDCert = []
                  , _mtxWdrl = mempty
                  , _mtxMint = NoMintSupport ()
                  }
            transactionState_withDepsInEpoch . at epoch %= Just . Map.insert txId (newTx, dependents) .
              maybe mempty id
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
            txId <- freshTxId
            raddr <- generateRewardAddress
            rutxo <- generateRewardsUtxo raddr (sumMyOutputs + myFee)
            -- A withdrawal should only have one dependency, which enables withdrawal
            ins <- case eIn of
              [(ActionEnables_Withdrawing, aId)] -> pure [(TransactionEnables_Withdrawing raddr, aId)]
              _ -> error "genTransactions: withdrawal has invalid dependencies"
            -- Tell my dependencies my transaction id
            dependents <- uses (transactionState_txDependents . at i) (maybe mempty id)
            addDependent i txId ins
            let newTx = ModelTx
                  { _mtxId = txId
                  , _mtxCollateral = ifSupportsPlutus (Proxy @(ScriptFeature era)) () Set.empty
                  , _mtxInputs = mempty
                  , _mtxOutputs = rutxo : Map.toList depOutputs
                  , _mtxFee = ModelValue (ModelValue_Inject (Coin myFee))
                  , _mtxDCert = []
                  , _mtxWdrl = Map.singleton raddr (ModelValue (ModelValue_Var (ModelValue_Reward raddr)))
                  , _mtxMint = NoMintSupport ()
                  }
            transactionState_withDepsInEpoch . at epoch %= Just . Map.insert txId (newTx, dependents) . maybe mempty id
            pure ()
          Action_Delegate -> pure ()
          Action_RegisterStake -> pure ()
          Action_RegisterPool -> pure ()
        pure gr'
  pure ()

-- TODO: Reuse reward addresses to simulate multiple epochs of rewards for the same stake
generateRewardAddress
  :: (MonadState (TransactionState era) m, MonadSupply Integer m)
  => m (ModelAddress (ScriptFeature era))
generateRewardAddress = do
  raddr <- freshRewardAddress
  transactionState_liveRewardAddresses %= (Set.insert raddr)
  pure raddr

generatePaymentAddress
  :: (MonadState (TransactionState era) m, MonadSupply Integer m, MonadGen m)
  => m (ModelAddress (ScriptFeature era))
generatePaymentAddress = do
  paddrs <- use transactionState_livePaymentAddresses
  frequency $
    [ (1, freshPaymentAddress >>= \paddr -> transactionState_livePaymentAddresses %= (Set.insert paddr) >> pure paddr)
    , (Set.size paddrs, elements (Set.toList paddrs))
    ]

-- TODO: handle this more elegantly
modelTxOut ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  (ModelAddress (ScriptFeature era)) ->
  (ModelValue (ValueFeature era) era) ->
  ModelTxOut era
modelTxOut x y = ModelTxOut x y
  $ ifSupportsPlutus (Proxy @(ScriptFeature era)) () Nothing


generateUtxosFor
  :: (MonadState (TransactionState era) m, MonadSupply Integer m, MonadGen m,
  KnownScriptFeature (ScriptFeature era))
  => Integer
  -> [Int] {- action ids -}
  -> m (Set ModelUTxOId)
generateUtxosFor totalValue = \case
-- No input actions were specified so we need to generate genesis UTxOs
  [] -> do
    utxoId <- freshUtxoId
    paddr <- generatePaymentAddress
    transactionState_genesisUtxo %= ((utxoId, paddr, Coin totalValue):)
    pure $ Set.singleton utxoId
  aId0:aIds -> do
    (leftoverValue, newUtxos) <- (\x y f -> foldlM f x y) (totalValue, mempty) aIds $ \(valueLeft, acc) aId -> do
      utxoId <- freshUtxoId
      amount <- choose (1, max 1 (valueLeft `div` 4)) -- TODO: Split up the value in a more sensible and robust way
      paddr <- generatePaymentAddress
      let newUtxo = Map.singleton aId (Map.singleton utxoId (modelTxOut paddr (ModelValue (ModelValue_Inject (Coin amount)))))
      pure (valueLeft - amount, Map.union newUtxo acc)
    xUtxos <- do
      utxoId <- freshUtxoId
      paddr <- generatePaymentAddress
      pure $ Map.singleton aId0 $ Map.singleton utxoId $
        modelTxOut paddr (ModelValue (ModelValue_Inject (Coin leftoverValue)))
    let allUtxos = Map.union newUtxos xUtxos
    transactionState_utxos %= Map.unionWith Map.union allUtxos
    pure $ mconcat $ fmap Map.keysSet $ Map.elems allUtxos

generateUtxo
  :: (MonadSupply Integer m, MonadState (TransactionState era) m, MonadGen m,
  KnownScriptFeature (ScriptFeature era))
  => Integer
  -> m (ModelUTxOId, ModelTxOut era)
generateUtxo value = do
  utxoId <- freshUtxoId
  paddr <- generatePaymentAddress
  pure $ (utxoId, modelTxOut paddr $ ModelValue (ModelValue_Inject (Coin value)))

generateRewardsUtxo
  :: (MonadSupply Integer m, MonadState (TransactionState era) m, MonadGen m,
  KnownScriptFeature (ScriptFeature era))
  => (ModelAddress (ScriptFeature era))
  -> Integer
  -> m (ModelUTxOId, ModelTxOut era)
generateRewardsUtxo rewardAddr deficit = do
  utxoId <- freshUtxoId
  paddr <- generatePaymentAddress
  pure $
    ( utxoId
    , modelTxOut paddr $ ModelValue $
        ModelValue_Var (ModelValue_Reward rewardAddr) `ModelValue_Sub` ModelValue_Inject (Coin deficit)
    )

-- When associating an action to a transaction id, add that transaction id to the set of dependents of all its enabling
-- dependencies
addDependent :: MonadState (TransactionState era) m => Int -> ModelTxId -> [(TransactionEnables era, Int)] -> m ()
addDependent aId txId eIn = do
  let newDeps = Map.fromList $ flip fmap eIn $ \(enable, depId) -> (depId, Map.singleton txId (NES.singleton enable))
  transactionState_txDependents %= Map.unionWith (Map.unionWith (<>)) newDeps

-- Orphans
instance MonadFail Gen where
  fail = error

instance MonadGen g => MonadGen (StateT s g) where
  liftGen = lift . liftGen
  variant n a = StateT $ \s -> variant n (runStateT a s)
  sized f = StateT $ \s -> sized (\i -> runStateT (f i) s)
  resize n a = StateT $ \s -> resize n (runStateT a s)
  choose = lift . choose

instance {-# OVERLAPPING #-} (Monad m, MonadFail m) => MonadSupply Int (StateT ActionState m) where
  supply = do
    x : xs <- use actionState_freshNames
    actionState_freshNames .= xs
    pure x
  peek = do
    x : _ <- use actionState_freshNames
    pure x
  exhausted = uses actionState_freshNames null

instance {-# OVERLAPPING #-} (Monad m, MonadFail m) => MonadSupply Integer (StateT (TransactionState era) m) where
  supply = do
    x : xs <- use transactionState_freshNames
    transactionState_freshNames .= xs
    pure x
  peek = do
    x : _ <- use transactionState_freshNames
    pure x
  exhausted = uses transactionState_freshNames null
