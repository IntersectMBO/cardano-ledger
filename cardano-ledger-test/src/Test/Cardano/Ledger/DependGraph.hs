{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.DependGraph where

import Cardano.Ledger.Coin
import Control.Lens hiding (elements)
import Control.Monad
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
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.Value
import Test.QuickCheck hiding (choose, elements, frequency, oneof, resize, shuffle, sized, variant)

-- TODO: Handle anti-dependencies. e.g. retiring a pool precludes delegating to it
-- or receiving rewards from it. likewise deregistering a staking cert precludes
-- delegating or receiving rewards. Delegating the same staking cert again
-- nullifies the previous delegation.

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
data Enables
  = Enables_Spending
  | Enables_StakeForDelegating
  | Enables_PoolForDelegating
  | Enables_Withdrawing
  deriving (Eq, Ord, Show)

-- Actually a node is an action in an epoch. We need the epoch to ensure consistency of
-- stake pool rewards.
newtype ActionGraph = ActionGraph {unActionGraph :: FGL.Gr (Int, Action) Enables}

minEpochs :: Int
minEpochs = 3

maxEpochs :: Int
maxEpochs = 3

minActionsPerEpoch :: Int
minActionsPerEpoch = 32

maxActionsPerEpoch :: Int
maxActionsPerEpoch = 2 * minActionsPerEpoch

minInputs :: Int
minInputs = 1

maxInputs :: Int
maxInputs = 5

data ActionState = ActionState
  { -- | Lazy supply of Ints
    _actionState_freshNames :: [Int],
    -- | Node ID to label and out-edges
    _actionState_withDeps :: Map Int (Int, Action, Map Int Enables),
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

outputTo :: (MonadGen m, MonadState ActionState m) => Int -> Int -> Int -> Set Int -> m ()
outputTo aId minOutputs maxOutputs spenders = do
  -- First decide how many and which actions to enable
  numOutputs <- choose (min minOutputs 0, max maxOutputs (Set.size spenders))
  chosenSpenders <- take numOutputs <$> shuffle (Set.toList spenders)
  forM_ chosenSpenders $ \i -> do
    actionState_withDeps . at i . _Just . _3 . at aId .= Just Enables_Spending
    mdeps <- fmap (view _3) <$> use (actionState_withDeps . at i)
    -- Decide whether or not this dependent is satisfied
    case mdeps of
      Nothing -> error "outputTo: impossible empty deps"
      Just deps -> do
        let numInputs = Map.size $ Map.filter (== Enables_Spending) deps
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
      actionState_withDeps . at wId . _Just . _3 . at aId .= Just Enables_Withdrawing
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
      actionState_withDeps . at target . _Just . _3 . at i .= Just Enables_PoolForDelegating
    actionState_needsPool %= (`Set.difference` targets)
  Action_RegisterStake -> do
    forM_ (Set.toList targets) $ \target ->
      actionState_withDeps . at target . _Just . _3 . at i .= Just Enables_StakeForDelegating
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
      actionState_withDeps . at dId . _Just . _3 . at i .= Just Enables_PoolForDelegating
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
      actionState_withDeps . at dId . _Just . _3 . at i .= Just Enables_StakeForDelegating
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
          (4, pure Action_RegisterStake),
          (4, pure Action_RegisterPool),
          (if i > minActionsPerEpoch then 2 else 0, pure Action_Delegate),
          (24, pure Action_Spend)
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

testGenActions :: IO ActionState
testGenActions = do
  s <- generate (execStateT genActions initialActionState)
  pure $ s & actionState_freshNames .~ []

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
maxFee = 10000

data TransactionState era = TransactionState
  { _transactionState_freshNames :: [Integer]
  , _transactionState_livePaymentAddresses :: Set ModelAddress
  , _transactionState_utxos :: Map Int {- action id -} (Map ModelUTxOId (ModelTxOut era))
  , _transactionState_txDependents :: Map {- action id -} Int (Map ModelTxId (NESet Enables))
  , _transactionState_genesisUtxo :: [(ModelUTxOId, ModelAddress, Coin)]
  , _transactionState_withDepsInEpoch :: Map {- epoch -} Int (Map ModelTxId (ModelTx era, Map ModelTxId (NESet Enables)))
  }

makeLenses ''TransactionState

initialTransactionState :: TransactionState era
initialTransactionState = TransactionState
  { _transactionState_freshNames = [1..]
  , _transactionState_livePaymentAddresses = mempty
  , _transactionState_utxos = mempty
  , _transactionState_txDependents = mempty
  , _transactionState_genesisUtxo = mempty
  , _transactionState_withDepsInEpoch = mempty
  }

freshTxId :: MonadSupply Integer m => m ModelTxId
freshTxId = ModelTxId <$> supply

freshUtxoId :: MonadSupply Integer m => m ModelUTxOId
freshUtxoId = ModelUTxOId <$> supply

freshPaymentAddress :: MonadSupply Integer m => m ModelAddress
freshPaymentAddress = ModelAddress . ("pay_" <>) . show <$> supply

freshPoolAddress :: MonadSupply Integer m => m ModelAddress
freshPoolAddress = ModelAddress . ("pool_" <>) . show <$> supply

genTransactions
  :: ( MonadGen m
     , MonadState (TransactionState era) m
     , MonadSupply Integer m
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
        dependents <- uses (transactionState_txDependents . at i) (maybe mempty id)
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
            let Sum sumMyOutputs = flip foldMap myOutputs $ \(ModelTxOut _ (ModelValue mv)) -> case mv of
                  ModelValue_Inject (Coin v) -> Sum v
                  _ -> error "genTransactions: utxo has abstract value"
            txId <- freshTxId
            -- Generate my fee and then ask my dependencies to cover my total outputs + fee
            myFee <- choose (minFee, maxFee)
            let ins = [ aId | (Enables_Spending, aId) <- eIn ]
            myInputs <- generateUtxosFor (sumMyOutputs + myFee) ins
            -- Tell my dependencies my transaction id
            addDependent i txId eIn
            -- Finally record the body of my transaction along with my dependents
            let newTx = ModelTx
                  { _mtxId = txId
                  , _mtxInputs = myInputs
                  , _mtxOutputs = Map.toList myOutputs
                  , _mtxFee = ModelValue (ModelValue_Inject (Coin myFee))
                  , _mtxDCert = []
                  , _mtxWdrl = mempty
                  , _mtxMint = undefined
                  }
            transactionState_withDepsInEpoch . at epoch %= Just . Map.insert txId (newTx, dependents) .
              maybe mempty id
            pure ()
          Action_Withdraw -> pure ()
          Action_Delegate -> pure ()
          Action_RegisterStake -> pure ()
          Action_RegisterPool -> pure ()
        pure gr'
  pure ()

generatePaymentAddress
  :: (MonadState (TransactionState era) m, MonadSupply Integer m, MonadGen m)
  => m ModelAddress
generatePaymentAddress = do
  paddrs <- use transactionState_livePaymentAddresses
  frequency $
    [ (1, freshPaymentAddress >>= \paddr -> transactionState_livePaymentAddresses %= (Set.insert paddr) >> pure paddr)
    , (Set.size paddrs, elements (Set.toList paddrs))
    ]

generateUtxosFor
  :: (MonadState (TransactionState era) m, MonadSupply Integer m, MonadGen m)
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
      let newUtxo = Map.singleton aId (Map.singleton utxoId (ModelTxOut paddr (ModelValue (ModelValue_Inject (Coin amount)))))
      pure (valueLeft - amount, Map.union newUtxo acc)
    xUtxos <- do
      utxoId <- freshUtxoId
      paddr <- generatePaymentAddress
      pure $ Map.singleton aId0 $ Map.singleton utxoId $
        ModelTxOut paddr (ModelValue (ModelValue_Inject (Coin leftoverValue)))
    let allUtxos = Map.union newUtxos xUtxos
    transactionState_utxos %= Map.unionWith Map.union allUtxos
    pure $ mconcat $ fmap Map.keysSet $ Map.elems allUtxos

generateUtxo
  :: (MonadSupply Integer m, MonadState (TransactionState era) m, MonadGen m)
  => Integer
  -> m (ModelUTxOId, ModelTxOut era)
generateUtxo value = do
  utxoId <- freshUtxoId
  paddr <- generatePaymentAddress
  pure $ (utxoId, ModelTxOut paddr $ ModelValue (ModelValue_Inject (Coin value)))

generateRewardsUtxo
  :: (MonadSupply Integer m, MonadState (TransactionState era) m, MonadGen m)
  => ModelAddress
  -> Integer
  -> m (ModelUTxOId, ModelTxOut era)
generateRewardsUtxo rewardAddr deficit = do
  utxoId <- freshUtxoId
  paddr <- generatePaymentAddress
  pure $
    ( utxoId
    , ModelTxOut paddr $ ModelValue $
        ModelValue_Var (ModelValue_Reward rewardAddr) `ModelValue_Sub` ModelValue_Inject (Coin deficit)
    )

-- When associating an action to a transaction id, add that transaction id to the set of dependents of all its enabling
-- depndencies
addDependent :: MonadState (TransactionState era) m => Int -> ModelTxId -> [(Enables, Int)] -> m ()
addDependent aId txId eIn = do
  let newDeps = Map.fromList $ flip fmap eIn $ \(enable, depId) -> (depId, Map.singleton txId (NES.singleton enable))
  transactionState_txDependents %= Map.unionWith (Map.unionWith (<>)) newDeps . Map.delete aId

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
