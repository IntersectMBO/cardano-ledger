{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Graph.Inductive as FGL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import QuickCheck.GenT
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.Address
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
    -- | Node ID to label
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
  forM_ chosenSpenders $ \i ->
    actionState_withDeps . at i . _Just . _3 . at aId .= Just Enables_Spending
  -- Then, decide how many of those spenders should be removed from needing inputs
  numDeleted <-
    frequency
      [ (4, pure 0),
        (1, choose (0, numOutputs))
      ]
  chosenDeleted <- take numDeleted <$> shuffle chosenSpenders
  actionState_needsInputs %= (`Set.difference` (Set.fromList chosenDeleted))

makeWithdrawal :: (MonadGen m, MonadState ActionState m) => Int -> Int -> Set Int -> m ()
makeWithdrawal aId epoch withdrawals = do
  case Set.toList withdrawals of
    [] -> pure ()
    _ : _ -> do
      wId <- elements (Set.toList withdrawals)
      actionState_withDeps . at wId . _Just . _3 . at aId .= Just Enables_Withdrawing
      actionState_needsDelegate . at epoch . _Just %= Set.delete wId

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
    actionState_withDeps . at i .= Just (epoch, a, mempty)
    -- Action specific logic. An action reports the dependencies it needs fulfilled
    -- and also chooses to fulfill some dependencies that have been emitted by actions
    -- that happen after it.
    case a of
      Action_Withdraw -> do
        -- Withdrawal produces an output that can be spent, so flip a coin if it's used
        spenders <- use actionState_needsInputs
        outputTo i 0 1 spenders

        -- Withdrawal can only occur if a delegation was made in the past
        actionState_needsDelegate . at epoch %= Just . maybe (Set.singleton i) (Set.insert i)
      Action_Delegate -> do
        -- Delegation can enable withdrawal of staking rewards
        -- A particular delegation can only enable a withdrawal once per epoch
        eligibleEpochs <- uses actionState_needsDelegate $ Map.filterWithKey (\e _ -> e >= epoch + 2)
        iforM_ eligibleEpochs $ \e withdrawals ->
          frequency
            [ (1, pure ()),
              (3, makeWithdrawal i e withdrawals)
            ]

        -- Delegation needs a staking certificate and registered pool
        actionState_needsStake %= Set.insert i
        actionState_needsPool %= Set.insert i
      Action_Spend -> do
        -- Spends create outputs which can then be used in later spends
        spenders <- use actionState_needsInputs
        outputTo i 0 4 spenders

        -- Spending requires inputs, which potentially could come from the genesis block
        actionState_needsInputs %= Set.insert i
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
  pure ()

genGraph :: (MonadGen m, MonadSupply Int m, MonadState ActionState m) => m ()
genGraph = do
  -- First figure out how many epochs we're aiming for.
  -- We need to do this here because delegation to a pool
  -- has to happen some number of epochs (2) before reward
  -- withdrawal.
  numEpochs <- choose (minEpochs, maxEpochs)
  forM_ [numEpochs, numEpochs -1 .. 1] genEpoch
  pure ()

testGenGraph :: IO ActionState
testGenGraph = do
  s <- generate (execStateT genGraph initialActionState)
  pure $ s & actionState_freshNames .~ []

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
