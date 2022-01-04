{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | QuickCheck generators for top level model ledger types.  see also:
--
-- * "Test.Cardano.Ledger.Model.Generators.Certificates"
-- * "Test.Cardano.Ledger.Model.Generators.Value"
-- * "Test.Cardano.Ledger.Model.Generators.Address"
-- * "Test.Cardano.Ledger.Model.Generators.TxOut"
-- * "Test.Cardano.Ledger.Model.Generators.Tx"
-- * "Test.Cardano.Ledger.Model.Generators.Script"
module Test.Cardano.Ledger.Model.Generators.Chain
  ( genModel,
  )
where

import Cardano.Ledger.BaseTypes
  ( Globals (..),
    epochInfo,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API (epochInfoFirst, epochInfoSize)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Control.Lens
  ( to,
    use,
    (.=),
  )
import Control.Monad (replicateM)
import Control.Monad.Reader.Class (asks)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Supply (MonadSupply (..))
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Traversable (for)
import Data.Void (Void)
import QuickCheck.GenT
  ( Gen,
    MonadGen,
    choose,
    liftGen,
    oneof,
    shuffle,
    sublistOf,
  )
import Test.Cardano.Ledger.Model.API
  ( ModelBlock (..),
    ModelEpoch (..),
    ModelGenesis (..),
    ModelLedger,
    applyModelBlocksMade,
    applyModelTx,
    getModelLedger_epoch,
    mkModelLedger,
    modelLedger,
    modelLedger_nes,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelBlocksMade (..),
  )
import Test.Cardano.Ledger.Model.FeatureSet (KnownRequiredFeatures)
import Test.Cardano.Ledger.Model.Fixup (repartition)
import Test.Cardano.Ledger.Model.Generators
  ( Faucet (..),
    HasGenModelM,
    ModelGeneratorContext (..),
    ModelGeneratorParams,
    ModelGeneratorParamsF (..),
    runGenModelM,
  )
import Test.Cardano.Ledger.Model.Generators.Address
  ( freshCredential,
    freshPaymentAddress,
  )
import Test.Cardano.Ledger.Model.Generators.Tx
  ( genModelTx,
    wouldSpendLastCollateral,
  )
import Test.Cardano.Ledger.Model.Generators.TxOut
  ( freshUTxOId,
    minOutput,
  )
import Test.Cardano.Ledger.Model.Generators.Value (unfoldModelValue)
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelGenesisDelegation,
    modelEpochState_ss,
    modelNewEpochState_es,
    modelSnapshot_pools,
    modelSnapshots_pstake,
  )
import Test.Cardano.Ledger.Model.PParams
  ( ModelPParams,
    ModelPParamsF (..),
    modelPParams,
  )
import Test.Cardano.Ledger.Model.Script (ModelAddress)
import Test.Cardano.Ledger.Model.Snapshot
  ( snapshotQueue_go,
  )
import Test.Cardano.Ledger.Model.TxOut (ModelUTxOId)
import Test.Cardano.Ledger.Model.Value (ModelValueF (..))

genBlocksMade :: HasGenModelM st era m => m ModelBlocksMade
genBlocksMade = do
  pools <-
    use
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ss . modelSnapshots_pstake . snapshotQueue_go . modelSnapshot_pools)
  currentEpoch <- use $ modelLedger . to getModelLedger_epoch
  EpochSize numSlots <- asks $ runIdentity . flip epochInfoSize currentEpoch . epochInfo . _modelGeneratorContext_globals
  pools' <- Map.fromList . take (fromEnum numSlots) <$> shuffle (Map.toList pools)

  -- TODO: Model scenarios where pools make varying amounts of blocks (e.g. 0 or many)
  let mblocksMadeWeights = 1 % max 1 (toInteger $ Map.size pools') <$ pools'
      mblocksMade = repartition (fromIntegral numSlots) mblocksMadeWeights

  pure $ ModelBlocksMade mblocksMade

genModelEpoch :: HasGenModelM st era m => m (ModelEpoch era)
genModelEpoch = do
  currentEpoch <- use $ modelLedger . to getModelLedger_epoch
  EpochSize numSlots <- asks $ runIdentity . flip epochInfoSize currentEpoch . epochInfo . _modelGeneratorContext_globals
  firstSlot <- asks $ runIdentity . flip epochInfoFirst currentEpoch . epochInfo . _modelGeneratorContext_globals

  -- we don't have to put a block in every slot.
  numSlotsUsed <- liftGen =<< asks (_modelGeneratorParams_numSlotsUsed . _modelGeneratorContext_modelGeneratorParams)
  slots <- take numSlotsUsed <$> sublistOf [0 .. SlotNo numSlots - 1]

  blocks <- for slots $ \slot' -> do
    numTxns <- liftGen =<< asks (_modelGeneratorParams_txnsPerSlot . _modelGeneratorContext_modelGeneratorParams)
    txns <- for [0 .. numTxns - 1] $ \txnNo -> do
      txn <- genModelTx
      wouldSpendLastCollateral slot' txn >>= \case
        False -> pure ()
        True -> do
          st <- State.get
          error $
            unlines
              [ "wouldSpendLastCollateral",
                show txn,
                show st
              ]
      applyModelTx (slot' + firstSlot) txnNo txn
      pure txn
    pure
      ModelBlock
        { _modelBlock_txSeq = txns,
          _modelBlock_slot = slot'
        }

  blocksMade <- genBlocksMade

  applyModelBlocksMade blocksMade

  pure
    ModelEpoch
      { _modelEpoch_blocks = blocks,
        _modelEpoch_blocksMade = blocksMade
      }

genGenesesUTxOs :: (MonadGen m, MonadSupply Integer m) => ModelGeneratorParams -> m (Map.Map ModelUTxOId (ModelAddress sf, Coin))
genGenesesUTxOs ctx = do
  genesisSupply <- liftGen (_modelGeneratorParams_genesesAcct ctx)
  g' <- liftGen $ unfoldModelValue @Void (Coin minOutput) (Val.inject genesisSupply)
  fmap Map.fromList $
    for (toList g') $ \(ModelValueF (x, _)) ->
      (\ui addr vl -> (ui, (addr, vl)))
        <$> freshUTxOId
        <*> freshPaymentAddress "gen"
        <*> pure x

genGenesisDelegates :: MonadSupply Integer m => Globals -> m ModelGenesisDelegation
genGenesisDelegates globals = do
  let q = quorum globals
  numDelegs <- pure (q + 2) -- TODO, other values could be reasonable.
  Map.fromList
    <$> replicateM
      (fromEnum numDelegs)
      ((,) <$> freshCredential "gen" <*> freshCredential "gen-deleg")

genPParams :: KnownRequiredFeatures era => Gen (ModelPParams era)
genPParams = do
  keyDeposit <- oneof [pure mempty, Coin <$> choose (1, 50_000_000)]
  poolDeposit <- oneof [pure mempty, Coin <$> choose (1, 50_000_000)]
  pure
    modelPParams
      { _modelPParams_keyDeposit = Identity keyDeposit,
        _modelPParams_poolDeposit = Identity poolDeposit
      }

genModel ::
  forall era.
  KnownRequiredFeatures era =>
  Globals ->
  ModelGeneratorParams ->
  Gen
    ( ModelGenesis era, -- [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)],
      [ModelEpoch era]
    )
genModel globals ctx = do
  pp <- liftGen genPParams
  let st0 :: Faucet (ModelLedger era)
      st0 = Faucet 0 $ mkModelLedger globals (ModelGenesis pp Map.empty $ Map.empty)

  runGenModelM (ModelGeneratorContext globals ctx) st0 $ do
    genesisUtxos <- genGenesesUTxOs ctx
    genDelegs <- genGenesisDelegates globals
    modelLedger .= mkModelLedger globals (ModelGenesis pp genDelegs genesisUtxos)

    numEpochs <- liftGen $ _modelGeneratorParams_epochs ctx
    epochs <- replicateM (fromEnum numEpochs) $ do
      epoch <- genModelEpoch
      pure epoch

    pure (ModelGenesis pp genDelegs genesisUtxos, epochs)
