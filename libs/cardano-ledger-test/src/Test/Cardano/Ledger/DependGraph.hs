{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.DependGraph where

import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (Globals, boundRational, epochInfo)
import Cardano.Ledger.Coin
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API (epochInfoFirst, epochInfoSize)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Control.Arrow ((&&&))
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Supply
import Control.Monad.Writer.CPS
import Data.Bool (bool)
import Data.Either
import Data.Foldable
import qualified Data.Graph.Inductive as FGL
import Data.Group
import Data.Group.GrpMap
import Data.HKD
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Proxy
import Data.Ratio ((%))
import Data.Semigroup.Foldable (fold1)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Data.Void
import GHC.Generics (Generic, (:*:) (..))
import qualified PlutusTx
import QuickCheck.GenT
import qualified System.Random
import Test.Cardano.Ledger.ModelChain
import Test.Cardano.Ledger.ModelChain.FeatureSet
import Test.Cardano.Ledger.ModelChain.Script
import Test.Cardano.Ledger.ModelChain.Value

data ModelGeneratorParamsF f = ModelGeneratorParams
  { _modelGeneratorParams_epochs :: f EpochNo,
    _modelGeneratorParams_genesesAcct :: f Coin,
    _modelGeneratorParams_numGenesesAcct :: f Int,
    _modelGeneratorParams_txnsPerSlot :: f Int,
    _modelGeneratorParams_numSlotsUsed :: f Int, -- TODO: make this a fraction of slots used.
    _modelGeneratorParams_numTxInputs :: f Int,
    _modelGeneratorParams_numDCerts :: f Int,
    _modelGeneratorParams_numWdrls :: f Int
  }
  deriving (Generic)

instance FFunctor ModelGeneratorParamsF where ffmap = ffmapDefault

instance FZip ModelGeneratorParamsF where fzipWith = gfzipWith

instance FRepeat ModelGeneratorParamsF where frepeat = gfrepeat

instance FFoldable ModelGeneratorParamsF where ffoldMap = ffoldMapDefault

instance FTraversable ModelGeneratorParamsF where ftraverse = gftraverse

type ModelGeneratorParams = ModelGeneratorParamsF Gen

defaultModelGeneratorParams :: ModelGeneratorParams
defaultModelGeneratorParams =
  ModelGeneratorParams
    { _modelGeneratorParams_epochs = choose (20, 30),
      _modelGeneratorParams_genesesAcct = Coin <$> choose (100_000 * minOutput, 45e12),
      _modelGeneratorParams_numGenesesAcct = choose (1, 20),
      _modelGeneratorParams_txnsPerSlot = choose (1, 20),
      _modelGeneratorParams_numSlotsUsed = choose (0, 100),
      _modelGeneratorParams_numTxInputs = frequency [(10, pure 1), (1, choose (1, 8))],
      _modelGeneratorParams_numDCerts = frequency [(10, pure 0), (1, choose (1, 5))],
      _modelGeneratorParams_numWdrls = choose (1, 5)
    }

data ModelGeneratorContext = ModelGeneratorContext
  { _modelGeneratorContext_globals :: !Globals,
    _modelGeneratorContext_modelGeneratorParams :: !ModelGeneratorParams
  }

modelGeneratorContext_globals :: Lens' ModelGeneratorContext Globals
modelGeneratorContext_globals a2fb s = (\b -> s {_modelGeneratorContext_globals = b}) <$> a2fb (_modelGeneratorContext_globals s)
{-# INLINE modelGeneratorContext_globals #-}

modelGeneratorContext_modelGeneratorParams :: Lens' ModelGeneratorContext ModelGeneratorParams
modelGeneratorContext_modelGeneratorParams a2fb s = (\b -> s {_modelGeneratorContext_modelGeneratorParams = b}) <$> a2fb (_modelGeneratorContext_modelGeneratorParams s)
{-# INLINE modelGeneratorContext_modelGeneratorParams #-}

instance HasGlobals ModelGeneratorContext where
  getGlobals = _modelGeneratorContext_globals

type HasGenModelM st era m =
  ( MonadReader ModelGeneratorContext m,
    State.MonadState st m,
    HasModelLedger era st,
    MonadGen m,
    KnownRequiredFeatures era,
    MonadSupply Integer m,
    Show st,
    MonadModelProvenance era m
  )

genInputs :: HasGenModelM st era m => AllowScripts (ScriptFeature era) -> m (Map ModelUTxOId (ModelTxOut era))
genInputs allowScripts = do
  actualUtxos <- uses (modelLedger . to getModelLedger_utxos) _modelUTxOMap_utxos
  utxos0 <- shuffle =<< uses (modelLedger . to getModelLedger_utxos) (mapMaybe (_2 . _2 . modelTxOut_address . modelAddress_pmt $ guardHaveCollateral allowScripts) . Map.toList . _modelUTxOMap_utxos)

  let spendable :: (Coin, ModelTxOut era) -> Coin
      spendable = fst

      go :: [(ModelUTxOId, (Coin, ModelTxOut era))] -> Coin -> [(ModelUTxOId, (Coin, ModelTxOut era))] -> [(ModelUTxOId, (Coin, ModelTxOut era))]
      go [] val acc
        | val >= Coin (minFee + minOutput) = acc
        | otherwise =
          error $
            unlines
              [ "insufficient UTxO's to proceed with generation.",
                show actualUtxos
              ]
      -- TODO, get rewards/fees back into circulation in generator.
      go (utxo : rest) val acc
        | val < Coin (minFee + minOutput) = go rest (val <> spendable (snd utxo)) (utxo : acc)
        | otherwise = acc

  numTxInputs <- liftGen =<< asks (_modelGeneratorParams_numTxInputs . _modelGeneratorContext_modelGeneratorParams)
  let utxos1 = (take numTxInputs utxos0)
      val1 = foldMap (spendable . snd) utxos1
  pure $ Map.fromList $ (fmap . fmap) snd $ go (drop numTxInputs utxos0) val1 utxos1

-- | divide a value into several "chunks"
-- y = unfoldModelValue minCoin x
-- PREC: minCoin =< coin x
-- POSTC: fold y === x .&&. all ((minCoin =<) . coin) y
unfoldModelValue :: forall x. Ord x => Coin -> ModelValueSimple x -> Gen (NonEmpty (ModelValueSimple x))
unfoldModelValue (Coin minValue) = go
  where
    splitMA :: Sum Integer -> Gen (Sum Integer)
    splitMA (Sum a) =
      frequency
        [ (1, pure (Sum a)),
          (1, Sum <$> choose (0, a)),
          (1, pure (mempty))
        ]

    go :: ModelValueSimple x -> Gen (NonEmpty (ModelValueSimple x))
    go m@(ModelValueSimple (Coin ada, ma))
      | ada <= 2 * minValue = pure (pure m)
      | otherwise = do
        adaL <- Coin <$> choose (minValue, ada - minValue)
        maL <- traverseGrpMap splitMA ma
        let adaR = Coin ada ~~ adaL
            maR = ma ~~ maL
            m' = (pure (ModelValueSimple (adaL, maL)) <> pure (ModelValueSimple (adaR, maR)))
        frequency
          [ (10, pure m'),
            (1, fold1 <$> traverse go m')
          ]

genScriptData :: forall sf r. KnownScriptFeature sf => ModelCredential r sf -> Gen (IfSupportsPlutus () (Maybe PlutusTx.Data) sf)
genScriptData addr = traverseSupportsPlutus id $
  ifSupportsPlutus (Proxy :: Proxy sf) () $ case addr of
    ModelKeyHashObj _ -> pure Nothing
    -- ModelScriptHashObj _ -> Just . PlutusTx.I <$> arbitrary
    ModelScriptHashObj _ -> Just . PlutusTx.I <$> pure 0

type AllowScripts sf = IfSupportsPlutus () Bool sf

genCollateral ::
  forall era m st.
  HasGenModelM st era m =>
  m (AllowScripts (ScriptFeature era), IfSupportsPlutus () (Set ModelUTxOId) (ScriptFeature era))
genCollateral = do
  res <- flip traverseSupportsPlutus (reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era))) $ \() -> do
    availableCollateralInputs <- uses (modelLedger . to getModelLedger_utxos) $ _modelUTxOMap_collateralUtxos
    numCollateralInputs <- choose (1, min 5 (Set.size availableCollateralInputs - 1))
    (collateral, rest) <- chooseElems numCollateralInputs availableCollateralInputs
    -- avoid spending the last unlocked utxo
    pure $
      if 10 > Set.size rest -- genInputs may use up to 8 inputs.
        then Set.empty
        else collateral

  pure (mapSupportsPlutus (not . Set.null) res, res)

guardHaveCollateral ::
  IfSupportsPlutus () Bool sf ->
  ModelCredential k sf ->
  Maybe (ModelCredential k sf)
guardHaveCollateral (SupportsPlutus False) (ModelScriptHashObj _) = Nothing
guardHaveCollateral _ x = Just x

genOutputs ::
  HasGenModelM st era m =>
  AllowScripts (ScriptFeature era) ->
  Map ModelUTxOId (ModelTxOut era) ->
  IfSupportsMint () (ModelValue (ValueFeature era) era) (ValueFeature era) ->
  m ([(ModelUTxOId, ModelTxOut era)], ModelValue 'ExpectAdaOnly era)
genOutputs haveCollateral ins mint = do
  -- by assumption, there are no rewards; since they would have been outputs to
  -- earlier transactions, and thus have different value now. thus the non-ada
  -- values are entirely known qty multi-asset outputs.
  let (ModelValueSimple (Coin inAda, ma)) = either (error . show) id $ evalModelValueSimple $ unModelValue $ fromSupportsMint (\() -> mempty) id mint <> foldMap _mtxo_value ins
  -- TODO: corner case, if the amount of inAda < minFee + minOutput && ma > 0;
  -- the inputs are unspendable, and the generator needs to abort.
  (fee, outVals) <-
    if
        | inAda < minFee -> error "input too small"
        | inAda < minFee + minOutput -> pure (inAda, [])
        | otherwise -> do
          fee <- choose (minFee, min (inAda - minOutput) maxFee)
          outVals <- liftGen $ unfoldModelValue (Coin minOutput) (ModelValueSimple (Coin inAda ~~ Coin fee, ma))
          pure (fee, toList outVals)

  delegates <-
    uses
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ss . modelSnapshots_pstake . snapshotQueue_mark . modelSnapshot_delegations)
      Map.keys

  outs <- for outVals $ \outVal -> do
    ui <- freshUtxoId
    addr <-
      oneof $
        (fmap pure $ mapMaybe ((modelAddress_pmt $ guardHaveCollateral haveCollateral) . _mtxo_address) $ toList ins)
          <> [genAddr haveCollateral "genOutputs"]
          <> if null delegates then [] else [freshWdrlAddress =<< elements delegates]
    dh <- liftGen $ genScriptData $ _modelAddress_pmt addr
    pure (ui, ModelTxOut addr (mkModelValue outVal) dh)
  pure (outs, ModelValue $ ModelValue_Inject $ Coin $ fee)

genDCert :: forall st era m. HasGenModelM st era m => AllowScripts (ScriptFeature era) -> m (ModelDCert era)
genDCert allowScripts = do
  stakeHolders <- uses (modelLedger . to getModelLedger_utxos) $ Map.keysSet . unGrpMap . _modelUTxOMap_stake
  registeredStake <- uses (modelLedger . to getModelLedger_rewards) $ Map.keysSet
  pools <-
    uses
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ss . modelSnapshots_pstake . snapshotQueue_mark . modelSnapshot_pools)
      $ Map.keys

  let unregisteredStake = Set.difference stakeHolders registeredStake
      registeredStake' = Set.filter (isJust . guardHaveCollateral allowScripts) registeredStake

  frequency $
    [(1, ModelCertPool . ModelRegPool <$> genModelPool)]
      <> [ (1, ModelCertDeleg . ModelRegKey <$> elements (Set.toList unregisteredStake))
           | not (null unregisteredStake)
         ]
      <> [ (1, fmap (ModelCertDeleg . ModelDelegate) $ ModelDelegation <$> elements (Set.toList registeredStake') <*> elements pools)
           | not (null registeredStake'),
             not (null pools)
         ]

genWdrl :: HasGenModelM st era m => AllowScripts (ScriptFeature era) -> m (Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
genWdrl allowScripts = do
  allRewards <- uses (modelLedger . to getModelLedger_rewards) $ Map.filter (/= Val.zero)
  numWdrls <- liftGen =<< asks (_modelGeneratorParams_numWdrls . _modelGeneratorContext_modelGeneratorParams)
  (rewards, _) <- chooseElems numWdrls $ Map.mapMaybeWithKey (\k v -> v <$ guardHaveCollateral allowScripts k) allRewards
  pure rewards

needCollateral ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Map ModelUTxOId (ModelTxOut era) ->
  Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era) ->
  IfSupportsMint () (ModelValue (ValueFeature era) era) (ValueFeature era) ->
  [ModelDCert ('FeatureSet (ValueFeature era) (ScriptFeature era))] ->
  Bool
needCollateral ins wdrls mint dcerts = case reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)) of
  NoPlutusSupport () -> False
  SupportsPlutus () ->
    has (traverse . modelTxOut_address . modelAddress_pmt . _ModelScriptHashObj) ins
      || has (traverse . _ModelScriptHashObj) (Map.keys wdrls)
      || fromSupportsMint (\() -> False) (any isPlutusMintAsset . unModelValue) mint
      || has (traverse . _ModelDelegate . modelDelegation_delegator . _ModelScriptHashObj) dcerts
  where
    isPlutusMintAsset :: ModelValueVars era (ValueFeature era) -> Bool
    isPlutusMintAsset (ModelValue_MA (ModelScript_PlutusV1 _, _)) = True
    isPlutusMintAsset _ = False

wouldSpendLastCollateral :: HasGenModelM st era m => SlotNo -> ModelTx era -> m Bool
wouldSpendLastCollateral slot txn = do
  g <- asks getGlobals
  ml' <- uses modelLedger (execModelM (applyModelTx slot 999 txn) g)
  pure . Set.null . _modelUTxOMap_collateralUtxos $ getModelLedger_utxos ml'

genModelTx :: forall era m st. SlotNo -> HasGenModelM st era m => m (ModelTx era)
genModelTx slot = do
  (haveCollateral, collateral) <- genCollateral
  ins <- genInputs haveCollateral
  wdrl <- fmap (ModelValue . ModelValue_Inject) <$> genWdrl haveCollateral

  mint <- pure $ ifSupportsMint (Proxy :: Proxy (ValueFeature era)) () mempty
  (outs, fee) <- genOutputs haveCollateral ins mint
  let txn =
        ModelTx
          { _mtxInputs = Map.keysSet ins,
            _mtxOutputs = outs,
            _mtxFee = fee <> fold wdrl, -- TODO, put withdwrawals in outputs sometimes.
            _mtxDCert = [],
            _mtxWdrl = wdrl,
            _mtxMint = mint,
            _mtxCollateral = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Set.empty,
            _mtxValidity = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () (IsValid True),
            _mtxRedeemers = Map.empty,
            _mtxWitnessSigs = Set.empty
          }

  dcerts <- do
    st0 <- use modelLedger
    applyModelTx slot 999 txn -- bogus txIx; we rewind after we've finished generating the tx.
    numDCerts <- liftGen =<< asks (_modelGeneratorParams_numDCerts . _modelGeneratorContext_modelGeneratorParams)
    dcerts <- replicateM numDCerts $ do
      dcert <- genDCert haveCollateral
      applyModelDCert dcert
      pure dcert

    modelLedger .= st0

    pure dcerts

  let nc = needCollateral ins wdrl mint dcerts
  uses modelLedger $
    witnessModelTx
      txn
        { _mtxDCert = dcerts,
          _mtxCollateral = if nc then collateral else _mtxCollateral txn
        }

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
      txn <- genModelTx slot'
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

graphHeads :: FGL.Graph gr => gr a b -> [FGL.LNode a]
graphHeads gr = mapMaybe f (FGL.nodes gr)
  where
    f n = do
      c <- fst (FGL.match n gr)
      guard $ null $ FGL.inn' c
      pure $ FGL.labNode' c

-- TODO: this could be a more interesting pool.
genModelPool :: (Show n, MonadSupply n m) => m (ModelPoolParams era)
genModelPool = do
  poolId <- freshPoolAddress
  poolVrf <- freshCredential "poolVrf"
  racct <- freshRewardAddress
  powner <- freshRewardAddress
  pure $ ModelPoolParams poolId poolVrf (Coin 0) (Coin 0) (fromJust $ boundRational $ 0 % 1) racct [powner]

minFee :: Integer
minFee = 100000

maxFee :: Integer
maxFee = 10 * minFee

-- TODO: monoidal-containers
newtype MonMap k v = MonMap {unMonMap :: Map k v}
  deriving (Functor)

instance (Ord k, Semigroup v) => Semigroup (MonMap k v) where
  MonMap xs <> MonMap ys = MonMap $ Map.unionWith (<>) xs ys

instance (Ord k, Semigroup v) => Monoid (MonMap k v) where
  mempty = MonMap Map.empty

genPartition :: [a] -> Gen ([a], [a])
genPartition xs = do
  size <- getSize
  let bias = frequency [(max 0 (100 - size), pure False), (max 0 size, pure True)]
  partitionEithers <$> traverse (\x -> bool (Left x) (Right x) <$> bias) xs

minOutput :: Integer
minOutput = 500_000

genGenesesUTxOs :: (MonadGen m, MonadSupply Integer m) => ModelGeneratorParams -> m [(ModelUTxOId, ModelAddress sf, Coin)]
genGenesesUTxOs ctx = do
  genesisSupply <- liftGen (_modelGeneratorParams_genesesAcct ctx)
  g' <- liftGen $ unfoldModelValue @Void (Coin minOutput) (Val.inject genesisSupply)
  xs <- for g' $ \(ModelValueSimple (x, _)) ->
    (,,)
      <$> freshUtxoId
      <*> freshPaymentAddress "gen"
      <*> pure x

  pure $ toList xs

-- TODO
genGenesisDelegates :: Applicative m => m ModelGenesisDelegation
genGenesisDelegates = pure Map.empty

newtype GenModelM era a = GenModelM {unGenModelM :: State.StateT (Faucet (ModelLedger era)) (ReaderT ModelGeneratorContext Gen) a}
  deriving (Functor, Applicative, Monad, MonadGen)

deriving newtype instance MonadReader ModelGeneratorContext (GenModelM era)

deriving newtype instance State.MonadState (Faucet (ModelLedger era)) (GenModelM era)

deriving newtype instance MonadSupply Integer (GenModelM era)

instance MonadModelProvenance era (GenModelM era) where
  setProvenance _ = pure ()
  setSlot _ _ = pure ()
  clearProvenance = pure ()
  delegProvenance _ = pure ()
  rewardOperatorProvenance _ _ = pure ()
  rewardMemberProvenance _ _ = pure ()
  wdrlProvenance _ = pure ()

runGenModelM ::
  KnownRequiredFeatures era =>
  ModelGeneratorContext ->
  Faucet (ModelLedger era) ->
  (forall st m. HasGenModelM st era m => m a) ->
  Gen a
runGenModelM r s k = runReaderT (State.evalStateT (unGenModelM k) s) r

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
  pp <- pure modelPParams -- TODO
  let st0 :: Faucet (ModelLedger era)
      st0 = Faucet 0 $ mkModelLedger globals (ModelGenesis pp Map.empty [])

  runGenModelM (ModelGeneratorContext globals ctx) st0 $ do
    genesisUtxos <- genGenesesUTxOs ctx
    genDelegs <- genGenesisDelegates
    modelLedger .= mkModelLedger globals (ModelGenesis pp genDelegs genesisUtxos)

    numEpochs <- liftGen $ _modelGeneratorParams_epochs ctx
    epochs <- replicateM (fromEnum numEpochs) $ do
      epoch <- genModelEpoch
      pure epoch

    pure (ModelGenesis pp genDelegs genesisUtxos, epochs)

freshUtxoId :: (Integral n, MonadSupply n m) => m ModelUTxOId
freshUtxoId = ModelUTxOId . toInteger <$> supply

freshPaymentAddress :: (Show n, MonadSupply n m) => String -> m (ModelAddress era)
freshPaymentAddress clue =
  ModelAddress
    <$> freshCredential ("pmt:" <> clue)
    <*> freshCredential ("stk:" <> clue)

freshPaymentScript ::
  MonadSupply Integer m =>
  m (ModelCredential 'Payment ('TyScriptFeature x 'True))
freshPaymentScript = do
  x <- supply
  pure $ ModelScriptHashObj $ ModelPlutusScript_Salt x $ ModelPlutusScript_Preprocessed SumsTo103

genPaymentCredential ::
  forall sf m.
  (KnownScriptFeature sf, MonadSupply Integer m, MonadGen m) =>
  AllowScripts sf ->
  String ->
  m (ModelCredential 'Payment sf)
genPaymentCredential haveCollateral clue =
  oneof $
    [freshCredential clue] <> case reifyScriptFeature (Proxy :: Proxy sf) of
      ScriptFeatureTag_None -> []
      ScriptFeatureTag_Simple -> []
      ScriptFeatureTag_PlutusV1 -> case haveCollateral of
        SupportsPlutus True -> [freshPaymentScript]
        _ -> []

freshStakeScript ::
  MonadSupply Integer m =>
  m (ModelCredential 'Staking ('TyScriptFeature x 'True))
freshStakeScript = do
  x <- supply
  pure $ ModelScriptHashObj $ ModelPlutusScript_Salt x $ ModelPlutusScript_Preprocessed RedeemerIs102

genStakingCredential ::
  forall sf m.
  (KnownScriptFeature sf, MonadSupply Integer m, MonadGen m) =>
  String ->
  m (ModelCredential 'Staking sf)
genStakingCredential clue =
  oneof $
    [freshCredential clue] <> case reifyScriptFeature (Proxy :: Proxy sf) of
      ScriptFeatureTag_None -> []
      ScriptFeatureTag_Simple -> []
      ScriptFeatureTag_PlutusV1 -> [freshStakeScript]

genAddr ::
  ( MonadGen m,
    MonadSupply Integer m,
    KnownScriptFeature sf
  ) =>
  AllowScripts sf ->
  String ->
  m (ModelAddress sf)
genAddr haveCollateral clue =
  ModelAddress
    <$> genPaymentCredential haveCollateral clue
    <*> genStakingCredential clue

freshCredential :: (Show n, MonadSupply n m) => String -> m (ModelCredential r era)
freshCredential clue = ModelKeyHashObj . (clue <>) . show <$> supply

freshRewardAddress :: (Show n, MonadSupply n m) => m (ModelCredential 'Staking era)
freshRewardAddress = ModelKeyHashObj . ("reward_" <>) . show <$> supply

freshPoolAddress :: (Show n, MonadSupply n m) => m ModelPoolId
freshPoolAddress = ModelPoolId <$> freshCredential "pool_"

freshWdrlAddress :: (Show n, MonadSupply n m) => ModelCredential 'Staking era -> m (ModelAddress era)
freshWdrlAddress c = do
  c' <- case c of
    ModelKeyHashObj x -> freshCredential ("wdrl-" <> x)
    ModelScriptHashObj _ -> freshCredential "wdrl"
  pure $ ModelAddress c' c

-- Orphans
deriving newtype instance System.Random.Random EpochNo -- TODO: this can be moved closer to the package that defines EpochNo

class ChooseElems a where
  chooseElems :: MonadGen m => Int -> a -> m (a, a)

mkSeqChooseElems ::
  MonadGen m =>
  (s -> Int) ->
  s ->
  (s -> a -> s) ->
  (Int -> s -> a) ->
  (Int -> s -> s) ->
  (Int -> s -> s) ->
  (s -> s -> s) ->
  Int ->
  s ->
  m (s, s)
mkSeqChooseElems seqSize emptySeq seqAppend seqElemAt seqTake seqDrop seqConcat =
  let go n (xs, ys) =
        let sz = seqSize ys
         in if n <= 0 || sz <= 0
              then pure (xs, ys)
              else do
                i <- choose (0, sz - 1)
                go (n -1) (seqAppend xs (seqElemAt i ys), seqTake i ys `seqConcat` seqDrop (i + 1) ys)
   in \n0 xs0 -> go n0 (emptySeq, xs0)
{-# INLINE mkSeqChooseElems #-}

instance ChooseElems (Seq a) where
  chooseElems = mkSeqChooseElems Seq.length mempty (:>) (flip Seq.index) Seq.take Seq.drop (<>)

instance ChooseElems (StrictSeq.StrictSeq a) where
  chooseElems = mkSeqChooseElems StrictSeq.length mempty (StrictSeq.:|>) (\i -> maybe (error "ChooseElems @StrictSeq") id . StrictSeq.lookup i) StrictSeq.take StrictSeq.drop (<>)

instance Ord k => ChooseElems (Map k a) where
  chooseElems = mkSeqChooseElems Map.size Map.empty (\xs (k, x) -> Map.insert k x xs) Map.elemAt Map.take Map.drop Map.union

instance Ord a => ChooseElems (Set a) where
  chooseElems = mkSeqChooseElems Set.size Set.empty (flip Set.insert) Set.elemAt Set.take Set.drop Set.union

instance MonadGen g => MonadGen (State.StateT s g) where
  liftGen = lift . liftGen
  variant n a = State.StateT $ \s -> variant n (State.runStateT a s)
  sized f = State.StateT $ \s -> sized (\i -> State.runStateT (f i) s)
  resize n a = State.StateT $ \s -> resize n (State.runStateT a s)
  choose = lift . choose

instance MonadGen g => MonadGen (ReaderT r g) where
  liftGen = lift . liftGen
  variant n a = ReaderT $ \s -> variant n (runReaderT a s)
  sized f = ReaderT $ \s -> sized (\i -> runReaderT (f i) s)
  resize n a = ReaderT $ \s -> resize n (runReaderT a s)
  choose = lift . choose

instance (Monoid w, MonadGen g) => MonadGen (WriterT w g) where
  liftGen = lift . liftGen
  variant n a = writerT $ variant n (runWriterT a)
  sized f = writerT $ sized (\i -> runWriterT (f i))
  resize n a = writerT $ resize n (runWriterT a)
  choose = lift . choose

data Faucet a = Faucet
  { _faucet_supply :: !Integer,
    _faucet_state :: !a
  }
  deriving (Show)

faucet_supply :: Lens' (Faucet a) Integer
faucet_supply a2fb s = (\b -> s {_faucet_supply = b}) <$> a2fb (_faucet_supply s)
{-# INLINE faucet_supply #-}

faucet_state :: Lens' (Faucet a) a
faucet_state a2fb s = (\b -> s {_faucet_state = b}) <$> a2fb (_faucet_state s)
{-# INLINE faucet_state #-}

instance HasModelLedger era a => HasModelLedger era (Faucet a) where
  modelLedger = faucet_state . modelLedger

instance {-# OVERLAPPING #-} (Monad m) => MonadSupply Integer (State.StateT (Faucet s) m) where
  supply = faucet_supply <<+= 1
  peek = use faucet_supply
  exhausted = pure False

data FixupValuesFlags = FixupValuesFlags
  { allowSigChanges :: Bool
  }

type ModelValueSimple' era = ModelValueSimple (ModelValueVars era (ValueFeature era))

data FixupValuesError era
  = FixupValues_BadWdrls
      (Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era))
      (Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era))
      (Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era))
      ModelTxId
  | FixupValues_BadWitness
      String
      (Set (ModelCredential 'Witness ShelleyScriptFeatures), Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits))
      ModelTxId
  | FixupValues_FeeTooSmall
      Integer
      (ModelValueSimple' era)
      (ModelValueSimple' era)
      ModelTxId

_FixupValues_BadWdrls ::
  Prism'
    (FixupValuesError era)
    ( (Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era)),
      (Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era)),
      (Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era)),
      ModelTxId
    )
_FixupValues_BadWdrls = prism (\(a, b, c, d) -> FixupValues_BadWdrls a b c d) $ \case
  FixupValues_BadWdrls a b c d -> Right (a, b, c, d)
  x -> Left x
{-# INLINE _FixupValues_BadWdrls #-}

_FixupValues_BadWitness ::
  Prism'
    (FixupValuesError era)
    ( String,
      (Set (ModelCredential 'Witness ShelleyScriptFeatures), Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)),
      ModelTxId
    )
_FixupValues_BadWitness = prism (\(a, b, c) -> FixupValues_BadWitness a b c) $ \case
  FixupValues_BadWitness a b c -> Right (a, b, c)
  x -> Left x
{-# INLINE _FixupValues_BadWitness #-}

_FixupValues_FeeTooSmall ::
  Prism'
    (FixupValuesError era)
    ( Integer,
      (ModelValueSimple' era),
      (ModelValueSimple' era),
      ModelTxId
    )
_FixupValues_FeeTooSmall = prism (\(a, b, c, d) -> FixupValues_FeeTooSmall a b c d) $ \case
  FixupValues_FeeTooSmall a b c d -> Right (a, b, c, d)
  x -> Left x
{-# INLINE _FixupValues_FeeTooSmall #-}

type FixupValuesErrors era = NonEmpty (FixupValuesError era)

fixupValues ::
  forall era.
  KnownRequiredFeatures era =>
  FixupValuesFlags ->
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  (Maybe (FixupValuesErrors era), (ModelGenesis era, [ModelEpoch era]))
fixupValues fvf globals (genesis, epochs) =
  let tellFixupErr ::
        forall m.
        Monad m =>
        FixupValuesError era ->
        WriterT (Maybe (FixupValuesErrors era)) m ()
      tellFixupErr = tell . Just . pure

      checkSigs ::
        forall m.
        HasModelM era (ModelLedger era) Globals m =>
        String ->
        ModelTx era ->
        WriterT (Maybe (FixupValuesErrors era)) m (ModelTx era)
      checkSigs hint mtx = do
        ml <- use modelLedger
        let wits = (_mtxWitnessSigs &&& _mtxRedeemers) mtx
            wits'@(witnessSigs', redeemers') = witnessModelTxImpl mtx ml
            mtx' =
              mtx
                { _mtxWitnessSigs = witnessSigs',
                  _mtxRedeemers = redeemers'
                }

        when (wits /= wits' && not (allowSigChanges fvf)) $
          tellFixupErr $
            FixupValues_BadWitness hint wits' (getModelTxId mtx)
        pure mtx'

      evalOrDie :: ModelValue (ValueFeature era) era -> ModelValue' (ValueFeature era) era
      evalOrDie = either (error . show) id . evalModelValueSimple . unModelValue

      fixWdrl ::
        forall m.
        HasModelM era (ModelLedger era) Globals m =>
        ModelTx era ->
        WriterT (Maybe (FixupValuesErrors era)) m (ModelTx era)
      fixWdrl tx = do
        rewards <- uses modelLedger getModelLedger_rewards
        let wdrlsWithErrors' =
              Map.merge
                Map.dropMissing
                (Map.mapMissing $ \_ -> Left)
                (Map.zipWithMatched $ \_ rwd _ -> Right (ModelValue $ ModelValue_Inject rwd))
                rewards
                (_mtxWdrl tx)
            wdrls' = Map.mapMaybe (preview _Right) wdrlsWithErrors'
            wdrlErrors = Map.mapMaybe (preview _Left) wdrlsWithErrors'
            wdrlKeys = Map.keysSet $ _mtxWdrl tx
            wdrlKeys' = Map.keysSet wdrls'

        when (wdrlKeys /= wdrlKeys' || not (null wdrlErrors)) $
          tellFixupErr $
            FixupValues_BadWdrls (_mtxWdrl tx) wdrls' wdrlErrors (getModelTxId tx)

        pure tx {_mtxWdrl = wdrls'}

      -- distribute an error amount across the provided outputs, evenly
      fixBalance ::
        forall m.
        HasModelM era (ModelLedger era) Globals m =>
        ModelTx era ->
        WriterT (Maybe (FixupValuesErrors era)) m (ModelTx era)
      fixBalance
        tx@( ModelTx
               { _mtxFee = getModelValueCoin -> Coin fee,
                 _mtxOutputs = outputs
               }
             ) = do
          pp <- uses (modelLedger . modelLedger_nes) $ getModelPParams
          utxo <- uses modelLedger $ getModelLedger_utxos
          poolParams <- use $ modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_pstate . modelPState_poolParams

          let consumed = evalOrDie $ getModelConsumed pp utxo tx
              produced = evalOrDie $ getModelProduced pp poolParams tx
              ModelValueSimple (Coin mada, GrpMap mma) = consumed ~~ produced
              Coin feeMin = getModelMinfee pp tx
              minUTxOs :: Map.Map ModelUTxOId Integer = fmap (unCoin . modelMinUTxOCoins pp) $ Map.fromList outputs

              (fee', outsMap) = flip State.runState (Map.fromList $ (fmap . fmap) (evalOrDie . _mtxo_value) outputs) $ do
                outCoins :: Map.Map ModelUTxOId Integer <-
                  State.gets $
                    Map.merge
                      (Map.mapMissing $ \_ _ -> 0)
                      Map.dropMissing
                      (Map.zipWithMatched $ \_ minVal val -> unCoin (Val.coin val) - minVal)
                      minUTxOs

                let coinTotal = (fee - feeMin) + sum outCoins

                    (Identity fee'' :*: outCoins') =
                      repartition (mada + coinTotal) (Identity (fee % coinTotal) :*: ((% coinTotal) <$> outCoins))
                    mergeValue k =
                      Map.merge
                        (Map.dropMissing)
                        (Map.preserveMissing)
                        (Map.zipWithMatched $ \_ -> set (atModelValueSimple k))

                State.modify $ mergeValue Nothing $ Map.unionWith (+) outCoins' minUTxOs

                ifor_ mma $ \assetId (Sum assetVal) -> do
                  outAssets ::
                    Map.Map ModelUTxOId Integer <-
                    State.gets $ fmap (view $ atModelValueSimple (Just assetId))
                  let assetTotal = sum outAssets
                      outAssets' = repartition (assetVal + assetTotal) ((% assetTotal) <$> outAssets)

                  State.modify $ mergeValue (Just assetId) $ outAssets'

                pure (fee'' + feeMin)

              outputs' =
                (fmap . imap)
                  (\ui -> modelTxOut_value .~ foldMapOf (ix ui) mkModelValue outsMap)
                  outputs

          when (fee' <= 0) $
            tellFixupErr $
              FixupValues_FeeTooSmall
                fee'
                consumed
                produced
                (getModelTxId tx)
          pure $
            tx
              { _mtxFee = modelValueInject (Coin fee'),
                _mtxOutputs = outputs'
              }

      fixTx ::
        forall m.
        HasModelM era (ModelLedger era) Globals m =>
        SlotNo ->
        Int ->
        ModelTx era ->
        WriterT (Maybe (FixupValuesErrors era)) m (ModelTx era)
      fixTx slot txIx tx = do
        tx' <- checkSigs "after" <=< fixBalance <=< checkSigs "between" <=< fixWdrl <=< checkSigs "before" $ tx

        lift $ applyModelTx slot txIx tx'
        pure tx'

      go ::
        forall m.
        HasModelM era (ModelLedger era) Globals m =>
        WriterT (Maybe (FixupValuesErrors era)) m [ModelEpoch era]
      go = do
        for epochs $ \(ModelEpoch blocks blocksMade) -> do
          blocks' <- for blocks $ \(ModelBlock slot txs) -> do
            lift $ applyModelTick slot
            block' <- do
              txs' <- ifor txs (fixTx slot)
              pure txs'
            pure $ ModelBlock slot block'
          lift $ applyModelBlocksMade blocksMade
          pure (ModelEpoch blocks' blocksMade)
      ((epochs', errs), _st') = modelM (runWriterT go) globals (mkModelLedger globals genesis)
   in (errs, (genesis, epochs'))
