{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.ModelChain.Fixup where

import Debug.Trace

import Cardano.Ledger.BaseTypes (Globals (..))
import Control.Arrow ((&&&))
import Data.Traversable
import Data.Group (Group(..))
import Control.Monad (
  (<=<)
  , when
  )
import Control.Lens
  ( Prism'
  , ifor
  , ix
  , foldMapOf
  , (.~)
  , (<<+=)
  , (%=)
  , _1
  , _2
  , imap
  , maximumOf
  , use
  , view
  , _Left
  , prism
  , _Right
  , preview
  , ifor_
  , set
  , uses
  )
import Data.Tuple (swap)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Data.Map (Map)
import qualified Data.Set as Set
import qualified PlutusTx
import Data.Ratio ((%))
import Data.Set (Set)
import Test.Cardano.Ledger.ModelChain.Script
  ( ModelCredential(..)
  , ModelAddress(..)
  )
import Test.Cardano.Ledger.ModelChain
  ( HasModelM
  , modelTxs
  , modelTotalDeposits
  , ModelLedger
  , ModelEpoch(..)
  , ModelBlock(..)
  , ModelTx(..)
  , modelTx_outputs
  , ModelUTxOId(..)
  , ModelValue
  , ModelGenesis(..)
  , ModelScriptPurpose(..)
  , ModelTxId
  , ModelValueVars
  , mkModelLedger
  , modelM
  , modelValueInject
  , getModelTxId
  , applyModelBlocksMade
  , getModelPParams
  , applyModelTick
  , getModelValueCoin
  , applyModelTx
  , ModelTxOut(..)
  , getModelLedger_utxos
  , modelTxOut_value
  , repartition
  , modelPState_poolParams
  , modelDPState_pstate
  , modelLState_dpstate
  , modelEpochState_ls
  , modelNewEpochState_es
  , modelLedger_nes
  , modelMinUTxOCoins
  , modelGenesis_utxos
  , modelLedger
  , getModelMinfee
  , getModelProduced
  , getModelConsumed
  , ModelValue(..)
  , getModelLedger_rewards
  , witnessModelTxImpl
  , ModelValue'
  , mkModelValue
  )
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Semigroup (Sum (..))
import GHC.Generics (Generic, (:*:) (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Ledger.Coin (Coin(..))
import Data.Group.GrpMap (GrpMap(..))
import Data.Functor.Identity (Identity(..))
import qualified Control.Monad.State.Strict as State
import Cardano.Slotting.Slot (SlotNo)
-- import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Trans.RWS.CPS (RWST, runRWST)
import Control.Monad.Writer.Class (MonadWriter(tell))
import Control.Monad.Trans.Class (lift)
import Test.Cardano.Ledger.ModelChain.Value
  (ModelValueSimple(..)
  , atModelValueSimple
  , ModelValueF(..)
  , evalModelValueSimple
  )

import Data.List.NonEmpty (NonEmpty(..))
import Cardano.Ledger.Keys (KeyRole (..))
import Control.DeepSeq
import Test.Cardano.Ledger.ModelChain.FeatureSet
  (ValueFeature
  , ShelleyScriptFeatures
  , KnownRequiredFeatures
  , TyValueExpected(..)
  , ScriptFeature
  )


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
  deriving (Generic, Show)

instance NFData (FixupValuesError era)

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

type MonadFixupValuesT era = RWST () (Maybe (FixupValuesErrors era)) (Integer, (ModelGenesis era))

fixupValues ::
  forall era.
  KnownRequiredFeatures era =>
  FixupValuesFlags ->
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  (Maybe (FixupValuesErrors era), (ModelGenesis era, [ModelEpoch era]))
fixupValues fvf globals (genesis, epochs) = (errs, (genesis', epochs'))
  where
      (epochs', (_, genesis'), errs) = fst $ modelM (runRWST go () (nextId, genesis)) globals (mkModelLedger globals genesis)

      go ::
        forall m.
        HasModelM era (ModelLedger era) Globals m =>
        MonadFixupValuesT era m [ModelEpoch era]
      go = do
        for epochs $ \(ModelEpoch blocks blocksMade) -> do
          blocks' <- for blocks $ \(ModelBlock slot txs) -> do
            lift $ applyModelTick slot
            block' <- do
              txs' <- ifor txs (fixTx fvf slot)
              pure txs'
            pure $ ModelBlock slot block'
          lift $ applyModelBlocksMade blocksMade
          pure (ModelEpoch blocks' blocksMade)

      nextId = succ $ maybe 0 id $
        max
          (unModelUTxOId . fst . fst <$> Map.maxViewWithKey (_modelGenesis_utxos genesis))
          (unModelUTxOId <$> maximumOf (traverse . modelTxs . modelTx_outputs . traverse . _1) epochs)
        -- (_  maximumOf (_1 . folded <> _) epochs)

fixTx ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  FixupValuesFlags ->
  SlotNo ->
  Int ->
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
fixTx fvf slot txIx tx = do
  tx' <- checkSigs fvf "after" <=< fixBalance <=< checkSigs fvf "between" <=< fixWdrl <=< checkSigs fvf "before" $ tx

  lift $ applyModelTx slot txIx tx'
  pure tx'


checkSigs ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  FixupValuesFlags ->
  String ->
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
checkSigs fvf hint mtx = do
  ml <- lift $ use modelLedger
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

fixWdrl ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
fixWdrl tx = do
  rewards <- lift $ uses modelLedger getModelLedger_rewards
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

traceClue :: Show a => String -> a -> a
traceClue clue x = trace (clue <> ": " <> show x) x

askForHelp
  :: Monad m => Integer
     -> ModelTx era
     -> MonadFixupValuesT era m (Set ModelUTxOId)
askForHelp qty tx = do
  helpId <- fmap ModelUTxOId $ _1 <<+= 1
  let helpCred = "fixup-help-utxo for: " <> show (getModelTxId tx)
  _2 . modelGenesis_utxos %= Map.insert helpId
    (ModelAddress (ModelKeyHashObj helpCred) (ModelKeyHashObj helpCred) , Coin qty)
  pure $ Set.insert helpId $ _mtxInputs tx

-- distribute an error amount across the provided outputs, evenly
fixBalance ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
fixBalance
  tx@( ModelTx
         { _mtxFee = getModelValueCoin -> Coin fee,
           _mtxOutputs = outputs
         }
       ) = do
    pp <- lift $ uses (modelLedger . modelLedger_nes) $ getModelPParams
    utxo <- lift $ uses modelLedger $ getModelLedger_utxos
    poolParams <- lift $ use $ modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_pstate . modelPState_poolParams

    let consumed@(ModelValueSimple (Coin consumedAda, _) ) = evalOrDie $ getModelConsumed pp utxo tx
        produced = evalOrDie $ getModelProduced pp poolParams tx
        ModelValueSimple (Coin mada, GrpMap mma) = consumed ~~ produced
        Coin feeMin = getModelMinfee pp tx
        minUTxOs :: Map.Map ModelUTxOId Integer = fmap (unCoin . modelMinUTxOCoins pp) $ Map.fromList outputs

        ((newInputs, fee'), outsMap) = flip State.runState (Map.fromList $ (fmap . fmap) (evalOrDie . _mtxo_value) outputs) $ do
          outCoins :: Map.Map ModelUTxOId Integer <-
            State.gets $
              Map.merge
                (Map.mapMissing $ \_ _ -> 0)
                Map.dropMissing
                (Map.zipWithMatched $ \_ minVal val -> unCoin (Val.coin val) - minVal)
                minUTxOs

          let coinTotal = (fee - feeMin) + sum outCoins

              workingBalance = traceClue "workingBalance" $
                consumedAda -
                (feeMin + sum minUTxOs) -
                (unCoin $ modelTotalDeposits pp poolParams $ _mtxDCert tx)

              (Identity fee'' :*: outCoins') =
                repartition workingBalance
                  (Identity (fee % coinTotal) :*: ((% coinTotal) <$> outCoins))
              mergeValue k =
                Map.merge
                  (Map.dropMissing)
                  (Map.preserveMissing)
                  (Map.zipWithMatched $ \_ -> set (atModelValueSimple k))


              (newInputs, newOuts) =
                if workingBalance > 0
                  then (pure (_mtxInputs tx), outCoins')
                  else (askForHelp workingBalance tx, 0 <$ outCoins')

          State.modify $ mergeValue Nothing $ Map.unionWith (+) newOuts minUTxOs

          ifor_ mma $ \assetId (Sum assetVal) -> do
            outAssets ::
              Map.Map ModelUTxOId Integer <-
              State.gets $ fmap (view $ atModelValueSimple (Just assetId))
            let assetTotal = sum outAssets
                outAssets' = repartition (assetVal + assetTotal) ((% assetTotal) <$> outAssets)

            State.modify $ mergeValue (Just assetId) $ outAssets'

          pure (newInputs, max 0 fee'' + feeMin)

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
    inputs' <- newInputs
    pure $
      tx
        { _mtxFee = modelValueInject (Coin fee'),
          _mtxOutputs = outputs',
          _mtxInputs = inputs'
        }

tellFixupErr ::
  forall m era.
  Monad m =>
  FixupValuesError era ->
  MonadFixupValuesT era m ()
tellFixupErr = tell . Just . pure

evalOrDie :: ModelValue (ValueFeature era) era -> ModelValue' (ValueFeature era) era
evalOrDie = either (error . show) id . evalModelValueSimple . unModelValue

