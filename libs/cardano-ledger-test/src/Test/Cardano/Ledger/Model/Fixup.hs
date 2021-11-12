{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Model.Fixup where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, word64ToCoin)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import qualified Cardano.Ledger.Val as Val
import Control.Arrow ((&&&))
import Control.Lens
  ( Lens',
    at,
    foldMapOf,
    folded,
    ifor,
    ifor_,
    imap,
    ix,
    maximumOf,
    over,
    preview,
    set,
    to,
    toListOf,
    use,
    uses,
    view,
    (.=),
    (.~),
    (<<+=),
    (<>=),
    _1,
    _2,
    _Just,
  )
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..))
import Data.Group (Group (..))
import Data.Group.GrpMap (GrpMap (..), mapGrpMap, mkGrpMap, zipWithGrpMap)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import GHC.Exts (fromString)
import GHC.Generics ((:*:) (..), (:.:) (..))
import qualified PlutusTx
import Test.Cardano.Ledger.Model.API
  ( HasModelM,
    ModelBlock (..),
    ModelEpoch (..),
    ModelGenesis (..),
    ModelLedger,
    applyModelBlocksMade,
    applyModelTick,
    applyModelTx,
    getModelLedger_rewards,
    getModelLedger_utxos,
    mkModelLedger,
    modelGenesis_utxos,
    modelLedger,
    modelLedger_nes,
    modelM,
  )
import Test.Cardano.Ledger.Model.Acnt
  ( ModelAcntF (..),
    modelAcntPot,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
    ModelValueVars (..),
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet,
    KnownRequiredFeatures,
    ScriptFeature,
    TyScriptFeature (..),
    ValueFeature,
    fromSupportsMint,
    fromSupportsPlutus,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelInstantaneousReward (..),
    modelDPState_dstate,
    modelDPState_pstate,
    modelDState_genDelegs,
    modelDState_iRwd,
    modelEpochState_acnt,
    modelEpochState_ls,
    modelLState_dpstate,
    modelLState_utxoSt,
    modelNewEpochState_es,
    modelPState_poolParams,
    modelUTxOState_utxo,
  )
import Test.Cardano.Ledger.Model.PParams
  ( getModelPParams,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential (..),
    ModelScript (..),
    coerceKeyRole',
    modelAddress_pmt,
    modelScriptNeededSigs,
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelDelegCert (..),
    ModelMIRCert (..),
    ModelMIRTarget (..),
    ModelScriptPurpose (..),
    ModelTx (..),
    getModelConsumed,
    getModelMinfee,
    getModelProduced,
    getModelTxId,
    modelCWitness,
    modelPoolParams_owners,
    modelTotalDeposits,
    modelTx_outputs,
    modelTxs,
    _ModelMIR,
    _ModelRegisterPool,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId (..),
    modelMinUTxOCoins,
    modelTxOut,
    modelTxOut_address,
    modelTxOut_value,
  )
import Test.Cardano.Ledger.Model.Value
  ( ModelValueF (..),
    atModelValueF,
  )

type ModelValueF' era = ModelValueF (ModelValueVars era (ValueFeature era))

data FixupValuesRestartError
  = FixupValuesRestartError_InsufficientFixupReserves
  deriving (Eq, Ord, Show)

data FixupValuesState era = FixupValuesState
  { _fixupValuesState_nextId :: !Integer,
    _fixupValuesState_genesis :: !(ModelGenesis era),
    _fixupValuesState_helpReserves :: !(ModelUTxOId, Coin)
  }

fixupValuesState_nextId :: Lens' (FixupValuesState era) Integer
fixupValuesState_nextId a2fb s = (\b -> s {_fixupValuesState_nextId = b}) <$> a2fb (_fixupValuesState_nextId s)
{-# INLINE fixupValuesState_nextId #-}

fixupValuesState_genesis :: Lens' (FixupValuesState era) (ModelGenesis era)
fixupValuesState_genesis a2fb s = (\b -> s {_fixupValuesState_genesis = b}) <$> a2fb (_fixupValuesState_genesis s)
{-# INLINE fixupValuesState_genesis #-}

fixupValuesState_helpReserves :: Lens' (FixupValuesState era) (ModelUTxOId, Coin)
fixupValuesState_helpReserves a2fb s = (\b -> s {_fixupValuesState_helpReserves = b}) <$> a2fb (_fixupValuesState_helpReserves s)
{-# INLINE fixupValuesState_helpReserves #-}

newtype MonadFixupValuesT era m a = MonadFixupValuesT
  { unMonadFixupValuesT :: State.StateT (FixupValuesState era) (Except.ExceptT FixupValuesRestartError m) a
  }
  deriving (Functor, Applicative, Monad)

deriving newtype instance Monad m => State.MonadState (FixupValuesState era) (MonadFixupValuesT era m)

deriving newtype instance Monad m => MonadError FixupValuesRestartError (MonadFixupValuesT era m)

instance MonadTrans (MonadFixupValuesT era) where
  lift = MonadFixupValuesT . lift . lift

runMonadFixupValuesT ::
  MonadFixupValuesT era m a ->
  FixupValuesState era ->
  m (Either FixupValuesRestartError (a, FixupValuesState era))
runMonadFixupValuesT (MonadFixupValuesT xs) s = Except.runExceptT $ State.runStateT xs s

fixupValues ::
  forall era.
  KnownRequiredFeatures era =>
  String ->
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  (ModelGenesis era, [ModelEpoch era])
fixupValues clue globals (genesis0, epochs) = loop (Coin 1000)
  where
    loop falseReserves
      | falseReserves > word64ToCoin (maxLovelaceSupply globals) = error "fixup uses too much help"
      | otherwise =
        let falseReserveUTXO = ModelUTxOId nextId0
            nextId = nextId0 + 1
            genesis = over modelGenesis_utxos (Map.insert falseReserveUTXO (fromString ("fixupValues reserves " <> clue), falseReserves)) genesis0
            s = FixupValuesState nextId genesis (falseReserveUTXO, falseReserves)
            (step, _ :: ModelLedger era) = modelM (runMonadFixupValuesT go s) globals (mkModelLedger globals genesis)
         in case step of
              Left FixupValuesRestartError_InsufficientFixupReserves ->
                loop (falseReserves <> falseReserves <> Coin 1)
              Right (epochs', (FixupValuesState _ genesis' _)) -> (genesis', epochs')

    go ::
      forall m.
      HasModelM era (ModelLedger era) Globals m =>
      MonadFixupValuesT era m [ModelEpoch era]
    go = do
      for epochs $ \(ModelEpoch blocks blocksMade) -> do
        blocks' <- for blocks $ \(ModelBlock slot txs) -> do
          lift $ applyModelTick slot
          block' <- do
            ifor txs $ \txIx tx -> do
              txMIR <- fixMIR tx
              txWdrl <- fixWdrl txMIR
              txBalance <- fixBalance txWdrl
              tx' <- checkSigs clue txBalance

              lift $ applyModelTx slot txIx tx'
              pure tx'

          pure $ ModelBlock slot block'
        lift $ applyModelBlocksMade blocksMade
        pure (ModelEpoch blocks' blocksMade)

    nextId0 =
      succ $
        maybe 0 id $
          max
            (unModelUTxOId . fst . fst <$> Map.maxViewWithKey (_modelGenesis_utxos genesis0))
            (unModelUTxOId <$> maximumOf (traverse . modelTxs . modelTx_outputs . traverse . _1) epochs)

checkSigs ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  String ->
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
checkSigs _hint mtx = do
  ml <- lift $ use modelLedger
  let mtx' = witnessModelTx mtx ml
  pure mtx'

fixWdrl ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
fixWdrl tx = do
  rewards <- lift $ uses modelLedger getModelLedger_rewards
  let wdrls' =
        Map.merge
          Map.dropMissing
          (Map.mapMissing $ \k v -> error $ unwords ["fixWdrl: ", show k, show v])
          (Map.zipWithMatched $ \_ rwd _ -> Val.inject rwd)
          rewards
          (_mtxWdrl tx)
  pure tx {_mtxWdrl = wdrls'}

fixMIR ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
fixMIR tx = do
  let coerceDeltaCoin = (coerce :: forall k. GrpMap k Coin -> GrpMap k DeltaCoin)
      unDeltaCoin (DeltaCoin x) = x

  pots <- lift $ use $ modelLedger_nes . modelNewEpochState_es . modelEpochState_acnt

  ( Comp1
      ( ModelAcnt
          { _modelAcnt_treasury = ModelInstantaneousReward dT0 (coerceDeltaCoin -> irT0),
            _modelAcnt_reserves = ModelInstantaneousReward dR0 (coerceDeltaCoin -> irR0)
          }
        )
    ) <-
    lift $ use $ modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_iRwd

  let otherPot = \case
        TreasuryMIR -> ReservesMIR
        ReservesMIR -> TreasuryMIR
      st0 = ModelAcnt (dT0, fold irT0, irT0) (dR0, fold irR0, irR0)

      dcert' = flip State.evalState st0 $
        for (_mtxDCert tx) $ \case
          x@(ModelCertPool {}) -> pure x
          ModelCertDeleg x ->
            ModelCertDeleg <$> case x of
              ModelDCertMir (ModelMIRCert pot target) ->
                ModelDCertMir . ModelMIRCert pot <$> case target of
                  ModelStakeAddressesMIR (mkGrpMap -> rwds) ->
                    ModelStakeAddressesMIR <$> do
                      (Coin delta, DeltaCoin totIrwds, irwds) <- use (modelAcntPot pot)
                      let Coin potAmount = view (modelAcntPot pot) pots
                          rwdsNeg =
                            zipWithGrpMap (\irwd rwd -> max rwd $ invert irwd) irwds $
                              mapGrpMap (min mempty) rwds
                          DeltaCoin totNeg = fold rwdsNeg

                          rwdsPos = mapGrpMap (max mempty) rwds
                          DeltaCoin totPos = fold rwdsPos
                          available = potAmount + delta
                          avail' = available - totIrwds - totNeg
                          -- a bit of rearranging the required property into the terms
                          -- of variables we have here.
                          -- . requiredForRewards <= available
                          -- . fold combinedMap <= potAmount <> delta
                          -- . fold (credCoinMap <> instantaneousRewards) <= potAmount <> delta
                          -- . (fold credCoinMap <> fold instantaneousRewards) <= potAmount <> delta
                          -- . fold credCoinMap <= potAmount <> delta ~~ fold instantaneousRewards
                          -- . fold (rwdsNeg <> rwdsPos) <= potAmount <> delta ~~ fold instantaneousRewards
                          -- . fold rwdsNeg <> fold rwdsPos <= potAmount <> delta ~~ fold instantaneousRewards
                          -- . fold rwdsPos <= potAmount <> delta ~~ fold instantaneousRewards ~~ fold rwdsNeg
                          rwdsPos' =
                            if avail' < totPos
                              then mkGrpMap $ fmap DeltaCoin $ repartition avail' ((% totPos) . unDeltaCoin <$> unGrpMap rwdsPos)
                              else rwdsPos

                          rwds' = rwdsNeg <> rwdsPos'

                      id
                        <>= ( set (modelAcntPot pot) (mempty, fold rwds', rwds') $
                                mempty
                            )

                      pure $ unGrpMap rwds'
                  ModelSendToOppositePotMIR qty ->
                    ModelSendToOppositePotMIR <$> do
                      (delta, totIrwds, _) <- use (modelAcntPot pot)
                      let potAmount = view (modelAcntPot pot) pots
                          avail = potAmount <> delta
                          qty' = min qty (addDeltaCoin avail $ invert totIrwds)
                      id
                        <>= ( set (modelAcntPot pot . _1) (invert qty')
                                . set (modelAcntPot (otherPot pot) . _1) qty'
                                $ mempty
                            )

                      pure qty'
              y -> pure y

  pure $ tx {_mtxDCert = dcert'}

askForHelp ::
  HasModelM era (ModelLedger era) Globals m =>
  Coin ->
  ModelTx era ->
  MonadFixupValuesT era m (Set ModelUTxOId)
askForHelp qty tx = do
  (reserveId, reserveAvail) <- use fixupValuesState_helpReserves

  unless (qty <= reserveAvail) $
    throwError FixupValuesRestartError_InsufficientFixupReserves

  helpId <- fmap ModelUTxOId $ fixupValuesState_nextId <<+= 1
  let helpCred = "fixup-help-utxo for: " <> show (getModelTxId tx)
      helpAddr = ModelAddress (ModelKeyHashObj helpCred) (ModelKeyHashObj helpCred)
      newReserves = reserveAvail ~~ qty

  fixupValuesState_helpReserves . _2 .= newReserves

  fixupValuesState_genesis . modelGenesis_utxos . at helpId .= Just (helpAddr, qty)
  fixupValuesState_genesis . modelGenesis_utxos . ix reserveId . _2 .= newReserves

  lift $ do
    modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo . at helpId .= Just (modelTxOut helpAddr $ Val.inject qty)
    modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo . ix reserveId . modelTxOut_value .= Val.inject newReserves

  pure $ Set.insert helpId $ _mtxInputs tx

-- distribute an error amount across the provided outputs, evenly
fixBalance ::
  forall m era.
  HasModelM era (ModelLedger era) Globals m =>
  ModelTx era ->
  MonadFixupValuesT era m (ModelTx era)
fixBalance
  tx@( ModelTx
         { _mtxFee = Val.coin -> Coin fee,
           _mtxOutputs = outputs
         }
       ) = do
    pp <- lift $ uses (modelLedger . modelLedger_nes) $ getModelPParams
    utxo <- lift $ uses modelLedger $ getModelLedger_utxos
    poolParams <- lift $ use $ modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_pstate . modelPState_poolParams

    let consumed@(ModelValueF (Coin consumedAda, _)) = unModelValue $ getModelConsumed pp utxo tx
        produced = unModelValue $ getModelProduced pp poolParams tx
        ModelValueF (_, GrpMap mma) = consumed ~~ produced
        Coin feeMin = getModelMinfee pp tx
        minUTxOs :: Map.Map ModelUTxOId Integer = fmap (unCoin . modelMinUTxOCoins pp) $ Map.fromList outputs

        ((newInputs, fee'), outsMap) = flip State.runState (Map.fromList $ (fmap . fmap) (unModelValue . _mtxo_value) outputs) $ do
          outCoins :: Map.Map ModelUTxOId Integer <-
            State.gets $
              Map.merge
                (Map.mapMissing $ \_ _ -> 0)
                Map.dropMissing
                (Map.zipWithMatched $ \_ minVal val -> unCoin (Val.coin val) - minVal)
                minUTxOs

          let coinTotal = (fee - feeMin) + sum outCoins

              workingBalance =
                consumedAda
                  - (feeMin + sum minUTxOs)
                  - (unCoin $ modelTotalDeposits pp poolParams $ _mtxDCert tx)

              (Identity fee'' :*: outCoins') =
                repartition
                  workingBalance
                  (Identity ((fee - feeMin) % coinTotal) :*: ((% coinTotal) <$> outCoins))
              mergeValue k =
                Map.merge
                  (Map.dropMissing)
                  (Map.preserveMissing)
                  (Map.zipWithMatched $ \_ -> set (atModelValueF k))

              (newInputs', newOuts) =
                if workingBalance > 0
                  then (pure (_mtxInputs tx), outCoins')
                  else (askForHelp (Coin $ negate workingBalance) tx, 0 <$ outCoins')

          State.modify $ mergeValue Nothing $ Map.unionWith (+) newOuts minUTxOs

          ifor_ mma $ \assetId (Sum assetVal) -> do
            outAssets ::
              Map.Map ModelUTxOId Integer <-
              State.gets $ fmap (view $ atModelValueF (Just assetId))
            let assetTotal = sum outAssets
                outAssets' = repartition (assetVal + assetTotal) ((% assetTotal) <$> outAssets)

            State.modify $ mergeValue (Just assetId) $ outAssets'

          let newFee =
                if workingBalance > 0
                  then fee'' + feeMin
                  else feeMin
          pure (newInputs', newFee)

        outputs' =
          (fmap . imap)
            (\ui -> modelTxOut_value .~ foldMapOf (ix ui) ModelValue outsMap)
            outputs

    inputs' <- newInputs
    pure $
      tx
        { _mtxFee = Val.inject (Coin fee'),
          _mtxOutputs = outputs',
          _mtxInputs = inputs'
        }

witnessModelTx ::
  forall (era :: FeatureSet). ModelTx era -> ModelLedger era -> ModelTx era
witnessModelTx mtx ml =
  let (witnessSigs, redeemers) = witnessModelTxImpl mtx ml
   in mtx
        { _mtxWitnessSigs = witnessSigs,
          _mtxRedeemers = redeemers
        }

-- TODO: there's some extra degrees of freedom hidden in this function, they
-- SEE: Fig18 [SL-D5]
-- should be exposed
-- - redeemer value/exunits
-- - how timelocks are signed (which n of m)
witnessModelTxImpl ::
  forall (era :: FeatureSet).
  ModelTx era ->
  ModelLedger era ->
  ( Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)),
    Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)
  )
witnessModelTxImpl mtx ml =
  let mkRdmr :: ModelScript (ScriptFeature era) -> ModelScriptPurpose era -> (PlutusTx.Data, ExUnits)
      mkRdmr _ _ = (PlutusTx.I 10, ExUnits 1 1)

      lookupOutput :: ModelUTxOId -> Maybe (ModelUTxOId, ModelCredential 'Payment (ScriptFeature era))
      lookupOutput ui = (,) ui <$> preview (to getModelLedger_utxos . at ui . _Just . modelTxOut_address @era . modelAddress_pmt) ml

      matchDCert :: ModelDCert era -> Maybe (ModelScriptPurpose era, ModelCredential 'Witness (ScriptFeature era))
      matchDCert cert = (,) (ModelScriptPurpose_Certifying cert) <$> modelCWitness cert

      witnessMint :: ModelValueVars era (ValueFeature era) -> (Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)), Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits))
      witnessMint = \case
        ModelValue_MA (modelPolicy, _) -> case modelPolicy of
          ModelScript_Timelock tl -> foldMap (\wit -> (Set.singleton wit, Map.empty)) (modelScriptNeededSigs tl)
          ModelScript_PlutusV1 _s1 ->
            let sp = ModelScriptPurpose_Minting modelPolicy
             in (Set.empty, Map.singleton sp (mkRdmr modelPolicy sp))

      witnessCredential ::
        ModelScriptPurpose era ->
        ModelCredential k (ScriptFeature era) ->
        ( Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False)),
          Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)
        )
      witnessCredential msp = \case
        ModelKeyHashObj k -> (Set.singleton (ModelKeyHashObj k), Map.empty)
        ModelScriptHashObj s -> (Set.empty, Map.singleton msp (mkRdmr (ModelScript_PlutusV1 s) msp))

      witnessGenesisDelegates = (Set.fromList $ fmap coerceKeyRole' $ toListOf (modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_genDelegs . folded) ml, Map.empty)

      witnessSigs :: Set (ModelCredential 'Witness ('TyScriptFeature 'False 'False))
      redeemers :: Map.Map (ModelScriptPurpose era) (PlutusTx.Data, ExUnits)
      (witnessSigs, redeemers) =
        foldMap (uncurry witnessCredential . first ModelScriptPurpose_Spending) (mapMaybe lookupOutput $ Set.toList $ _mtxInputs mtx)
          <> foldMap (uncurry witnessCredential) (mapMaybe matchDCert $ _mtxDCert mtx)
          <> foldMapOf (traverse . _ModelRegisterPool . modelPoolParams_owners . traverse) ((,Map.empty) . Set.singleton . coerceKeyRole') (_mtxDCert mtx)
          <> foldMap (const witnessGenesisDelegates) (mapMaybe (preview _ModelMIR) $ _mtxDCert mtx)
          <> foldMap (uncurry witnessCredential . (ModelScriptPurpose_Rewarding &&& id)) (Map.keys $ _mtxWdrl mtx)
          <> fromSupportsMint mempty (foldMap witnessMint . unModelValue) (_mtxMint mtx)
          <> fromSupportsPlutus
            mempty
            (foldMap (uncurry witnessCredential . first ModelScriptPurpose_Spending) . mapMaybe lookupOutput . Set.toList)
            (_mtxCollateral mtx)
   in (witnessSigs, redeemers)

-- apportion a container with weights.  this doesn't normalize the container, if
-- the weights don't total unity, then the result will not total the given
-- amount.
repartition :: forall a b t. (Traversable t, Integral a, RealFrac b) => a -> t b -> t a
repartition total weights = State.evalState (traverse step weights) (0 :: b)
  where
    step weight = do
      err <- State.get
      let fracValue :: b
          fracValue = err + fromIntegral total * weight
          value :: a
          value = round fracValue
      State.put (fracValue - fromIntegral value)
      pure value
