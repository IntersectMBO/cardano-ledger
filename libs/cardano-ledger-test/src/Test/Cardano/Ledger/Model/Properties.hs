{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Model.Properties where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.BaseTypes (Globals (..), boundRational)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary.Value (AssetName (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Cardano.Ledger.Val (Val (..))
import qualified Cardano.Ledger.Val as Val
import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Lens
import Control.Monad (guard, when)
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.Class
import Control.State.Transition.Extended
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Functor.Contravariant (Predicate (..))
import Data.HKD
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Endo (..))
import qualified Data.Set as Set
import Data.Some (Some (Some))
import Data.String (IsString (..))
import Data.Typeable
import GHC.Generics hiding (to)
import GHC.Natural
import qualified PlutusTx
import System.CPUTime
import Test.Cardano.Ledger.Model.API
import Test.Cardano.Ledger.Model.BaseTypes
import Test.Cardano.Ledger.Model.Elaborators
import Test.Cardano.Ledger.Model.Elaborators.Alonzo ()
import Test.Cardano.Ledger.Model.Elaborators.Shelley ()
import Test.Cardano.Ledger.Model.FeatureSet
import Test.Cardano.Ledger.Model.Fixup (fixupValues)
import Test.Cardano.Ledger.Model.Generators
  ( ModelGeneratorParamsF (..),
    defaultModelGeneratorParams,
  )
import Test.Cardano.Ledger.Model.Generators.Chain (genModel)
import Test.Cardano.Ledger.Model.Generators.Shrinking
import Test.Cardano.Ledger.Model.PParams
import Test.Cardano.Ledger.Model.Properties.Utils
import Test.Cardano.Ledger.Model.Prov
import Test.Cardano.Ledger.Model.Rules (ModelPredicateFailure (..))
import Test.Cardano.Ledger.Model.Script
import Test.Cardano.Ledger.Model.Tx
import Test.Cardano.Ledger.Model.TxOut
import Test.Cardano.Ledger.Model.UTxO
import Test.Cardano.Ledger.Model.Value
import Test.Cardano.Ledger.Rational
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.Tasty
import Test.Tasty.QuickCheck

modelMACoin ::
  (ValueFeature era ~ 'ExpectAnyOutput) =>
  ModelScript (ScriptFeature era) ->
  [(AssetName, Integer)] ->
  ModelValue 'ExpectAnyOutput era
modelMACoin script assets = foldMap f assets
  where
    f (asset, qty) = ModelValue $ mkModelValueF' mempty $ Map.singleton (ModelValue_MA (script, asset)) qty

modelCoin :: Val v => Integer -> v
modelCoin = inject . Coin

modelGenesis :: KnownRequiredFeatures era => Map.Map ModelUTxOId (ModelAddress (ScriptFeature era), Coin) -> ModelGenesis era
modelGenesis =
  ModelGenesis
    modelPParams
    (Map.fromList [(fromString ("gd" <> show n), fromString ("gd" <> show n <> ":1")) | n <- [(1 :: Int) .. 7]])

scriptArity :: Tag -> Natural
scriptArity Spend = 3
scriptArity Mint = 2
scriptArity Cert = 2
scriptArity Rewrd = 2

alwaysSucceedsPlutusAddress :: ModelAddress ('TyScriptFeature x 'True)
alwaysSucceedsPlutusAddress =
  ModelAddress
    (ModelScriptHashObj $ ModelPlutusScript_AlwaysSucceeds $ scriptArity Spend)
    (ModelScriptHashObj $ ModelPlutusScript_AlwaysSucceeds $ scriptArity Cert)

purpleModelScript :: ModelScript ('TyScriptFeature 'True x)
purpleModelScript = ModelScript_Timelock $ ModelTimelock_AllOf []

bobCoinScript :: ModelScript ('TyScriptFeature 'True x)
bobCoinScript = ModelScript_Timelock $ ModelTimelock_Signature "BobCoin"

modelPlutusScript :: Natural -> ModelScript ('TyScriptFeature x 'True)
modelPlutusScript = ModelScript_PlutusV1 . ModelPlutusScript_AlwaysSucceeds

instance IsString AssetName where
  fromString = AssetName . BS.pack

modelTestDelegations ::
  forall era proxy.
  ( ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.TxOut era),
    Show (Core.Script era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era)),
    Show (LedgerState.LedgerState era)
  ) =>
  proxy era ->
  Coin ->
  Coin ->
  Bool ->
  ModelAddress AllScriptFeatures ->
  [TestTree]
modelTestDelegations proxy keyDeposit poolDeposit needsCollateral stakeAddr@(ModelAddress _ stakeCred) =
  let modelPool =
        ModelPoolParams
          "pool1"
          "pool1'"
          (Coin 0)
          (Coin 0)
          (fromJust $ boundRational $ 0 % 1)
          "rewardAcct"
          ["poolOwner"]
      modelDelegation = ModelCertDeleg $ ModelDelegate (ModelDelegation stakeCred "pool1")
      stkRdmr x = SupportsPlutus (x <$ guard needsCollateral)
      noRdmr = SupportsPlutus Nothing
      allAtOnce =
        [ ModelBlock
            0
            [ modelTx
                { _mtxInputs = [(0, noRdmr)],
                  _mtxWitnessSigs = Set.fromList $ ["unstaked", "pool1", "poolOwner"] <> mapMaybe mkWitness [_modelAddress_pmt stakeAddr],
                  _mtxCollateral = SupportsPlutus $ Set.fromList [x | x <- [0], needsCollateral],
                  _mtxOutputs =
                    [ (100, modelTxOut "unstaked" (modelCoin 9_800_000_000_000 <-> Val.inject (keyDeposit <+> poolDeposit))),
                      (101, modelTxOut stakeAddr (modelCoin 100_000_000_000))
                    ],
                  _mtxFee = modelCoin 100_000_000_000,
                  _mtxDCert =
                    [ (ModelCertDeleg $ ModelRegKey stakeCred, noRdmr),
                      (ModelCertPool $ ModelRegPool modelPool, noRdmr),
                      (modelDelegation, stkRdmr (PlutusTx.I 5, ExUnits 1 1))
                    ]
                }
            ]
        ]
      oneAtATime =
        [ ModelBlock
            0
            [ modelTx
                { _mtxInputs = [(0, noRdmr)],
                  _mtxWitnessSigs = Set.fromList ["unstaked"],
                  _mtxOutputs =
                    [ (1, modelTxOut "unstaked" (modelCoin 9_875_000_000_000)),
                      (101, modelTxOut stakeAddr (modelCoin 100_000_000_000))
                    ],
                  _mtxFee = modelCoin 25_000_000_000
                }
            ],
          ModelBlock
            1
            [ modelTx
                { _mtxInputs = [(1, noRdmr)],
                  _mtxWitnessSigs = Set.fromList ["unstaked"],
                  _mtxOutputs =
                    [ (2, modelTxOut "unstaked" (modelCoin 9_850_000_000_000 <-> Val.inject (keyDeposit)))
                    ],
                  _mtxFee = modelCoin 25_000_000_000,
                  _mtxDCert = [(ModelCertDeleg $ ModelRegKey stakeCred, noRdmr)]
                }
            ],
          ModelBlock
            2
            [ modelTx
                { _mtxInputs = [(2, noRdmr)],
                  _mtxWitnessSigs = Set.fromList ["unstaked", "pool1", "poolOwner"],
                  _mtxOutputs =
                    [ (3, modelTxOut "unstaked" (modelCoin 9_825_000_000_000 <-> Val.inject (keyDeposit <+> poolDeposit)))
                    ],
                  _mtxFee = modelCoin 25_000_000_000,
                  _mtxDCert = [(ModelCertPool $ ModelRegPool modelPool, noRdmr)]
                }
            ],
          ModelBlock
            3
            [ modelTx
                { _mtxInputs = [(3, noRdmr)],
                  _mtxWitnessSigs = Set.fromList $ ["unstaked"] <> mapMaybe mkWitness [_modelAddress_pmt stakeAddr],
                  _mtxCollateral = SupportsPlutus $ Set.fromList [x | x <- [3], needsCollateral],
                  _mtxOutputs =
                    [ (100, modelTxOut "unstaked" (modelCoin 9_800_000_000_000 <-> Val.inject (keyDeposit <+> poolDeposit)))
                    ],
                  _mtxFee = modelCoin 25_000_000_000,
                  _mtxDCert =
                    [ (modelDelegation, stkRdmr (PlutusTx.I 5, ExUnits 1 1))
                    ]
                }
            ]
        ]
      genAct =
        modelGenesis
          [ (0, ("unstaked", Coin 10_000_000_000_000))
          ]
          & set (modelGenesis_pp . modelPParams_keyDeposit . _Wrapped) keyDeposit
            . set (modelGenesis_pp . modelPParams_poolDeposit . _Wrapped) poolDeposit
      checkAllWithdrawnRewards nes ems =
        let rewards = observeRewards (ModelTxId Set.empty) (nes, ems)
         in counterexample (show rewards) $ Coin 0 === fold rewards

      mkWitness = filterModelCredential (FeatureTag ValueFeatureTag_AdaOnly ScriptFeatureTag_None)
      reward = modelCoin 9_088_909_090_909
      go reg =
        testChainModelInteractionWith
          proxy
          checkAllWithdrawnRewards
          genAct
          [ ModelEpoch reg (ModelBlocksMade $ Map.fromList []),
            ModelEpoch [] (ModelBlocksMade $ Map.fromList []),
            ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)]), -- TODO: get this from context somehow
            ModelEpoch [] (ModelBlocksMade $ Map.fromList []),
            ModelEpoch
              [ ModelBlock
                  0
                  [ modelTx
                      { _mtxInputs = [(100, noRdmr)],
                        _mtxWitnessSigs = Set.fromList $ ["unstaked"] <> mapMaybe mkWitness [_modelAddress_pmt stakeAddr],
                        _mtxCollateral = SupportsPlutus $ Set.fromList [x | x <- [100], needsCollateral],
                        _mtxOutputs =
                          [ (103, modelTxOut "unstaked" (modelCoin 9_700_000_000_000 <-> Val.inject (keyDeposit <+> poolDeposit))),
                            (104, modelTxOut "reward-less-minimum" (liftModelValue $ reward <-> modelCoin 100_000_000)),
                            (105, modelTxOut "minimum" (modelCoin 100_000_000))
                          ],
                        _mtxFee = modelCoin 100_000_000_000,
                        _mtxWdrl = Map.singleton stakeCred (reward, stkRdmr (PlutusTx.I 5, ExUnits 1 1))
                      }
                  ]
              ]
              (ModelBlocksMade $ Map.fromList [])
          ]
   in [ testProperty "allAtOnce" $ go allAtOnce,
        testProperty "oneAtATime" $ go oneAtATime
      ]

genModel' ::
  forall era proxy.
  ( KnownRequiredFeatures era
  ) =>
  proxy era ->
  Globals ->
  Gen
    ( ModelShrinkingPhase,
      ( ModelGenesis era,
        [ModelEpoch era]
      )
    )
genModel' _ globals = do
  (a, b) <-
    genModel @era globals $
      defaultModelGeneratorParams
        { -- TODO: solve "zero withdrawal" issue, which is that some model
          -- generated withdrawals correspond to zero rewards (esp in alonzo).
          -- These numbers are chosen so that the "zero withdrawal" issue occurs
          -- rarely.
          _modelGeneratorParams_epochs = choose (10, 12),
          _modelGeneratorParams_txnsPerSlot = frequency [(1, pure 1), (10, choose (2, 5))],
          _modelGeneratorParams_numSlotsUsed = choose (2, 5)
        }
  pure (NotShrunk, (a, b))

simulateChainModel ::
  KnownRequiredFeatures era =>
  Globals ->
  ModelGenesis era ->
  [ModelEpoch era] ->
  ModelLedger era
simulateChainModel globals g e =
  flip State.execState (mkModelLedger globals g) $
    for_ e $ \mepoch -> do
      State.modify $ execModelM (applyModelEpoch mepoch) globals

prop_simulateChainModel ::
  ( KnownRequiredFeatures era,
    Testable prop
  ) =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  prop ->
  Property
prop_simulateChainModel globals (g, e) = execPropertyWriter $ do
  tell $ Endo $ counterexample ("genesis:\t" <> show g)
  ((), st') :: ((), ModelLedger era) <- flip
    State.runStateT
    (mkModelLedger globals g)
    $ for_ e $ \mepoch -> do
      st <- State.get
      tellProperty $ counterexample ("ledger:\t" <> show st)
      tellProperty $ counterexample ("epoch:\t" <> show mepoch)
      State.modify $ execModelM (applyModelEpoch mepoch) globals
  tellProperty $ counterexample ("final ledger state:" <> show st')

prop_simulateChainModel' ::
  forall era prop.
  ( KnownRequiredFeatures era,
    Testable prop
  ) =>
  Globals ->
  (ModelGenesis era, [ModelEpoch era]) ->
  prop ->
  Property
prop_simulateChainModel' globals (genesis, epochs) = execPropertyWriter $ do
  let ml0 = (mkModelLedger globals genesis)
      go = do
        for_ epochs $ \mepoch -> do
          applyModelEpoch mepoch

  ((), (prov, _)) <- modelMWithProvT go globals (emptyModelProvenanceState, ml0)
  let poolsWithPledge = Set.fromList $ fmap _mppId $ toListOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((> mempty) . _mppPledge)) epochs
      intersectingPoolIds = Set.intersection poolsWithPledge $ fold $ _modelProvenanceState_poolPerformance prov

  tellProperty $ cover 5 (not $ null intersectingPoolIds) "Pools earned rewards when pledging"
  tellProperty $ counterexample $ show prov

tellProperty :: MonadWriter (Endo Property) m => (Property -> Property) -> m ()
tellProperty = tell . Endo

tellProperty_ :: (MonadWriter (Endo Property) m, Testable prop) => prop -> m ()
tellProperty_ = tell . Endo . (.&&.)

execPropertyWriter :: Testable prop => CPS.Writer (Endo Property) () -> prop -> Property
execPropertyWriter x k = (flip appEndo (property k)) . CPS.execWriter $ x

execPropertyWriter_ :: CPS.Writer (Endo Property) () -> Property
execPropertyWriter_ x = (flip appEndo (property True)) . CPS.execWriter $ x

prop_null :: (Foldable f, Show (f a)) => f a -> Property
prop_null xs = counterexample (interpret res ++ show xs) res
  where
    res = null xs
    interpret True = "null "
    interpret False = "not . null $ "

checkElaboratorResult ::
  ( Show (LedgerState.LedgerState era),
    Show (Core.Tx era),
    Show (Core.PParams era)
  ) =>
  LedgerState.NewEpochState era ->
  EraElaboratorState era ->
  Property
checkElaboratorResult _nes ees = execPropertyWriter_ $ do
  let stats = (_eesStats ees)
  tellProperty $ counterexample $ (<>) "stats:" $ show stats
  tellProperty_ $ prop_null $ _eeStats_adaConservedErrors stats
  -- tellProperty $ cover 90 (_eeStats_badWdrls stats * 10 <= _eeStats_wdrls stats) "zero withdrawals < 10%"
  tellProperty $ cover 50 (_eeStats_badWdrls stats <= 0) "zero withdrawals"
  pure ()

data ModelStats f = ModelStats
  { _numberOfEpochs :: !(f Int),
    _numberOfTransactions :: !(f Int),
    _numberOfCerts :: !(f Int),
    _blocksMade :: !(f Natural),
    _numberOfDelegations :: !(f Int),
    _withdrawals :: !(f Int),
    _scriptUTxOs :: !(f Int),
    _scriptWdrls :: !(f Int),
    _poolsWithCost :: !(f Int),
    _poolsWithZeroMargin :: !(f Int),
    _poolsWithMaxMargin :: !(f Int),
    _poolsWithIntermediateMargin :: !(f Int),
    _poolsWithPledge :: !(f Int),
    _numberOfMints :: !(f Int),
    _numberOfMintBurns :: !(f Int)
  }
  deriving (Generic)

deriving instance
  ( Show (f Natural),
    Show (f Int)
  ) =>
  Show (ModelStats f)

instance FFunctor ModelStats where ffmap = ffmapDefault

instance FZip ModelStats where fzipWith = gfzipWith

instance FRepeat ModelStats where frepeat = gfrepeat

instance FFoldable ModelStats where ffoldMap = ffoldMapDefault

instance FTraversable ModelStats where ftraverse = gftraverse

mstats :: forall era. ModelStats ((->) [ModelEpoch era])
mstats =
  ModelStats
    { _numberOfEpochs = lengthOf (traverse),
      _numberOfTransactions = lengthOf (traverse . modelTxs),
      _numberOfCerts = lengthOf (traverse . modelDCerts),
      _blocksMade = sumOf (traverse . modelEpoch_blocksMade . _ModelBlocksMade . traverse),
      _numberOfDelegations = lengthOf (traverse . modelDCerts . _ModelDelegate),
      _withdrawals = lengthOf (traverse . modelTxs . modelTx_wdrl . traverse),
      _scriptUTxOs =
        lengthOf (traverse . modelTxOut_address . modelAddress_pmt . traverseModelScriptHashObj)
          . uncurry Map.restrictKeys
          . ((_modelUTxOMap_utxos . collectModelUTxOs) &&& collectModelInputs),
      _scriptWdrls =
        lengthOf (traverse . traverseModelScriptHashObj) . (=<<) Map.keys . toListOf (traverse . modelTxs . modelTx_wdrl),
      _poolsWithCost = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((> mempty) . _mppCost)),
      _poolsWithZeroMargin = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((== minBound) . _mppMargin)),
      _poolsWithMaxMargin = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((== maxBound) . _mppMargin)),
      _poolsWithIntermediateMargin = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((\x -> x > minBound && x < maxBound) . _mppMargin)),
      _poolsWithPledge = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((> mempty) . _mppPledge)),
      _numberOfMints =
        lengthOf
          ( traverse . modelTxs . modelTx_mint . traverseSupportsFeature
              . filtered (any (any (/= 0) . fst))
          ),
      _numberOfMintBurns =
        lengthOf
          ( traverse . modelTxs . modelTx_mint . traverseSupportsFeature
              . filtered (any (any (< 0) . fst))
          )
    }

shelleyFeatureTag, alonzoFeatureTag :: Some FeatureTag
shelleyFeatureTag = Some $ eraFeatureSet (Proxy :: Proxy (ShelleyEra C_Crypto))
alonzoFeatureTag = Some $ eraFeatureSet (Proxy :: Proxy (AlonzoEra C_Crypto))

mstatsCover :: ModelStats (Const (Some FeatureTag, Double, String) :*: Predicate)
mstatsCover =
  ModelStats
    { _numberOfEpochs = Const (shelleyFeatureTag, 90, "number of epochs") :*: Predicate (> 5),
      _numberOfTransactions = Const (shelleyFeatureTag, 90, "number of transactions") :*: Predicate (> 5),
      _numberOfCerts = Const (shelleyFeatureTag, 50, "number of certs") :*: Predicate (> 5),
      _blocksMade = Const (shelleyFeatureTag, 50, "blocks made") :*: Predicate (> 250),
      _numberOfDelegations = Const (shelleyFeatureTag, 10, "number of delegation") :*: Predicate (> 5),
      _withdrawals = Const (shelleyFeatureTag, 10, "withdrawals") :*: Predicate (> 0),
      _scriptUTxOs = Const (alonzoFeatureTag, 60, "script locked utxos") :*: Predicate (> 5),
      _scriptWdrls = Const (alonzoFeatureTag, 25, "script locked withdrarwals") :*: Predicate (> 0),
      _poolsWithCost = Const (shelleyFeatureTag, 0, "pool has costs") :*: Predicate (> 0),
      _poolsWithZeroMargin = Const (shelleyFeatureTag, 0, "pool has no margin") :*: Predicate (> 0),
      _poolsWithMaxMargin = Const (shelleyFeatureTag, 0, "pool has max margin") :*: Predicate (> 0),
      _poolsWithIntermediateMargin = Const (shelleyFeatureTag, 0, "pool has some margin") :*: Predicate (> 0),
      _poolsWithPledge = Const (shelleyFeatureTag, 0, "pool has pledge") :*: Predicate (> 0),
      _numberOfMints = Const (alonzoFeatureTag, 0, "number of mint") :*: Predicate (> 0),
      _numberOfMintBurns = Const (alonzoFeatureTag, 0, "number of mint burns") :*: Predicate (> 0)
    }

collectModelUTxOs :: [ModelEpoch era] -> ModelUTxOMap era
collectModelUTxOs epochs =
  fold $
    [ set (at ui) (Just txo) mempty
      | tx <- toListOf (traverse . modelEpoch_blocks . traverse . modelBlock_txSeq . traverse) epochs,
        (ui, txo) <- _mtxOutputs tx
    ]

collectModelInputs :: [ModelEpoch era] -> Set.Set ModelUTxOId
collectModelInputs epochs =
  fold $
    [ Map.keysSet $ _mtxInputs tx
      | tx <- toListOf (traverse . modelEpoch_blocks . traverse . modelBlock_txSeq . traverse) epochs
    ]

propModelStats ::
  forall era prop proxy.
  (Testable prop, KnownRequiredFeatures era) =>
  -- [(ModelUTxOId, ModelAddress (ScriptFeature era), Coin)] ->
  proxy era ->
  [ModelEpoch era] ->
  prop ->
  Property
propModelStats proxy epochs =
  let era = reifyRequiredFeatures proxy
   in execPropertyWriter $
        ffor_ (fzipWith (:*:) mstats mstatsCover) $ \(f :*: (Const (Some era', pct, tag) :*: Predicate threshhold)) ->
          when (preceedsModelEra era' era) $
            tellProperty $ cover pct (threshhold $ f epochs) tag

examineModel ::
  [ModelEpoch era] ->
  ModelStats ((,) Bool)
examineModel epochs = fzipWith (\f (_ :*: Predicate p) -> let x = f epochs in (p x, x)) mstats mstatsCover

modelGenTest ::
  forall era proxy.
  ( ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (LedgerState.LedgerState era),
    Show (Core.Tx era),
    Show (Core.TxOut era),
    Show (Core.Script era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  proxy era ->
  Property
modelGenTest proxy =
  forAllShrink
    (genModel' (reifyRequiredFeatures $ Proxy @(EraFeatureSet era)) testGlobals)
    (shrinkModel testGlobals)
    -- (traverse shrinkModelSimple)
    $ \(_ :: ModelShrinkingPhase, ab@(a, b)) ->
      ( execPropertyWriter $ do
          tellProperty $ propModelStats (Proxy @(EraFeatureSet era)) b
          tellProperty $ prop_simulateChainModel' testGlobals ab
      )
        (testChainModelInteractionWith' proxy checkElaboratorResult a b)

testModelShrinking ::
  forall era proxy.
  ( ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.TxOut era),
    Show (Core.Script era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era)),
    Show (LedgerState.LedgerState era)
  ) =>
  proxy era ->
  Property
testModelShrinking proxy =
  forAll (genModel' (reifyRequiredFeatures $ Proxy @(EraFeatureSet era)) testGlobals) (\(_, x) -> propertyShrinking proxy x)

propertyShrinkingImpl ::
  forall era prop.
  ( Testable prop,
    KnownRequiredFeatures era
  ) =>
  ((ModelGenesis era, [ModelEpoch era]) -> prop) ->
  (ModelGenesis era, [ModelEpoch era]) ->
  Property
propertyShrinkingImpl callback originalModel = flip execPropertyWriter () $ do
  discardModel <- case discardUnnecessaryTxnsImpl testGlobals originalModel of
    Nothing -> do
      tellProperty $ cover 55 False "discard failed to shrink"
      pure originalModel
    Just discardImplModel -> do
      tellProperty $ counterexample $ "discardImplModel: " <> show discardImplModel

      let discardModel = fixupValues "discard/propShrinkImpl" testGlobals discardImplModel
          numBefore = (lengthOf (_2 . traverse . modelTxs)) originalModel
          numAfter = (lengthOf (_2 . traverse . modelTxs)) discardImplModel

      tellProperty $ counterexample $ "discardModel: " <> show discardImplModel
      tellProperty $ cover 55 (numAfter < numBefore) "numAfter < numBefore"

      case discardUnnecessaryTxns testGlobals discardModel of
        Nothing -> pure ()
        Just discard2Model -> do
          tellProperty $ counterexample $ "discard2Model: " <> show discardImplModel
          let numAfter' = (lengthOf (_2 . traverse . modelTxs)) discard2Model
          tellProperty $ cover 55 (numAfter' == numAfter) "numAfter' == numAfter"

      pure discardModel

  (collapseModel, didCollapse) <- case collapseTransactions testGlobals discardModel of
    Nothing -> do
      pure (discardModel, False)
    Just collapseModel -> do
      tellProperty $ counterexample $ "collapseModel: " <> show collapseModel
      let numAfterCollapse = (lengthOf (_2 . traverse . modelTxs)) collapseModel
          numAfter = (lengthOf (_2 . traverse . modelTxs)) discardModel

      tellProperty $ cover 55 (numAfterCollapse < numAfter) "numAfterCollapse < numAfter"
      pure (collapseModel, True)

  tellProperty $ cover 55 didCollapse "collapse was able to shrink"

  tellProperty_ $ callback collapseModel

propertyShrinking ::
  forall era proxy.
  ( Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.TxOut era),
    Show (Core.Script era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era)),
    Show (LedgerState.LedgerState era),
    ElaborateEraModel era
  ) =>
  proxy era ->
  (ModelGenesis (EraFeatureSet era), [ModelEpoch (EraFeatureSet era)]) ->
  Property
propertyShrinking proxy =
  propertyShrinkingImpl
    (\(i', j') -> testChainModelInteractionWith' proxy checkElaboratorResult i' j')

propertyModelShrinking ::
  forall era.
  KnownRequiredFeatures era =>
  (ModelGenesis era, [ModelEpoch era]) ->
  Property
propertyModelShrinking =
  propertyShrinkingImpl
    (\ij' -> prop_simulateChainModel testGlobals ij' True)

time :: NFData t => String -> IO t -> IO t
time clue a = do
  start <- getCPUTime
  !v <- force <$> a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (1e12)
  putStrLn $ unwords [clue, "time:", show (diff :: Double), "sec"]
  return v

generateOneExample :: IO ()
generateOneExample = do
  let proxy = (Proxy :: Proxy (AlonzoEra C_Crypto))
      proxy' = eraFeatureSet proxy
  (_ :: ModelShrinkingPhase, (a, b)) <- time "generate" $ generate $ genModel' proxy' testGlobals
  time "examine" $ print $ examineModel b
  _mresult <- time "modelApp" $ pure $ simulateChainModel testGlobals a b
  result <- time "elaborate" $ pure $ fst &&& (_eesStats . snd . snd) $ chainModelInteractionWith proxy a b
  print result

testDelegCombinations ::
  forall era proxy.
  ( ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (LedgerState.LedgerState era),
    Show (Core.Tx era),
    Show (Core.TxOut era),
    Show (Core.Script era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  proxy era ->
  TestTree
testDelegCombinations proxy =
  testGroup
    "deleg"
    [ testGroup (unwords [keyTestName, depositTestName]) $ modelTestDelegations proxy keyDeposit poolDeposit requiresCollateral stakeCred
      | (keyTestName, requiresCollateral, stakeCred) <-
          [ ("keyHash", False, "keyHashStake"),
            ("plutus", True, alwaysSucceedsPlutusAddress)
          ],
        (depositTestName, keyDeposit, poolDeposit) <-
          [ ("No deposit", Coin 0, Coin 0),
            ("has deposit", Coin 50, Coin 70)
          ]
    ]

-- | some hand-written model based unit tests
modelUnitTests ::
  forall era proxy.
  ( ElaborateEraModel era,
    Eq (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (LedgerState.NewEpochState era),
    Show (Core.Tx era),
    Show (Core.TxOut era),
    Show (Core.Script era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  proxy era ->
  TestTree
modelUnitTests proxy =
  testGroup
    (show $ typeRep proxy)
    [ testProperty "gen" $ modelGenTest proxy,
      -- FIXME: keeps failing occasionally: Needs work.
      -- testProperty "gen Always shrink" $ testModelShrinking proxy,
      testProperty "test pool parameters" $ uncurry (testChainModelInteractionWith' proxy (\_ _ -> True)) testPoolParamModel,
      testProperty "noop" $ testChainModelInteraction proxy (modelGenesis []) [],
      testProperty "noop-2" $
        testChainModelInteraction
          proxy
          ( modelGenesis
              [ (0, ("alice", Coin 1_000_000)),
                (1, ("bob", Coin 1_000_000))
              ]
          )
          [ModelEpoch [] mempty],
      testDelegCombinations proxy,
      testProperty "xfer" $
        testChainModelInteraction
          proxy
          ( modelGenesis
              [ (0, ("alice", Coin 1_000_000_000))
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 <-> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxWitnessSigs = Set.fromList ["alice"]
                      }
                  ]
              ]
              mempty
          ],
      testProperty "gendeleg" $
        testChainModelInteraction
          proxy
          ( ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 <-> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                          ],
                        _mtxDCert =
                          [ (ModelCertDeleg $ ModelDCertGenesis $ ModelGenesisDelegCert "gd1" "gd1:2", noRdmr)
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxWitnessSigs = Set.fromList ["alice", "gd1"]
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mir" $
        testChainModelInteraction
          proxy
          ( ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 <-> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                          ],
                        _mtxDCert =
                          [ ( ModelCertDeleg $
                                ModelDCertMir $
                                  ModelMIRCert ReservesMIR $
                                    ModelStakeAddressesMIR [("carol", DeltaCoin 100_123)],
                              noRdmr
                            ),
                            (ModelCertDeleg $ ModelRegKey "carol", noRdmr)
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxWitnessSigs =
                          Set.fromList
                            ("alice" : [fromString ("gd" <> show n <> ":1") | n <- [(1 :: Int) .. 5]])
                      }
                  ]
              ]
              mempty
          ],
      testProperty "unbalanced" $
        testChainModelInteractionRejection
          proxy
          (ModelValueNotConservedUTxO (modelCoin 1_000_000_000) (modelCoin 101_000_000))
          ( modelGenesis
              [ (0, ("alice", Coin 1_000_000_000))
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs = [(1, modelTxOut "bob" $ modelCoin 100_000_000)],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ],
      testProperty "xfer-2" $
        testChainModelInteraction
          proxy
          ( modelGenesis
              [ (0, ("alice", Coin 1_000_000_000))
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 <-> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ],
                ModelBlock
                  2
                  [ modelTx
                      { _mtxInputs = [(2, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ (3, modelTxOut "bob" (modelCoin 100_000_000)),
                            (4, modelTxOut "alice" (modelCoin 1_000_000_000 <-> (2 :: Int) <×> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mint" $
        ( testChainModelInteraction
            proxy
            ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
        )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> (modelCoin 1_000_000)
                                    <+> modelMACoin purpleModelScript [("purp", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint [(purpleModelScript, ([("purp", 1234)], noRdmr))]
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mint-2" $
        ( testChainModelInteraction
            proxy
            ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
        )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice", "BobCoin"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> (modelCoin 1_000_000)
                                    <+> modelMACoin bobCoinScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint [(bobCoinScript, ([("BOB", 1234)], noRdmr))]
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mint-3" $
        ( testChainModelInteraction
            proxy
            ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
        )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> (modelCoin 1_000_000)
                                    <+> modelMACoin purpleModelScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint [(purpleModelScript, ([("BOB", 1234)], noRdmr))]
                      },
                    modelTx
                      { _mtxInputs = [(1, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 2,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> ((3 :: Int) <×> modelCoin 1_000_000)
                                    <+> modelMACoin purpleModelScript [("BOB", 1134)]
                                )
                            ),
                            ( 3,
                              modelTxOut
                                "carol"
                                ( modelCoin 1_000_000
                                    <+> modelMACoin purpleModelScript [("BOB", 100)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mint-4" $
        ( testChainModelInteraction
            proxy
            ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
        )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice", "BobCoin"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> (modelCoin 1_000_000)
                                    <+> modelMACoin bobCoinScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint [(bobCoinScript, ([("BOB", 1234)], noRdmr))]
                      },
                    modelTx
                      { _mtxInputs = [(1, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 2,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> ((3 :: Int) <×> modelCoin 1_000_000)
                                    <+> modelMACoin bobCoinScript [("BOB", 1134)]
                                )
                            ),
                            ( 3,
                              modelTxOut
                                "carol"
                                ( modelCoin 1_000_000
                                    <+> modelMACoin bobCoinScript [("BOB", 100)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mint-burn" $
        ( testChainModelInteraction
            proxy
            ( modelGenesis
                [ (0, ("alice", Coin 1_000_000_000))
                ]
            )
        )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice", "BobCoin"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> (modelCoin 1_000_000)
                                    <+> modelMACoin bobCoinScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint =
                          SupportsMint
                            [(bobCoinScript, ([("BOB", 1234)], noRdmr))]
                      },
                    modelTx
                      { _mtxInputs = [(1, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 2,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> ((3 :: Int) <×> modelCoin 1_000_000)
                                    <+> modelMACoin bobCoinScript [("BOB", 1134)]
                                )
                            ),
                            ( 3,
                              modelTxOut
                                "carol"
                                ( modelCoin 1_000_000
                                    <+> modelMACoin bobCoinScript [("BOB", 100)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000
                      },
                    modelTx
                      { _mtxInputs = [(3, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["carol", "BobCoin"],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint =
                          SupportsMint
                            [(bobCoinScript, ([("BOB", -100)], noRdmr))]
                      }
                  ]
              ]
              mempty
          ],
      testProperty "mint-plutus" $
        testChainModelInteraction
          proxy
          ( modelGenesis
              [ (0, ("alice", Coin 1_000_000_000))
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxCollateral = SupportsPlutus (Set.fromList [0]),
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 <-> (modelCoin 1_000_000)
                                    <+> modelMACoin (modelPlutusScript 3) [("purp", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint =
                          SupportsMint
                            [(modelPlutusScript 3, ([("purp", 1234)], mkRdmr (PlutusTx.I 7, ExUnits 1 1)))]
                      }
                  ]
              ]
              mempty
          ],
      testProperty "tx-plutus" $
        testChainModelInteraction
          proxy
          ( modelGenesis
              [ (0, ("alice", Coin 1_000_000_000))
              ]
          )
          [ ModelEpoch
              [ ModelBlock
                  1
                  [ modelTx
                      { _mtxInputs = [(0, noRdmr)],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            ( 2,
                              (modelTxOut alwaysSucceedsPlutusAddress (modelCoin 1_000_000_000 <-> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                                { _mtxo_data = SupportsPlutus $ Just $ PlutusTx.I 7
                                }
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ],
                ModelBlock
                  2
                  [ modelTx
                      { _mtxInputs = [(2, mkRdmr (PlutusTx.I 7, ExUnits 1 1))],
                        _mtxWitnessSigs = Set.fromList ["bob"],
                        _mtxCollateral = SupportsPlutus (Set.fromList [1]),
                        _mtxOutputs =
                          [ (3, modelTxOut "bob" (modelCoin 100_000_000)),
                            (4, modelTxOut "alice" (modelCoin 1_000_000_000 <-> (2 :: Int) <×> (modelCoin 100_000_000 <+> modelCoin 1_000_000)))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ]
    ]
  where
    mkRdmr = SupportsPlutus . Just
    noRdmr = SupportsPlutus Nothing

shrinkSimpleTestData :: [ModelEpoch AllModelFeatures]
shrinkSimpleTestData =
  [ ModelEpoch
      [ ModelBlock 1 [modelTx {_mtxInputs = [(1, noRdmr)]}, modelTx {_mtxInputs = [(2, noRdmr)]}, modelTx {_mtxInputs = [(3, noRdmr)]}],
        ModelBlock 2 [modelTx {_mtxInputs = [(4, noRdmr)]}, modelTx {_mtxInputs = [(5, noRdmr)]}, modelTx {_mtxInputs = [(6, noRdmr)]}],
        ModelBlock 3 [modelTx {_mtxInputs = [(7, noRdmr)]}, modelTx {_mtxInputs = [(8, noRdmr)]}, modelTx {_mtxInputs = [(9, noRdmr)]}]
      ]
      mempty,
    ModelEpoch
      [ ModelBlock 4 [modelTx {_mtxInputs = [(10, noRdmr)]}, modelTx {_mtxInputs = [(11, noRdmr)]}, modelTx {_mtxInputs = [(12, noRdmr)]}],
        ModelBlock 5 [modelTx {_mtxInputs = [(13, noRdmr)]}, modelTx {_mtxInputs = [(14, noRdmr)]}, modelTx {_mtxInputs = [(15, noRdmr)]}],
        ModelBlock 6 [modelTx {_mtxInputs = [(16, noRdmr)]}, modelTx {_mtxInputs = [(17, noRdmr)]}, modelTx {_mtxInputs = [(18, noRdmr)]}]
      ]
      mempty,
    ModelEpoch
      [ ModelBlock 7 [modelTx {_mtxInputs = [(19, noRdmr)]}, modelTx {_mtxInputs = [(20, noRdmr)]}, modelTx {_mtxInputs = [(21, noRdmr)]}],
        ModelBlock 8 [modelTx {_mtxInputs = [(22, noRdmr)]}, modelTx {_mtxInputs = [(23, noRdmr)]}, modelTx {_mtxInputs = [(24, noRdmr)]}],
        ModelBlock 9 [modelTx {_mtxInputs = [(25, noRdmr)]}, modelTx {_mtxInputs = [(26, noRdmr)]}, modelTx {_mtxInputs = [(27, noRdmr)]}]
      ]
      mempty
  ]
  where
    noRdmr = SupportsPlutus Nothing

shrinkDiscardTestData :: [ModelEpoch AllModelFeatures]
shrinkDiscardTestData =
  let defaultTxOut = modelTxOut "bob" (modelCoin 0)
      mkTestPool poolId racct owners =
        ModelPoolParams
          { _mppId = ModelPoolId $ ModelKeyHashObj poolId,
            _mppVrm = ModelKeyHashObj poolId,
            _mppPledge = Coin 0,
            _mppCost = Coin 0,
            _mppMargin = fromJust $ boundRational $ 0 % 1,
            _mppRAcnt = racct,
            _mppOwners = owners
          }
      testPool = mkTestPool "pool1" "rewardAcct" ["poolOwner"]
      testPool2 = mkTestPool "pool2" "rewardAcct2" ["poolOwner2"]
      testPool3 = mkTestPool "pool3" "rewardAcct3" ["poolOwner3"]
      noRdmr = SupportsPlutus Nothing
   in [ ModelEpoch
          [ ModelBlock
              1
              [ modelTx {_mtxInputs = [(1, noRdmr)], _mtxOutputs = [(6, defaultTxOut)]},
                modelTx {_mtxInputs = [(2, noRdmr)], _mtxOutputs = [(5, defaultTxOut)]},
                modelTx {_mtxInputs = [(3, noRdmr)], _mtxOutputs = [(4, defaultTxOut)]}
              ],
            ModelBlock
              2
              [ modelTx {_mtxInputs = [(4, noRdmr)], _mtxOutputs = [(8, defaultTxOut), (9, defaultTxOut)]},
                modelTx {_mtxInputs = [(5, noRdmr)], _mtxDCert = [(ModelCertDeleg $ ModelRegKey "someAddress3", noRdmr)]},
                modelTx {_mtxInputs = [(6, noRdmr)], _mtxOutputs = [(7, defaultTxOut)]}
              ],
            ModelBlock
              3
              [ modelTx {_mtxInputs = [(7, noRdmr)], _mtxOutputs = [(11, defaultTxOut), (12, defaultTxOut)]},
                modelTx
                  { _mtxInputs = [(8, noRdmr)],
                    _mtxDCert =
                      [(ModelCertPool $ ModelRegPool testPool3, noRdmr)]
                  },
                modelTx {_mtxInputs = [(9, noRdmr)], _mtxOutputs = [(10, defaultTxOut)]}
              ]
          ]
          mempty,
        ModelEpoch
          [ ModelBlock
              4
              [ modelTx {_mtxInputs = [(10, noRdmr)], _mtxOutputs = [(14, defaultTxOut), (15, defaultTxOut)]},
                modelTx {_mtxInputs = [(11, noRdmr)]},
                modelTx
                  { _mtxInputs = [(12, noRdmr)],
                    _mtxOutputs = [(13, defaultTxOut)],
                    _mtxDCert =
                      [(ModelCertDeleg $ ModelDelegate (ModelDelegation "someAddress3" "pool3"), noRdmr)]
                  }
              ],
            ModelBlock
              5
              [ modelTx {_mtxInputs = [(13, noRdmr)], _mtxOutputs = [(17, defaultTxOut), (18, defaultTxOut)]},
                modelTx {_mtxInputs = [(14, noRdmr)], _mtxDCert = [(ModelCertDeleg $ ModelRegKey "someAddress2", noRdmr)]},
                modelTx {_mtxInputs = [(15, noRdmr)], _mtxOutputs = [(16, defaultTxOut)]}
              ],
            ModelBlock
              6
              [ modelTx {_mtxInputs = [(16, noRdmr)], _mtxOutputs = [(20, defaultTxOut), (21, defaultTxOut)]},
                modelTx {_mtxInputs = [(17, noRdmr)], _mtxDCert = [(ModelCertPool $ ModelRegPool testPool2, noRdmr)]},
                modelTx {_mtxInputs = [(18, noRdmr)], _mtxOutputs = [(19, defaultTxOut)]}
              ]
          ]
          mempty,
        ModelEpoch
          [ ModelBlock
              7
              [ modelTx {_mtxInputs = [(19, noRdmr)], _mtxOutputs = [(23, defaultTxOut), (24, defaultTxOut)]},
                modelTx {_mtxInputs = [(20, noRdmr)], _mtxDCert = [(ModelCertDeleg $ ModelRegKey "someAddress", noRdmr)]},
                modelTx {_mtxInputs = [(21, noRdmr)], _mtxOutputs = [(22, defaultTxOut)]}
              ],
            ModelBlock
              8
              [ modelTx {_mtxInputs = [(22, noRdmr)], _mtxOutputs = [(26, defaultTxOut), (27, defaultTxOut)]},
                modelTx
                  { _mtxInputs = [(23, noRdmr)],
                    _mtxDCert =
                      [(ModelCertPool $ ModelRegPool testPool, noRdmr)]
                  },
                modelTx {_mtxInputs = [(24, noRdmr)], _mtxOutputs = [(25, defaultTxOut)]}
              ],
            ModelBlock
              9
              [ modelTx {_mtxInputs = [(25, noRdmr)]},
                modelTx
                  { _mtxInputs = [(26, noRdmr)],
                    _mtxDCert =
                      [(ModelCertDeleg $ ModelDelegate (ModelDelegation "someAddress2" "pool2"), noRdmr)]
                  },
                modelTx
                  { _mtxInputs = [(27, noRdmr)],
                    _mtxDCert =
                      [(ModelCertDeleg $ ModelDelegate (ModelDelegation "someAddress" "pool1"), noRdmr)]
                  }
              ]
          ]
          mempty
      ]

testShrinkModelSimple :: [((), [ModelEpoch AllModelFeatures])]
testShrinkModelSimple = shrinkModelSimple ((), shrinkSimpleTestData)

testPoolParamModel :: forall era. KnownRequiredFeatures era => (ModelGenesis era, [ModelEpoch era])
testPoolParamModel =
  let modelPool =
        ModelPoolParams
          { _mppId = "pool1",
            _mppVrm = "pool1'",
            _mppPledge = Coin 500,
            _mppCost = Coin 0,
            _mppMargin = (fromJust $ boundRational $ 0 % 1),
            _mppRAcnt = "rewardAcct",
            _mppOwners = ["poolOwner"]
          }
      noRdmr = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Nothing
   in ( ( modelGenesis
            [ (0, ("unstaked", Coin 100_001_000_000)),
              (1, ("poolOwner", Coin 1_000_000)),
              (2, ("staked", Coin 1_000_000))
            ]
        ),
        [ ModelEpoch
            [ ModelBlock
                0
                [ modelTx
                    { _mtxInputs = [(0, noRdmr)],
                      _mtxOutputs = [(3, modelTxOut "unstaked" (modelCoin 1000000))],
                      _mtxWitnessSigs = Set.fromList $ ["unstaked", "pool1", "poolOwner", "staked"],
                      _mtxFee = modelCoin 100_000_000_000,
                      _mtxDCert =
                        [ (ModelCertDeleg $ ModelRegKey "staked", noRdmr),
                          (ModelCertDeleg $ ModelRegKey "poolOwner", noRdmr),
                          (ModelCertPool $ ModelRegPool modelPool, noRdmr),
                          (ModelCertDeleg $ ModelDelegate (ModelDelegation "staked" "pool1"), noRdmr),
                          (ModelCertDeleg $ ModelDelegate (ModelDelegation "poolOwner" "pool1"), noRdmr)
                        ]
                    },
                  modelTx
                    { _mtxInputs = [(3, noRdmr)],
                      _mtxWitnessSigs = Set.fromList $ ["unstaked", "pool1", "poolOwner"],
                      _mtxFee = modelCoin 1_000_000,
                      _mtxDCert =
                        [ (ModelCertPool $ ModelRegPool modelPool {_mppMargin = maxBound}, noRdmr)
                        ]
                    }
                ]
            ]
            mempty,
          ModelEpoch [] mempty,
          ModelEpoch [] (ModelBlocksMade $ Map.fromList [("pool1", 100)]),
          ModelEpoch [] mempty,
          ModelEpoch [] mempty
        ]
      )

modelShrinkingUnitTests :: TestTree
modelShrinkingUnitTests =
  testGroup
    "model-shrinking-unit-tests"
    [ testProperty "test simple shrink" $
        let noRdmr = SupportsPlutus Nothing
            x = shrinkModelSimple ((), shrinkSimpleTestData)
            y =
              ( (),
                [ ModelEpoch
                    [ ModelBlock 1 [modelTx {_mtxInputs = [(1, noRdmr)]}, modelTx {_mtxInputs = [(2, noRdmr)]}, modelTx {_mtxInputs = [(3, noRdmr)]}],
                      ModelBlock 2 [modelTx {_mtxInputs = [(4, noRdmr)]}, modelTx {_mtxInputs = [(5, noRdmr)]}, modelTx {_mtxInputs = [(6, noRdmr)]}],
                      ModelBlock 3 [modelTx {_mtxInputs = [(7, noRdmr)]}, modelTx {_mtxInputs = [(8, noRdmr)]}, modelTx {_mtxInputs = [(9, noRdmr)]}]
                    ]
                    mempty,
                  ModelEpoch
                    [ ModelBlock 4 [modelTx {_mtxInputs = [(10, noRdmr)]}, modelTx {_mtxInputs = [(11, noRdmr)]}, modelTx {_mtxInputs = [(12, noRdmr)]}],
                      ModelBlock 5 [modelTx {_mtxInputs = [(13, noRdmr)]}, modelTx {_mtxInputs = [(14, noRdmr)]}, modelTx {_mtxInputs = [(15, noRdmr)]}],
                      ModelBlock 6 [modelTx {_mtxInputs = [(16, noRdmr)]}, modelTx {_mtxInputs = [(17, noRdmr)]}, modelTx {_mtxInputs = [(18, noRdmr)]}]
                    ]
                    mempty,
                  ModelEpoch
                    [ ModelBlock 7 [modelTx {_mtxInputs = [(19, noRdmr)]}, modelTx {_mtxInputs = [(20, noRdmr)]}, modelTx {_mtxInputs = [(21, noRdmr)]}],
                      ModelBlock 8 [modelTx {_mtxInputs = [(22, noRdmr)]}, modelTx {_mtxInputs = [(23, noRdmr)]}, modelTx {_mtxInputs = [(24, noRdmr)]}],
                      ModelBlock 9 [modelTx {_mtxInputs = [(25, noRdmr)]}, modelTx {_mtxInputs = [(26, noRdmr)]}]
                    ]
                    mempty
                ]
              )
         in head x === ((), [ModelEpoch [ModelBlock 1 []] mempty])
              .&&. List.last x === y,
      testProperty "test pledge stuff" $
        prop_simulateChainModel' @AllModelFeatures testGlobals testPoolParamModel True
    ]

modelUnitTests_ :: TestTree
modelUnitTests_ =
  testGroup
    "model-unit-tests"
    [ modelUnitTests (Proxy :: Proxy (ShelleyEra C_Crypto)),
      modelUnitTests (Proxy :: Proxy (AlonzoEra C_Crypto)),
      modelShrinkingUnitTests
    ]

defaultTestMain :: IO ()
defaultTestMain = defaultMain modelUnitTests_
