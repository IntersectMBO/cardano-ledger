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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ModelChain.Properties where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (..))
import Cardano.Ledger.BaseTypes (Globals (..), NonNegativeInterval, UnitInterval, boundRational)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary.Value (AssetName (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Lens
import Control.Monad (when)
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
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Monoid (Endo (..))
import qualified Data.Ratio
import qualified Data.Set as Set
import Data.Some (Some (Some))
import Data.String (IsString (..))
import Data.Typeable
import GHC.Generics hiding (to)
import GHC.Natural
import qualified PlutusTx
import System.CPUTime
import Test.Cardano.Ledger.DependGraph (ModelGeneratorParamsF (..), defaultModelGeneratorParams, genModel)
import Test.Cardano.Ledger.ModelChain.Fixup (_FixupValues_FeeTooSmall, fixupValues, FixupValuesFlags(..))
import Test.Cardano.Ledger.Elaborators
import Test.Cardano.Ledger.Elaborators.Alonzo ()
import Test.Cardano.Ledger.Elaborators.Shelley ()
import Test.Cardano.Ledger.ModelChain hiding ((/))
import Test.Cardano.Ledger.ModelChain.Address
import Test.Cardano.Ledger.ModelChain.FeatureSet
import Test.Cardano.Ledger.ModelChain.Script
import Test.Cardano.Ledger.ModelChain.Shrinking
import Test.Cardano.Ledger.ModelChain.Utils
import Test.Cardano.Ledger.ModelChain.Value
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.Tasty
import Test.Tasty.QuickCheck

class IsRatio r where
  (%) :: Integer -> Integer -> r

instance IsRatio UnitInterval where
  n % d = maybe (error "IsRatio UnitInterval") id $ boundRational $ n Data.Ratio.% d

instance IsRatio Rational where
  (%) = (Data.Ratio.%)

instance IsRatio NonNegativeInterval where
  n % d = maybe (error "IsRatio NonNegativeInterval") id $ boundRational $ n Data.Ratio.% d

modelMACoin ::
  (ValueFeature era ~ 'ExpectAnyOutput) =>
  ModelScript (ScriptFeature era) ->
  [(AssetName, Integer)] ->
  ModelValue 'ExpectAnyOutput era
modelMACoin script assets = foldMap f assets
  where
    f (asset, qty) = ModelValue $ ModelValue_Scale qty $ ModelValue_Var $ ModelValue_MA (script, asset)

modelMACoin' ::
  (ValueFeature era ~ 'ExpectAnyOutput) =>
  ModelScript (ScriptFeature era) ->
  [(AssetName, Integer)] ->
  ModelValue 'ExpectAnyOutput era
modelMACoin' script assets = foldMap f assets
  where
    f (asset, qty) = ModelValue $ ModelValue_Scale qty $ ModelValue_Var $ ModelValue_MA (script, asset)

modelCoin :: Integer -> ModelValue era k
modelCoin = ModelValue . ModelValue_Inject . Coin

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

infixl 7 $*

($*) :: Integer -> ModelValue era k -> ModelValue era k
x $* ModelValue y = ModelValue (ModelValue_Scale x y)

infixl 6 $+

($+) :: ModelValue era k -> ModelValue era k -> ModelValue era k
ModelValue x $+ ModelValue y = ModelValue (ModelValue_Add x y)

infixl 6 $-

($-) :: ModelValue era k -> ModelValue era k -> ModelValue era k
ModelValue x $- ModelValue y = ModelValue (ModelValue_Sub x y)

purpleModelScript :: ModelScript ('TyScriptFeature 'True x)
purpleModelScript = ModelScript_Timelock $ ModelTimelock_AllOf []

bobCoinScript :: ModelScript ('TyScriptFeature 'True x)
bobCoinScript = ModelScript_Timelock $ ModelTimelock_Signature "BobCoin"

modelPlutusScript :: Natural -> ModelScript ('TyScriptFeature x 'True)
modelPlutusScript = ModelScript_PlutusV1 . ModelPlutusScript_AlwaysSucceeds

instance IsString AssetName where
  fromString = AssetName . BS.pack

modelTestDelegations ::
  ( ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era
  ) =>
  proxy era ->
  Bool ->
  ModelAddress AllScriptFeatures ->
  [TestTree]
modelTestDelegations proxy needsCollateral stakeAddr@(ModelAddress _ stakeCred) =
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
      allAtOnce =
        [ ModelBlock
            0
            [ modelTx
                { _mtxInputs = Set.fromList [0],
                  _mtxWitnessSigs = Set.fromList $ ["unstaked", "pool1", "poolOwner"] <> mapMaybe mkWitness [_modelAddress_pmt stakeAddr],
                  _mtxRedeemers = Map.fromList [x | x <- [(ModelScriptPurpose_Certifying modelDelegation, (PlutusTx.I 5, ExUnits 1 1))], needsCollateral],
                  _mtxCollateral = SupportsPlutus $ Set.fromList [x | x <- [0], needsCollateral],
                  _mtxOutputs =
                    [ (100, modelTxOut "unstaked" (modelCoin 9_800_000_000_000)),
                      (101, modelTxOut stakeAddr (modelCoin 100_000_000_000))
                    ],
                  _mtxFee = modelCoin 100_000_000_000,
                  _mtxDCert =
                    [ ModelCertDeleg $ ModelRegKey stakeCred,
                      ModelCertPool $ ModelRegPool modelPool,
                      modelDelegation
                    ]
                }
            ]
        ]
      oneAtATime =
        [ ModelBlock
            0
            [ modelTx
                { _mtxInputs = Set.fromList [0],
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
                { _mtxInputs = Set.fromList [1],
                  _mtxWitnessSigs = Set.fromList ["unstaked"],
                  _mtxOutputs =
                    [ (2, modelTxOut "unstaked" (modelCoin 9_850_000_000_000))
                    ],
                  _mtxFee = modelCoin 25_000_000_000,
                  _mtxDCert = [ModelCertDeleg $ ModelRegKey stakeCred]
                }
            ],
          ModelBlock
            2
            [ modelTx
                { _mtxInputs = Set.fromList [2],
                  _mtxWitnessSigs = Set.fromList ["unstaked", "pool1", "poolOwner"],
                  _mtxOutputs =
                    [ (3, modelTxOut "unstaked" (modelCoin 9_825_000_000_000))
                    ],
                  _mtxFee = modelCoin 25_000_000_000,
                  _mtxDCert = [ModelCertPool $ ModelRegPool modelPool]
                }
            ],
          ModelBlock
            3
            [ modelTx
                { _mtxInputs = Set.fromList [3],
                  _mtxWitnessSigs = Set.fromList $ ["unstaked"] <> mapMaybe mkWitness [_modelAddress_pmt stakeAddr],
                  _mtxRedeemers = Map.fromList [x | x <- [(ModelScriptPurpose_Certifying modelDelegation, (PlutusTx.I 5, ExUnits 1 1))], needsCollateral],
                  _mtxCollateral = SupportsPlutus $ Set.fromList [x | x <- [3], needsCollateral],
                  _mtxOutputs =
                    [ (100, modelTxOut "unstaked" (modelCoin 9_800_000_000_000))
                    ],
                  _mtxFee = modelCoin 25_000_000_000,
                  _mtxDCert =
                    [ modelDelegation
                    ]
                }
            ]
        ]
      genAct =
        modelGenesis
          [ (0, ("unstaked", Coin 10_000_000_000_000))
          ]
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
                      { _mtxInputs = Set.fromList [100],
                        _mtxWitnessSigs = Set.fromList $ ["unstaked"] <> mapMaybe mkWitness [_modelAddress_pmt stakeAddr],
                        _mtxRedeemers = Map.fromList [x | x <- [(ModelScriptPurpose_Rewarding stakeCred, (PlutusTx.I 5, ExUnits 1 1))], needsCollateral],
                        _mtxCollateral = SupportsPlutus $ Set.fromList [x | x <- [100], needsCollateral],
                        _mtxOutputs =
                          [ (103, modelTxOut "unstaked" (modelCoin 9_700_000_000_000)),
                            (104, modelTxOut "reward-less-minimum" (reward $- modelCoin 100_000_000)),
                            (105, modelTxOut "minimum" (modelCoin 100_000_000))
                          ],
                        _mtxFee = modelCoin 100_000_000_000,
                        _mtxWdrl = Map.singleton stakeCred reward
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
          _modelGeneratorParams_epochs = pure 8, -- choose (10, 12),
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
      intersectingPoolIds = Set.intersection poolsWithPledge $ _modelProvenanceState_poolPerformance prov

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
  ( LedgerState.TransUTxOState Show era,
    Show (Core.Tx era)
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
    _poolsWithPledge :: !(f Int)
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
        lengthOf (traverse . _2 . modelTxOut_address . modelAddress_pmt . traverseModelScriptHashObj)
          . uncurry Map.restrictKeys
          . ((_modelUTxOMap_utxos . collectModelUTxOs) &&& collectModelInputs),
      _scriptWdrls =
        lengthOf (traverse . traverseModelScriptHashObj) . (=<<) Map.keys . toListOf (traverse . modelTxs . modelTx_wdrl),
      _poolsWithCost = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((> mempty) . _mppCost)),
      _poolsWithZeroMargin = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((== minBound) . _mppMargin)),
      _poolsWithMaxMargin = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((== maxBound) . _mppMargin)),
      _poolsWithIntermediateMargin = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((\x -> x > minBound && x < maxBound) . _mppMargin)),
      _poolsWithPledge = lengthOf (traverse . modelDCerts . _ModelRegisterPool . filtered ((> mempty) . _mppPledge))
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
      _poolsWithPledge = Const (shelleyFeatureTag, 0, "pool has pledge") :*: Predicate (> 0)
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
    [ _mtxInputs tx
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
    LedgerState.TransUTxOState Show era,
    Show (Core.Tx era),
    Show (Core.Script era)
  ) =>
  proxy era ->
  Property
modelGenTest proxy =
  forAllShrink
    (genModel' (reifyRequiredFeatures $ Proxy @(EraFeatureSet era)) testGlobals)
    (shrinkModel testGlobals)
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
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era
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
propertyShrinkingImpl callback (a, b) =
  let (a', b') =
        maybe (a, b) id $
          discardUnnecessaryTxnsImpl testGlobals (a, b)
      (errs, (a'', b'')) = fixupValues (FixupValuesFlags True) testGlobals (a', b')
      numBefore = (lengthOf (traverse . modelTxs)) b
      numAfter = (lengthOf (traverse . modelTxs)) b'

      {-(errs', ij@(i, j)) =
        maybe (mempty, (a', b')) id $
          discardUnnecessaryTxnsWithErrors testGlobals (a', b')
      numAfter' = (lengthOf (traverse . modelTxs)) j

      (errs'', ij'@(_, j')) =
        maybe (mempty, (i, j)) id $
          collapseTransactionsWithErrors testGlobals (i, j)
      numAfterCollapse = (lengthOf (traverse . modelTxs)) j'-}

      allErrs = errs -- <> errs' <> errs''

      --mustAbort = has (traverse . traverse . _FixupValues_FeeTooSmall) allErrs
      mustAbort = toListOf (traverse . traverse . _FixupValues_FeeTooSmall) allErrs
   in ( execPropertyWriter $ do
          tellProperty $ cover 55 (numAfter < numBefore) "numAfter < numBefore"
          --tellProperty $ cover 55 (numAfter' == numAfter) "numAfter' == numAfter"

          --tellProperty $ counterexample $ "\nafter discarding: " <> (show ij)

          --tellProperty $ cover 55 (numAfterCollapse < numAfter) "numAfterCollapse < numAfter"
          --tellProperty $ counterexample $ "\nshrinking result after collapsing: " <> (show ij')
          -- tellProperty $ counterexample ("a'b': " <> show (a', b'))
          -- tellProperty $ counterexample (show mustAbort)
          tellProperty_ $ null mustAbort

          tellProperty $ cover 10 (isNothing errs) "no errors from discard"
          tellProperty $ cover 90 (isNothing allErrs) "no fixup errors"
          --tellProperty $ cover 10 (not mustAbort) "no fatal fixup errors"
      )
        (callback (a'', b''))--ij')

propertyShrinking ::
  forall era proxy.
  ( Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era,
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

generateOneExample' :: IO ()
generateOneExample' = do
  let proxy = (Proxy :: Proxy (ShelleyEra C_Crypto))
      proxy' = eraFeatureSet proxy
  (_ :: ModelShrinkingPhase, ab@(a, b)) <- time "generate" $ generate $ genModel' proxy' testGlobals
  time "examine" $ print $ examineModel b
  mab' <- time "s1" $ pure $ discardUnnecessaryTxnsImpl testGlobals (a, b)
  for_ mab' $ \ab' -> do
    (errs, ab'') <- time "s1'" $ pure $ fixupValues (FixupValuesFlags True) testGlobals ab'
    for_  errs $ \err -> do
      print ("ab", ab)
      print ("ab' == discardUnnecessaryTxnsImpl ab", ab)
      print ("ab'' == fixupValues ab'", ab'')
      print errs
    time "examine'" $ print $ examineModel $ snd ab''
    mresult <- time "modelApp" $ pure $ simulateChainModel testGlobals a b
    putStrLn "done"
    -- print mresult

-- | some hand-written model based unit tests
modelUnitTests ::
  forall era proxy.
  ( ElaborateEraModel era,
    Eq (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Value era),
    LedgerState.TransUTxOState Show era,
    Show (Core.Tx era),
    Show (Core.Script era)
  ) =>
  proxy era ->
  TestTree
modelUnitTests proxy =
  testGroup
    (show $ typeRep proxy)
    [ testProperty "gen" $ modelGenTest proxy,
      testProperty "gen Always shrink" $ checkCoverage $ testModelShrinking proxy,
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
      testGroup "deleg-keyHash" $ modelTestDelegations proxy False "keyHashStake",
      testGroup "deleg-plutus" $ modelTestDelegations proxy True alwaysSucceedsPlutusAddress,
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
                          ],
                        _mtxDCert =
                          [ ModelCertDeleg $ ModelDCertGenesis $ ModelGenesisDelegCert "gd1" "gd1:2"
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
                          ],
                        _mtxDCert =
                          [ ModelCertDeleg $
                              ModelDCertMir $
                                ModelMIRCert ReservesMIR $
                                  ModelStakeAddressesMIR [("carol", DeltaCoin 100_123)],
                            ModelCertDeleg $ ModelRegKey "carol"
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
                      { _mtxInputs = (Set.fromList [0]),
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            (2, modelTxOut "alice" (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ],
                ModelBlock
                  2
                  [ modelTx
                      { _mtxInputs = Set.fromList [2],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ (3, modelTxOut "bob" (modelCoin 100_000_000)),
                            (4, modelTxOut "alice" (modelCoin 1_000_000_000 $- 2 $* (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (modelCoin 1_000_000)
                                    $+ modelMACoin purpleModelScript [("purp", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin purpleModelScript [("purp", 1234)])
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice", "BobCoin"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (modelCoin 1_000_000)
                                    $+ modelMACoin bobCoinScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin bobCoinScript [("BOB", 1234)])
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (modelCoin 1_000_000)
                                    $+ modelMACoin purpleModelScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin purpleModelScript [("BOB", 1234)])
                      },
                    modelTx
                      { _mtxInputs = Set.fromList [1],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 2,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (3 $* modelCoin 1_000_000)
                                    $+ modelMACoin purpleModelScript [("BOB", 1134)]
                                )
                            ),
                            ( 3,
                              modelTxOut
                                "carol"
                                ( modelCoin 1_000_000
                                    $+ modelMACoin purpleModelScript [("BOB", 100)]
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice", "BobCoin"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (modelCoin 1_000_000)
                                    $+ modelMACoin bobCoinScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin bobCoinScript [("BOB", 1234)])
                      },
                    modelTx
                      { _mtxInputs = Set.fromList [1],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 2,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (3 $* modelCoin 1_000_000)
                                    $+ modelMACoin bobCoinScript [("BOB", 1134)]
                                )
                            ),
                            ( 3,
                              modelTxOut
                                "carol"
                                ( modelCoin 1_000_000
                                    $+ modelMACoin bobCoinScript [("BOB", 100)]
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice", "BobCoin"],
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (modelCoin 1_000_000)
                                    $+ modelMACoin bobCoinScript [("BOB", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin bobCoinScript [("BOB", 1234)])
                      },
                    modelTx
                      { _mtxInputs = Set.fromList [1],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ ( 2,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (3 $* modelCoin 1_000_000)
                                    $+ modelMACoin bobCoinScript [("BOB", 1134)]
                                )
                            ),
                            ( 3,
                              modelTxOut
                                "carol"
                                ( modelCoin 1_000_000
                                    $+ modelMACoin bobCoinScript [("BOB", 100)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000
                      },
                    modelTx
                      { _mtxInputs = Set.fromList [3],
                        _mtxWitnessSigs = Set.fromList ["carol", "BobCoin"],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin bobCoinScript [("BOB", -100)])
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxRedeemers = Map.singleton (ModelScriptPurpose_Minting $ modelPlutusScript 3) (PlutusTx.I 7, ExUnits 1 1),
                        _mtxCollateral = SupportsPlutus (Set.fromList [0]),
                        _mtxOutputs =
                          [ ( 1,
                              modelTxOut
                                "alice"
                                ( modelCoin 1_000_000_000 $- (modelCoin 1_000_000)
                                    $+ modelMACoin (modelPlutusScript 3) [("purp", 1234)]
                                )
                            )
                          ],
                        _mtxFee = modelCoin 1_000_000,
                        _mtxMint = SupportsMint (modelMACoin (modelPlutusScript 3) [("purp", 1234)])
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
                      { _mtxInputs = Set.fromList [0],
                        _mtxWitnessSigs = Set.fromList ["alice"],
                        _mtxOutputs =
                          [ (1, modelTxOut "bob" (modelCoin 100_000_000)),
                            ( 2,
                              (modelTxOut alwaysSucceedsPlutusAddress (modelCoin 1_000_000_000 $- (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
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
                      { _mtxInputs = Set.fromList [2],
                        _mtxWitnessSigs = Set.fromList ["bob"],
                        _mtxRedeemers = Map.singleton (ModelScriptPurpose_Spending 2) (PlutusTx.I 7, ExUnits 1 1),
                        _mtxCollateral = SupportsPlutus (Set.fromList [1]),
                        _mtxOutputs =
                          [ (3, modelTxOut "bob" (modelCoin 100_000_000)),
                            (4, modelTxOut "alice" (modelCoin 1_000_000_000 $- 2 $* (modelCoin 100_000_000 $+ modelCoin 1_000_000)))
                          ],
                        _mtxFee = modelCoin 1_000_000
                      }
                  ]
              ]
              mempty
          ]
    ]

shrinkSimpleTestData :: [ModelEpoch AllModelFeatures]
shrinkSimpleTestData =
  [ ModelEpoch
      [ ModelBlock 1 [modelTx {_mtxInputs = [1]}, modelTx {_mtxInputs = [2]}, modelTx {_mtxInputs = [3]}],
        ModelBlock 2 [modelTx {_mtxInputs = [4]}, modelTx {_mtxInputs = [5]}, modelTx {_mtxInputs = [6]}],
        ModelBlock 3 [modelTx {_mtxInputs = [7]}, modelTx {_mtxInputs = [8]}, modelTx {_mtxInputs = [9]}]
      ]
      mempty,
    ModelEpoch
      [ ModelBlock 4 [modelTx {_mtxInputs = [10]}, modelTx {_mtxInputs = [11]}, modelTx {_mtxInputs = [12]}],
        ModelBlock 5 [modelTx {_mtxInputs = [13]}, modelTx {_mtxInputs = [14]}, modelTx {_mtxInputs = [15]}],
        ModelBlock 6 [modelTx {_mtxInputs = [16]}, modelTx {_mtxInputs = [17]}, modelTx {_mtxInputs = [18]}]
      ]
      mempty,
    ModelEpoch
      [ ModelBlock 7 [modelTx {_mtxInputs = [19]}, modelTx {_mtxInputs = [20]}, modelTx {_mtxInputs = [21]}],
        ModelBlock 8 [modelTx {_mtxInputs = [22]}, modelTx {_mtxInputs = [23]}, modelTx {_mtxInputs = [24]}],
        ModelBlock 9 [modelTx {_mtxInputs = [25]}, modelTx {_mtxInputs = [26]}, modelTx {_mtxInputs = [27]}]
      ]
      mempty
  ]

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
   in [ ModelEpoch
          [ ModelBlock
              1
              [ modelTx {_mtxInputs = [1], _mtxOutputs = [(6, defaultTxOut)]},
                modelTx {_mtxInputs = [2], _mtxOutputs = [(5, defaultTxOut)]},
                modelTx {_mtxInputs = [3], _mtxOutputs = [(4, defaultTxOut)]}
              ],
            ModelBlock
              2
              [ modelTx {_mtxInputs = [4], _mtxOutputs = [(8, defaultTxOut), (9, defaultTxOut)]},
                modelTx {_mtxInputs = [5], _mtxDCert = [ModelCertDeleg $ ModelRegKey "someAddress3"]},
                modelTx {_mtxInputs = [6], _mtxOutputs = [(7, defaultTxOut)]}
              ],
            ModelBlock
              3
              [ modelTx {_mtxInputs = [7], _mtxOutputs = [(11, defaultTxOut), (12, defaultTxOut)]},
                modelTx {_mtxInputs = [8], _mtxDCert = [ModelCertPool $ ModelRegPool testPool3]},
                modelTx {_mtxInputs = [9], _mtxOutputs = [(10, defaultTxOut)]}
              ]
          ]
          mempty,
        ModelEpoch
          [ ModelBlock
              4
              [ modelTx {_mtxInputs = [10], _mtxOutputs = [(14, defaultTxOut), (15, defaultTxOut)]},
                modelTx {_mtxInputs = [11]},
                modelTx {_mtxInputs = [12], _mtxOutputs = [(13, defaultTxOut)], _mtxDCert = [ModelCertDeleg $ ModelDelegate (ModelDelegation "someAddress3" "pool3")]}
              ],
            ModelBlock
              5
              [ modelTx {_mtxInputs = [13], _mtxOutputs = [(17, defaultTxOut), (18, defaultTxOut)]},
                modelTx {_mtxInputs = [14], _mtxDCert = [ModelCertDeleg $ ModelRegKey "someAddress2"]},
                modelTx {_mtxInputs = [15], _mtxOutputs = [(16, defaultTxOut)]}
              ],
            ModelBlock
              6
              [ modelTx {_mtxInputs = [16], _mtxOutputs = [(20, defaultTxOut), (21, defaultTxOut)]},
                modelTx {_mtxInputs = [17], _mtxDCert = [ModelCertPool $ ModelRegPool testPool2]},
                modelTx {_mtxInputs = [18], _mtxOutputs = [(19, defaultTxOut)]}
              ]
          ]
          mempty,
        ModelEpoch
          [ ModelBlock
              7
              [ modelTx {_mtxInputs = [19], _mtxOutputs = [(23, defaultTxOut), (24, defaultTxOut)]},
                modelTx {_mtxInputs = [20], _mtxDCert = [ModelCertDeleg $ ModelRegKey "someAddress"]},
                modelTx {_mtxInputs = [21], _mtxOutputs = [(22, defaultTxOut)]}
              ],
            ModelBlock
              8
              [ modelTx {_mtxInputs = [22], _mtxOutputs = [(26, defaultTxOut), (27, defaultTxOut)]},
                modelTx {_mtxInputs = [23], _mtxDCert = [ModelCertPool $ ModelRegPool testPool]},
                modelTx {_mtxInputs = [24], _mtxOutputs = [(25, defaultTxOut)]}
              ],
            ModelBlock
              9
              [ modelTx {_mtxInputs = [25]},
                modelTx {_mtxInputs = [26], _mtxDCert = [ModelCertDeleg $ ModelDelegate (ModelDelegation "someAddress2" "pool2")]},
                modelTx {_mtxInputs = [27], _mtxDCert = [ModelCertDeleg $ ModelDelegate (ModelDelegation "someAddress" "pool1")]}
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
            _mppPledge = (Coin 500),
            _mppCost = (Coin 0),
            _mppMargin = (fromJust $ boundRational $ 0 % 1),
            _mppRAcnt = "rewardAcct",
            _mppOwners = ["poolOwner"]
          }
  in (
      ( modelGenesis
          [ (0, ("unstaked", Coin 100_001_000_000)),
            (1, ("poolOwner", Coin 1_000_000)),
            (2, ("staked", Coin 1_000_000))
          ]
      ),
      [ ModelEpoch [
          ModelBlock
            0
            [ modelTx
                { _mtxInputs = Set.fromList [0],
                  _mtxOutputs = [(3, modelTxOut "unstaked" (modelCoin 1000000))],
                  _mtxWitnessSigs = Set.fromList $ ["unstaked", "pool1", "poolOwner", "staked"],
                  _mtxFee = modelCoin 100_000_000_000,
                  _mtxDCert =
                    [ ModelCertDeleg $ ModelRegKey "staked",
                      ModelCertDeleg $ ModelRegKey "poolOwner",
                      ModelCertPool $ ModelRegPool modelPool,
                      ModelCertDeleg $ ModelDelegate (ModelDelegation "staked" "pool1"),
                      ModelCertDeleg $ ModelDelegate (ModelDelegation "poolOwner" "pool1")
                    ]
                },
              modelTx
                  { _mtxInputs = Set.fromList [3],
                    _mtxWitnessSigs = Set.fromList $ ["unstaked", "pool1", "poolOwner"],
                    _mtxFee = modelCoin 1_000_000,
                    _mtxDCert =
                      [ ModelCertPool $ ModelRegPool modelPool {_mppMargin = maxBound}
                      ]
                  }
            ]

      ] mempty,
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
        let x = shrinkModelSimple ((), shrinkSimpleTestData)
            y =
              ( (),
                [ ModelEpoch
                    [ ModelBlock 1 [modelTx {_mtxInputs = [1]}, modelTx {_mtxInputs = [2]}, modelTx {_mtxInputs = [3]}],
                      ModelBlock 2 [modelTx {_mtxInputs = [4]}, modelTx {_mtxInputs = [5]}, modelTx {_mtxInputs = [6]}],
                      ModelBlock 3 [modelTx {_mtxInputs = [7]}, modelTx {_mtxInputs = [8]}, modelTx {_mtxInputs = [9]}]
                    ]
                    mempty,
                  ModelEpoch
                    [ ModelBlock 4 [modelTx {_mtxInputs = [10]}, modelTx {_mtxInputs = [11]}, modelTx {_mtxInputs = [12]}],
                      ModelBlock 5 [modelTx {_mtxInputs = [13]}, modelTx {_mtxInputs = [14]}, modelTx {_mtxInputs = [15]}],
                      ModelBlock 6 [modelTx {_mtxInputs = [16]}, modelTx {_mtxInputs = [17]}, modelTx {_mtxInputs = [18]}]
                    ]
                    mempty,
                  ModelEpoch
                    [ ModelBlock 7 [modelTx {_mtxInputs = [19]}, modelTx {_mtxInputs = [20]}, modelTx {_mtxInputs = [21]}],
                      ModelBlock 8 [modelTx {_mtxInputs = [22]}, modelTx {_mtxInputs = [23]}, modelTx {_mtxInputs = [24]}],
                      ModelBlock 9 [modelTx {_mtxInputs = [25]}, modelTx {_mtxInputs = [26]}]
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
    [ --modelUnitTests (Proxy :: Proxy (ShelleyEra C_Crypto)),
      modelUnitTests (Proxy :: Proxy (AlonzoEra C_Crypto)),
      modelShrinkingUnitTests
    ]

defaultTestMain :: IO ()
defaultTestMain = defaultMain modelUnitTests_
