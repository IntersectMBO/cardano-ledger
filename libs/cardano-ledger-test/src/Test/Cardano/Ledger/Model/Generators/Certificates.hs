{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.Generators.Certificates where

import Cardano.Ledger.BaseTypes
  ( Globals (..),
    UnitInterval,
    boundRational,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    DeltaCoin (..),
    toDeltaCoin,
    word64ToCoin,
  )
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Control.Lens
  ( to,
    use,
    uses,
    view,
  )
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Control.Monad.State.Strict as State
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable (fold, toList)
import Data.Functor.Identity (Identity (..))
import Data.Group (Group (..))
import Data.Group.GrpMap (GrpMap (..), grpMap, grpMapSingleton)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromJust,
    isJust,
    mapMaybe,
  )
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics ((:.:) (Comp1))
import QuickCheck.GenT
  ( MonadGen,
    choose,
    elements,
    frequency,
    liftGen,
    oneof,
    vectorOf,
  )
import Test.Cardano.Ledger.Model.API
  ( HasModelLedger,
    getModelLedger_rewards,
    getModelLedger_utxos,
    modelLedger,
    modelLedger_nes,
  )
import Test.Cardano.Ledger.Model.Acnt
  ( lookupModelAcnt,
    modelAcnt_reserves,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelPoolId,
    getGlobals,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet (..),
    FeatureTag (..),
    ScriptFeature,
    ScriptFeatureTag (..),
    TyScriptFeature (..),
    TyValueExpected (..),
    ValueFeatureTag (..),
    ifSupportsPlutus,
  )
import Test.Cardano.Ledger.Model.Generators
  ( AllowScripts,
    HasGenModelM,
    ModelGeneratorContext,
    chooseElems,
  )
import Test.Cardano.Ledger.Model.Generators.Address
  ( freshCredential,
    freshPoolAddress,
    freshRewardAddress,
    genStakingCredential,
  )
import Test.Cardano.Ledger.Model.Generators.Script
  ( genRedeemer,
    guardHaveCollateral,
  )
import Test.Cardano.Ledger.Model.Generators.Value (unfoldModelValue)
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelInstantaneousReward (..),
    modelDPStatedpsDState,
    modelDPStatedpsPState,
    modelDState_delegations,
    modelDState_iRwd,
    modelEpochState_acnt,
    modelEpochState_ls,
    modelEpochState_ss,
    modelLState_dpstate,
    modelLState_utxoSt,
    modelNewEpochState_es,
    modelPState_poolParams,
    modelSnapshot_pools,
    modelSnapshots_pstake,
    modelUTxOState_fees,
    modelUTxOState_utxo,
  )
import Test.Cardano.Ledger.Model.PParams
  ( ModelPParamsF (..),
    getModelPParams,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential,
    filterModelCredential,
    liftModelCredential,
  )
import Test.Cardano.Ledger.Model.Snapshot
  ( snapshotQueue_mark,
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelDelegCert (..),
    ModelDelegation (..),
    ModelMIRCert (..),
    ModelMIRTarget (..),
    ModelPoolCert (..),
    ModelPoolParams (..),
    ModelRedeemer,
    ModelScriptPurpose (..),
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
  )
import Test.Cardano.Ledger.Model.Value
  ( ModelValueF (..),
  )

genRegPool ::
  forall st era m.
  HasGenModelM st era m =>
  m [(Int, m (ModelDCert era))]
genRegPool = pure [(1, ModelCertPool . ModelRegPool <$> genModelPool)]

genRegKey ::
  forall st era m.
  HasGenModelM st era m =>
  m [(Int, m (ModelDCert era))]
genRegKey = do
  stakeHolders <-
    uses (modelLedger . to getModelLedger_utxos) $
      Map.keysSet . unGrpMap . _modelUTxOMap_stake
  registeredStake <-
    uses (modelLedger . to getModelLedger_rewards) $
      Map.keysSet
  let unregisteredStake = Set.difference stakeHolders registeredStake
  pure [(1, ModelCertDeleg . ModelRegKey <$> elements (Set.toList unregisteredStake)) | not (null unregisteredStake)]

genRegPoolOwnerKey ::
  forall st era m.
  HasGenModelM st era m =>
  m [(Int, m (ModelDCert era))]
genRegPoolOwnerKey = do
  registeredStake <- uses (modelLedger . to getModelLedger_rewards) $ Map.keysSet
  allDelegations <-
    uses
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPStatedpsDState . modelDState_delegations)
      $ Set.fromList . Map.toList
  pools <-
    use
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ss . modelSnapshots_pstake . snapshotQueue_mark . modelSnapshot_pools)

  let unDelegatedOwners = Set.difference allPoolOwners allDelegations
      allPoolOwners =
        foldMap
          ( \p ->
              foldMap (\cred -> Set.singleton (liftModelCredential cred, _mppId p)) $
                filter (flip Set.member registeredStake . liftModelCredential) $ _mppOwners p
          )
          pools

  pure
    [ ( 1,
        fmap (ModelCertDeleg . ModelDelegate) $
          uncurry ModelDelegation <$> elements (Set.toList unDelegatedOwners)
      )
      | not (null unDelegatedOwners)
    ]

genDelegation ::
  forall st era m.
  HasGenModelM st era m =>
  AllowScripts (ScriptFeature era) ->
  m [(Int, m (ModelDCert era, ModelRedeemer (ScriptFeature era)))]
genDelegation allowScripts = do
  registeredStake <- uses (modelLedger . to getModelLedger_rewards) $ Map.keysSet
  pools <-
    use
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ss . modelSnapshots_pstake . snapshotQueue_mark . modelSnapshot_pools)
  let registeredStake' = Set.filter (isJust . guardHaveCollateral allowScripts) registeredStake
  pure
    [ ( 1,
        do
          cert' <-
            ModelDelegation
              <$> elements (Set.toList registeredStake')
              <*> elements (Map.keys pools)
          let cert = ModelCertDeleg $ ModelDelegate cert'
          rdmr <- genRedeemer $ ModelScriptPurpose_Certifying cert
          pure (cert, rdmr)
      )
      | not (null registeredStake'),
        not (null $ Map.keys pools)
    ]

genMIR ::
  forall st era m.
  HasGenModelM st era m =>
  m [(Int, m (ModelDCert era))]
genMIR = do
  acnts <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_acnt)
  Comp1 irs <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPStatedpsDState . modelDState_iRwd)
  registeredStake <- uses (modelLedger . to getModelLedger_rewards) $ Map.keysSet
  pure $
    concat
      [ [ (,) 1 $ do
            amt <- oneof [pure minSend, pure maxSend, choose (minSend, maxSend)]
            pure $ ModelCertDeleg . ModelDCertMir . ModelMIRCert pot . ModelSendToOppositePotMIR $ amt
          | pot <- [minBound .. maxBound],
            let acnt = lookupModelAcnt acnts pot
                (ModelInstantaneousReward otherPot ir') = lookupModelAcnt irs pot
                totalExistingIR = fold ir'
                remaining = acnt <> otherPot ~~ totalExistingIR
                minSend = Coin 0
                maxSend = remaining,
            maxSend > mempty
        ],
        [ (,) 1 $ do
            -- add rewards
            sendAmt <- choose (minSend, maxSend)
            sendAmts <- liftGen $ unfoldModelValue @Void (Coin 1) (Val.inject sendAmt)
            GrpMap sendAddrs <-
              foldMapA
                ( \(ModelValueF (amt, _)) ->
                    grpMapSingleton
                      <$> oneof
                        ( [genStakingCredential "MIR"]
                            <> [elements (toList $ registeredStake) | not (null registeredStake)]
                        )
                      <*> pure (toDeltaCoin amt)
                )
                sendAmts
            pure $
              ModelCertDeleg . ModelDCertMir . ModelMIRCert pot . ModelStakeAddressesMIR $
                sendAddrs
          | pot <- [minBound .. maxBound],
            let acnt = lookupModelAcnt acnts pot
                (ModelInstantaneousReward otherPot ir') = lookupModelAcnt irs pot
                available = acnt <> otherPot
                totalExistingIR = fold ir'
                remaining = available ~~ totalExistingIR
                minSend = mempty
                maxSend = remaining,
            remaining > mempty
        ],
        [ (,) 1 $ do
            -- subtract rewards
            n <- choose (1, length ir)
            (ir', _) <- chooseElems n ir
            ir'' <- traverse (choose . (,DeltaCoin (-1)) . invert . toDeltaCoin) ir'
            pure $
              ModelCertDeleg . ModelDCertMir . ModelMIRCert pot . ModelStakeAddressesMIR $
                ir''
          | pot <- [minBound .. maxBound],
            let (ModelInstantaneousReward _ (GrpMap ir)) = lookupModelAcnt irs pot,
            not (null ir)
        ]
      ]

genDCert ::
  forall st era m.
  HasGenModelM st era m =>
  AllowScripts (ScriptFeature era) ->
  m (ModelDCert era, ModelRedeemer (ScriptFeature era))
genDCert allowScripts = do
  frequency
    =<< foldA
      [ withNoRedeemer genRegPool,
        withNoRedeemer genRegKey,
        withNoRedeemer genRegPoolOwnerKey,
        genDelegation allowScripts,
        withNoRedeemer genMIR
      ]
  where
    noRedeemer = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Nothing
    withNoRedeemer ::
      m [(Int, m (ModelDCert era))] ->
      m [(Int, m (ModelDCert era, ModelRedeemer (ScriptFeature era)))]
    withNoRedeemer = fmap . fmap . fmap . fmap $ \c -> (c, noRedeemer)
    foldA :: forall t w m'. (Foldable t, Monoid w, Applicative m') => t (m' w) -> m' w
    foldA = foldMapA id
    {-# INLINE foldA #-}

foldMapA :: forall t w m a. (Foldable t, Monoid w, Applicative m) => (a -> m w) -> t a -> m w
foldMapA = coerce (foldMap :: (a -> Ap m w) -> t a -> Ap m w)
{-# INLINE foldMapA #-}

adjustPledge :: MonadGen m => Coin -> m Coin
adjustPledge x = do
  let fifthOfX = (unCoin x) `div` 5
  randXAmount <- choose (0, fifthOfX)
  let adjustedX = (4 * fifthOfX) + randXAmount
  pure $ Coin adjustedX

genPoolParamPledgeOwner ::
  HasGenModelM st era m =>
  m (Coin, ModelCredential 'Staking (ScriptFeature era))
genPoolParamPledgeOwner = do
  globals <- asks getGlobals
  utxoMap <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo)
  owner <- getPoolParamAddress
  let ownerStake = (adjustPledge . fst) $ view (grpMap owner) $ _modelUTxOMap_stake utxoMap
      maxVal = (maxLovelaceSupply globals) + 1
  pledge <-
    frequency
      [ (1, pure $ Coin 0),
        (10, ownerStake),
        (1, pure $ word64ToCoin maxVal)
      ]
  pure (pledge, owner)

genPoolParamCost :: (MonadGen m, HasModelLedger era s, State.MonadState s m, MonadReader ModelGeneratorContext m) => m Coin
genPoolParamCost = do
  globals <- asks getGlobals
  minPoolCost <- uses (modelLedger . modelLedger_nes) (_modelPParams_minPoolCost . getModelPParams)
  reserves <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_acnt . modelAcnt_reserves)
  fees <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_fees)
  pools <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPStatedpsPState . modelPState_poolParams)
  let maxVal = (maxLovelaceSupply globals) + 1
      numOfPools = (\x -> bool x 1 (0 == x)) $ length $ Map.keys pools
      maxReasonableVal = ((unCoin reserves) + (unCoin fees)) `div` (toInteger numOfPools)
  frequency
    [ (1, pure $ runIdentity minPoolCost),
      (1, Coin <$> (choose (unCoin $ runIdentity minPoolCost, maxReasonableVal))),
      (1, pure $ word64ToCoin maxVal)
    ]

choosePoolAddr :: HasGenModelM st era m => [ModelCredential 'Staking sf] -> m (ModelCredential 'Staking sf)
choosePoolAddr existingAddrs =
  frequency $
    [(1, freshRewardAddress)]
      <> [(1, elements existingAddrs) | not (null existingAddrs)]

getPoolParamAddress :: HasGenModelM st era m => m (ModelCredential 'Staking (ScriptFeature era))
getPoolParamAddress = do
  utxoMap <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo)
  let creds = toList $ Map.keysSet $ unGrpMap $ _modelUTxOMap_stake utxoMap
  choosePoolAddr creds

getFilteredPoolParamAddress :: HasGenModelM st era m => m (ModelCredential 'Staking (ScriptFeature ('FeatureSet 'ExpectAnyOutput ('TyScriptFeature 'False 'False))))
getFilteredPoolParamAddress = do
  utxoMap <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo)
  let creds = toList $ Map.keysSet $ unGrpMap $ _modelUTxOMap_stake utxoMap
      addrList = mapMaybe (filterModelCredential (FeatureTag ValueFeatureTag_AnyOutput ScriptFeatureTag_None)) creds
  choosePoolAddr addrList

genPoolOwners :: HasGenModelM st era m => m [(ModelCredential 'Staking (ScriptFeature ('FeatureSet 'ExpectAnyOutput ('TyScriptFeature 'False 'False))))]
genPoolOwners = do
  numOfOwners <-
    frequency
      [ (1, pure (1 :: Int)),
        (1, choose (2 :: Int, 10))
      ]
  vectorOf numOfOwners getFilteredPoolParamAddress

genPoolMargin :: MonadGen m => m (UnitInterval)
genPoolMargin = do
  n <- choose (1, 98)
  d <- choose (2, 99)
  frequency $
    [ (1, pure $ (fromJust . boundRational) $ 0 % 1),
      (1, pure $ (fromJust . boundRational) 1)
    ]
      <> [(1, pure $ (fromJust . boundRational) $ n % d) | n < d]

getPoolParamIdVrf :: HasGenModelM st era m => m (ModelPoolId, ModelCredential 'StakePool ('TyScriptFeature 'False 'False))
getPoolParamIdVrf = do
  pools <-
    uses
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPStatedpsPState . modelPState_poolParams)
      $ Map.elems
  let poolIdVrfPair = (\p -> (_mppId p, _mppVrm p)) <$> pools
  frequency $
    [(1, elements poolIdVrfPair) | not (null poolIdVrfPair)]
      <> [(1, (,) <$> freshPoolAddress <*> (freshCredential "poolVrf"))]

genModelPool :: HasGenModelM st era m => m (ModelPoolParams era)
genModelPool = do
  (poolId, poolVrf) <- getPoolParamIdVrf
  cost <- genPoolParamCost
  (pledge, racct) <- genPoolParamPledgeOwner
  powners <- genPoolOwners
  margin <- genPoolMargin
  pure $ ModelPoolParams poolId poolVrf pledge cost margin racct powners
