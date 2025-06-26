{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Conway.Gen where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.PoolParams
import Control.Monad.State.Strict (MonadState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import qualified System.Random.Stateful as R
import Test.Cardano.Ledger.Binary.Random (QC (..))
import Test.Cardano.Ledger.Conway.Arbitrary
import Test.Cardano.Ledger.Core.Arbitrary
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

instance R.UniformRange EpochNo where
  uniformRM (EpochNo l, EpochNo h) g = EpochNo <$> R.uniformRM (l, h) g

genSize :: MonadGen m => m Int
genSize =
  frequency [(1, pure 0), (100, getPositive <$> arbitrary), (10, choose (0, 1000))]

freshCredential ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen s m, MonadGen m) => m (Credential r)
freshCredential =
  frequency [(5, KeyHashObj <$> freshKeyHash)] -- (2, pickScriptHash)] TODO: implement script pool,
  -- where picking actually removes from the pool to enforce uniqueness

genAccountState ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen s m, MonadGen m) => m (ConwayAccountState era)
genAccountState = do
  casBalance <- frequency [(2, pure mempty), (10, arbitrary)]
  casDeposit <- arbitrary
  casStakePoolDelegation <- frequency [(2, pure SNothing), (5, SJust <$> freshKeyHash)]
  casDRepDelegation <-
    frequency
      [ (3, pure SNothing)
      , (2, pure $ SJust DRepAlwaysAbstain)
      , (1, pure $ SJust DRepAlwaysNoConfidence)
      , (10, SJust . DRepCredential <$> freshCredential)
      ]
  pure ConwayAccountState {..}

genAccounts ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen s m) => m (ConwayAccounts era)
genAccounts = do
  n <- genSize
  ConwayAccounts . Map.fromList <$> replicateM n ((,) <$> freshCredential <*> genAccountState)

genPState ::
  (EraAccounts era, HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen s m) =>
  EpochNo ->
  DState era ->
  m (PState era)
genPState curEpochNo dState = do
  let accountsMap = dState ^. accountsL . accountsMapL
      accountsStakePoolDelegations = Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap
      stakePoolDelegators =
        Map.fromListWith (<>) $
          [ (stakePoolId, Set.singleton stakeCred)
          | (stakeCred, stakePoolId) <- Map.toList accountsStakePoolDelegations
          ]
      genStakePoolState :: KeyHash 'StakePool -> Set.Set (Credential 'Staking) -> m PoolParams
      genStakePoolState ppId _delegators = do
        ppVrf <- freshKeyHashVRF
        ppPledge <- Coin . getNonNegative <$> arbitrary
        ppCost <- Coin . getNonNegative <$> arbitrary
        ppMargin <- arbitrary
        ppRewardAccount <-
          RewardAccount Testnet
            <$> frequency
              [ -- (1, freshCredential)
                -- ,
                (100, elements (Map.keys accountsMap))
              ]
        nOwners <- genSize
        ppOwners <-
          frequency
            [ -- (1, freshKeyHash)
              -- ,

              ( 100
              , liftGen $ uniformSubSet
                  (Just nOwners)
                  (Set.fromList [kh | KeyHashObj kh <- Map.keys accountsMap])
                  QC
              )
            ]
        ppRelays <- arbitrary
        ppMetadata <- arbitrary
        pure PoolParams {..}
  stakePoolsWithDelegation <- Map.traverseWithKey genStakePoolState stakePoolDelegators
  stakePoolsWithoutDelegation <-
    Map.fromList
      <$> listOf (freshKeyHash >>= \poolId -> (,) poolId <$> genStakePoolState poolId Set.empty)
  let psStakePoolParams = Map.union stakePoolsWithDelegation stakePoolsWithoutDelegation
      psFutureStakePoolParams = mempty
  psRetiring <-
    uniformSubMap Nothing psStakePoolParams QC
      >>= traverse (\_ -> uniformRM (curEpochNo, binOpEpochNo (+) curEpochNo (EpochNo 20)))
  psDeposits <-
    traverse (\_ -> Coin . getNonNegative <$> arbitrary) $
      psStakePoolParams <> psFutureStakePoolParams
  pure PState {..}

genVState ::
  (ConwayEraAccounts era, HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen s m) =>
  EpochNo ->
  DState era ->
  m (VState era)
genVState curEpochNo dState = do
  let accountsDRepDelegations =
        Map.mapMaybe (^. dRepDelegationAccountStateL) $ dState ^. accountsL . accountsMapL
      dRepDelegators =
        Map.fromListWith (<>) $
          [ (dRepCred, Set.singleton stakeCred)
          | (stakeCred, DRepCredential dRepCred) <- Map.toList accountsDRepDelegations
          ]
      genDRepState drepDelegs = do
        let lowEpochNo
              | curEpochNo < EpochNo 10 = curEpochNo
              | otherwise = EpochNo 10
        drepExpiry <-
          frequency
            [ (1, uniformRM (binOpEpochNo (-) curEpochNo lowEpochNo, curEpochNo))
            , (20, uniformRM (curEpochNo, binOpEpochNo (+) curEpochNo (EpochNo 20)))
            ]
        drepAnchor <- arbitrary
        drepDeposit <- Coin . getPositive <$> arbitrary
        pure DRepState {..}
  dRepsWithDelegation <- traverse genDRepState dRepDelegators
  dRepsWithoutDelegation <-
    Map.fromList <$> listOf ((,) <$> freshCredential <*> genDRepState Set.empty)
  vsCommitteeState <- genCommitteeState
  vsNumDormantEpochs <- frequency [(20, pure 0), (2, arbitrary), (10, uniformRM (0, 10))]
  pure
    VState
      { vsDReps = Map.union dRepsWithDelegation dRepsWithoutDelegation
      , ..
      }

genCommitteeState ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen s m) => m (CommitteeState era)
genCommitteeState = do
  n <- genSize
  CommitteeState . Map.fromList <$> vectorOf n ((,) <$> freshCredential <*> genCommitteeAuthorization)

genCommitteeAuthorization ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen s m) => m CommitteeAuthorization
genCommitteeAuthorization =
  frequency
    [ (10, CommitteeHotCredential <$> freshCredential)
    , (1, CommitteeMemberResigned <$> arbitrary)
    ]
