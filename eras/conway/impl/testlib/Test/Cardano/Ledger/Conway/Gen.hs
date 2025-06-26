{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Conway.Gen where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes (GenDelegs (..))
import Control.Monad (join)
import Control.Monad.State.Strict (MonadState)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import qualified System.Random.Stateful as R
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

instance R.UniformRange EpochNo where
  uniformRM (EpochNo l, EpochNo h) g = EpochNo <$> R.uniformRM (l, h) g

genSize :: MonadGen m => m Int
genSize =
  frequency [(1, pure 0), (10, choose (1, 1000)), (89, getPositive <$> arbitrary)]

freshCredential ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) => m (Credential r)
freshCredential =
  frequency [(5, KeyHashObj <$> freshKeyHash)] -- (2, pickScriptHash)] TODO: implement script pool,
  -- where picking actually removes from the pool to enforce uniqueness

genAccountState ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) =>
  Set (KeyHash StakePool) -> m (ConwayAccountState era)
genAccountState ppIds = do
  casBalance <-
    CompactCoin
      <$> frequency [(10, pure 0), (10, uniformRM (0, 1_000_000_000_000)), (80, arbitrary)]
  casDeposit <- arbitrary
  casStakePoolDelegation <- frequency [(2, pure Nothing), (8, uniformSetElem ppIds)]
  casDRepDelegation <-
    frequency
      [ (3, pure Nothing)
      , (2, pure $ Just DRepAlwaysAbstain)
      , (1, pure $ Just DRepAlwaysNoConfidence)
      , (10, Just . DRepCredential <$> freshCredential)
      ]
  pure ConwayAccountState {..}

genAccounts ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  Set (KeyHash StakePool) ->
  m (ConwayAccounts era)
genAccounts ppIds = do
  n <- genSize
  ConwayAccounts . Map.fromList <$> replicateM n ((,) <$> freshCredential <*> genAccountState ppIds)

genDState ::
  ( Accounts era ~ ConwayAccounts era
  , HasKeyPairs s
  , MonadState s m
  , MonadGen m
  , HasStatefulGen g m
  ) =>
  Set (KeyHash StakePool) ->
  m (DState era)
genDState ppIds = do
  let
    dsFutureGenDelegs = Map.empty
    dsGenDelegs = GenDelegs Map.empty
    dsIRewards = InstantaneousRewards Map.empty Map.empty mempty mempty
  dsAccounts <- genAccounts ppIds
  pure DState {..}

genStakePoolParams ::
  ( MonadGen m
  , HasKeyPairs s
  , MonadState s m
  , HasStatefulGen g m
  , EraAccounts era
  ) =>
  Accounts era -> KeyHash StakePool -> m StakePoolParams
genStakePoolParams accounts sppId = do
  let accountsMap = accounts ^. accountsMapL
  sppVrf <- arbitrary
  -- TODO `freshVRFKeyHash` ^ that adds VRF private key to state and
  -- occasionally crfeates duplicates
  sppPledge <- Coin . getNonNegative <$> arbitrary
  sppCost <- Coin . getNonNegative <$> arbitrary
  sppMargin <- arbitrary
  sppAccountAddress <-
    AccountAddress Testnet . AccountId
      <$> frequency
        [ (1, freshCredential)
        , (100, join (maybe freshCredential (pure . fst) <$> uniformMapElem accountsMap))
        ]
  nOwners <- genSize
  sppOwners <-
    frequency
      [ (1, Set.singleton <$> freshKeyHash)
      ,
        ( 100
        , uniformSubSet
            (Just nOwners)
            (Set.fromList [kh | KeyHashObj kh <- Map.keys accountsMap])
        )
      ]
  sppRelays <- arbitrary
  sppMetadata <- arbitrary
  pure StakePoolParams {..}

genPState ::
  forall s g m era.
  (EraAccounts era, HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  Set (KeyHash StakePool) ->
  EpochNo ->
  DState era ->
  m (PState era)
genPState ppIds curEpochNo dState = do
  let accounts = dState ^. accountsL
      accountsMap = accounts ^. accountsMapL
      accountsStakePoolDelegations = Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap
      stakePoolsDelegations =
        Map.fromListWith (<>) $
          [ (pId, Set.singleton stakeCred)
          | (stakeCred, pId) <- Map.toList accountsStakePoolDelegations
          ]
      lookupStakePoolDelegations pId = Map.findWithDefault mempty pId stakePoolsDelegations
  stakePoolParams <-
    sequence $ Map.fromSet (genStakePoolParams accounts) ppIds
  stakePools <-
    Map.traverseWithKey
      (\pId sp -> mkStakePoolState <$> arbitrary <*> pure (lookupStakePoolDelegations pId) <*> pure sp)
      stakePoolParams
  futureStakePoolParams <-
    Map.fromElems sppId <$> listOf (freshKeyHash >>= genStakePoolParams accounts)
  let
    allStakePools = stakePoolParams <> futureStakePoolParams
    vrfKeyHashes =
      Map.map (`nonZeroOr` error "Impossible: negative VRF count") $
        Map.fromListWith (+) [(sppVrf spp, 1) | spp <- Map.elems allStakePools]
  retiringStakePools <-
    uniformSubMap Nothing stakePools
      >>= traverse (\_ -> uniformRM (curEpochNo, binOpEpochNo (+) curEpochNo (EpochNo 20)))
  pure $
    PState
      { psVRFKeyHashes = vrfKeyHashes
      , psStakePools = stakePools
      , psFutureStakePoolParams = futureStakePoolParams
      , psRetiring = retiringStakePools
      }

genVState ::
  (ConwayEraAccounts era, HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
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
        drepDeposit <- arbitrary
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
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) => m (CommitteeState era)
genCommitteeState = do
  n <- genSize
  CommitteeState . Map.fromList <$> vectorOf n ((,) <$> freshCredential <*> genCommitteeAuthorization)

genCommitteeAuthorization ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) => m CommitteeAuthorization
genCommitteeAuthorization =
  frequency
    [ (10, CommitteeHotCredential <$> freshCredential)
    , (1, CommitteeMemberResigned <$> arbitrary)
    ]

genCertState ::
  ( ConwayEraAccounts era
  , Accounts era ~ ConwayAccounts era
  , HasKeyPairs s
  , MonadState s m
  , MonadGen m
  , HasStatefulGen g m
  ) =>
  EpochNo -> m (ConwayCertState era)
genCertState curEpochNo = do
  ppIds <- arbitrary
  conwayCertDState <- genDState ppIds
  conwayCertPState <- genPState ppIds curEpochNo conwayCertDState
  conwayCertVState <- genVState curEpochNo conwayCertDState
  pure ConwayCertState {..}
