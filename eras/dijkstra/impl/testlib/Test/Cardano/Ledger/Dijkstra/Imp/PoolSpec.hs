{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.PoolSpec (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppMaxPledgeLeverageL)
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsCertStateL, nesEsL)
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure (..))
import Cardano.Ledger.State (
  EraCertState,
  StakePoolParams (..),
  casReservesL,
  certPStateL,
  chainAccountStateL,
  psFutureStakePoolParams,
  psStakePools,
  psVRFKeyHashes,
  sppVrfL,
  spsVrf,
 )
import Data.Coerce (coerce)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

-- | Slightly less than half of the total supply, leaving the rest in circulation.
reserves :: Coin
reserves = Coin 20_000_000_000_000_000

ownerStake :: Coin
ownerStake = Coin 10_000_000_000_000

delegatorStake :: Coin
delegatorStake = Coin 90_000_000_000_000

registerPoolWithPledge ::
  DijkstraEraImp era =>
  Coin ->
  ImpTestM era (KeyHash StakePool, [Credential Staking])
registerPoolWithPledge pledge = do
  poolId <- freshKeyHash
  ownerKeyHash <- freshKeyHash
  delegatorKeyHash <- freshKeyHash
  let owner = KeyHashObj ownerKeyHash
      delegator = KeyHashObj delegatorKeyHash
  -- Give the stake credentials some stake to delegate.
  ownerPayment <- freshKeyHash @Payment
  delegatorPayment <- freshKeyHash @Payment
  sendCoinTo_ (mkAddr ownerPayment owner) ownerStake
  sendCoinTo_ (mkAddr delegatorPayment delegator) delegatorStake
  -- The pool pays its rewards into the account of its owner.
  ownerAccountAddress <- registerStakeCredential owner
  _ <- registerStakeCredential delegator
  minPoolCost <- getsPParams ppMinPoolCostL
  registerPoolWithParams
    ( \poolParams ->
        poolParams
          { sppPledge = pledge
          , sppOwners = Set.singleton ownerKeyHash
          , sppCost = minPoolCost
          , sppMargin = 0 %! 1
          }
    )
    poolId
    ownerAccountAddress
  delegateStake owner poolId
  delegateStake delegator poolId
  pure (poolId, [owner, delegator])

-- | The total rewards that have been paid out to a stake pool and its delegators.
poolRewards :: (HasCallStack, EraCertState era) => [Credential Staking] -> ImpTestM era Coin
poolRewards = fmap fold . traverse getBalance

-- | Register two pools that are identical, except that the second one declares a pledge
-- that is a thousandth of the pledge of the first one, then have both of them mint the
-- same number of blocks, and report the rewards that each of them earned.
--
-- The first pool is well pledged: its pledge is a tenth of its stake, which is exactly
-- the leverage that `maxPledgeLeverage` is set to whenever it is set in this spec.
rewardsOfWellAndOverPledgedPools ::
  DijkstraEraImp era =>
  ImpTestM era (Coin, Coin)
rewardsOfWellAndOverPledgedPools = do
  -- ImpSpec starts out with the whole supply accounted for in the reserves, while at the
  -- same time holding all of it in the initial UTxO, which leaves nothing in circulation.
  -- Rewards are handed out of the reserves and are proportional to the stake of a pool
  -- relative to the ADA in circulation, so both need to be realistic for a pool to earn a
  -- sensible amount of rewards.
  modifyNES $ nesEsL . chainAccountStateL . casReservesL .~ reserves
  wellPledged <- registerPoolWithPledge ownerStake
  overLeveraged <- registerPoolWithPledge $ Coin (unCoin ownerStake `div` 1_000)
  -- Pay out the pledges and delegations, then let the stake distribution settle into the
  -- snapshot that the rewards for the epoch after the next one are computed from.
  passNEpochs 2
  -- Both pools mint the same number of blocks, so that they have the same apparent
  -- performance. The transactions also fill up the fee pot that is handed out as rewards.
  replicateM_ 3 $
    forM_ [fst wellPledged, fst overLeveraged] $ \poolId ->
      withIssuerAndTxsInBlock_ (coerce poolId) $ do
        addr <- freshKeyAddr_
        sendCoinTo_ addr $ Coin 1_000_000_000
  -- Rewards for an epoch are only handed out two epoch boundaries later.
  passNEpochs 3
  (,) <$> poolRewards (snd wellPledged) <*> poolRewards (snd overLeveraged)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "POOL" $ do
  describe "Register and re-register pools" $ do
    it "re-register a pool with its own future VRF" $ do
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      tx <- registerPoolTx <$> poolParams kh vrfNew
      submitTx_ tx
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      -- re-registering with the VRF already recorded in the pool's own
      -- future params should succeed
      submitTx_ tx
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      expectVRFs [vrf, vrfNew]
      passEpoch
      expectPool kh (Just vrfNew)
      expectFuturePool kh Nothing
      expectVRFs [vrfNew]

    it "keep tracking the active VRF after re-registering with it and then with a fresh one" $ do
      (kh, vrf) <- registerNewPool
      -- re-register with the pool's own active VRF ...
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectFuturePool kh (Just vrf)
      expectVRFs [vrf]
      -- ... and then with a fresh one
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      -- the pool keeps producing blocks with the original VRF until the
      -- epoch boundary, so it must still be tracked
      expectPool kh (Just vrf)
      expectVRFs [vrf, vrfNew]
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= \tx ->
        submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered khNew vrf)
      passEpoch
      expectPool kh (Just vrfNew)
      expectVRFs [vrfNew]
      -- after the epoch boundary the original VRF can be taken over
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
      expectVRFs [vrf, vrfNew]

    it "re-registering with the active VRF releases the pending future VRF" $ do
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      expectVRFs [vrf, vrfNew]
      -- going back to the active VRF frees the previously requested one
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectFuturePool kh (Just vrf)
      expectVRFs [vrf]
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrfNew >>= submitTx_
      expectPool khNew (Just vrfNew)
      expectVRFs [vrf, vrfNew]

  describe "maxPledgeLeverage" $ do
    -- The pledge influence factor also rewards a pool for pledging more, which would
    -- make the two pools below earn different rewards for a reason that has nothing to
    -- do with the pledge leverage. Setting it to zero isolates the leverage cap.
    let withoutPledgeInfluence = modifyPParams $ \pp -> pp & ppA0L .~ 0 %! 1

    it "is not enforced when it is not set" $ do
      withoutPledgeInfluence
      (wellPledgedRewards, overLeveragedRewards) <- rewardsOfWellAndOverPledgedPools
      wellPledgedRewards `shouldSatisfy` (> Coin 0)
      overLeveragedRewards `shouldBe` wellPledgedRewards

    it "lowers the rewards of a pool that is leveraged beyond it" $ do
      withoutPledgeInfluence
      modifyPParams $ \pp ->
        pp & ppMaxPledgeLeverageL .~ MaxPledgeLeverage (SJust (10 %! 1))
      (wellPledgedRewards, overLeveragedRewards) <- rewardsOfWellAndOverPledgedPools
      -- The leverage of the well pledged pool is exactly the maximum, so it is rewarded
      -- for all of its stake, just like it would have been without the cap.
      wellPledgedRewards `shouldSatisfy` (> Coin 0)
      -- The over-leveraged pool is only rewarded for ten times its pledge, which is a
      -- thousandth of the stake it actually has, so it earns roughly a thousandth of what
      -- the well pledged pool earns. It is not cut off from the rewards entirely.
      overLeveragedRewards `shouldSatisfy` (> Coin 0)
      Coin (100 * unCoin overLeveragedRewards) `shouldSatisfy` (< wellPledgedRewards)
  where
    registerNewPool = do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      submitTx_ . registerPoolTx =<< poolParams kh vrf
      expectPool kh (Just vrf)
      pure (kh, vrf)
    registerPoolTx pps =
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert pps)
    expectPool poolKh mbVrf = do
      pools <- psStakePools <$> getPState
      spsVrf <$> Map.lookup poolKh pools `shouldBe` mbVrf
    expectFuturePool poolKh mbVrf = do
      fps <- psFutureStakePoolParams <$> getPState
      sppVrf <$> Map.lookup poolKh fps `shouldBe` mbVrf
    expectVRFs vrfs =
      Map.keysSet . psVRFKeyHashes <$> getPState `shouldReturn` Set.fromList vrfs
    poolParams ::
      KeyHash StakePool ->
      VRFVerKeyHash StakePoolVRF ->
      ImpTestM era (StakePoolParams era)
    poolParams kh vrf = do
      pps <- registerAccountAddress >>= freshPoolParams kh
      pure $ pps & sppVrfL .~ vrf
    getPState = getsNES @era $ nesEsL . esLStateL . lsCertStateL . certPStateL
