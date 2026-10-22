{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.PoolSpec (spec, dijkstraOnlySpec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppMaxPledgeLeverageL, ppMinPoolMarginL)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Keys (asWitness, coerceKeyRole, witVKeyHash)
import Cardano.Ledger.Plutus.CostModels (emptyCostModels)
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Data.Coerce (coerce)
import Data.Foldable (fold, for_, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Data.Word
import Lens.Micro ((%~), (&), (.~), (^.))
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
  ImpTestM era (KeyHash StakePool, (Credential Staking, Credential Staking))
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
  pure (poolId, (owner, delegator))

-- | The total rewards that have been paid out to a stake pool and its delegator.
poolRewards ::
  (HasCallStack, EraCertState era) => (Credential Staking, Credential Staking) -> ImpTestM era Coin
poolRewards (o, d) = fold <$> traverse @[] getBalance [o, d]

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
    forM_ ([fst wellPledged, fst overLeveraged] :: [KeyHash StakePool]) $ \poolId ->
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
      pps <- poolParams kh vrfNew
      submitAnyTx_ $ registerPoolTx pps
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      -- re-registering with the VRF already recorded in the pool's own
      -- future params should succeed
      submitAnyTx_ $ registerPoolTx pps
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      passEpoch
      expectPool kh (Just vrfNew)
      expectFuturePool kh Nothing
      expectVRFs [(vrfNew, 1)]

    it "keep tracking the active VRF after re-registering with it and then with a fresh one" $ do
      (kh, vrf) <- registerNewPool
      -- re-register with the pool's own active VRF ...
      poolParams kh vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectFuturePool kh (Just vrf)
      expectVRFs [(vrf, 1)]
      -- ... and then with a fresh one
      vrfNew <- freshKeyHashVRF
      poolParams kh vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      -- the pool keeps producing blocks with the original VRF until the
      -- epoch boundary, so it must still be tracked
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= \tx ->
        submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered khNew vrf)
      passEpoch
      expectPool kh (Just vrfNew)
      expectVRFs [(vrfNew, 1)]
      -- after the epoch boundary the original VRF can be taken over ...
      poolParams khNew vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectPool khNew (Just vrf)
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      -- ... but only by a single pool
      kh3 <- freshKeyHash
      registerPoolTx <$> poolParams kh3 vrf >>= \tx ->
        submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered kh3 vrf)

    it "a pending future VRF cannot be claimed by another pool" $ do
      (kh1, vrf1) <- registerNewPool
      (kh2, vrf2) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      poolParams kh1 vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectPool kh1 (Just vrf1)
      expectFuturePool kh1 (Just vrfNew)
      expectVRFs [(vrf1, 1), (vrf2, 1), (vrfNew, 1)]
      -- a VRF is taken as soon as a re-registration requests it, so neither a new pool ...
      kh3 <- freshKeyHash
      registerPoolTx <$> poolParams kh3 vrfNew >>= \tx ->
        submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered kh3 vrfNew)
      -- ... nor another registered pool may claim it
      registerPoolTx <$> poolParams kh2 vrfNew >>= \tx ->
        submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered kh2 vrfNew)
      expectFuturePool kh2 Nothing
      expectVRFs [(vrf1, 1), (vrf2, 1), (vrfNew, 1)]

    it "re-registering with the active VRF releases the pending future VRF" $ do
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      poolParams kh vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      -- going back to the active VRF frees the previously requested one
      poolParams kh vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectFuturePool kh (Just vrf)
      expectVRFs [(vrf, 1)]
      khNew <- freshKeyHash
      poolParams khNew vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectPool khNew (Just vrfNew)
      expectVRFs [(vrf, 1), (vrfNew, 1)]

    it "oscillating between the active and a fresh VRF within an epoch keeps the active one taken" $ do
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      -- switch to a fresh VRF, back to the active one and to the fresh one again
      poolParams kh vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      poolParams kh vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectVRFs [(vrf, 1)]
      poolParams kh vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      -- the active VRF stays in use until the epoch boundary, so it must stay taken
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= \tx ->
        submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered khNew vrf)
      passEpoch
      expectPool kh (Just vrfNew)
      expectVRFs [(vrfNew, 1)]
      poolParams khNew vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectVRFs [(vrf, 1), (vrfNew, 1)]

    describe "a VRF shared by two pools" $ do
      -- GHC 9.14 requires the type signatures of these helpers: without them, type checking
      -- this module does not finish and the build times out.
      let registerTwoPoolsSharingVRF ::
            ImpTestM era (KeyHash StakePool, KeyHash StakePool, VRFVerKeyHash StakePoolVRF)
          registerTwoPoolsSharingVRF = do
            (kh1, vrf) <- registerNewPool
            kh2 <- registerPoolSharingVRF vrf
            expectVRFs [(vrf, 2)]
            pure (kh1, kh2, vrf)
          switchToFreshVRF :: KeyHash StakePool -> ImpTestM era (VRFVerKeyHash StakePoolVRF)
          switchToFreshVRF kh = do
            vrfNew <- freshKeyHashVRF
            poolParams kh vrfNew >>= \pps -> submitAnyTx_ $ registerPoolTx pps
            pure vrfNew
          expectTaken :: VRFVerKeyHash StakePoolVRF -> ImpTestM era ()
          expectTaken vrf = do
            kh <- freshKeyHash
            registerPoolTx <$> poolParams kh vrf >>= \tx ->
              submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered kh vrf)
          -- The shared VRF has been released: only the given counts are left, and another
          -- pool can register with it.
          expectReleased ::
            VRFVerKeyHash StakePoolVRF -> [(VRFVerKeyHash StakePoolVRF, Word64)] -> ImpTestM era ()
          expectReleased vrf vrfs = do
            expectVRFs vrfs
            kh <- freshKeyHash
            poolParams kh vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
            expectPool kh (Just vrf)
            expectVRFs $ (vrf, 1) : vrfs

      it "cannot be kept by a holder that re-registers" $ do
        (kh1, _, vrf) <- registerTwoPoolsSharingVRF
        -- neither pool may keep the shared VRF when re-registering ...
        registerPoolTx <$> poolParams kh1 vrf >>= \tx ->
          submitFailingTx tx (pure . injectFailure $ VRFKeyHashAlreadyRegistered kh1 vrf)
        -- ... but either may switch to a fresh one
        vrfNew <- switchToFreshVRF kh1
        expectFuturePool kh1 (Just vrfNew)
        expectVRFs [(vrf, 2), (vrfNew, 1)]
        -- and the shared VRF stays taken while any pool still uses it
        expectTaken vrf

      it "stays taken across the epoch boundary when one holder moves away" $ do
        -- When one holder switches to a fresh VRF, the other pool's reference to the
        -- shared VRF must survive the epoch boundary.
        (kh1, kh2, vrf) <- registerTwoPoolsSharingVRF
        vrfNew1 <- switchToFreshVRF kh1
        expectVRFs [(vrf, 2), (vrfNew1, 1)]
        passEpoch
        expectPool kh1 (Just vrfNew1)
        expectVRFs [(vrf, 1), (vrfNew1, 1)]
        -- the VRF is still in use by the other pool, so it cannot be claimed
        expectTaken vrf
        -- it only becomes available once the last holder moves away as well
        vrfNew2 <- switchToFreshVRF kh2
        passEpoch
        expectReleased vrf [(vrfNew1, 1), (vrfNew2, 1)]

      it "is released once both holders move away in the same epoch" $ do
        (kh1, kh2, vrf) <- registerTwoPoolsSharingVRF
        vrfNew1 <- switchToFreshVRF kh1
        vrfNew2 <- switchToFreshVRF kh2
        expectVRFs [(vrf, 2), (vrfNew1, 1), (vrfNew2, 1)]
        passEpoch
        expectPool kh1 (Just vrfNew1)
        expectPool kh2 (Just vrfNew2)
        -- no pool holds the shared VRF any more, so it is up for grabs again
        expectReleased vrf [(vrfNew1, 1), (vrfNew2, 1)]

      it "is released once both holders have retired" $ do
        (kh1, kh2, vrf) <- registerTwoPoolsSharingVRF
        -- retiring one of the two pools leaves the VRF in use by the other one ...
        AnyLevelTx retireTx1 <- retirePoolTx kh1 (EpochInterval 1)
        submitAnyTx_ retireTx1
        passEpoch
        expectPool kh1 Nothing
        expectPool kh2 (Just vrf)
        expectVRFs [(vrf, 1)]
        expectTaken vrf
        -- ... and only once that one has retired as well does the VRF become available
        AnyLevelTx retireTx2 <- retirePoolTx kh2 (EpochInterval 1)
        submitAnyTx_ retireTx2
        passEpoch
        expectPool kh2 Nothing
        expectReleased vrf []

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

  describe "BLS PoolReg" $ do
    let
      getPools = getsNES $ nesEsL . epochStateStakePoolsL

    it "registers a pool with a valid BLS key and proof of possession" $ do
      ownerBlsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust ownerBlsKey)
      epochNo <- getsNES nesELL
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState ownerBlsKey epochNo))

    it "fails to re-register an existing pool with an invalid BLS proof of possession" $ do
      ownerBlsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust ownerBlsKey)
      pStateBefore <- getPools
      invalidOwnerBlsKey <- BlsKey <$> arbitrary <*> arbitrary
      submitFailingTx
        (registerPoolTx pps {sppBlsKey = SJust invalidOwnerBlsKey})
        [injectFailure $ BlsKeyInvalidProofOfPossession (sppId pps) invalidOwnerBlsKey]
      passEpoch
      getPools `shouldReturn` pStateBefore

    it "fails to register a new pool with an invalid BLS proof of possession" $ do
      pps <- freshStakePool
      invalidOwnerBlsKey <- BlsKey <$> arbitrary <*> arbitrary
      submitFailingTx
        (registerPoolTx pps {sppBlsKey = SJust invalidOwnerBlsKey})
        [injectFailure $ BlsKeyInvalidProofOfPossession (sppId pps) invalidOwnerBlsKey]
      pools <- getPools
      expectNothing $ Map.lookup (sppId pps) pools

    it "re-registering with the same and different BLS key" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      registeredIn <- getsNES nesELL
      passEpoch
      submitAnyTx_ $ registerPoolTx pps
      passEpoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))
      newBlsKey <- freshBlsKey
      renewalSubmittedIn <- getsNES nesELL
      submitAnyTx_ $ registerPoolTx pps {sppBlsKey = SJust newBlsKey}
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))
      passEpoch
      -- dated one epoch after the re-registration was submitted,
      -- because re-registration is applied by POOLREAP on entering the next epoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState newBlsKey (succ renewalSubmittedIn)))

    it "re-registering with the same BLS key within the same epoch" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      submitAnyTx_ $ registerPoolTx pps
      registeredIn <- getsNES nesELL
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))
      passEpoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))

    it "re-registering with a new BLS key within the same epoch" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      newBlsKey <- freshBlsKey
      submitAnyTx_ $ registerPoolTx pps {sppBlsKey = SJust newBlsKey}
      registeredIn <- getsNES nesELL
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))
      passEpoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState newBlsKey (succ registeredIn)))

    it "changing other pool parameters does not renew the BLS key" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      registeredIn <- getsNES nesELL
      passEpoch
      minPoolCost <- getsPParams ppMinPoolCostL
      submitAnyTx_ $ registerPoolTx pps {sppCost = minPoolCost <> Coin 1}
      passEpoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))

    it "dropping the BLS key clears it" $ do
      pps <- registerPoolWithBlsKey . SJust =<< freshBlsKey
      passEpoch
      submitAnyTx_ $ registerPoolTx pps {sppBlsKey = SNothing}
      passEpoch
      expectBlsKeyState (sppId pps) SNothing

    it "dropping and re-registering the BLS key within one epoch does not renew it" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      registeredIn <- getsNES nesELL
      passEpoch
      submitAnyTx_ $ registerPoolTx pps {sppBlsKey = SNothing}
      submitAnyTx_ $ registerPoolTx pps
      passEpoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))

    it "re-registering a dropped BLS key starts a fresh lifetime" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      passEpoch
      submitAnyTx_ $ registerPoolTx pps {sppBlsKey = SNothing}
      passEpoch
      expectBlsKeyState (sppId pps) SNothing
      reRegisteredIn <- getsNES nesELL
      submitAnyTx_ $ registerPoolTx pps
      expectBlsKeyState (sppId pps) SNothing
      passEpoch
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey (succ reRegisteredIn)))

    it "un-retiring a pool by re-registration does not renew its BLS key" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      registeredIn <- getsNES nesELL
      AnyLevelTx tx <- retirePoolTx (sppId pps) (EpochInterval 2)
      submitAnyTx_ tx
      submitAnyTx_ $ registerPoolTx pps
      passNEpochs 3
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredIn))

    it "a reaped pool that registers again starts a fresh lifetime" $ do
      blsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust blsKey)
      AnyLevelTx tx <- retirePoolTx (sppId pps) (EpochInterval 1)
      submitAnyTx_ tx
      passNEpochs 2
      expectPool (sppId pps) Nothing
      registeredAgainIn <- getsNES nesELL
      submitAnyTx_ $ registerPoolTx pps
      expectBlsKeyState (sppId pps) (SJust (BlsKeyState blsKey registeredAgainIn))

  describe "BLS key uniqueness" $ do
    it "a new pool cannot take the active key of another pool" $ do
      key <- freshBlsKey
      _ <- registerBlsPool (SJust key)
      expectBlsKeys [key]
      expectBlsKeyTakenByNewPool key
      -- the key stays taken across the epoch boundary
      passEpoch
      expectBlsKeys [key]
      expectBlsKeyTakenByNewPool key

    it "a registered pool cannot take the active key of another pool" $ do
      key1 <- freshBlsKey
      key2 <- freshBlsKey
      _ <- registerBlsPool (SJust key1)
      withKey <- registerBlsPool (SJust key2)
      withoutKey <- registerBlsPool SNothing
      expectBlsKeys [key1, key2]
      -- neither a pool with a key of its own nor one without any may take it
      expectBlsKeyTaken withKey key1
      expectBlsKeyTaken withoutKey key1
      expectBlsKeys [key1, key2]

    it "a pending future key cannot be claimed by another pool" $ do
      key1 <- freshBlsKey
      kh1 <- registerBlsPool (SJust key1)
      kh2 <- registerBlsPool SNothing
      newKey <- freshBlsKey
      reregisterBlsPool kh1 (SJust newKey)
      expectActiveBlsKey kh1 (SJust key1)
      expectFutureBlsKey kh1 (SJust newKey)
      expectBlsKeys [key1, newKey]
      -- a key is taken as soon as a re-registration requests it, so neither a new pool ...
      expectBlsKeyTakenByNewPool newKey
      -- ... nor another registered pool may claim it
      expectBlsKeyTaken kh2 newKey
      expectBlsKeys [key1, newKey]

    it "a pool can re-register with its own active key" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      reregisterBlsPool kh (SJust key)
      expectFutureBlsKey kh (SJust key)
      expectBlsKeys [key]
      passEpoch
      expectActiveBlsKey kh (SJust key)
      expectBlsKeys [key]

    it "a pool can re-register with its own pending future key" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      -- registering with the key in the pool's own future params should succeed
      reregisterBlsPool kh (SJust newKey)
      expectActiveBlsKey kh (SJust key)
      expectFutureBlsKey kh (SJust newKey)
      expectBlsKeys [key, newKey]
      passEpoch
      expectActiveBlsKey kh (SJust newKey)
      expectBlsKeys [newKey]

    it "a pending future key is released as soon as the pool replaces it" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      futureKey <- freshBlsKey
      reregisterBlsPool kh (SJust futureKey)
      newFutureKey <- freshBlsKey
      reregisterBlsPool kh (SJust newFutureKey)
      expectFutureBlsKey kh (SJust newFutureKey)
      expectBlsKeys [key, newFutureKey]
      -- the replaced key is free right away, while the active one stays taken
      other <- registerBlsPool (SJust futureKey)
      expectActiveBlsKey other (SJust futureKey)
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectActiveBlsKey kh (SJust newFutureKey)
      expectBlsKeys [futureKey, newFutureKey]

    it "the active key stays taken when the future parameters move away from it" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      reregisterBlsPool kh (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      -- the future parameters no longer use the key, but the active ones still do
      expectBlsKeys [key, newKey]
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectActiveBlsKey kh (SJust newKey)
      expectBlsKeys [newKey]

    it "a key is released when its holder switches to a fresh one" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      -- the pool keeps using the key until the epoch boundary, so it stays taken
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectActiveBlsKey kh (SJust newKey)
      expectBlsKeys [newKey]
      -- now another pool can take it over ...
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [key, newKey]
      -- ... but only a single one
      expectBlsKeyTakenByNewPool key

    it "a key is released when its holder drops it" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      reregisterBlsPool kh SNothing
      expectActiveBlsKey kh (SJust key)
      expectBlsKeys [key]
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectActiveBlsKey kh SNothing
      expectBlsKeys []
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [key]

    it "a key is released when its holder retires" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      AnyLevelTx tx <- retirePoolTx kh (EpochInterval 1)
      submitAnyTx_ tx
      expectBlsKeys [key]
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectPool kh Nothing
      expectBlsKeys []
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [key]

    it "retiring with a pending future key releases both keys" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      AnyLevelTx tx <- retirePoolTx kh (EpochInterval 1)
      submitAnyTx_ tx
      expectBlsKeys [key, newKey]
      passEpoch
      expectPool kh Nothing
      expectBlsKeys []
      _ <- registerBlsPool (SJust key)
      _ <- registerBlsPool (SJust newKey)
      expectBlsKeys [key, newKey]

    it "only the public key counts, not the proof of possession" $ do
      key <- freshBlsKey
      _ <- registerBlsPool (SJust key)
      bogusProof <- arbitrary
      let bogusKey = key {blsPossessionProof = bogusProof}
      kh <- freshKeyHash
      pps <- blsPoolParams kh (SJust bogusKey)
      -- the key is rejected twice: its public key is taken and its proof is not valid
      submitFailingTx
        (registerPoolTx pps)
        [ injectFailure $ BlsKeyInvalidProofOfPossession kh bogusKey
        , injectFailure $ BlsKeyAlreadyRegistered kh bogusKey
        ]
      expectBlsKeys [key]

  describe "CIP-23" $ do
    for_ @[] [minBound, maxBound] $ \margin -> do
      it ("minimum pool margin (" <> show margin <> ")") $ do
        (poolId, (poolOwner, poolDelegator)) <- registerPoolWithPledge $ Coin 0

        pools <- psStakePools <$> getPState
        poolState <- expectJust $ Map.lookup poolId pools
        poolState ^. spsMarginL `shouldBe` minBound

        modifyPParams $ ppMinPoolMarginL .~ margin
        pParams <- getsNES $ nesEsL . curPParamsEpochStateL
        logToExpr $ pParams & ppCostModelsL .~ emptyCostModels

        passEpoch
        withIssuerAndTxsInBlock_ (coerceKeyRole poolId) $
          pure ()
        passEpoch
        replicateM_ 100 $
          withIssuerAndTxsInBlock_ (coerceKeyRole poolId) $
            pure ()
        passNEpochs 2

        certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
        logToExpr certState

        let accountsMap = certState ^. certDStateL . accountsG . accountsMapL
        ownerState <- expectJust $ Map.lookup poolOwner accountsMap
        delegatorState <- expectJust $ Map.lookup poolDelegator accountsMap

        -- The owner always gets some rewards
        let ownerBalance = fromCompact $ ownerState ^. balanceAccountStateL
        ownerBalance `shouldNotBe` Coin 0

        -- Unless the margin is 1, the delegator should get a share of the
        -- remainder rewards that are left after the cost is subtracted.
        -- When the margin is 1, the delegator gets nothing because the pool
        -- takes everything, both the cost and the remainder.
        let delegatorBalance = fromCompact $ delegatorState ^. balanceAccountStateL
        if margin /= maxBound
          then delegatorBalance `shouldNotBe` Coin 0
          else delegatorBalance `shouldBe` Coin 0

  describe "Re-registrations that only change the BLS key" $ do
    -- TODO: Re-enable these in conformance once the spec is up to date
    describe "Owner witnesses are not needed" $ do
      disableInConformanceIt "to rotate the BLS key" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        newBlsKey <- freshBlsKey
        let rotated = pps {sppBlsKey = SJust newBlsKey}
        expectNotAllOwnerWits pps =<< submitAnyTx (registerPoolTx rotated)
        expectFutureParams rotated
        passEpoch
        expectActiveBlsKey (sppId pps) (SJust newBlsKey)

      disableInConformanceIt "to add a BLS key" $ do
        pps <- registerPoolWithOwners SNothing
        newBlsKey <- freshBlsKey
        let rotated = pps {sppBlsKey = SJust newBlsKey}
        expectNotAllOwnerWits pps =<< submitAnyTx (registerPoolTx rotated)
        expectFutureParams rotated
        passEpoch
        expectActiveBlsKey (sppId pps) (SJust newBlsKey)

      disableInConformanceIt "to drop the BLS key" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        let rotated = pps {sppBlsKey = SNothing}
        expectNotAllOwnerWits pps =<< submitAnyTx (registerPoolTx rotated)
        expectFutureParams rotated
        passEpoch
        expectActiveBlsKey (sppId pps) SNothing

      disableInConformanceIt
        "to cancel a retirement by re-registering with the same parameters"
        $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          AnyLevelTx tx <- retirePoolTx (sppId pps) (EpochInterval 2)
          submitAnyTx_ tx
          expectRetiring (sppId pps) True
          expectNotAllOwnerWits pps =<< submitAnyTx (registerPoolTx pps)
          -- re-registration cancels the retirement with the cold key alone
          expectRetiring (sppId pps) False
          expectFutureParams pps

      disableInConformanceIt "to rotate the BLS key on top of a pending update" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        let updated = pps {sppCost = sppCost pps <> Coin 1}
        submitTx_ $ registerPoolTx updated
        newBlsKey <- freshBlsKey
        -- note: the bls key is updated on top of the pending update, and only
        -- the cold key is needed
        let rotated = updated {sppBlsKey = SJust newBlsKey}
        expectNotAllOwnerWits pps =<< submitAnyTx (registerPoolTx rotated)
        expectFutureParams rotated
        passEpoch
        expectActiveBlsKey (sppId pps) (SJust newBlsKey)

      disableInConformanceIt "to rotate the BLS key in a sub-transaction" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        newBlsKey <- freshBlsKey
        let rotated = pps {sppBlsKey = SJust newBlsKey}
        tx <- submitTx $ mkTopTxWithSubTxs [registerPoolTx rotated]
        ownerWitsByLevel pps tx `shouldBe` (False, [False])
        expectFutureParams rotated
        passEpoch
        expectActiveBlsKey (sppId pps) (SJust newBlsKey)

      disableInConformanceIt
        "to rotate the BLS key on top of an update made by a sub-transaction"
        $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          newBlsKey <- freshBlsKey
          let updated = pps {sppCost = sppCost pps <> Coin 1}
              rotated = updated {sppBlsKey = SJust newBlsKey}
          -- the sub-transaction needs owners, but the top one doesn't
          tx <-
            submitTx $
              mkTopTxWithSubTxs [registerPoolTx updated]
                & bodyTxL . certsTxBodyL .~ [RegPoolTxCert rotated]
          ownerWitsByLevel pps tx `shouldBe` (False, [True])
          expectFutureParams rotated
          passEpoch
          expectActiveBlsKey (sppId pps) (SJust newBlsKey)

      disableInConformanceIt
        "to rotate the BLS key on top of an update made by a preceding sub-transaction"
        $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          newBlsKey <- freshBlsKey
          let updated = pps {sppCost = sppCost pps <> Coin 1}
              rotated = updated {sppBlsKey = SJust newBlsKey}
          tx <- submitTx $ mkTopTxWithSubTxs [registerPoolTx updated, registerPoolTx rotated]
          ownerWitsByLevel pps tx `shouldBe` (False, [True, False])
          expectFutureParams rotated

      -- None of the certificates are applied in a phase-2 invalid transaction,
      -- so the owners don't need to witness it, whatever it changes
      disableInConformanceIt
        "to rotate the BLS key on top of an update made by a sub-transaction, in a phase-2 invalid transaction"
        $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          newBlsKey <- freshBlsKey
          let updated = pps {sppCost = sppCost pps <> Coin 1}
              rotated = updated {sppBlsKey = SJust newBlsKey}
          tx <-
            submitPhase2Invalid
              =<< switchTxToPhase2InvalidLegacyMode
                ( mkTopTxWithSubTxs [registerPoolTx updated]
                    & bodyTxL . certsTxBodyL .~ [RegPoolTxCert rotated]
                )
          ownerWitsByLevel pps tx `shouldBe` (False, [True, False])
          expectFuturePool (sppId pps) Nothing

      disableInConformanceIt
        "to rotate the BLS key on top of an update made by a preceding sub-transaction, in a phase-2 invalid transaction"
        $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          newBlsKey <- freshBlsKey
          let updated = pps {sppCost = sppCost pps <> Coin 1}
              rotated = updated {sppBlsKey = SJust newBlsKey}
          tx <-
            submitPhase2Invalid
              =<< switchTxToPhase2InvalidLegacyMode
                (mkTopTxWithSubTxs [registerPoolTx updated, registerPoolTx rotated])
          ownerWitsByLevel pps tx `shouldBe` (False, [True, False, False])
          expectFuturePool (sppId pps) Nothing

    describe "Owner witnesses are needed" $ do
      describe "to rotate the BLS key and change" $ do
        let changes :: [(String, StakePoolParams era -> ImpTestM era (StakePoolParams era))]
            changes =
              [ ("the VRF", \pps -> (\vrf -> pps {sppVrf = vrf}) <$> freshKeyHashVRF)
              , ("the pledge", \pps -> pure pps {sppPledge = sppPledge pps <> Coin 1})
              , ("the cost", \pps -> pure pps {sppCost = sppCost pps <> Coin 1})
              , ("the margin", \pps -> pure pps {sppMargin = 1 %! 2})
              ,
                ( "the reward account"
                , \pps -> (\accountAddress -> pps {sppAccountAddress = accountAddress}) <$> registerAccountAddress
                )
              ,
                ( "the owners"
                , \pps -> (\owner -> pps {sppOwners = Set.insert owner (sppOwners pps)}) <$> freshKeyHash
                )
              , ("the relays", \pps -> (\relay -> pps {sppRelays = sppRelays pps SSeq.|> relay}) <$> arbitrary)
              ,
                ( "the metadata"
                , \pps -> (\url -> pps {sppMetadata = SJust (PoolMetadata url mempty)}) <$> arbitrary
                )
              ]
        forM_ changes $ \(field, change) ->
          it field $ do
            pps <- registerPoolWithOwners . SJust =<< freshBlsKey
            newBlsKey <- freshBlsKey
            changed <- change pps
            expectOwnerWitsNeeded changed $ registerPoolTx changed {sppBlsKey = SJust newBlsKey}

      it "to rotate the BLS key in one certificate and change the cost in another" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        newBlsKey <- freshBlsKey
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [ RegPoolTxCert pps {sppBlsKey = SJust newBlsKey}
                     , RegPoolTxCert pps {sppCost = sppCost pps <> Coin 1}
                     ]
        expectOwnerWitsNeeded pps tx
        submitTx_ tx
        expectFutureParams pps {sppCost = sppCost pps <> Coin 1}

      disableInConformanceIt
        "to re-register with the active parameters while an update is pending"
        $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          -- the owners sign an update of the cost, which becomes pending
          submitTx_ $ registerPoolTx pps {sppCost = sppCost pps <> Coin 1}
          -- going back to the active parameters throws that update away, so the cold key
          -- alone can't do it
          expectOwnerWitsNeeded pps $ registerPoolTx pps

    -- Note: re-registration that matches the active parameters except for the BLS key,
    -- while a pending update changes something else, will discard that update.
    -- The POOL rule rejects it, even when all the owners witness it.
    describe "Discarding a pending update" $ do
      describe "is rejected, even when the owners witness it" $ do
        disableInConformanceIt "after a pending update" $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          let updated = pps {sppCost = sppCost pps <> Coin 1}
          submitTx_ $ registerPoolTx updated
          newBlsKey <- freshBlsKey
          submitFailingTxM (registerPoolTx pps {sppBlsKey = SJust newBlsKey}) $ \tx -> do
            ownerWitsByLevel pps tx `shouldBe` (True, [])
            pure [discardsPendingUpdateFailure pps]
          expectFutureParams updated

        disableInConformanceIt "after an update made by a sub-transaction" $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          newBlsKey <- freshBlsKey
          submitFailingTxM
            ( mkTopTxWithSubTxs [registerPoolTx pps {sppCost = sppCost pps <> Coin 1}]
                & bodyTxL . certsTxBodyL .~ [RegPoolTxCert pps {sppBlsKey = SJust newBlsKey}]
            )
            $ \tx -> do
              ownerWitsByLevel pps tx `shouldBe` (True, [True])
              pure [discardsPendingUpdateFailure pps]
          expectFuturePool (sppId pps) Nothing

        disableInConformanceIt "after an update made by a preceding sub-transaction" $ do
          pps <- registerPoolWithOwners . SJust =<< freshBlsKey
          newBlsKey <- freshBlsKey
          let rotated = pps {sppBlsKey = SJust newBlsKey}
          submitFailingTxM
            ( mkTopTxWithSubTxs
                [registerPoolTx pps {sppCost = sppCost pps <> Coin 1}, registerPoolTx rotated]
            )
            $ \tx -> do
              ownerWitsByLevel pps tx `shouldBe` (False, [True, True])
              pure [subTxDiscardsPendingUpdateFailure pps]
          expectFuturePool (sppId pps) Nothing

        disableInConformanceIt
          "after an update made by an earlier certificate in the same transaction"
          $ do
            pps <- registerPoolWithOwners . SJust =<< freshBlsKey
            newBlsKey <- freshBlsKey
            submitFailingTxM
              ( mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL
                    .~ [ RegPoolTxCert pps {sppCost = sppCost pps <> Coin 1}
                       , RegPoolTxCert pps {sppBlsKey = SJust newBlsKey}
                       ]
              )
              $ \tx -> do
                ownerWitsByLevel pps tx `shouldBe` (True, [])
                pure [discardsPendingUpdateFailure pps]
            expectFuturePool (sppId pps) Nothing

      disableInConformanceIt "is not rejected when the pending update only changes the BLS key" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        newBlsKey <- freshBlsKey
        submitTx_ $ registerPoolTx pps {sppBlsKey = SJust newBlsKey}
        otherBlsKey <- freshBlsKey
        let rotated = pps {sppBlsKey = SJust otherBlsKey}
        expectNotAllOwnerWits pps =<< submitAnyTx (registerPoolTx rotated)
        expectFutureParams rotated

      disableInConformanceIt "is accepted when the owners revert the pending update first" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        submitTx_ $ registerPoolTx pps {sppCost = sppCost pps <> Coin 1}
        newBlsKey <- freshBlsKey
        let rotated = pps {sppBlsKey = SJust newBlsKey}
        -- the first certificate brings the pending parameters back to the active ones, so
        -- the second one has no update to discard
        tx <-
          submitTx $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert pps, RegPoolTxCert rotated]
        ownerWitsByLevel pps tx `shouldBe` (True, [])
        expectFutureParams rotated

      -- Certificates aren't applied in a phase-2 invalid transaction, so POOL never checks them
      disableInConformanceIt "is not checked in a phase-2 invalid transaction" $ do
        pps <- registerPoolWithOwners . SJust =<< freshBlsKey
        let updated = pps {sppCost = sppCost pps <> Coin 1}
        submitTx_ $ registerPoolTx updated
        newBlsKey <- freshBlsKey
        submitPhase2Invalid_
          =<< switchTxToPhase2InvalidLegacyMode
            ( mkTopTxWithSubTxs [registerPoolTx pps]
                & bodyTxL . certsTxBodyL .~ [RegPoolTxCert pps {sppBlsKey = SJust newBlsKey}]
            )
        expectFutureParams updated
  where
    registerNewPool = do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      poolParams kh vrf >>= \pps -> submitAnyTx_ $ registerPoolTx pps
      expectPool kh (Just vrf)
      pure (kh, vrf)
    registerPoolTx pps =
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert pps)
    -- Two pools can only share a VRF if both registered it before VRFs had to be
    -- unique, in which case the hard fork to protocol version 11 recorded the VRF
    -- with a count of two. Registering a pool and then rewriting its VRF puts the
    -- state into the same shape.
    registerPoolSharingVRF vrf = do
      (kh, ownVrf) <- registerNewPool
      modifyNES $
        nesEsL . esLStateL . lsCertStateL . certPStateL %~ \ps ->
          ps
            & psStakePoolsL %~ Map.adjust (spsVrfL .~ vrf) kh
            & psVRFKeyHashesL %~ addVRFKeyHashOccurrence vrf . Map.delete ownVrf
      expectPool kh (Just vrf)
      pure kh
    registerPoolWithBlsKey mbBlsKey = do
      pps <- freshStakePool
      let ppsWithBlsKey = pps {sppBlsKey = mbBlsKey}
      impAnn "Registering a new stake pool" $
        submitAnyTx_ $
          registerPoolTx ppsWithBlsKey
      pure ppsWithBlsKey
    retirePoolTx kh retirementInterval = do
      curEpochNo <- getsNES nesELL
      pure $
        AnyLevelTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.singleton (RetirePoolTxCert kh (addEpochInterval curEpochNo retirementInterval))
    expectPool poolKh mbVrf = do
      pools <- psStakePools <$> getPState
      spsVrf <$> Map.lookup poolKh pools `shouldBe` mbVrf
    expectFuturePool poolKh mbVrf = do
      fps <- psFutureStakePoolParams <$> getPState
      sppVrf <$> Map.lookup poolKh fps `shouldBe` mbVrf
    expectVRFs vrfs =
      psVRFKeyHashes
        <$> getPState
          `shouldReturn` Map.fromList [(vrf, unsafeNonZero n) | (vrf, n) <- vrfs]
    expectBlsKeyState poolId expected = do
      sps <- expectJust . Map.lookup poolId . psStakePools =<< getPState
      spsBlsKey sps `shouldBe` expected
    blsPoolParams ::
      KeyHash StakePool ->
      StrictMaybe BlsKey ->
      ImpTestM era (StakePoolParams era)
    blsPoolParams kh mbBlsKey = do
      pps <- registerAccountAddress >>= freshPoolParams kh
      pure pps {sppBlsKey = mbBlsKey}
    registerBlsPool :: StrictMaybe BlsKey -> ImpTestM era (KeyHash StakePool)
    registerBlsPool mbBlsKey = do
      kh <- freshKeyHash
      reregisterBlsPool kh mbBlsKey
      expectActiveBlsKey kh mbBlsKey
      pure kh
    -- Registers the pool, or re-registers it if it already is one
    reregisterBlsPool :: KeyHash StakePool -> StrictMaybe BlsKey -> ImpTestM era ()
    reregisterBlsPool kh mbBlsKey =
      blsPoolParams kh mbBlsKey >>= \pps -> submitAnyTx_ $ registerPoolTx pps
    expectActiveBlsKey :: KeyHash StakePool -> StrictMaybe BlsKey -> ImpTestM era ()
    expectActiveBlsKey kh mbBlsKey = do
      pools <- psStakePools <$> getPState
      sps <- expectJust $ Map.lookup kh pools
      bksKey <$> spsBlsKey sps `shouldBe` mbBlsKey
    expectFutureBlsKey :: KeyHash StakePool -> StrictMaybe BlsKey -> ImpTestM era ()
    expectFutureBlsKey kh mbBlsKey = do
      fps <- psFutureStakePoolParams <$> getPState
      spp <- expectJust $ Map.lookup kh fps
      sppBlsKey spp `shouldBe` mbBlsKey
    expectBlsKeys :: [BlsKey] -> ImpTestM era ()
    expectBlsKeys keys = psBlsKeyHashes <$> getPState `shouldReturn` Set.fromList (hashBlsKey <$> keys)
    -- Registering the pool with the BLS key fails, because another pool is using it
    expectBlsKeyTaken :: KeyHash StakePool -> BlsKey -> ImpTestM era ()
    expectBlsKeyTaken kh key = do
      tx <- registerPoolTx <$> blsPoolParams kh (SJust key)
      submitFailingTx tx [injectFailure $ BlsKeyAlreadyRegistered kh key]
    expectBlsKeyTakenByNewPool :: BlsKey -> ImpTestM era ()
    expectBlsKeyTakenByNewPool key = do
      kh <- freshKeyHash
      expectBlsKeyTaken kh key
    poolParams ::
      KeyHash StakePool ->
      VRFVerKeyHash StakePoolVRF ->
      ImpTestM era (StakePoolParams era)
    poolParams kh vrf = do
      pps <- registerAccountAddress >>= freshPoolParams kh
      pure $ pps & sppVrfL .~ vrf
    -- Parameters of a new pool that has owners
    poolParamsWithOwners :: StrictMaybe BlsKey -> ImpTestM era (StakePoolParams era)
    poolParamsWithOwners mbBlsKey = do
      kh <- freshKeyHash
      owners <- Set.fromList <$> replicateM 2 freshKeyHash
      (\pps -> pps {sppOwners = owners}) <$> blsPoolParams kh mbBlsKey
    -- Registers a new pool that has owners
    registerPoolWithOwners :: StrictMaybe BlsKey -> ImpTestM era (StakePoolParams era)
    registerPoolWithOwners mbBlsKey = do
      pps <- poolParamsWithOwners mbBlsKey
      submitTx_ $ registerPoolTx pps
      pure pps
    -- Removes the witnesses of the owners of the pool from a transaction
    dropOwnerWits :: StakePoolParams era -> Tx l era -> ImpTestM era (Tx l era)
    dropOwnerWits pps tx = pure $ tx & witsTxL . addrTxWitsL .~ nonOwnerKeyWits
      where
        keyWits = tx ^. witsTxL . addrTxWitsL
        nonOwnerKeyWits = Set.filter ((`Set.notMember` ownerWits) . witVKeyHash) keyWits
        ownerWits = Set.map asWitness (sppOwners pps)
    -- Expects this transaction to fail for missing the owners of the pool
    expectOwnerWitsNeeded :: StakePoolParams era -> Tx TopTx era -> ImpTestM era ()
    expectOwnerWitsNeeded pps tx = do
      ownerWits <- expectJust $ NES.fromSet (Set.map asWitness (sppOwners pps))
      pStateBefore <- getPState
      withPostFixup (dropOwnerWits pps) $
        submitFailingTx tx [injectFailure $ MissingVKeyWitnessesUTXOW @era ownerWits]
      getPState `shouldReturn` pStateBefore
    discardsPendingUpdateFailure :: StakePoolParams era -> PredicateFailure (EraRule "LEDGER" era)
    discardsPendingUpdateFailure pps =
      injectFailure $ StakePoolUpdateConflictsWithPendingUpdate @era (sppId pps)
    subTxDiscardsPendingUpdateFailure :: StakePoolParams era -> PredicateFailure (EraRule "LEDGER" era)
    subTxDiscardsPendingUpdateFailure pps =
      injectFailure $
        DijkstraSubPoolPredFailure @era (StakePoolUpdateConflictsWithPendingUpdate (sppId pps))
    -- True if the top-level transaction and each of its sub-transactions (in order) have the
    -- witnesses of all the owners of the pool
    ownerWitsByLevel :: StakePoolParams era -> Tx TopTx era -> (Bool, [Bool])
    ownerWitsByLevel pps tx =
      (hasOwnerWits tx, hasOwnerWits <$> toList (tx ^. bodyTxL . subTransactionsTxBodyL))
      where
        ownerWits = Set.map asWitness (sppOwners pps)
        hasOwnerWits :: Tx l era -> Bool
        hasOwnerWits =
          (ownerWits `Set.isSubsetOf`) . Set.map witVKeyHash . (^. witsTxL . addrTxWitsL)
    -- Expects neither the transaction nor its sub-transactions to have the witnesses of all
    -- the owners of the pool
    expectNotAllOwnerWits :: StakePoolParams era -> Tx TopTx era -> ImpTestM era ()
    expectNotAllOwnerWits pps tx =
      ownerWitsByLevel pps tx `shouldSatisfy` \(topTx, subTxs) -> not (or (topTx : subTxs))
    expectFutureParams :: StakePoolParams era -> ImpTestM era ()
    expectFutureParams pps =
      Map.lookup (sppId pps) . psFutureStakePoolParams <$> getPState `shouldReturn` Just pps
    expectRetiring :: KeyHash StakePool -> Bool -> ImpTestM era ()
    expectRetiring kh expected =
      Map.member kh . psRetiring <$> getPState `shouldReturn` expected

-- | These need to run the POOL rule on its own, which needs the concrete rule
-- types of Dijkstra, so can't be bundled in the era-polymorphic ones above
dijkstraOnlySpec :: SpecWith (ImpInit (LedgerSpec DijkstraEra))
dijkstraOnlySpec = describe "POOL" $ do
  describe "Register and re-register pools" $ do
    it "re-register a pool from the genesis with its own VRF" $ do
      stakePoolParams <- freshStakePool
      -- set up before the injection, since no transaction can follow it (see `runPool`)
      newStakePoolParams <- freshStakePool
      let vrf = sppVrf stakePoolParams
      -- register the pool the way a network that starts in Dijkstra does: by injecting it
      -- from the genesis rather than through POOL
      injectGenesisStaking [stakePoolParams] [] []
      -- the pool can re-register with the VRF it is already using ...
      runPool (RegPool stakePoolParams) >>= expectRightDeep_
      -- ... because its VRF is tracked just like that of a pool registered through POOL ...
      psVRFKeyHashes <$> getPState `shouldReturn` [(vrf, knownNonZeroBounded @1)]
      -- ... which also keeps any other pool from registering with it
      runPool (RegPool newStakePoolParams {sppVrf = vrf})
        `shouldReturn` Left [VRFKeyHashAlreadyRegistered (sppId newStakePoolParams) vrf]

    it "re-register a pool from the genesis with its own BLS key" $ do
      blsKey <- freshBlsKey
      stakePoolParams <- (\pps -> pps {sppBlsKey = SJust blsKey}) <$> freshStakePool
      -- set up before the injection, since no transaction can follow it (see `runPool`)
      newStakePoolParams <- freshStakePool
      injectGenesisStaking [stakePoolParams] [] []
      -- the pool can re-register with the BLS key it is already using ...
      runPool (RegPool stakePoolParams) >>= expectRightDeep_
      -- ... because its BLS key is tracked just like that of a pool registered through POOL ...
      psBlsKeyHashes <$> getPState `shouldReturn` [hashBlsKey blsKey]
      -- ... which also keeps any other pool from registering with it
      runPool (RegPool newStakePoolParams {sppBlsKey = SJust blsKey})
        `shouldReturn` Left [BlsKeyAlreadyRegistered (sppId newStakePoolParams) blsKey]
  where
    -- The deposits of stake pools from the genesis never make it into the deposit pot,
    -- which the assertions of LEDGER reject, so POOL is run on its own.
    runPool poolCert = do
      poolEnv <- Shelley.PoolEnv <$> getsNES nesELL <*> getsPParams id
      pState <- getPState
      fmap fst <$> tryRunImpRule @"POOL" poolEnv pState poolCert
