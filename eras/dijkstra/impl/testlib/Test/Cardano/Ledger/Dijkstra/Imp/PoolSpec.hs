{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.PoolSpec (spec, dijkstraOnlySpec) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppMaxPledgeLeverageL)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Genesis
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Transition
import Cardano.Ledger.State
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Foldable (fold)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Word
import Lens.Micro ((%~), (&), (.~))
import qualified System.FS.Sim.MockFS as MockFS
import System.FS.Sim.STM
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
      tx <- registerPoolTx <$> poolParams kh vrfNew
      submitTx_ tx
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      -- re-registering with the VRF already recorded in the pool's own
      -- future params should succeed
      submitTx_ tx
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
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectFuturePool kh (Just vrf)
      expectVRFs [(vrf, 1)]
      -- ... and then with a fresh one
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
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
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
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
      registerPoolTx <$> poolParams kh1 vrfNew >>= submitTx_
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
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      -- going back to the active VRF frees the previously requested one
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectFuturePool kh (Just vrf)
      expectVRFs [(vrf, 1)]
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrfNew >>= submitTx_
      expectPool khNew (Just vrfNew)
      expectVRFs [(vrf, 1), (vrfNew, 1)]

    it "oscillating between the active and a fresh VRF within an epoch keeps the active one taken" $ do
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      -- switch to a fresh VRF, back to the active one and to the fresh one again
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      expectVRFs [(vrf, 1), (vrfNew, 1)]
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectVRFs [(vrf, 1)]
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
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
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
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
            registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
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
            registerPoolTx <$> poolParams kh vrf >>= submitTx_
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
        retirePoolTx kh1 (EpochInterval 1) >>= submitTx_
        passEpoch
        expectPool kh1 Nothing
        expectPool kh2 (Just vrf)
        expectVRFs [(vrf, 1)]
        expectTaken vrf
        -- ... and only once that one has retired as well does the VRF become available
        retirePoolTx kh2 (EpochInterval 1) >>= submitTx_
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
      mkPoolRegTxFromParams pps =
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert pps)

      getPools = getsNES $ nesEsL . epochStateStakePoolsL

    it "registers a pool with a valid BLS key and proof of possession" $ do
      pps <- freshStakePool
      ownerBlsKey <- freshBlsKey
      let ppsWithBlsKey = pps {sppBlsKey = SJust ownerBlsKey}
      submitTxAnn_ "Registering a new stake pool" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert ppsWithBlsKey)
      pools <- getPools
      stakePoolState <- expectJust $ Map.lookup (sppId pps) pools
      bksKey <$> spsBlsKey stakePoolState `shouldBe` SJust ownerBlsKey

    it "fails to re-register an existing pool with an invalid BLS proof of possession" $ do
      pps <- freshStakePool
      ownerBlsKey <- freshBlsKey
      let ppsWithBlsKey = pps {sppBlsKey = SJust ownerBlsKey}
      submitTxAnn_ "Registering a new stake pool" $
        mkPoolRegTxFromParams ppsWithBlsKey
      pStateBefore <- getPools
      invalidOwnerBlsKey <- BlsKey <$> arbitrary <*> arbitrary
      -- TODO: remove `withDisabledPostSubmitTxHook` once the Agda spec includes BLS
      -- proof of possession validation for pool registration.
      -- See https://github.com/IntersectMBO/formal-ledger-specifications/pull/1300
      withDisabledPostSubmitTxHook $
        submitFailingTx
          (mkPoolRegTxFromParams pps {sppBlsKey = SJust invalidOwnerBlsKey})
          [injectFailure $ BlsKeyInvalidProofOfPossession (sppId pps) invalidOwnerBlsKey]
      passEpoch
      getPools `shouldReturn` pStateBefore

    it "fails to register a new pool with an invalid BLS proof of possession" $ do
      pps <- freshStakePool
      invalidOwnerBlsKey <- BlsKey <$> arbitrary <*> arbitrary
      -- TODO: remove `withDisabledPostSubmitTxHook` once the Agda spec includes BLS
      -- proof of possession validation for pool registration.
      -- See https://github.com/IntersectMBO/formal-ledger-specifications/pull/1300
      withDisabledPostSubmitTxHook $
        submitFailingTx
          (mkPoolRegTxFromParams pps {sppBlsKey = SJust invalidOwnerBlsKey})
          [injectFailure $ BlsKeyInvalidProofOfPossession (sppId pps) invalidOwnerBlsKey]
      pools <- getPools
      expectNothing $ Map.lookup (sppId pps) pools

  describe "BLS key uniqueness" $ do
    it "a new pool cannot take the active key of another pool" $ do
      key <- freshBlsKey
      _ <- registerBlsPool (SJust key)
      expectBlsKeys [(key, 1)]
      expectBlsKeyTakenByNewPool key
      -- the key stays taken across the epoch boundary
      passEpoch
      expectBlsKeys [(key, 1)]
      expectBlsKeyTakenByNewPool key

    it "a registered pool cannot take the active key of another pool" $ do
      key1 <- freshBlsKey
      key2 <- freshBlsKey
      _ <- registerBlsPool (SJust key1)
      withKey <- registerBlsPool (SJust key2)
      withoutKey <- registerBlsPool SNothing
      expectBlsKeys [(key1, 1), (key2, 1)]
      -- neither a pool with a key of its own nor one without any may take it
      expectBlsKeyTaken withKey key1
      expectBlsKeyTaken withoutKey key1
      expectBlsKeys [(key1, 1), (key2, 1)]

    it "a pending future key cannot be claimed by another pool" $ do
      key1 <- freshBlsKey
      kh1 <- registerBlsPool (SJust key1)
      kh2 <- registerBlsPool SNothing
      newKey <- freshBlsKey
      reregisterBlsPool kh1 (SJust newKey)
      expectActiveBlsKey kh1 (SJust key1)
      expectFutureBlsKey kh1 (SJust newKey)
      expectBlsKeys [(key1, 1), (newKey, 1)]
      -- a key is taken as soon as a re-registration requests it, so neither a new pool ...
      expectBlsKeyTakenByNewPool newKey
      -- ... nor another registered pool may claim it
      expectBlsKeyTaken kh2 newKey
      expectBlsKeys [(key1, 1), (newKey, 1)]

    it "re-registering with the active key keeps a single reference to it" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      reregisterBlsPool kh (SJust key)
      expectFutureBlsKey kh (SJust key)
      expectBlsKeys [(key, 1)]
      passEpoch
      expectActiveBlsKey kh (SJust key)
      expectBlsKeys [(key, 1)]

    it "a pool can re-register with its own pending future key" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      -- registering with the key in the pool's own future params should succeed
      reregisterBlsPool kh (SJust newKey)
      expectActiveBlsKey kh (SJust key)
      expectFutureBlsKey kh (SJust newKey)
      expectBlsKeys [(key, 1), (newKey, 1)]
      passEpoch
      expectActiveBlsKey kh (SJust newKey)
      expectBlsKeys [(newKey, 1)]

    it "a key is released when its holder switches to a fresh one" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      -- the pool keeps using the key until the epoch boundary, so it stays taken
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectActiveBlsKey kh (SJust newKey)
      expectBlsKeys [(newKey, 1)]
      -- now another pool can take it over ...
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [(key, 1), (newKey, 1)]
      -- ... but only a single one
      expectBlsKeyTakenByNewPool key

    it "a key is released when its holder drops it" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      reregisterBlsPool kh SNothing
      expectActiveBlsKey kh (SJust key)
      expectBlsKeys [(key, 1)]
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectActiveBlsKey kh SNothing
      expectBlsKeys []
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [(key, 1)]

    it "a key is released when its holder retires" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      retirePoolTx kh (EpochInterval 1) >>= submitTx_
      expectBlsKeys [(key, 1)]
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectPool kh Nothing
      expectBlsKeys []
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [(key, 1)]

    it "retiring with a pending future key releases both keys" $ do
      key <- freshBlsKey
      kh <- registerBlsPool (SJust key)
      newKey <- freshBlsKey
      reregisterBlsPool kh (SJust newKey)
      retirePoolTx kh (EpochInterval 1) >>= submitTx_
      expectBlsKeys [(key, 1), (newKey, 1)]
      passEpoch
      expectPool kh Nothing
      expectBlsKeys []
      _ <- registerBlsPool (SJust key)
      _ <- registerBlsPool (SJust newKey)
      expectBlsKeys [(key, 1), (newKey, 1)]

    it "only the public key counts, not the proof of possession" $ do
      key <- freshBlsKey
      _ <- registerBlsPool (SJust key)
      bogusProof <- arbitrary
      let bogusKey = key {blsPossessionProof = bogusProof}
      kh <- freshKeyHash
      pps <- blsPoolParams kh (SJust bogusKey)
      -- the key is rejected twice: its public key is taken and its proof is not valid
      -- TODO: remove `withDisabledPostSubmitTxHook` once the Agda spec pinned by
      -- cardano-ledger requires BLS keys to be unique.
      -- See https://github.com/IntersectMBO/cardano-ledger/pull/6149
      withDisabledPostSubmitTxHook $
        submitFailingTx
          (registerPoolTx pps)
          [ injectFailure $ BlsKeyAlreadyRegistered kh bogusKey
          , injectFailure $ BlsKeyInvalidProofOfPossession kh bogusKey
          ]
      expectBlsKeys [(key, 1)]
  where
    registerNewPool = do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      submitTx_ . registerPoolTx =<< poolParams kh vrf
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
    retirePoolTx kh retirementInterval = do
      curEpochNo <- getsNES nesELL
      pure $
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
    reregisterBlsPool kh mbBlsKey = blsPoolParams kh mbBlsKey >>= submitTx_ . registerPoolTx
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
    expectBlsKeys :: [(BlsKey, Word64)] -> ImpTestM era ()
    expectBlsKeys keys =
      psBlsKeyHashes
        <$> getPState
          `shouldReturn` Map.fromList [(hashBlsKey key, unsafeNonZero n) | (key, n) <- keys]
    -- Registering the pool with the BLS key fails, because another pool is using it
    expectBlsKeyTaken :: KeyHash StakePool -> BlsKey -> ImpTestM era ()
    expectBlsKeyTaken kh key = do
      tx <- registerPoolTx <$> blsPoolParams kh (SJust key)
      -- TODO: remove `withDisabledPostSubmitTxHook` once the Agda spec pinned by
      -- cardano-ledger requires BLS keys to be unique.
      -- See https://github.com/IntersectMBO/cardano-ledger/pull/6149
      withDisabledPostSubmitTxHook $
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

-- | Tests that need the `TransitionConfig` of Dijkstra, which the era-polymorphic tests
-- above cannot construct.
dijkstraOnlySpec :: SpecWith (ImpInit (LedgerSpec DijkstraEra))
dijkstraOnlySpec = describe "POOL" $ do
  describe "Register and re-register pools" $ do
    it "re-register a pool from the genesis with its own VRF" $ do
      stakePoolParams <- freshStakePool
      -- set up before the injection, since no transaction can follow it (see `runPool`)
      newStakePoolParams <- freshStakePool
      let vrf = sppVrf stakePoolParams
      injectGenesisStakePools [stakePoolParams]
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
      injectGenesisStakePools [stakePoolParams]
      -- the pool can re-register with the BLS key it is already using ...
      runPool (RegPool stakePoolParams) >>= expectRightDeep_
      -- ... because its BLS key is tracked just like that of a pool registered through POOL ...
      psBlsKeyHashes <$> getPState `shouldReturn` [(hashBlsKey blsKey, knownNonZeroBounded @1)]
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
    -- Register the stake pools the way a network that starts in Dijkstra does: by
    -- injecting them from the genesis rather than through POOL.
    injectGenesisStakePools stakePools = do
      shelleyGenesis <- initGenesis @ShelleyEra
      alonzoGenesis <- initGenesis @AlonzoEra
      conwayGenesis <- initGenesis @ConwayEra
      dijkstraGenesis <- initGenesis @DijkstraEra
      let staking =
            ShelleyGenesisStaking
              { sgsPools = ListMap.fromList [(sppId spp, coerce spp) | spp <- stakePools]
              , sgsStake = mempty
              }
          transitionConfig =
            mkShelleyTransitionConfig shelleyGenesis {sgStaking = staking}
              & mkTransitionConfig NoGenesis
              & mkTransitionConfig NoGenesis
              & mkTransitionConfig alonzoGenesis
              & mkTransitionConfig NoGenesis
              & mkTransitionConfig conwayGenesis
              & mkTransitionConfig dijkstraGenesis
      nes <- getsNES id
      injectedNes <- liftIO $ do
        fs <- simHasFS' MockFS.empty
        injectIntoTestState fs transitionConfig nes
      modifyNES $ const injectedNes
