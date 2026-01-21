{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Cardano.Ledger.Shelley.Imp.PoolSpec (spec) where

import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure (..))
import Cardano.Ledger.State
import Data.Map.Strict as Map
import Data.Proxy
import Lens.Micro
import Test.Cardano.Ledger.Binary.Arbitrary (genByteArray)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec :: forall era. ShelleyEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "POOL" $ do
  describe "Register and re-register pools" $ do
    it "register a pool with too low cost" $ do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      minPoolCost <- getsPParams ppMinPoolCostL
      tooLowCost <- Coin <$> choose (0, unCoin minPoolCost)
      pps <- poolParams kh vrf
      let tx = registerPoolTx (pps & sppCostL .~ tooLowCost)
      submitFailingTx
        tx
        [injectFailure $ StakePoolCostTooLowPOOL $ Mismatch tooLowCost minPoolCost]

    it "register a pool with a reward account having the wrong network id" $ do
      pv <- getsPParams ppProtocolVersionL
      rewardCredential <- KeyHashObj <$> freshKeyHash
      let badRewardAccount =
            RewardAccount
              { raNetwork = Mainnet
              , raCredential = rewardCredential
              }
      kh <- freshKeyHash
      pps <- freshPoolParams kh badRewardAccount
      let tx = registerPoolTx pps
      if pvMajor pv < natVersion @5
        then
          submitTx_ tx
        else
          submitFailingTx tx [injectFailure $ WrongNetworkPOOL (Mismatch Mainnet Testnet) kh]

    it "register a pool with too big metadata" $ do
      pv <- getsPParams ppProtocolVersionL
      let maxMetadataSize = sizeHash (Proxy :: Proxy HASH)
      tooBigSize <- choose (maxMetadataSize + 1, maxMetadataSize + 50)
      metadataHash <- liftGen $ genByteArray $ fromIntegral tooBigSize
      url <- arbitrary
      let metadata = PoolMetadata url metadataHash
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      pps <- poolParams kh vrf
      let tx = registerPoolTx (pps & sppMetadataL .~ SJust metadata)
      if pvMajor pv < natVersion @5
        then
          submitTx_ tx
        else
          submitFailingTx tx [injectFailure $ PoolMedataHashTooBig kh (fromIntegral tooBigSize)]

    it "register a new pool with an already registered VRF" $ do
      pv <- getsPParams ppProtocolVersionL
      (kh, vrf) <- registerNewPool
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= \tx ->
        if pvMajor pv < natVersion @11
          then do
            submitTx_ tx
            expectPool khNew (Just vrf)
          else do
            submitFailingTx tx [injectFailure $ VRFKeyHashAlreadyRegistered khNew vrf]
            expectPool khNew Nothing
      expectPool kh (Just vrf)

    it "re-register a pool and change its delegations in the same epoch" $ do
      (poolKh, _) <- registerNewPool
      (poolKh2, _) <- registerNewPool
      stakeCred <- KeyHashObj <$> freshKeyHash
      _ <- registerStakeCredential stakeCred
      stakeCred2 <- KeyHashObj <$> freshKeyHash
      _ <- registerStakeCredential stakeCred2
      delegateStake stakeCred poolKh
      vrf1 <- freshKeyHashVRF
      registerPoolTx <$> poolParams poolKh vrf1 >>= \tx -> do
        submitTx_ tx
        expectPoolDelegs poolKh (Just [stakeCred])
        delegateStake stakeCred2 poolKh
        expectPoolDelegs poolKh (Just [stakeCred, stakeCred2])
        passEpoch
        expectPoolDelegs poolKh (Just [stakeCred, stakeCred2])

      vrf2 <- freshKeyHashVRF
      registerPoolTx <$> poolParams poolKh vrf2 >>= \tx -> do
        submitTx_ tx
        expectPoolDelegs poolKh (Just [stakeCred, stakeCred2])

        unRegTxCert <- genUnRegTxCert stakeCred2
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [unRegTxCert]
        expectPoolDelegs poolKh (Just [stakeCred])
        delegateStake stakeCred poolKh2
        expectPoolDelegs poolKh (Just [])
        passEpoch
        expectPoolDelegs poolKh (Just [])

    it "re-register a pool with an already registered VRF" $ do
      pv <- getsPParams ppProtocolVersionL
      (kh1, vrf1) <- registerNewPool
      (kh2, vrf2) <- registerNewPool
      registerPoolTx <$> poolParams kh1 vrf2 >>= \tx ->
        if pvMajor pv < natVersion @11
          then do
            submitTx_ tx
            expectPool kh1 (Just vrf1)
            expectFuturePool kh1 (Just vrf2)
            passEpoch
            expectPool kh1 (Just vrf2)
            expectPool kh2 (Just vrf2)
          else do
            submitFailingTx tx [injectFailure $ VRFKeyHashAlreadyRegistered kh1 vrf2]
            expectPool kh1 (Just vrf1)
            expectFuturePool kh1 Nothing

    it "re-register a pool with its own VRF" $ do
      (kh, vrf) <- registerNewPool
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrf)
      passEpoch
      expectPool kh (Just vrf)
      expectFuturePool kh Nothing

    it "re-register a pool with a fresh VRF" $ do
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      expectPool kh (Just vrf)
      expectFuturePool kh (Just vrfNew)
      passEpoch
      expectPool kh (Just vrfNew)
      expectVRFs [vrfNew]
      -- now the original VRF can be reused
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
      expectVRFs [vrf, vrfNew]

    it "register a new pool with the VRF of a re-registered pool " $ do
      pv <- getsPParams ppProtocolVersionL
      (kh, _) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      -- re-register pool with a new vrf
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      passEpoch
      -- try to register a new pool with the new vrf
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrfNew >>= \tx ->
        if pvMajor pv < natVersion @11
          then do
            submitTx_ tx
            expectPool kh (Just vrfNew)
            expectPool khNew (Just vrfNew)
          else
            submitFailingTx tx [injectFailure $ VRFKeyHashAlreadyRegistered khNew vrfNew]

    it "after the epoch changes, reuse VRFs that get overwritten" $ do
      (kh, vrf) <- registerNewPool
      vrf1 <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrf1 >>= submitTx_
      expectVRFs [vrf, vrf1]
      vrf2 <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrf2 >>= submitTx_
      expectVRFs [vrf, vrf2]
      vrf3 <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrf3 >>= submitTx_
      expectVRFs [vrf, vrf3]
      passEpoch
      expectPool kh (Just vrf3)
      expectVRFs [vrf3]
      -- reuse VRFs that didn't get used
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf1 >>= submitTx_
      expectPool khNew (Just vrf1)
      expectVRFs [vrf1, vrf3]
      -- the original pool can be re-registered with one of the discarded VRFs too
      registerPoolTx <$> poolParams kh vrf2 >>= submitTx_
      expectVRFs [vrf1, vrf2, vrf3]
      passEpoch
      expectVRFs [vrf1, vrf2]
      -- the original pool can be re-registered with the original VRF too
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectVRFs [vrf, vrf1, vrf2]
      passEpoch
      expectVRFs [vrf, vrf1]

    it "before the epoch changes, try to reuse VRFs that get overwritten" $ do
      pv <- getsPParams ppProtocolVersionL
      (kh, vrf) <- registerNewPool
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      -- try to register a pool with the original VRF that got overwritten
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= \tx ->
        if pvMajor pv < natVersion @11
          then do
            submitTx_ tx
            expectPool kh (Just vrf)
            expectPool khNew (Just vrf)
            passEpoch
            expectPool kh (Just vrfNew)
            expectPool khNew (Just vrf)
          else do
            submitFailingTx tx [injectFailure $ VRFKeyHashAlreadyRegistered khNew vrf]

  describe "Retiring pools" $ do
    it "retire an unregistered pool" $ do
      khNew <- freshKeyHash
      retirePoolTx khNew (EpochInterval 10) >>= \tx ->
        submitFailingTx tx [injectFailure $ StakePoolNotRegisteredOnKeyPOOL khNew]

    it "retire a pool with too high a retirement epoch" $ do
      (kh, _) <- registerNewPool
      maxRetireInterval <- getsPParams ppEMaxL
      curEpochNo <- getsNES nesELL
      let maxRetireIntervalPlus =
            EpochInterval $ fromIntegral $ unEpochInterval maxRetireInterval + 1
      let supplied = addEpochInterval curEpochNo maxRetireIntervalPlus

      retirePoolTx kh maxRetireIntervalPlus >>= \tx ->
        submitFailingTx
          tx
          [ injectFailure $
              StakePoolRetirementWrongEpochPOOL
                (Mismatch supplied curEpochNo)
                (Mismatch supplied (addEpochInterval curEpochNo maxRetireInterval))
          ]
      expectRetiring False kh

    it "retire a pool with too low a retirement epoch" $ do
      (kh, _) <- registerNewPool
      curEpochNo <- getsNES nesELL
      maxRetireInterval <- getsPParams ppEMaxL
      retirePoolTx kh (EpochInterval 0) >>= \tx ->
        submitFailingTx
          tx
          [ injectFailure $
              StakePoolRetirementWrongEpochPOOL
                (Mismatch curEpochNo curEpochNo)
                (Mismatch curEpochNo (addEpochInterval curEpochNo maxRetireInterval))
          ]
      expectRetiring False kh

    it "re-register a retiring pool with an already registered vrf" $ do
      pv <- getsPParams ppProtocolVersionL
      (kh1, _) <- registerNewPool
      (_, vrf2) <- registerNewPool
      retirePoolTx kh1 (EpochInterval 10) >>= submitTx_
      registerPoolTx <$> poolParams kh1 vrf2 >>= \tx ->
        if pvMajor pv < natVersion @11
          then do
            submitTx_ tx
            expectRetiring False kh1
            expectFuturePool kh1 (Just vrf2)
            passEpoch
            expectPool kh1 (Just vrf2)
          else do
            submitFailingTx tx [injectFailure $ VRFKeyHashAlreadyRegistered kh1 vrf2]
            expectRetiring True kh1
            expectFuturePool kh1 Nothing

    it "re-register retiring pool with its own VRF" $ do
      (kh, vrf) <- registerNewPool
      retirePoolTx kh (EpochInterval 10) >>= submitTx_
      expectRetiring True kh
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectPool kh (Just vrf)
      expectRetiring False kh

    it "re-register a retiring pool with a fresh VRF" $ do
      (kh, vrf) <- registerNewPool
      retirePoolTx kh (EpochInterval 10) >>= submitTx_
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      expectRetiring False kh
      expectFuturePool kh (Just vrfNew)
      passEpoch
      expectPool kh (Just vrfNew)
      expectVRFs [vrfNew]
      -- now the original VRF can be reused
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_

    it "register a pool with the VRF of a retiring pool" $ do
      pv <- getsPParams ppProtocolVersionL
      (kh, vrf) <- registerNewPool
      let retirement = 1
      retirePoolTx kh (EpochInterval retirement) >>= submitTx_
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= \tx ->
        if pvMajor pv < natVersion @11
          then do
            submitTx_ tx
            expectPool khNew (Just vrf)
          else do
            submitFailingTx tx [injectFailure $ VRFKeyHashAlreadyRegistered khNew vrf]
            expectPool khNew Nothing
      expectRetiring True kh
      passNEpochs (fromIntegral retirement)
      expectRetiring False khNew
      expectPool kh Nothing

    it "retiring a pool clears its delegations" $ do
      (poolKh, _) <- registerNewPool
      let retirement = 1
      stakeCred1 <- do
        cred <- KeyHashObj <$> freshKeyHash
        _ <- registerStakeCredential cred
        delegateStake cred poolKh
        pure cred

      retirePoolTx poolKh (EpochInterval retirement) >>= submitTx_
      expectPoolDelegs poolKh (Just [stakeCred1])
      stakeCred2 <- do
        cred <- KeyHashObj <$> freshKeyHash
        _ <- registerStakeCredential cred
        delegateStake cred poolKh
        pure cred
      expectPoolDelegs poolKh (Just [stakeCred1, stakeCred2])

      passNEpochs (fromIntegral retirement)
      expectPoolDelegs poolKh Nothing

  describe "Retired pools" $ do
    it "re-register a pool with the same keyhash and VRF " $ do
      (kh, vrf) <- registerNewPool
      let retirement = 1
      retirePoolTx kh (EpochInterval retirement) >>= submitTx_
      passNEpochs (fromIntegral retirement)
      expectPool kh Nothing
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectPool kh (Just vrf)
      expectVRFs [vrf]

    it "register a pool with the VRF of a retired pool" $ do
      (kh, vrf) <- registerNewPool
      let retirement = 1
      retirePoolTx kh (EpochInterval retirement) >>= submitTx_
      expectRetiring True kh
      passNEpochs (fromIntegral retirement)
      expectRetiring False kh
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
      expectPool khNew (Just vrf)
      expectRetiring False khNew
      expectVRFs [vrf]
  where
    registerNewPool = do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      submitTx_ . registerPoolTx =<< poolParams kh vrf
      expectPool kh (Just vrf)
      pure (kh, vrf)
    registerPoolTx pps =
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL .~ [RegPoolTxCert pps]
    retirePoolTx kh retirementInterval = do
      curEpochNo <- getsNES nesELL
      let retirement = addEpochInterval curEpochNo retirementInterval
      pure $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert kh retirement]
    expectPool poolKh mbVrf = do
      pps <- psStakePools <$> getPState
      spsVrf <$> Map.lookup poolKh pps `shouldBe` mbVrf
    expectFuturePool poolKh mbVrf = do
      fps <- psFutureStakePoolParams <$> getPState
      sppVrf <$> Map.lookup poolKh fps `shouldBe` mbVrf
    expectPoolDelegs poolKh delegs = do
      pps <- psStakePools <$> getPState
      spsDelegators <$> Map.lookup poolKh pps `shouldBe` delegs
    expectRetiring isRetiring poolKh = do
      retiring <- psRetiring <$> getPState
      assertBool
        ("Expected 'retiring' status of: " <> show poolKh <> " to be: " <> show isRetiring)
        $ Map.member poolKh retiring == isRetiring
    expectVRFs vrfs = do
      whenMajorVersionAtLeast @11 $
        Map.keysSet . psVRFKeyHashes <$> getPState `shouldReturn` vrfs
    poolParams ::
      KeyHash StakePool ->
      VRFVerKeyHash StakePoolVRF ->
      ImpTestM era StakePoolParams
    poolParams kh vrf = do
      pps <- registerRewardAccount >>= freshPoolParams kh
      pure $ pps & sppVrfL .~ vrf
    getPState = getsNES @era $ nesEsL . esLStateL . lsCertStateL . certPStateL
