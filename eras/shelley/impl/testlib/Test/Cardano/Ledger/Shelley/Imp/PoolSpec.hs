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
import Cardano.Ledger.State (PoolMetadata (..), ppCostL, ppMetadataL, ppVrfL, spsVrf)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Lens.Micro
import Test.Cardano.Ledger.Binary.Arbitrary (genByteString)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyPoolPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "POOL" $ do
  describe "Register and re-register pools" $ do
    it "register a pool with too low cost" $ do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      minPoolCost <- getsPParams ppMinPoolCostL
      tooLowCost <- Coin <$> choose (0, unCoin minPoolCost)
      let pps = (\p -> p & ppCostL .~ tooLowCost) <$> poolParams kh vrf
      registerPoolTx <$> pps >>= \tx ->
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
      let pps = freshPoolParams kh badRewardAccount
      registerPoolTx <$> pps >>= \tx ->
        if pvMajor pv < natVersion @5
          then
            submitTx_ tx
          else
            submitFailingTx tx [injectFailure $ WrongNetworkPOOL (Mismatch Mainnet Testnet) kh]

    it "register a pool with too big metadata" $ do
      pv <- getsPParams ppProtocolVersionL
      let maxMetadataSize = sizeHash (Proxy :: Proxy HASH)
      tooBigSize <- choose (maxMetadataSize + 1, maxMetadataSize + 50)
      metadataHash <- liftGen $ genByteString $ fromIntegral tooBigSize
      url <- arbitrary
      let metadata = PoolMetadata url metadataHash
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      let pps = (\p -> p & ppMetadataL .~ SJust metadata) <$> poolParams kh vrf
      registerPoolTx <$> pps >>= \tx ->
        if pvMajor pv < natVersion @5
          then
            submitTx_ tx
          else
            submitFailingTx tx [injectFailure $ PoolMedataHashTooBig kh (fromIntegral tooBigSize)]

    it "register a new pool with an already registered VRF" $ do
      (kh, vrf) <- registerNewPool
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
      expectPool khNew (Just vrf)
      expectPool kh (Just vrf)

    it "re-register a pool with an already registered VRF" $ do
      (kh1, vrf1) <- registerNewPool
      (kh2, vrf2) <- registerNewPool
      registerPoolTx <$> poolParams kh1 vrf2 >>= submitTx_
      expectPool kh1 (Just vrf1)
      expectFuturePool kh1 (Just vrf2)
      passEpoch
      expectPool kh1 (Just vrf2)
      expectPool kh2 (Just vrf2)

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
      (kh1, _) <- registerNewPool
      (_, vrf2) <- registerNewPool
      retirePoolTx kh1 (EpochInterval 10) >>= submitTx_
      registerPoolTx <$> poolParams kh1 vrf2 >>= submitTx_
      expectRetiring False kh1
      expectFuturePool kh1 (Just vrf2)
      passEpoch
      expectPool kh1 (Just vrf2)

    it "re-register retiring pool with its own VRF" $ do
      (kh, vrf) <- registerNewPool
      retirePoolTx kh (EpochInterval 10) >>= submitTx_
      expectRetiring True kh
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectPool kh (Just vrf)
      expectRetiring False kh

    it "re-register a retiring pool with a fresh VRF" $ do
      (kh, _) <- registerNewPool
      retirePoolTx kh (EpochInterval 10) >>= submitTx_
      vrfNew <- freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrfNew >>= submitTx_
      expectRetiring False kh
      expectFuturePool kh (Just vrfNew)
      passEpoch
      expectPool kh (Just vrfNew)

    it "register a pool with the VRF of a retiring pool" $ do
      (kh, vrf) <- registerNewPool
      let retirement = 1
      retirePoolTx kh (EpochInterval retirement) >>= submitTx_
      khNew <- freshKeyHash
      registerPoolTx <$> poolParams khNew vrf >>= submitTx_
      expectPool khNew (Just vrf)
      expectRetiring True kh
      passNEpochs (fromIntegral retirement)
      expectPool khNew (Just vrf)
      expectRetiring False khNew
      expectPool kh Nothing

  describe "Retired pools" $ do
    it "re-register a pool with the same keyhash and VRF " $ do
      (kh, vrf) <- registerNewPool
      let retirement = 1
      retirePoolTx kh (EpochInterval retirement) >>= submitTx_
      passNEpochs (fromIntegral retirement)
      expectPool kh Nothing
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
      expectPool kh (Just vrf)

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
  where
    registerNewPool = do
      (kh, vrf) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      registerPoolTx <$> poolParams kh vrf >>= submitTx_
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
      fps <- psFutureStakePools <$> getPState
      spsVrf <$> Map.lookup poolKh fps `shouldBe` mbVrf
    expectRetiring isRetiring poolKh = do
      retiring <- psRetiring <$> getPState
      assertBool
        ("Expected 'retiring' status of: " <> show poolKh <> " to be: " <> show isRetiring)
        $ Map.member poolKh retiring == isRetiring
    poolParams kh vrf = do
      pps <- registerRewardAccount >>= freshPoolParams kh
      pure $ pps & ppVrfL .~ vrf
    getPState = getsNES @era $ nesEsL . esLStateL . lsCertStateL . certPStateL
