{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubPoolSpec (spec) where

import Cardano.Crypto.Hash.Class (hashSize)
import Cardano.Ledger.Address (accountAddressIdL, accountAddressNetworkIdL)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Mismatch (..),
  Network (..),
  StrictMaybe (..),
  addEpochInterval,
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraPoolPredFailure (..), DijkstraSubPoolPredFailure (..))
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsCertStateL, nesELL, nesEpochStateL)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)
import Test.Cardano.Base.Bytes (genByteArray)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

lookupStakePoolParams ::
  ShelleyEraImp era =>
  KeyHash StakePool ->
  ImpTestM era (Maybe StakePoolState)
lookupStakePoolParams spKH = do
  stakePools <- getsNES $ nesEpochStateL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
  pure $ Map.lookup spKH stakePools

lookupFutureStakePoolParams ::
  ShelleyEraImp era =>
  KeyHash StakePool ->
  ImpTestM era (Maybe (StakePoolParams era))
lookupFutureStakePoolParams spKH = do
  stakePools <-
    getsNES $ nesEpochStateL . esLStateL . lsCertStateL . certPStateL . psFutureStakePoolParamsL
  pure $ Map.lookup spKH stakePools

expectStakePoolParams ::
  DijkstraEraImp era => KeyHash StakePool -> Maybe (StakePoolParams era) -> ImpTestM era ()
expectStakePoolParams kh expected = impAnn "expectStakePoolParams" $ do
  actual <- lookupStakePoolParams kh
  impAnn "accountId" $
    (view spsAccountIdL <$> actual)
      `shouldBeExpr` (view (sppAccountAddressL . accountAddressIdL) <$> expected)
  impAnn "blsKey" $
    (fmap bksKey . view spsBlsKeyL <$> actual) `shouldBeExpr` (view sppBlsKeyL <$> expected)
  impAnn "vrf" $ (view spsVrfL <$> actual) `shouldBeExpr` (view sppVrfL <$> expected)
  impAnn "pledge" $ (view spsPledgeL <$> actual) `shouldBeExpr` (view sppPledgeL <$> expected)
  impAnn "cost" $ (view spsCostL <$> actual) `shouldBeExpr` (view sppCostL <$> expected)
  impAnn "margin" $ (view spsMarginL <$> actual) `shouldBeExpr` (view sppMarginL <$> expected)
  impAnn "owners" $ (view spsOwnersL <$> actual) `shouldBeExpr` (view sppOwnersL <$> expected)
  impAnn "relays" $ (view spsRelaysL <$> actual) `shouldBeExpr` (view sppRelaysL <$> expected)
  impAnn "metadata" $ (view spsMetadataL <$> actual) `shouldBeExpr` (view sppMetadataL <$> expected)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBPOOL" $ do
  describe "Pool registration" $ do
    it "Fails when registering a new pool with the same VRF" $ do
      stakePoolParams1 <- freshStakePool
      let
        vrfKey = stakePoolParams1 ^. sppVrfL
      stakePoolParams2 <- freshStakePool
      submitTxAnn_ "Registering the first pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams1
                   ]
          ]
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [ RegPoolTxCert $ stakePoolParams2 & sppVrfL .~ vrfKey
                     ]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            Dijkstra.VRFKeyHashAlreadyRegistered (stakePoolParams2 ^. sppIdL) vrfKey
        ]
    it "Fails when registering a pool with an invalid network ID" $ do
      stakePoolParams <- freshStakePool
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [ RegPoolTxCert $
                         stakePoolParams & sppAccountAddressL . accountAddressNetworkIdL .~ Mainnet
                     ]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            WrongNetworkPOOL (Mismatch Mainnet Testnet) (stakePoolParams ^. sppIdL)
        ]

    -- Disabled in conformance for the same reason as the corresponding top-level POOL
    -- test: https://github.com/IntersectMBO/formal-ledger-specifications/issues/1293
    disableInConformanceIt "Fails when the metadata hash is too big" $ do
      stakePoolParams <- freshStakePool
      let maxMetadataHashSize = fromIntegral $ hashSize (Proxy :: Proxy HASH)
      tooBigSize <- choose (maxMetadataHashSize + 1, maxMetadataHashSize + 50)
      metadataHash <- liftGen $ genByteArray tooBigSize
      url <- arbitrary
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [ RegPoolTxCert $
                         stakePoolParams & sppMetadataL .~ SJust (PoolMetadata url metadataHash)
                     ]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            PoolMedataHashTooBig (stakePoolParams ^. sppIdL) tooBigSize
        ]
  describe "Pool re-registration" $ do
    it "Fails when re-registering an existing pool with the same VRF as another one" $ do
      stakePoolParams1 <- freshStakePool
      let
        vrfKey = stakePoolParams1 ^. sppVrfL
      stakePoolParams2 <- freshStakePool
      submitTxAnn_ "Registering the first pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams1
                   ]
          ]
      submitTxAnn_ "Registering the second pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams2
                   ]
          ]
      impAnn "Reregistering the second pool" $
        submitFailingTx
          ( mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL
                    .~ [ RegPoolTxCert $ stakePoolParams2 & sppVrfL .~ vrfKey
                       ]
              ]
          )
          [ injectFailure . DijkstraSubPoolPredFailure $
              VRFKeyHashAlreadyRegistered (stakePoolParams2 ^. sppIdL) vrfKey
          ]
    it "Fails when re-registering an existing pool with an invalid network ID" $ do
      stakePoolParams <- freshStakePool
      submitTxAnn_ "Registering the pool for the first time" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams
                   ]
          ]
      impAnn "Reregistering the pool" $
        submitFailingTx
          ( mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL
                    .~ [ RegPoolTxCert $
                           stakePoolParams
                             & sppAccountAddressL . accountAddressNetworkIdL .~ Mainnet
                       ]
              ]
          )
          [ injectFailure . DijkstraSubPoolPredFailure $
              WrongNetworkPOOL (Mismatch Mainnet Testnet) (stakePoolParams ^. sppIdL)
          ]
    it "Fails to register a new pool with a VRF claimed by a re-registration" $ do
      spKH <- freshKeyHash
      stakePoolParams <- freshPoolParams spKH =<< registerAccountAddress
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert stakePoolParams]
          ]
      newStakePoolParams <- freshPoolParams spKH =<< registerAccountAddress
      let newVrf = newStakePoolParams ^. sppVrfL
      submitTxAnn_ "Re-register pool with a fresh VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert newStakePoolParams]
          ]
      otherStakePoolParams <- freshStakePool
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [RegPoolTxCert $ otherStakePoolParams & sppVrfL .~ newVrf]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            VRFKeyHashAlreadyRegistered (otherStakePoolParams ^. sppIdL) newVrf
        ]
    it "Re-registering again with a fresh VRF releases the previously claimed one" $ do
      spKH <- freshKeyHash
      stakePoolParams <- freshPoolParams spKH =<< registerAccountAddress
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert stakePoolParams]
          ]
      reRegStakePoolParams1 <- freshPoolParams spKH =<< registerAccountAddress
      submitTxAnn_ "Re-register pool with a fresh VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert reRegStakePoolParams1]
          ]
      reRegStakePoolParams2 <- freshPoolParams spKH =<< registerAccountAddress
      submitTxAnn_ "Re-register pool with another fresh VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert reRegStakePoolParams2]
          ]
      otherStakePoolParams <- freshStakePool
      submitTxAnn_ "Register a new pool with the released VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert $
                       otherStakePoolParams & sppVrfL .~ (reRegStakePoolParams1 ^. sppVrfL)
                   ]
          ]
      anotherStakePoolParams <- freshStakePool
      let claimedVrf = reRegStakePoolParams2 ^. sppVrfL
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [RegPoolTxCert $ anotherStakePoolParams & sppVrfL .~ claimedVrf]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            VRFKeyHashAlreadyRegistered (anotherStakePoolParams ^. sppIdL) claimedVrf
        ]
    it "Tracks the VRF through a sequence of re-registrations" $ do
      khPool <- freshKeyHash
      accountAddr <- registerAccountAddress
      oldVRF <- arbitrary
      pps <- freshPoolParams khPool accountAddr
      let
        oldPPS = pps & sppVrfL .~ oldVRF
      submitTxAnn_ "Registering a new stake pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert oldPPS]
          ]
      expectStakePoolParams khPool $ Just oldPPS
      newVRF <- arbitrary
      let
        newPPS = pps & sppVrfL .~ newVRF
      submitTxAnn_ "Change VRF key" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert newPPS]
          ]
      expectStakePoolParams khPool $ Just oldPPS
      do
        Just futureParams <- lookupFutureStakePoolParams khPool
        futureParams ^. sppVrfL `shouldBe` newPPS ^. sppVrfL
      let maxMetadataHashSize = fromIntegral $ hashSize (Proxy :: Proxy HASH)
      metadataHashSize <- choose (0, maxMetadataHashSize)
      newMetadata <- PoolMetadata <$> arbitrary <*> liftGen (genByteArray metadataHashSize)
      submitTxAnn_ "Same VRF key" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert $ newPPS & sppMetadataL .~ SJust newMetadata]
          ]
      Just futureParams <- lookupFutureStakePoolParams khPool
      futureParams ^. sppVrfL `shouldBe` newPPS ^. sppVrfL
      futureParams ^. sppMetadataL `shouldBe` SJust newMetadata
      submitTxAnn_ "Back to the active VRF key" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert oldPPS]
          ]
      anotherVRF <- arbitrary
      submitTxAnn_ "Another VRF key" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert $ pps & sppVrfL .~ anotherVRF]
          ]
      -- The pool keeps using its active VRF key until the epoch boundary, so it must stay taken
      otherPPS <- freshStakePool
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [RegPoolTxCert $ otherPPS & sppVrfL .~ oldVRF]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            VRFKeyHashAlreadyRegistered (otherPPS ^. sppIdL) oldVRF
        ]
  describe "Pool retirement" $ do
    it "Fails when pool is not registered" $ do
      spKH <- freshKeyHash
      currentEpoch <- getsNES nesELL
      EpochNo maxEpoch <- addEpochInterval currentEpoch <$> getsPParams ppEMaxL
      retireEpoch <- EpochNo <$> choose (succ $ unEpochNo currentEpoch, maxEpoch)
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert spKH retireEpoch]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            StakePoolNotRegisteredOnKeyPOOL spKH
        ]
    it "Fails when retirement epoch is outside valid range" $ do
      spKH <- freshKeyHash
      stakePoolParams <- freshPoolParams spKH =<< registerAccountAddress
      submitTxAnn_ "Registering a stake pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert stakePoolParams]
          ]
      expectStakePoolParams spKH $ Just stakePoolParams
      currentEpoch <- getsNES nesELL
      maxEpoch <- addEpochInterval currentEpoch <$> getsPParams ppEMaxL
      retireEpoch <-
        frequency
          [ (1, pure $ succ maxEpoch)
          , (1, pure currentEpoch)
          , (49, EpochNo <$> choose (0, unEpochNo currentEpoch))
          , (49, EpochNo <$> choose (unEpochNo $ succ maxEpoch, maxBound))
          ]
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert spKH retireEpoch]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure $
            StakePoolRetirementWrongEpochPOOL
              (Mismatch retireEpoch currentEpoch)
              (Mismatch retireEpoch maxEpoch)
        ]
  describe "BLS PoolReg" $ do
    let
      getPools = psStakePools <$> getPState

    it "fails to re-register an existing pool with an invalid BLS proof of possession" $ do
      ownerBlsKey <- freshBlsKey
      pps <- registerPoolWithBlsKey (SJust ownerBlsKey)
      pStateBefore <- getPools
      invalidOwnerBlsKey <- BlsKey <$> arbitrary <*> arbitrary
      submitFailingTx
        (registerPoolTx pps {sppBlsKey = SJust invalidOwnerBlsKey})
        [ injectFailure . DijkstraSubPoolPredFailure $
            BlsKeyInvalidProofOfPossession (sppId pps) invalidOwnerBlsKey
        ]
      passEpoch
      getPools `shouldReturn` pStateBefore

    it "fails to register a new pool with an invalid BLS proof of possession" $ do
      pps <- freshStakePool
      invalidOwnerBlsKey <- BlsKey <$> arbitrary <*> arbitrary
      submitFailingTx
        (registerPoolTx pps {sppBlsKey = SJust invalidOwnerBlsKey})
        [ injectFailure . DijkstraSubPoolPredFailure $
            BlsKeyInvalidProofOfPossession (sppId pps) invalidOwnerBlsKey
        ]
      pools <- getPools
      expectNothing $ Map.lookup (sppId pps) pools

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
      submitTx_ =<< retirePoolTx kh (EpochInterval 1)
      expectBlsKeys [key]
      expectBlsKeyTakenByNewPool key
      passEpoch
      expectNoPool kh
      expectBlsKeys []
      other <- registerBlsPool (SJust key)
      expectActiveBlsKey other (SJust key)
      expectBlsKeys [key]

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
        [ injectFailure . DijkstraSubPoolPredFailure $ BlsKeyInvalidProofOfPossession kh bogusKey
        , injectFailure . DijkstraSubPoolPredFailure $ BlsKeyAlreadyRegistered kh bogusKey
        ]
      expectBlsKeys [key]
  where
    registerPoolTx pps =
      mkTopTxWithSubTxs
        [ mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [RegPoolTxCert pps]
        ]
    retirePoolTx kh retirementInterval = do
      curEpochNo <- getsNES nesELL
      pure $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [RetirePoolTxCert kh (addEpochInterval curEpochNo retirementInterval)]
          ]
    registerPoolWithBlsKey mbBlsKey = do
      pps <- freshStakePool
      let ppsWithBlsKey = pps {sppBlsKey = mbBlsKey}
      submitTxAnn_ "Registering a new stake pool" $
        registerPoolTx ppsWithBlsKey
      pure ppsWithBlsKey
    expectNoPool poolKh = do
      pools <- psStakePools <$> getPState
      expectNothing $ Map.lookup poolKh pools
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
    expectBlsKeys :: [BlsKey] -> ImpTestM era ()
    expectBlsKeys keys = psBlsKeyHashes <$> getPState `shouldReturn` Set.fromList (hashBlsKey <$> keys)
    -- Registering the pool with the BLS key fails, because another pool is using it
    expectBlsKeyTaken :: KeyHash StakePool -> BlsKey -> ImpTestM era ()
    expectBlsKeyTaken kh key = do
      tx <- registerPoolTx <$> blsPoolParams kh (SJust key)
      submitFailingTx tx [injectFailure . DijkstraSubPoolPredFailure $ BlsKeyAlreadyRegistered kh key]
    expectBlsKeyTakenByNewPool :: BlsKey -> ImpTestM era ()
    expectBlsKeyTakenByNewPool key = do
      kh <- freshKeyHash
      expectBlsKeyTaken kh key
