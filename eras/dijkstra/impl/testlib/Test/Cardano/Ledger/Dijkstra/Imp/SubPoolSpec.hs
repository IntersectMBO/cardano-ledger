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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraSubPoolPredFailure (..))
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsCertStateL, nesELL, nesEpochStateL)
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure (..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)
import Test.Cardano.Base.Bytes (genByteArray)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

genValidStakePoolParams ::
  ShelleyEraImp era => KeyHash StakePool -> ImpTestM era (StakePoolParams era)
genValidStakePoolParams spKH = do
  stakePoolParams <- arbitrary
  spCost <- getsPParams ppPoolDepositL
  pure $
    stakePoolParams
      & sppIdL .~ spKH
      & sppCostL .~ spCost
      & sppAccountAddressL . accountAddressNetworkIdL .~ Testnet

lookupStakePoolParams ::
  ShelleyEraImp era =>
  KeyHash StakePool ->
  ImpTestM era (Maybe StakePoolState)
lookupStakePoolParams spKH = do
  stakePools <- getsNES $ nesEpochStateL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
  pure $ Map.lookup spKH stakePools

expectStakePoolParams ::
  DijkstraEraImp era => KeyHash StakePool -> Maybe (StakePoolParams era) -> ImpTestM era ()
expectStakePoolParams kh expected = impAnn "expectStakePoolParams" $ do
  actual <- lookupStakePoolParams kh
  impAnn "accountId" $
    (view spsAccountIdL <$> actual)
      `shouldBeExpr` (view (sppAccountAddressL . accountAddressIdL) <$> expected)
  impAnn "blsKey" $ (view spsBlsKeyL <$> actual) `shouldBeExpr` (view sppBlsKeyL <$> expected)
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
    it "Can register a pool" $ do
      spKH <- freshKeyHash
      stakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams
                   ]
          ]
      expectStakePoolParams spKH $ Just stakePoolParams
    it "Fails when registering a new pool with the same VRF" $ do
      stakePoolParams1 <- genValidStakePoolParams =<< freshKeyHash
      let
        vrfKey = stakePoolParams1 ^. sppVrfL
      stakePoolParams2 <- genValidStakePoolParams =<< freshKeyHash
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
            VRFKeyHashAlreadyRegistered (stakePoolParams2 ^. sppIdL) vrfKey
        ]
    it "Fails when registering a pool with an invalid network ID" $ do
      stakePoolParams <- genValidStakePoolParams =<< freshKeyHash
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

    it "Fails when the declared cost is too low" $ do
      stakePoolParams <- genValidStakePoolParams =<< freshKeyHash
      expectedCost <- getsPParams ppMinPoolCostL
      declaredCost <- Coin <$> choose (0, pred $ unCoin expectedCost)
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [ RegPoolTxCert $ stakePoolParams & sppCostL .~ declaredCost
                     ]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure . StakePoolCostTooLowPOOL $
            Mismatch declaredCost expectedCost
        ]

    -- Disabled in conformance for the same reason as the corresponding top-level POOL
    -- test: https://github.com/IntersectMBO/formal-ledger-specifications/issues/1293
    disableInConformanceIt "Fails when the metadata hash is too big" $ do
      stakePoolParams <- genValidStakePoolParams =<< freshKeyHash
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
    it "With completely new parameters" $ do
      spKH <- freshKeyHash
      oldStakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert $ oldStakePoolParams & sppIdL .~ spKH
                   ]
          ]
      newStakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert $ newStakePoolParams & sppIdL .~ spKH
                   ]
          ]
      expectStakePoolParams spKH $ Just oldStakePoolParams
      passEpoch
      expectStakePoolParams spKH $ Just newStakePoolParams
    it "With the same parameters" $ do
      spKH <- freshKeyHash
      oldStakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert $ oldStakePoolParams & sppIdL .~ spKH
                   ]
          ]
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert $ oldStakePoolParams & sppIdL .~ spKH
                   ]
          ]
      expectStakePoolParams spKH $ Just oldStakePoolParams
      passEpoch
      expectStakePoolParams spKH $ Just oldStakePoolParams
    it "Fails when re-registering an existing pool with the same VRF as another one" $ do
      stakePoolParams1 <- genValidStakePoolParams =<< freshKeyHash
      let
        vrfKey = stakePoolParams1 ^. sppVrfL
      stakePoolParams2 <- genValidStakePoolParams =<< freshKeyHash
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
      stakePoolParams <- genValidStakePoolParams =<< freshKeyHash
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
    it "Fails when the declared cost is too low" $ do
      stakePoolParams <- genValidStakePoolParams =<< freshKeyHash
      expectedCost <- getsPParams ppMinPoolCostL
      declaredCost <- Coin <$> choose (0, pred $ unCoin expectedCost)
      submitTxAnn_ "Registering the pool for the first time" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams
                   ]
          ]
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [ RegPoolTxCert $ stakePoolParams & sppCostL .~ declaredCost
                     ]
            ]
        )
        [ injectFailure . DijkstraSubPoolPredFailure . StakePoolCostTooLowPOOL $
            Mismatch declaredCost expectedCost
        ]
    it "Cancels a pending retirement" $ do
      spKH <- freshKeyHash
      stakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert stakePoolParams]
          ]
      currentEpoch <- getsNES nesELL
      let retireEpoch = addEpochInterval currentEpoch $ EpochInterval 1
      submitTxAnn_ "Retire pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert spKH retireEpoch]
          ]
      newStakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Re-register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert newStakePoolParams]
          ]
      passNEpochs 2
      expectStakePoolParams spKH $ Just newStakePoolParams
    it "Fails to register a new pool with a VRF claimed by a re-registration" $ do
      spKH <- freshKeyHash
      stakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert stakePoolParams]
          ]
      newStakePoolParams <- genValidStakePoolParams spKH
      let newVrf = newStakePoolParams ^. sppVrfL
      submitTxAnn_ "Re-register pool with a fresh VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert newStakePoolParams]
          ]
      otherStakePoolParams <- genValidStakePoolParams =<< freshKeyHash
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
      stakePoolParams <- genValidStakePoolParams spKH
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert stakePoolParams]
          ]
      reRegStakePoolParams1 <- genValidStakePoolParams spKH
      submitTxAnn_ "Re-register pool with a fresh VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert reRegStakePoolParams1]
          ]
      reRegStakePoolParams2 <- genValidStakePoolParams spKH
      submitTxAnn_ "Re-register pool with another fresh VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegPoolTxCert reRegStakePoolParams2]
          ]
      otherStakePoolParams <- genValidStakePoolParams =<< freshKeyHash
      submitTxAnn_ "Register a new pool with the released VRF" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert $
                       otherStakePoolParams & sppVrfL .~ (reRegStakePoolParams1 ^. sppVrfL)
                   ]
          ]
      anotherStakePoolParams <- genValidStakePoolParams =<< freshKeyHash
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
  describe "Pool retirement" $ do
    it "Can retire a pool" $ do
      spKH <- freshKeyHash
      stakePoolParams <- genValidStakePoolParams spKH
      currentEpoch <- getsNES nesELL
      EpochNo maxEpoch <- addEpochInterval currentEpoch <$> getsPParams ppEMaxL
      retireEpoch <- EpochNo <$> choose (succ $ unEpochNo currentEpoch, maxEpoch)
      submitTxAnn_ "Register pool" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegPoolTxCert stakePoolParams
                   ]
          ]
      submitTx_ $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RetirePoolTxCert spKH retireEpoch
                   ]
          ]
      let
        numEpochsUntilRetired = fromIntegral $ unEpochNo retireEpoch - unEpochNo currentEpoch
      expectStakePoolParams spKH $ Just stakePoolParams
      passNEpochsChecking (numEpochsUntilRetired - 1) $ expectStakePoolParams spKH $ Just stakePoolParams
      passEpoch
      expectStakePoolParams spKH Nothing
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
      stakePoolParams <- genValidStakePoolParams spKH
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
          [ (1, pure currentEpoch)
          , (49, EpochNo <$> choose (0, unEpochNo currentEpoch))
          , (49, EpochNo <$> choose (unEpochNo maxEpoch, maxBound))
          , (1, pure maxEpoch)
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
