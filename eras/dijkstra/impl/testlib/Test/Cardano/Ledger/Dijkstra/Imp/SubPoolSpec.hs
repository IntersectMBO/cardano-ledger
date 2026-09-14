{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubPoolSpec (spec) where

import Cardano.Ledger.Address (accountAddressIdL, accountAddressNetworkIdL)
import Cardano.Ledger.BaseTypes (EpochNo (..), Mismatch (..), Network (..), addEpochInterval)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraSubPoolPredFailure (..))
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsCertStateL, nesELL, nesEpochStateL)
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure (..))
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)
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
  describe "Positive" $ do
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
    describe "Can re-register a pool" $ do
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
  describe "Negative" $ do
    describe "Reusing VRF key" $ do
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
  describe "Fails when registering a pool with an invalid network ID" $ do
    it "When registering a fresh pool" $ do
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
  describe "Fails when the declared cost is too low" $ do
    it "When registering a fresh pool" $ do
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
  it "Fails when trying to retire a pool in the current or previous epoch" $ do
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
