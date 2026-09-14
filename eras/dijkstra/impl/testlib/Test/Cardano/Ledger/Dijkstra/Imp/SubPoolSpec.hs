{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubPoolSpec (spec) where

import Cardano.Ledger.Address (accountAddressIdL, accountAddressNetworkIdL)
import Cardano.Ledger.BaseTypes (Network (..))
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
    it "Can re-register a pool" $ do
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
    it "Can retire a pool" $ do
      spKH <- freshKeyHash
      stakePoolParams <- genValidStakePoolParams spKH
      epochNo <- getsNES nesELL
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
                .~ [ RetirePoolTxCert spKH $ succ epochNo
                   ]
          ]
      expectStakePoolParams spKH $ Just stakePoolParams
      passEpoch
      expectStakePoolParams spKH Nothing
  describe "Negative" $ do
    it "Fails when the VRF key is already in use" $ do
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
