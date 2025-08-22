{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.HardForkSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "VRF Keyhashes get populated at v11 HardFork" $ do
    -- Since we're testing the HardFork to 11, the test only makes sense for protocol version 10
    whenMajorVersion @10 $ do
      (kh1, vrf1) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      registerStakePool kh1 vrf1
      (kh2, vrf2) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      registerStakePool kh2 vrf2
      vrf3 <- freshKeyHashVRF
      -- re-register with a new key, so vrf1 should not be present after the hard fork
      registerStakePool kh1 vrf3
      -- register a new pool with an existing vrf
      kh3 <- freshKeyHash
      registerStakePool kh3 vrf2
      -- register and retire a pool before the hard fork, so vrf4 should not be present after the hard fork
      (kh4, vrf4) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      registerStakePool kh4 vrf4
      retireStakePool kh4 (EpochInterval 1)
      -- register and schedule retirement for after the hard fork, so vrf5 should be present after the hard fork
      (kh5, vrf5) <- (,) <$> freshKeyHash <*> freshKeyHashVRF
      registerStakePool kh5 vrf5
      retireStakePool kh5 (EpochInterval 5)

      expectVRFs [] -- VRF keyhashes in PState is not yet populated
      enactHardForkV11
      expectVRFs [vrf2, vrf3, vrf5]
  where
    enactHardForkV11 = do
      modifyPParams $ \pp ->
        pp
          & ppDRepVotingThresholdsL . dvtHardForkInitiationL .~ 0 %! 1
          & ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 0 %! 1
      let pv11 = ProtVer (natVersion @11) 0
      committee <- registerInitialCommittee
      govActionId <- submitGovAction $ HardForkInitiation SNothing pv11
      submitYesVoteCCs_ committee govActionId
      passNEpochs 2
      getProtVer `shouldReturn` pv11
    registerStakePool kh vrf = do
      pps <- registerRewardAccount >>= freshPoolParams kh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [RegPoolTxCert $ pps & ppVrfL .~ vrf]
    retireStakePool kh retirementInterval = do
      curEpochNo <- getsNES nesELL
      let retirement = addEpochInterval curEpochNo retirementInterval
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert kh retirement]
    expectVRFs vrfs =
      psVRFKeyHashes <$> getPState `shouldReturn` Set.fromList vrfs
    getPState = getsNES @era $ nesEsL . esLStateL . lsCertStateL . certPStateL
