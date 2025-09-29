{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.UtxoSpec (
  spec,
  conwayEraSpecificSpec,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.Rules (ConwayUtxosPredFailure (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..))
import Cardano.Ledger.Credential
import Cardano.Ledger.Plutus.Language (
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.UTxO (getShelleyMinFeeTxUtxo)
import Cardano.Ledger.State hiding (balance)
import Cardano.Ledger.Val
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum, inputsOverlapsWithRefInputs)

spec :: forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Certificates" $ do
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/926
    -- TODO: Re-enable after issues are resolved, by removing this override
    disableInConformanceIt "Reg/UnReg collect and refund correct amounts" $ do
      utxoStart <- getUTxO
      accountDeposit <- getsPParams ppKeyDepositL
      stakePoolDeposit <- getsPParams ppPoolDepositL
      dRepDeposit <- getsPParams ppDRepDepositL
      cred0 <- KeyHashObj <$> freshKeyHash @'Staking
      cred1 <- KeyHashObj <$> freshKeyHash @'Staking
      cred2 <- KeyHashObj <$> freshKeyHash @'Staking
      cred3 <- KeyHashObj <$> freshKeyHash @'Staking
      cred4 <- KeyHashObj <$> freshKeyHash @'Staking
      poolId <- freshKeyHash
      poolParams <- freshPoolParams poolId (RewardAccount Testnet cred0)
      dRepCred <- KeyHashObj <$> freshKeyHash @'DRepRole
      let delegatee = DelegStakeVote poolId (DRepCredential dRepCred)
      anchor <- arbitrary
      txRegister <-
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.fromList
                [ RegPoolTxCert poolParams
                , RegDRepTxCert dRepCred dRepDeposit anchor
                , RegDepositDelegTxCert cred0 delegatee accountDeposit
                , RegDepositTxCert cred1 accountDeposit
                , RegDepositTxCert cred2 accountDeposit
                , RegDepositTxCert cred3 accountDeposit
                , UnRegDepositTxCert cred1 accountDeposit
                , UnRegDepositTxCert cred2 accountDeposit
                , RegDepositTxCert cred4 accountDeposit
                ]
      utxoAfterRegister <- getUTxO
      -- Overwrite deposit protocol parameters in order to ensure they does not affect refunds
      modifyPParams
        ( \pp ->
            pp
              & ppKeyDepositL .~ Coin 1
              & ppPoolDepositL .~ Coin 2
              & ppDRepDepositL .~ Coin 3
        )
      (sumUTxO utxoStart <-> sumUTxO utxoAfterRegister)
        `shouldBe` inject
          ( (txRegister ^. bodyTxL . feeTxBodyL)
              <+> ((3 :: Int) <×> accountDeposit) -- Only three accounts retained that are still registered
              <+> stakePoolDeposit
              <+> dRepDeposit
          )
      curEpochNo <- getsNES nesELL
      txUnRegister <-
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.fromList
                [ RetirePoolTxCert poolId (succ curEpochNo)
                , UnRegDRepTxCert dRepCred dRepDeposit
                , UnRegDepositTxCert cred3 accountDeposit
                , UnRegDepositTxCert cred4 accountDeposit
                ]
      utxoAfterUnRegister <- getUTxO
      let totalFees = (txRegister ^. bodyTxL . feeTxBodyL) <+> (txUnRegister ^. bodyTxL . feeTxBodyL)
      fees <- getsNES (nesEsL . esLStateL . lsUTxOStateL . utxosFeesL)
      totalFees `shouldBe` fees
      -- only deposits for stake pool and its account are not refunded at this point
      (sumUTxO utxoStart <-> sumUTxO utxoAfterUnRegister)
        `shouldBe` inject (totalFees <+> stakePoolDeposit <+> accountDeposit)
      passEpoch
      -- Check for successfull pool refund
      getBalance cred0 `shouldReturn` stakePoolDeposit
  describe "Reference scripts" $ do
    let
      nativeScript = RequireSignature @era <$> freshKeyHash
      checkMinFeeUsingRefScripts refScripts = do
        modifyPParams (ppMinFeeRefScriptCostPerByteL .~ (10 %! 1))
        scriptTxIn <- nativeScript >>= impAddNativeScript >>= produceScript
        refIns <- produceRefScripts (NE.fromList refScripts)
        tx <- submitTxWithRefInputs scriptTxIn refIns
        minFeeDiff <- do
          utxo <- getUTxO
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          pure $ getMinFeeTxUtxo pp tx utxo <-> getShelleyMinFeeTxUtxo pp tx
        refScriptFee <- getsPParams ppMinFeeRefScriptCostPerByteL
        -- we check that the difference between conway and shelleyMinFee computation is exactly
        -- the size of the sizes of the reference scripts
        minFeeDiff
          `shouldBe` Coin
            ( floor $
                fromIntegral @Int @Rational (sum $ originalBytesSize <$> refScripts)
                  * unboundRational refScriptFee
            )
      distinctScripts = do
        nativeScripts <- replicateM 3 (fromNativeScript <$> nativeScript)
        Just plutusScriptV2 <- pure $ mkPlutusScript @era $ alwaysSucceedsNoDatum SPlutusV2
        Just plutusScriptV3 <- pure $ mkPlutusScript @era $ alwaysSucceedsNoDatum SPlutusV3
        pure $ nativeScripts ++ [fromPlutusScript plutusScriptV2, fromPlutusScript plutusScriptV3]

    it "required reference script counts towards the minFee calculation" $ do
      spendingScript <- nativeScript
      checkMinFeeUsingRefScripts [fromNativeScript spendingScript]

    it "reference scripts not required for spending the input count towards the minFee calculation" $ do
      spendingScript <- nativeScript
      extraScripts <- distinctScripts
      checkMinFeeUsingRefScripts $ fromNativeScript spendingScript : extraScripts

    it "a scripts referenced several times counts for each reference towards the minFee calculation" $ do
      spendingScript <- nativeScript
      extraScripts <- distinctScripts
      checkMinFeeUsingRefScripts $
        [fromNativeScript spendingScript, fromNativeScript spendingScript]
          ++ extraScripts
          ++ extraScripts

    let scriptHash lang = hashPlutusScript $ inputsOverlapsWithRefInputs lang
    it "Cannot run scripts that expect inputs and refInputs to overlap (PV 9/10)" $ do
      whenMajorVersionAtMost @10 $ do
        txIn <- produceScript $ scriptHash SPlutusV3
        submitFailingTx @era
          (mkTxWithRefInputs txIn (NE.fromList [txIn]))
          [ injectFailure $ BabbageNonDisjointRefInputs [txIn]
          ]
    it "Same script cannot appear in regular and reference inputs in PlutusV3 (PV 11)" $ whenMajorVersionAtLeast @11 $ do
      txIn <- produceScript $ scriptHash SPlutusV3
      submitFailingTx @era
        (mkTxWithRefInputs txIn (NE.fromList [txIn]))
        [ injectFailure $
            CollectErrors [BadTranslation . inject $ ReferenceInputsNotDisjointFromInputs @era [txIn]]
        ]

conwayEraSpecificSpec ::
  forall era.
  ( ConwayEraImp era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
conwayEraSpecificSpec = do
  describe "Certificates" $ do
    it "Reg/UnReg collect and refund correct amounts" $ do
      utxoStart <- getUTxO
      accountDeposit <- getsPParams ppKeyDepositL
      stakePoolDeposit <- getsPParams ppPoolDepositL
      dRepDeposit <- getsPParams ppDRepDepositL
      cred0 <- KeyHashObj <$> freshKeyHash @'Staking
      cred1 <- KeyHashObj <$> freshKeyHash @'Staking
      cred2 <- KeyHashObj <$> freshKeyHash @'Staking
      cred3 <- KeyHashObj <$> freshKeyHash @'Staking
      cred4 <- KeyHashObj <$> freshKeyHash @'Staking
      poolId <- freshKeyHash
      poolParams <- freshPoolParams poolId (RewardAccount Testnet cred0)
      dRepCred <- KeyHashObj <$> freshKeyHash @'DRepRole
      let delegatee = DelegStakeVote poolId (DRepCredential dRepCred)
      anchor <- arbitrary
      txRegister <-
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.fromList
                [ RegPoolTxCert poolParams
                , RegDRepTxCert dRepCred dRepDeposit anchor
                , RegDepositDelegTxCert cred0 delegatee accountDeposit
                , RegTxCert cred1
                , RegDepositTxCert cred2 accountDeposit
                , RegDepositTxCert cred3 accountDeposit
                , UnRegTxCert cred2
                , UnRegDepositTxCert cred1 accountDeposit
                , RegDepositTxCert cred4 accountDeposit
                ]
      utxoAfterRegister <- getUTxO
      -- Overwrite deposit protocol parameters in order to ensure they does not affect refunds
      modifyPParams
        ( \pp ->
            pp
              & ppKeyDepositL .~ Coin 1
              & ppPoolDepositL .~ Coin 2
              & ppDRepDepositL .~ Coin 3
        )
      (sumUTxO utxoStart <-> sumUTxO utxoAfterRegister)
        `shouldBe` inject
          ( (txRegister ^. bodyTxL . feeTxBodyL)
              <+> ((3 :: Int) <×> accountDeposit) -- Only three accounts retained that are still registered
              <+> stakePoolDeposit
              <+> dRepDeposit
          )
      curEpochNo <- getsNES nesELL
      txUnRegister <-
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ SSeq.fromList
                [ RetirePoolTxCert poolId (succ curEpochNo)
                , UnRegDRepTxCert dRepCred dRepDeposit
                , UnRegTxCert cred3
                , UnRegDepositTxCert cred4 accountDeposit
                ]
      utxoAfterUnRegister <- getUTxO
      let totalFees = (txRegister ^. bodyTxL . feeTxBodyL) <+> (txUnRegister ^. bodyTxL . feeTxBodyL)
      fees <- getsNES (nesEsL . esLStateL . lsUTxOStateL . utxosFeesL)
      totalFees `shouldBe` fees
      -- only deposits for stake pool and its account are not refunded at this point
      (sumUTxO utxoStart <-> sumUTxO utxoAfterUnRegister)
        `shouldBe` inject (totalFees <+> stakePoolDeposit <+> accountDeposit)
      passEpoch
      -- Check for successfull pool refund
      getBalance cred0 `shouldReturn` stakePoolDeposit
