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
  conwayOnlySpec,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Credential
import Cardano.Ledger.Plutus (hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley (ShelleyUtxoPredFailure (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.UTxO (getShelleyMinFeeTxUtxo)
import Cardano.Ledger.State
import Cardano.Ledger.Val
import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsWithDatum, alwaysSucceedsNoDatum)

spec :: forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "Certificates" $ do
    it "Reg/UnReg collect and refund correct amounts" $ do
      utxoStart <- getUTxO
      accountDeposit <- getsPParams ppKeyDepositL
      stakePoolDeposit <- getsPParams ppPoolDepositL
      dRepDeposit <- getsPParams ppDRepDepositL
      cred0 <- KeyHashObj <$> freshKeyHash @Staking
      cred1 <- KeyHashObj <$> freshKeyHash @Staking
      cred2 <- KeyHashObj <$> freshKeyHash @Staking
      cred3 <- KeyHashObj <$> freshKeyHash @Staking
      cred4 <- KeyHashObj <$> freshKeyHash @Staking
      poolId <- freshKeyHash
      poolParams <- freshPoolParams poolId (AccountAddress Testnet (AccountId cred0))
      dRepCred <- KeyHashObj <$> freshKeyHash @DRepRole
      let delegatee = DelegStakeVote poolId (DRepCredential dRepCred)
      anchor <- arbitrary
      -- TODO: Switch to `submitAnyTx` once `addSubTxIn` no longer submits a separate transaction
      -- during fixup. Its fee is not part of `txRegister`'s fee, so the UTxO difference check
      -- below fails
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
    it "DRepNotRegistered" $ do
      dRepCred <- KeyHashObj <$> freshKeyHash
      -- Make sure deposit is positive
      dRepDeposit <- (<> Coin 1) <$> arbitrary
      addr1 <- freshKeyAddr_
      let txAmount = Coin 2000000
      txIn <- sendCoinTo addr1 txAmount
      addr2 <- freshKeyAddr_
      (_, rootTxOut) <- getImpRootTxOut
      let
        rootTxOutValue = rootTxOut ^. valueTxOutL
        txBody =
          mkBasicTxBody
            & inputsTxBodyL .~ [txIn]
            & outputsTxBodyL .~ [mkBasicTxOut addr2 mempty]
        addUnRegDRepTxCert :: Tx TopTx era -> Tx TopTx era
        addUnRegDRepTxCert tx =
          tx
            & bodyTxL . certsTxBodyL <>~ [UnRegDRepTxCert dRepCred dRepDeposit :: TxCert era]
            & witsTxL .~ mkBasicTxWits
      withPostFixup (updateAddrTxWits . addUnRegDRepTxCert) $
        submitFailingTx
          (mkBasicTx txBody)
          [ injectFailure $ Conway.ConwayDRepNotRegistered dRepCred
          , injectFailure $
              Shelley.ValueNotConservedUTxO $
                Mismatch
                  (rootTxOutValue <> inject txAmount <> inject dRepDeposit)
                  (rootTxOutValue <> inject txAmount)
          ]
  describe "Reference scripts" $ do
    let
      nativeScript = RequireSignature @era <$> freshKeyHash
      checkMinFeeUsingRefScripts refScripts = do
        modifyPParams (ppMinFeeRefScriptCostPerByteL .~ (10 %! 1))
        scriptTxIn <- nativeScript >>= impAddNativeScript >>= produceScript
        refIns <- produceRefScripts (NE.fromList refScripts)
        tx <- submitTx $ mkTxWithInputsAndRefInputs [scriptTxIn] (NE.toList refIns)
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
        plutusScripts <- forM (eraLanguages @era) $ \lang -> withSLanguage lang $ \slang -> do
          Just plutusScript <- pure $ mkPlutusScript @era $ alwaysSucceedsNoDatum slang
          pure $ fromPlutusScript plutusScript
        pure $ nativeScripts ++ plutusScripts

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

    it "Reference script sizes contributed by individual transactions" $ do
      script <- nativeScript
      let scriptSize = originalBytesSize script
      scriptSpendIn <- impAddNativeScript script >>= produceScript
      scriptSpendIn2 <- impAddNativeScript script >>= produceScript

      let mkTxWithExpectedSize expectedSize txAction = do
            utxo <- getUTxO
            tx <- txAction
            txNonDistinctRefScriptsSize utxo tx `shouldBe` expectedSize
            pure tx

      -- producing a reference script contributes nothing
      refScriptTx1 <-
        mkTxWithExpectedSize 0 $
          produceRefScriptsTx (fromNativeScript script :| [])
      let refScriptTx1In = txInAt 0 refScriptTx1
      refScriptTx2 <-
        mkTxWithExpectedSize 0 $
          produceRefScriptsTx (fromNativeScript script :| [])
      -- spend script using the reference script
      void $
        mkTxWithExpectedSize scriptSize $
          submitTx $
            mkTxWithInputsAndRefInputs [scriptSpendIn] [refScriptTx1In]
      -- spend using two ref inputs, which are counted separately even though they hold the same script
      void $
        mkTxWithExpectedSize (2 * scriptSize) $
          submitTx $
            mkTxWithInputsAndRefInputs [scriptSpendIn2] [refScriptTx1In, txInAt 0 refScriptTx2]
      -- spend the root utxo
      rootIn <- fst <$> getImpRootTxOut
      void $
        mkTxWithExpectedSize scriptSize $
          submitTx $
            mkTxWithInputsAndRefInputs [rootIn] [refScriptTx1In]
      -- spending the output that holds the reference script also counts it, since the size
      -- is measured over inputs and reference inputs alike
      void $
        mkTxWithExpectedSize scriptSize $
          submitTx $
            mkTxWithInputsAndRefInputs [refScriptTx1In] []

    it "Use a reference script in a collateral output" $ do
      -- produce an utxo with a failing script
      lang <- elements $ eraLanguages @era
      failingPlutusTxIn <- withSLanguage lang $ \slang -> do
        let plutus = alwaysFailsWithDatum slang
        produceScript $ hashPlutusScript plutus

      -- produce a utxo with a succeeding script
      script <- nativeScript
      scriptTxIn <- impAddNativeScript script >>= produceScript
      let scriptSize = originalBytesSize script

      -- prepare a txout with the succeeding script as reference script
      ProtVer pv _ <- getProtVer
      collRefScriptTxOut <- do
        addr <-
          if pv < natVersion @12
            then freshKeyAddr_
            else freshKeyAddrNoPtr_
        pure $ mkBasicTxOut addr mempty & referenceScriptTxOutL .~ pure (fromNativeScript script)

      -- submit an invalid transaction which attempts to consume the failing script
      -- and specifies as collateral return the txout with reference script
      utxo <- getUTxO
      createCollateralTx <-
        -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1029
        -- TODO: Re-enable after issue is resolved, by removing this override
        withDisabledPostSubmitTxHook $
          submitPhase2Invalid $
            mkBasicTx
              ( mkBasicTxBody
                  & inputsTxBodyL .~ [failingPlutusTxIn]
                  & collateralReturnTxBodyL .~ pure collRefScriptTxOut
              )
      -- the transaction that produces the collateral output contributes nothing itself
      txNonDistinctRefScriptsSize utxo createCollateralTx `shouldBe` 0

      -- applying the invalid transaction creates its collateral return, so the reference
      -- script it carries is counted for the next transaction
      utxoWithCollateralReturn <- getUTxO
      let refScriptTxIn = txInAt 1 createCollateralTx
      useCollateralTx <- submitTx $ mkTxWithInputsAndRefInputs [scriptTxIn] [refScriptTxIn]
      txNonDistinctRefScriptsSize utxoWithCollateralReturn useCollateralTx `shouldBe` scriptSize

conwayOnlySpec ::
  forall era.
  ( ConwayEraImp era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
conwayOnlySpec = describe "UTXO" $ do
  describe "Certificates" $ do
    it "Reg/UnReg collect and refund correct amounts" $ do
      utxoStart <- getUTxO
      accountDeposit <- getsPParams ppKeyDepositL
      stakePoolDeposit <- getsPParams ppPoolDepositL
      dRepDeposit <- getsPParams ppDRepDepositL
      cred0 <- KeyHashObj <$> freshKeyHash @Staking
      cred1 <- KeyHashObj <$> freshKeyHash @Staking
      cred2 <- KeyHashObj <$> freshKeyHash @Staking
      cred3 <- KeyHashObj <$> freshKeyHash @Staking
      cred4 <- KeyHashObj <$> freshKeyHash @Staking
      poolId <- freshKeyHash
      poolParams <- freshPoolParams poolId (AccountAddress Testnet (AccountId cred0))
      dRepCred <- KeyHashObj <$> freshKeyHash @DRepRole
      let delegatee = DelegStakeVote poolId (DRepCredential dRepCred)
      anchor <- arbitrary
      txRegister <-
        submitAnyTx $
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
        submitAnyTx $
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
