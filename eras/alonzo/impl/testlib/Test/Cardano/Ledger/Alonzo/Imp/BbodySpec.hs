{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Imp.BbodySpec (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyPredFailure (TooManyExUnits))
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec ::
  forall era.
  AlonzoEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "BBODY" $ do
  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let
          alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang :: ScriptHash
          alwaysFailsWithDatumHash = hashPlutusScript $ alwaysFailsWithDatum slang :: ScriptHash
          alwaysSucceedsNoDatumHash = hashPlutusScript $ alwaysSucceedsNoDatum slang :: ScriptHash
          alwaysFailsNoDatumHash = hashPlutusScript $ alwaysFailsNoDatum slang :: ScriptHash
          evenRedeemerNoDatumHash = hashPlutusScript $ evenRedeemerNoDatum slang :: ScriptHash

        it "succeeds with eight Plutus scripts" $ do
          accountAddress <- registerStakeCredential $ ScriptHashObj evenRedeemerNoDatumHash
          txCert <- genUnRegTxCert $ ScriptHashObj evenRedeemerNoDatumHash

          withTxsInBlock_ $ do
            impAnn "notValidatingTx" $ do
              txIn <- produceScript alwaysFailsWithDatumHash
              submitPhase2Invalid_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
            impAnn "validatingTx" $ do
              txIn <- produceScript alwaysSucceedsWithDatumHash
              submitTx_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]

            impAnn "notValidatingTxWithMint" $ do
              submitPhase2Invalid_ =<< mkTokenMintingTx alwaysFailsNoDatumHash
            impAnn "validatingTxWithMint" $ do
              submitTx_ =<< mkTokenMintingTx alwaysSucceedsNoDatumHash

            maxExUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL

            let dex i = (Data $ P.I i, maxExUnits)
                rPurpose = mkRewardingPurpose (AsIx 0)
                cPurpose = mkCertifyingPurpose (AsIx 0)

            impAnn "notValidatingTxWithWithdrawal" $ do
              submitPhase2Invalid_ $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . withdrawalsTxBodyL .~ Withdrawals [(accountAddress, mempty)]
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert rPurpose (dex 1)
            impAnn "validatingTxWithWithdrawal" $ do
              submitTx_ $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . withdrawalsTxBodyL .~ Withdrawals [(accountAddress, mempty)]
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert rPurpose (dex 0)

            impAnn "notValidatingTxWithCert" $ do
              submitPhase2Invalid_ $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ [txCert]
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert cPurpose (dex 1)
            impAnn "validatingTxWithCert" $ do
              submitTx_ $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ [txCert]
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert cPurpose (dex 0)

        it "enforces ppMaxBlockExUnits" $ do
          maxBlockUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxBlockExUnitsL
          maxTxUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL

          let
            ExUnits bMem bSteps = maxBlockUnits
            ExUnits tMem tSteps = maxTxUnits
            txCount = 1 + max (bMem `div` tMem) (bSteps `div` tSteps)
            mismatch =
              Mismatch
                { mismatchExpected = maxBlockUnits
                , mismatchSupplied = ExUnits (txCount * tMem) (txCount * tSteps)
                }

          txIns <- replicateM (fromIntegral txCount) $ produceScript alwaysSucceedsWithDatumHash

          let
            purpose = mkSpendingPurpose (AsIx 0)
            dex = (Data (P.I 0), maxTxUnits)
            buildTxs =
              for_ txIns $ \txIn ->
                submitTx_ $
                  mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL .~ [txIn]
                    & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert purpose dex

          withTxsInFailingBlock
            buildTxs
            [injectFailure $ TooManyExUnits mismatch]
