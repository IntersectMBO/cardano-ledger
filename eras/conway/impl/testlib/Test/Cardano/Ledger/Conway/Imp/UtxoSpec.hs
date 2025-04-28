{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.TxBody (referenceInputsTxBodyL)
import Cardano.Ledger.Babbage.TxOut (referenceScriptTxOutL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..))
import Cardano.Ledger.MemoBytes (getMemoRawBytes)
import Cardano.Ledger.Plutus.Language (
  Plutus (..),
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.UTxO (getShelleyMinFeeTxUtxo)
import Cardano.Ledger.State (getMinFeeTxUtxo)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
import qualified Data.ByteString.Short as SBS (length)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum, inputsOverlapsWithRefInputs)

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec =
  describe "Reference scripts" $ do
    it "required reference script counts towards the minFee calculation" $ do
      spendingScript <- nativeScript
      checkMinFee spendingScript [fromNativeScript spendingScript]

    it "reference scripts not required for spending the input count towards the minFee calculation" $ do
      spendingScript <- nativeScript
      extraScripts <- distinctScripts
      checkMinFee spendingScript $
        fromNativeScript spendingScript : extraScripts

    it "a scripts referenced several times counts for each reference towards the minFee calculation" $ do
      spendingScript <- nativeScript
      extraScripts <- distinctScripts
      checkMinFee spendingScript $
        [fromNativeScript spendingScript, fromNativeScript spendingScript]
          ++ extraScripts
          ++ extraScripts
    describe "disjoint inputs and reference inputs" $ do
      let
        scriptHash lang = hashPlutusScript $ inputsOverlapsWithRefInputs lang
        mkTestTx :: TxIn -> Tx era
        mkTestTx txIn =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
            & bodyTxL . referenceInputsTxBodyL .~ Set.singleton txIn

      it "Cannot run scripts that expect inputs and refInputs to overlap (PV 9/10)" $ do
        whenMajorVersionAtMost @10 $ do
          txIn <- produceScript $ scriptHash SPlutusV3
          submitFailingTx @era
            (mkTestTx txIn)
            [ injectFailure $ BabbageNonDisjointRefInputs [txIn]
            ]
      it "Same script cannot appear in regular and reference inputs in PlutusV3 (PV 11)" $ whenMajorVersionAtLeast @11 $ do
        txIn <- produceScript $ scriptHash SPlutusV3
        submitFailingTx @era
          (mkTestTx txIn)
          [ injectFailure $
              CollectErrors [BadTranslation . inject $ ReferenceInputsNotDisjointFromInputs @era [txIn]]
          ]
  where
    checkMinFee :: HasCallStack => NativeScript era -> [Script era] -> ImpTestM era ()
    checkMinFee scriptToSpend refScripts = do
      refScriptFee <- setRefScriptFee
      logString "lock an input with a script"
      scriptSpendIn <- createScriptUtxo scriptToSpend
      logString
        "create outputs with reference scripts and the return them mapped to their corresponding inputs"
      refScriptInToScripts <- createRefScriptsUtxos refScripts
      logString "spend the initial input by passing the reference scripts"
      tx <- spendScriptUsingRefScripts scriptSpendIn $ Map.keysSet refScriptInToScripts
      logString
        "compute the difference between the current-era minFee and that computed in pre-Conway eras"
      minFeeDiff <- conwayDiffMinFee tx
      logString "check that the difference is the sum of the sizes of the passed reference scripts"
      minFeeDiff
        `shouldBe` Coin
          ( floor $
              fromIntegral @Int @Rational (sum $ scriptSize <$> refScriptInToScripts)
                * unboundRational refScriptFee
          )

    distinctScripts :: HasCallStack => ImpTestM era [Script era]
    distinctScripts = do
      nativeScripts <-
        (fromNativeScript @era <$>)
          <$> replicateM 3 nativeScript
      let
        psh1 = hashPlutusScript $ alwaysSucceedsNoDatum SPlutusV2
      ps1 <- impAnn "Expecting Plutus script" . expectJust $ impLookupPlutusScript psh1
      let
        psh2 = hashPlutusScript $ alwaysSucceedsNoDatum SPlutusV3
      ps2 <- impAnn "Expecting Plutus script" . expectJust $ impLookupPlutusScript psh2
      let plutusScripts = [fromPlutusScript ps1, fromPlutusScript ps2]
      pure $ nativeScripts ++ plutusScripts

    conwayDiffMinFee :: Tx era -> ImpTestM era Coin
    conwayDiffMinFee tx = do
      utxo <- getUTxO
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      pure $ getMinFeeTxUtxo pp tx utxo <-> getShelleyMinFeeTxUtxo pp tx

    createScriptUtxo :: HasCallStack => NativeScript era -> ImpTestM era TxIn
    createScriptUtxo script = do
      scriptAddr <- addScriptAddr script
      tx <-
        submitTx . mkBasicTx $
          mkBasicTxBody
            & outputsTxBodyL @era
              .~ SSeq.fromList [mkBasicTxOut @era scriptAddr mempty]
      pure $ txInAt (0 :: Int) tx

    createRefScriptsUtxos ::
      HasCallStack => [Script era] -> ImpTestM era (Map.Map TxIn (Script era))
    createRefScriptsUtxos scripts = do
      rootOut <- snd <$> getImpRootTxOut
      let outs =
            scripts
              <&> ( \s ->
                      mkBasicTxOut @era (rootOut ^. addrTxOutL) mempty
                        & referenceScriptTxOutL @era .~ SJust s
                  )
      tx <-
        submitTx . mkBasicTx $
          mkBasicTxBody
            & outputsTxBodyL @era
              .~ SSeq.fromList outs
      let refIns = (`txInAt` tx) <$> [0 .. length scripts - 1]
      pure $ Map.fromList $ refIns `zip` scripts

    spendScriptUsingRefScripts ::
      HasCallStack => TxIn -> Set.Set TxIn -> ImpTestM era (Tx era)
    spendScriptUsingRefScripts scriptIn refIns =
      submitTxAnn "spendScriptUsingRefScripts" . mkBasicTx $
        mkBasicTxBody
          & inputsTxBodyL @era .~ Set.singleton scriptIn
          & referenceInputsTxBodyL @era .~ refIns

    nativeScript :: ImpTestM era (NativeScript era)
    nativeScript = do
      requiredKeyHash <- freshKeyHash
      let script = RequireAllOf (SSeq.singleton (RequireSignature @era requiredKeyHash))
      _ <- impAddNativeScript script
      pure script

    addScriptAddr :: NativeScript era -> ImpTestM era Addr
    addScriptAddr script = do
      scriptHash <- impAddNativeScript script
      stakingKeyHash <- freshKeyHash @'Staking
      pure $ mkAddr scriptHash stakingKeyHash

    scriptSize :: Script era -> Int
    scriptSize = \case
      TimelockScript tl -> SBS.length $ getMemoRawBytes tl
      PlutusScript ps -> withPlutusScript ps (SBS.length . unPlutusBinary . plutusBinary)

    setRefScriptFee :: ImpTestM era NonNegativeInterval
    setRefScriptFee = do
      let refScriptFee = 10 %! 1
      modifyPParams $ ppMinFeeRefScriptCostPerByteL .~ refScriptFee
      pure refScriptFee
