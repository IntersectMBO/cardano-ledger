{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.TxBody (referenceInputsTxBodyL)
import Cardano.Ledger.Babbage.TxOut (referenceScriptTxOutL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.MemoBytes (getMemoRawBytes)
import Cardano.Ledger.Plutus.Language (SLanguage (..), hashPlutusScript, plutusBinary)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.UTxO (getShelleyMinFeeTxUtxo)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (getMinFeeTxUtxo)
import Cardano.Ledger.Val
import qualified Data.ByteString.Short as SBS (length)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkScriptAddr)
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceeds3)

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spec = describe "UTxO" $ do
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
  where
    checkMinFee :: NativeScript era -> [Script era] -> ImpTestM era ()
    checkMinFee scriptToSpend refScripts = do
      refScriptFee <- setRefScriptFee
      logEntry "lock an input with a script"
      scriptSpendIn <- createScriptUtxo scriptToSpend
      logEntry
        "create outputs with reference scripts and the return them mapped to their corresponding inputs"
      refScriptInToScripts <- createRefScriptsUtxos refScripts
      logEntry "spend the initial input by passing the reference scripts"
      tx <- spendScriptUsingRefScripts scriptSpendIn $ Map.keysSet refScriptInToScripts
      logEntry
        "compute the difference between the current-era minFee and that computed in pre-Conway eras"
      minFeeDiff <- conwayDiffMinFee tx
      logEntry "check that the difference is the sum of the sizes of the passed reference scripts"
      minFeeDiff
        `shouldBe` Coin
          ( floor $
              fromIntegral @Int @Rational (sum $ scriptSize <$> refScriptInToScripts)
                * unboundRational refScriptFee
          )

    distinctScripts :: ImpTestM era [Script era]
    distinctScripts = do
      nativeScripts <-
        (fromNativeScript @era <$>)
          <$> replicateM 3 nativeScript
      let
        psh1 = hashPlutusScript $ alwaysSucceeds3 SPlutusV3
      ps1 <- impAnn "Expecting Plutus script" . expectJust $ impLookupPlutusScriptMaybe psh1
      let
        psh2 = hashPlutusScript $ alwaysSucceeds3 SPlutusV3
      ps2 <- impAnn "Expecting Plutus script" . expectJust $ impLookupPlutusScriptMaybe psh2
      let plutusScripts = [fromPlutusScript ps1, fromPlutusScript ps2]
      pure $ nativeScripts ++ plutusScripts

    conwayDiffMinFee :: Tx era -> ImpTestM era Coin
    conwayDiffMinFee tx = do
      utxo <- getUTxO
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      pure $ getMinFeeTxUtxo pp tx utxo <-> getShelleyMinFeeTxUtxo pp tx

    createScriptUtxo :: NativeScript era -> ImpTestM era (TxIn (EraCrypto era))
    createScriptUtxo script = do
      scriptAddr <- addScriptAddr script
      tx <-
        submitTx . mkBasicTx $
          mkBasicTxBody
            & outputsTxBodyL @era
              .~ SSeq.fromList [mkBasicTxOut @era scriptAddr (inject (Coin 1000))]
      pure $ txInAt (0 :: Int) tx

    createRefScriptsUtxos :: [Script era] -> ImpTestM era (Map.Map (TxIn (EraCrypto era)) (Script era))
    createRefScriptsUtxos scripts = do
      rootOut <- snd <$> lookupImpRootTxOut
      let outs =
            scripts
              <&> ( \s ->
                      mkBasicTxOut @era (rootOut ^. addrTxOutL) (inject (Coin 100))
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
      TxIn (EraCrypto era) -> Set.Set (TxIn (EraCrypto era)) -> ImpTestM era (Tx era)
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

    addScriptAddr :: NativeScript era -> ImpTestM era (Addr (EraCrypto era))
    addScriptAddr script = do
      kpStaking1 <- lookupKeyPair =<< freshKeyHash
      scriptHash <- impAddNativeScript script
      pure $ mkScriptAddr scriptHash kpStaking1

    scriptSize :: Script era -> Int
    scriptSize = \case
      TimelockScript tl -> SBS.length $ getMemoRawBytes tl
      PlutusScript ps -> withPlutusScript ps (SBS.length . unPlutusBinary . plutusBinary)

    setRefScriptFee :: ImpTestM era NonNegativeInterval
    setRefScriptFee = do
      let refScriptFee = 10 %! 1
      modifyPParams $ ppMinFeeRefScriptCostPerByteL .~ refScriptFee
      pure refScriptFee
