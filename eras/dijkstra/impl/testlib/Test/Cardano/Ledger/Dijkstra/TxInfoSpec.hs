{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerLevelTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  toPlutusTxInfoForPurpose,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..), toAsPurpose)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Inject (..),
  Network (..),
  ProtVer (..),
  TxIx (..),
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceIntervals (..),
  DijkstraEraScript,
 )
import Cardano.Ledger.Dijkstra.State (UTxO (..))
import Cardano.Ledger.Dijkstra.TxBody (receivingScriptTargets)
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..), transTxRedeemersV4)
import Cardano.Ledger.Plutus (
  Datum (..),
  Language (..),
  PlutusArgs (..),
  SLanguage (..),
  TxOutSource (..),
  assocMapToList,
  dataToBinaryData,
  getPlutusData,
  hashPlutusScript,
  plutusLanguage,
  transCoinToValue,
  transCred,
  transSafeHash,
  transScriptHash,
  transTxIx,
 )
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (..))
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Alonzo.Era (mkTestLedgerTxInfo)
import Test.Cardano.Ledger.Common hiding (context, output)
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import qualified Test.Cardano.Ledger.Plutus.Examples as Plutus

spec ::
  forall era.
  ( EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , EraPlutusTxInfo PlutusV4 era
  , Inject (DijkstraContextError era) (ContextError era)
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  , EraUTxO era
  , Arbitrary (Value era)
  , AlonzoEraTxWits era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  Spec
spec = describe "TxInfo" $ do
  let mkLocalLedgerTxInfo utxo tx =
        let ei = epochInfo testGlobals
            ss = systemStart testGlobals
         in mkTestLedgerTxInfo (ProtVer (eraProtVerLow @era) 0) ei ss utxo tx
  prop "legacy missing-input error order is unchanged without protected addresses" $ do
    first <- arbitrary
    second <- arbitrary `suchThat` (/= first)
    let input = max first second
        referenceInput = min first second
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & inputsTxBodyL .~ [input]
              & referenceInputsTxBodyL .~ [referenceInput]
        lti = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
        expected = inject $ Alonzo.TranslationLogicMissingInput @era input
    pure $ do
      toPlutusTxInfoForPurpose SPlutusV1 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
      toPlutusTxInfoForPurpose SPlutusV2 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
      toPlutusTxInfoForPurpose SPlutusV3 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
  prop "V1-V3 reject the Receiving purpose explicitly" $ do
    hash <- arbitrary
    let tx = mkBasicTx @era @TopTx mkBasicTxBody
        lti = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
        purpose = ReceivingPurpose $ AsIxItem 0 hash
        expected = inject $ Alonzo.PlutusPurposeNotSupported @era (ReceivingPurpose $ AsItem hash)
    pure $ do
      toPlutusScriptPurpose SPlutusV1 lti purpose `shouldBeLeft` expected
      toPlutusScriptPurpose SPlutusV2 lti purpose `shouldBeLeft` expected
      toPlutusScriptPurpose SPlutusV3 lti purpose `shouldBeLeft` expected
  describe "protected addresses" $ do
    prop "V4 preserves protection and accounts in outputs, inputs and reference inputs" $ do
      paymentCred <- arbitrary
      accountCred <- arbitrary
      val <- arbitrary
      input <- arbitrary
      referenceInput <- arbitrary `suchThat` (/= input)
      let output = mkBasicTxOut (AddrProtected Testnet paymentCred (StakeRefBase accountCred)) val
          utxo = UTxO [(input, output), (referenceInput, output)]
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [input]
                & referenceInputsTxBodyL .~ [referenceInput]
                & outputsTxBodyL .~ [output]
          lti = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
          expected = PV4.AddressProtected (transCred paymentCred) (Just $ PV4.AccountId $ transCred accountCred)
      pure $ case toPlutusTxInfoForPurpose SPlutusV4 lti (SpendingPurpose AsPurpose) of
        Right info -> do
          map PV4.txOutAddress (PV4.txInfoOutputs info) `shouldBe` [expected]
          map (PV4.txOutAddress . PV4.txInInfoResolved) (PV4.txInfoInputs info) `shouldBe` [expected]
          map (PV4.txOutAddress . PV4.txInInfoResolved) (PV4.txInfoReferenceInputs info) `shouldBe` [expected]
        Left err -> expectationFailure $ "Failed to translate protected V4 context: " <> show err
    prop "V1-V3 reject protected outputs even with a key payment credential" $ do
      keyHash <- arbitrary
      val <- arbitrary
      let output = mkBasicTxOut (AddrProtected Testnet (KeyHashObj keyHash) StakeRefNull) val
          tx = mkBasicTx @era @TopTx $ mkBasicTxBody & outputsTxBodyL .~ [output]
          lti = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          expected = inject $ ProtectedAddressNotSupported @era (TxOutFromOutput $ TxIx 0)
      pure $ do
        toPlutusTxInfoForPurpose SPlutusV1 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
        toPlutusTxInfoForPurpose SPlutusV2 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
        toPlutusTxInfoForPurpose SPlutusV3 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
    prop "V1 hides protected references; V1-V3 reject protected consumed inputs" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      referenceInput <- arbitrary
      let output = mkBasicTxOut (AddrProtected Testnet paymentCred StakeRefNull) val
          utxo = UTxO [(referenceInput, output)]
          tx = mkBasicTx @era @TopTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [referenceInput]
          lti = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
          inputTx = mkBasicTx @era @TopTx $ mkBasicTxBody & inputsTxBodyL .~ [referenceInput]
          inputLti = mkLocalLedgerTxInfo utxo inputTx $ LedgerTopTxInfo mempty
          expected = inject $ ProtectedAddressNotSupported @era (TxOutFromInput referenceInput)
      pure $ do
        toPlutusTxInfoForPurpose SPlutusV1 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
        toPlutusTxInfoForPurpose SPlutusV2 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
        toPlutusTxInfoForPurpose SPlutusV3 lti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
        toPlutusTxInfoForPurpose SPlutusV1 inputLti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
        toPlutusTxInfoForPurpose SPlutusV2 inputLti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
        toPlutusTxInfoForPurpose SPlutusV3 inputLti (SpendingPurpose AsPurpose) `shouldBeLeft` expected
    prop "ordinary references without an inline datum remain supported by V1-V3" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      input <- arbitrary
      let output = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
          utxo = UTxO [(input, output)]
          tx = mkBasicTx @era @TopTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [input]
          lti = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
      pure $ do
        toPlutusTxInfoForPurpose SPlutusV1 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
        toPlutusTxInfoForPurpose SPlutusV2 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
        toPlutusTxInfoForPurpose SPlutusV3 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
    prop "V1 preserves hidden-reference missing, Byron and inline-datum errors" $ do
      paymentCred <- arbitrary
      bootstrap <- arbitrary
      val <- arbitrary
      input <- arbitrary
      datum <- arbitrary
      let ordinary = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
          protected = mkBasicTxOut (AddrProtected Testnet paymentCred StakeRefNull) val
          byron = mkBasicTxOut (AddrBootstrap bootstrap) val
          inline output = output & datumTxOutL .~ Datum (dataToBinaryData datum)
          tx = mkBasicTx @era @TopTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [input]
          translate utxo =
            toPlutusTxInfoForPurpose
              SPlutusV1
              (mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty)
              (SpendingPurpose AsPurpose)
          source = TxOutFromInput input
      pure $ do
        translate mempty `shouldBeLeft` inject (Alonzo.TranslationLogicMissingInput @era input)
        translate (UTxO [(input, byron)]) `shouldBeLeft` inject (ByronTxOutInContext @era source)
        forM_ ([ordinary, protected, byron] :: [TxOut era]) $ \output ->
          translate (UTxO [(input, inline output)])
            `shouldBeLeft` inject (InlineDatumsNotSupported @era source)
    prop "legacy top contexts do not reject protected outputs visible only to a child" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      let output = mkBasicTxOut (AddrProtected Testnet paymentCred StakeRefNull) val
          sub = mkBasicTx @era @SubTx $ mkBasicTxBody & outputsTxBodyL .~ [output]
          tx = mkBasicTx @era @TopTx $ mkBasicTxBody & subTransactionsTxBodyL .~ [sub]
          lti = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
      pure $ do
        toPlutusTxInfoForPurpose SPlutusV1 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
        toPlutusTxInfoForPurpose SPlutusV2 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
        toPlutusTxInfoForPurpose SPlutusV3 lti (SpendingPurpose AsPurpose) `shouldSatisfy` isRight
  describe "PlutusV4" $ do
    prop "shared Receiving translation matches pointer inversion for mixed targets and errors" $ do
      val <- arbitrary
      redeemer <- arbitrary
      exUnits <- arbitrary
      input <- arbitrary
      keyHash <- arbitrary
      let firstPlutus = Plutus.alwaysSucceedsNoDatum SPlutusV4
          secondPlutus = Plutus.inputsOutputsAreNotEmptyNoDatum SPlutusV4
          firstHash = hashPlutusScript firstPlutus
          secondHash = hashPlutusScript secondPlutus
          firstScript = fromPlutusScript $ errorFail $ mkPlutusScript firstPlutus
          secondScript = fromPlutusScript $ errorFail $ mkPlutusScript secondPlutus
          nativeScript = fromNativeScript @era (RequireAllOf [])
          nativeHash = hashScript nativeScript
      unavailableHash <-
        arbitrary `suchThat` (`notElem` ([firstHash, secondHash, nativeHash] :: [ScriptHash]))
      let protected hash = mkBasicTxOut (AddrProtected Testnet (ScriptHashObj hash) StakeRefNull) val
          body =
            mkBasicTxBody
              & inputsTxBodyL .~ [input]
              & guardsTxBodyL .~ [ScriptHashObj firstHash]
              & outputsTxBodyL
                .~ [ protected firstHash
                   , protected nativeHash
                   , protected unavailableHash
                   , protected secondHash
                   , protected firstHash
                   , mkBasicTxOut (Addr Testnet (ScriptHashObj firstHash) StakeRefNull) val
                   , mkBasicTxOut (AddrProtected Testnet (KeyHashObj keyHash) StakeRefNull) val
                   ]
          targets = receivingScriptTargets body
          receivingPtr hash =
            head [ReceivingPurpose (AsIx ix) | AsIxItem ix targetHash <- targets, hash == targetHash]
          redeemers =
            Map.fromList
              [ (SpendingPurpose $ AsIx 0, (redeemer, exUnits))
              , (GuardingPurpose $ AsIx 0, (redeemer, exUnits))
              , (receivingPtr firstHash, (redeemer, exUnits))
              , (receivingPtr secondHash, (redeemer, exUnits))
              ]
          tx =
            mkBasicTx @era @TopTx body
              & witsTxL . scriptTxWitsL
                .~ Map.fromList [(firstHash, firstScript), (secondHash, secondScript), (nativeHash, nativeScript)]
              & witsTxL . rdmrsTxWitsL . unRedeemersL .~ redeemers
          utxo = UTxO [(input, mkBasicTxOut (Addr Testnet (ScriptHashObj firstHash) StakeRefNull) val)]
          lti = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
          compareTranslators info =
            transTxRedeemersV4 info `shouldBe` Babbage.transTxRedeemers SPlutusV4 info
          withPointer ptr =
            mkLocalLedgerTxInfo
              utxo
              (tx & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.singleton ptr (redeemer, exUnits))
              (LedgerTopTxInfo mempty)
          unknownReceiving = ReceivingPurpose $ AsIx $ fromIntegral $ length targets
      pure $ do
        length targets `shouldBe` 4
        Map.size (ltiScriptHashesUsed lti) `shouldBe` 4
        compareTranslators lti
        case transTxRedeemersV4 lti of
          Left err -> expectationFailure $ "Mixed-purpose translation failed: " <> show err
          Right translated -> length (assocMapToList translated) `shouldBe` 4
        forM_
          ( [ receivingPtr nativeHash
            , receivingPtr unavailableHash
            , unknownReceiving
            , SpendingPurpose $ AsIx 1
            , GuardingPurpose $ AsIx 1
            ] ::
              [PlutusPurpose AsIx era]
          )
          $ \ptr -> do
            compareTranslators (withPointer ptr)
            transTxRedeemersV4 (withPointer ptr) `shouldSatisfy` isLeft
        transTxRedeemersV4 (withPointer unknownReceiving)
          `shouldBeLeft` inject (RedeemerPointerPointsToNothing @era unknownReceiving)
    prop "Receiving context carries its executing hash and no implicit datum" $ do
      val <- arbitrary
      redeemer <- arbitrary
      exUnits <- arbitrary
      let plutusScript = Plutus.alwaysSucceedsNoDatum SPlutusV4
          scriptHash = hashPlutusScript plutusScript
          script = errorFail $ mkPlutusScript plutusScript
          output = mkBasicTxOut (AddrProtected Testnet (ScriptHashObj scriptHash) StakeRefNull) val
          tx =
            mkBasicTx @era @TopTx (mkBasicTxBody & outputsTxBodyL .~ [output])
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton (ReceivingPurpose $ AsIx 0) (redeemer, exUnits)
              & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash (fromPlutusScript script)
          lti = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          purpose = ReceivingPurpose $ AsIxItem 0 scriptHash
      pure $ case unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 lti) of
        Left err -> expectationFailure $ "Failed to translate Receiving info: " <> show err
        Right info -> do
          toPlutusScriptPurpose SPlutusV4 lti purpose
            `shouldBe` Right (PV4.Receiving $ transScriptHash scriptHash)
          case toPlutusArgs SPlutusV4 lti info purpose redeemer of
            Left err -> expectationFailure $ "Failed to translate Receiving args: " <> show err
            Right (PlutusV4Args context) -> do
              PV4.scriptContextScriptInfo context `shouldBe` PV4.ReceivingScript
              PV4.scriptContextScriptHash context `shouldBe` transScriptHash scriptHash
              map fst (PV4.protectedOutputsAt (transScriptHash scriptHash) info) `shouldBe` [0]
    prop "top-level Guarding preserves child protected outputs and Receiving hashes" $ do
      val <- arbitrary
      redeemer <- arbitrary
      exUnits <- arbitrary
      let plutusScript = Plutus.alwaysSucceedsNoDatum SPlutusV4
          scriptHash = hashPlutusScript plutusScript
          script = errorFail $ mkPlutusScript plutusScript
          output = mkBasicTxOut (AddrProtected Testnet (ScriptHashObj scriptHash) StakeRefNull) val
          sub =
            mkBasicTx @era @SubTx (mkBasicTxBody & outputsTxBodyL .~ [output])
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton (ReceivingPurpose $ AsIx 0) (redeemer, exUnits)
              & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash (fromPlutusScript script)
          subLti = mkLocalLedgerTxInfo mempty sub $ LedgerSubTxInfo (TxIx 0)
          tx =
            mkBasicTx @era @TopTx
              (mkBasicTxBody & subTransactionsTxBodyL .~ [sub] & guardsTxBodyL .~ [ScriptHashObj scriptHash])
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton (GuardingPurpose $ AsIx 0) (redeemer, exUnits)
              & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash (fromPlutusScript script)
          lti =
            mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo $ Map.singleton (txIdTx sub) (mkTxInfoResult subLti)
          purpose = GuardingPurpose $ AsIxItem 0 scriptHash
          expected = PV4.AddressProtected (transCred $ ScriptHashObj scriptHash) Nothing
      pure $ case unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 lti) of
        Left err -> expectationFailure $ "Failed to translate Guarding info: " <> show err
        Right info -> case toPlutusArgs SPlutusV4 lti info purpose redeemer of
          Left err -> expectationFailure $ "Failed to translate Guarding args: " <> show err
          Right (PlutusV4Args context) -> case PV4.scriptContextScriptInfo context of
            PV4.GuardingScript _ (Just topInfo) -> do
              map PV4.txOutAddress (PV4.ttisOutputs $ PV4.topTxInfoSimplified topInfo) `shouldBe` [expected]
              map (map PV4.txOutAddress . PV4.txInfoOutputs) (PV4.topTxInfoSubTransactions topInfo)
                `shouldBe` [[expected]]
              PV4.ttisRedeemerHashes (PV4.topTxInfoSimplified topInfo) `shouldBe` [transScriptHash scriptHash]
            _ -> expectationFailure "Top-level Guarding has no full batch view"
    prop "Threads the sub-transaction index into txInfoSubTxIx" $ \(txIx :: TxIx) -> do
      let
        tx = mkBasicTx @era @SubTx mkBasicTxBody
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerSubTxInfo txIx
      case unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo) of
        Right txInfo ->
          PV4.txInfoSubTxIx txInfo `shouldBe` Just (transTxIx txIx)
        Left failure ->
          expectationFailure $ "Failed to translate sub-transaction TxInfo: " <> show failure
    prop "Fails translation when Ptr present in outputs" $ do
      paymentCred <- arbitrary
      ptr <- arbitrary
      val <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred (StakeRefPtr ptr)) val
      txIn <- arbitrary
      paymentCred2 <- arbitrary
      stakeRef <- oneof [StakeRefBase <$> arbitrary, pure StakeRefNull]
      let
        utxo =
          UTxO
            [ (txIn, mkBasicTxOut (Addr Testnet paymentCred2 stakeRef) val)
            ]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
        ledgerTxInfo = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
      pure $
        toPlutusTxInfoForPurpose SPlutusV4 ledgerTxInfo (SpendingPurpose AsPurpose)
          `shouldBeLeft` inject (PointerPresentInOutput @era (TxOutFromOutput $ TxIx 0))
    prop "Fails translation when Byron addresses present in outputs" $ do
      ba0 <- arbitrary
      ba2 <- arbitrary
      paymentCred <- arbitrary
      val0 <- arbitrary
      val1 <- arbitrary
      val2 <- arbitrary
      let
        txOuts =
          [ mkBasicTxOut (AddrBootstrap ba0) val0
          , mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val1
          , mkBasicTxOut (AddrBootstrap ba2) val2
          ]
        tx = mkBasicTx @era @TopTx $ mkBasicTxBody & outputsTxBodyL .~ txOuts
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
      pure $
        toPlutusTxInfoForPurpose SPlutusV4 ledgerTxInfo (SpendingPurpose AsPurpose)
          `shouldBeLeft` inject (ByronTxOutInContext @era (TxOutFromOutput $ TxIx 0))
    prop "Reports the first error kind when Ptr and Byron outputs are mixed" $ do
      pc0 <- arbitrary
      pc2 <- arbitrary
      ptr0 <- arbitrary
      ptr2 <- arbitrary
      bootstrapAddr <- arbitrary
      val0 <- arbitrary
      val1 <- arbitrary
      val2 <- arbitrary
      let
        txOuts =
          [ mkBasicTxOut (Addr Testnet pc0 (StakeRefPtr ptr0)) val0
          , mkBasicTxOut (AddrBootstrap bootstrapAddr) val1
          , mkBasicTxOut (Addr Testnet pc2 (StakeRefPtr ptr2)) val2
          ]
        tx = mkBasicTx @era @TopTx $ mkBasicTxBody & outputsTxBodyL .~ txOuts
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
      pure $
        toPlutusTxInfoForPurpose SPlutusV4 ledgerTxInfo (SpendingPurpose AsPurpose)
          `shouldBeLeft` inject (PointerPresentInOutput @era (TxOutFromOutput $ TxIx 0))
    prop "Translates outputs in the order they appear in the TxBody" $ do
      pc0 <- arbitrary
      pc1 <- arbitrary
      pc2 <- arbitrary
      val0 <- arbitrary
      val1 <- arbitrary
      val2 <- arbitrary
      let
        txOuts =
          [ mkBasicTxOut (Addr Testnet pc0 StakeRefNull) val0
          , mkBasicTxOut (Addr Testnet pc1 StakeRefNull) val1
          , mkBasicTxOut (Addr Testnet pc2 StakeRefNull) val2
          ]
        tx = mkBasicTx @era @TopTx $ mkBasicTxBody & outputsTxBodyL .~ txOuts
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
      pure $
        case toPlutusTxInfoForPurpose SPlutusV4 ledgerTxInfo (SpendingPurpose AsPurpose) of
          Right txInfo ->
            map PV4.txOutAddress (PV4.txInfoOutputs txInfo)
              `shouldBe` [PV4.Address (transCred pc) Nothing | pc <- [pc0, pc1, pc2]]
          err -> expectationFailure $ "Failed to translate TxInfo: " <> show err
    describe "toPlutusTxInfo" $ do
      prop "succeeds when purpose points at a script hash" $ do
        paymentCred1 <- arbitrary
        stakeRef1 <- oneof [StakeRefBase <$> arbitrary, pure StakeRefNull]
        stakeRef2 <- oneof [StakeRefBase <$> arbitrary, pure StakeRefNull]
        coin1 <- arbitrary
        coin2 <- arbitrary
        txIn <- arbitrary
        redeemer <- arbitrary
        exUnits <- arbitrary
        let plutusScript = Plutus.alwaysSucceedsNoDatum SPlutusV4
            script = errorFail $ mkPlutusScript plutusScript
        let
          proxy = Proxy @PlutusV4
          scriptHash = hashPlutusScript plutusScript
          paymentCred2 = ScriptHashObj scriptHash
          txOut = mkBasicTxOut (Addr Testnet paymentCred1 stakeRef1) (Val.inject coin1)
          utxo =
            UTxO
              [
                ( txIn
                , mkBasicTxOut (Addr Testnet paymentCred2 stakeRef2) (Val.inject coin2)
                )
              ]
          tx =
            mkBasicTx @era @TopTx
              ( mkBasicTxBody
                  & outputsTxBodyL .~ [txOut]
                  & inputsTxBodyL .~ [txIn]
              )
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton (SpendingPurpose $ AsIx 0) (redeemer, exUnits)
              & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash (fromPlutusScript script)
          lti = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
          purpose = SpendingPurpose @era $ AsIxItem 0 txIn
          TxIn (TxId txIdHash) (TxIx txIx) = txIn
          TxId txBodyHash = txIdTx tx
          txInRef = PV4.TxOutRef (PV4.TxId $ transSafeHash txIdHash) (toInteger txIx)
          transStakeRef (StakeRefBase cred) = Just . PV4.AccountId $ transCred cred
          transStakeRef _ = Nothing
          addr1 = PV4.Address (transCred paymentCred1) (transStakeRef stakeRef1)
          addr2 = PV4.Address (transCred paymentCred2) (transStakeRef stakeRef2)
        pure $ case toPlutusTxInfoForPurpose proxy lti (hoistPlutusPurpose toAsPurpose purpose) of
          Right txInfo ->
            txInfo
              `shouldBe` PV4.TxInfo
                { PV4.txInfoWithdrawals = PV4.unsafeFromList []
                , PV4.txInfoVotes = PV4.unsafeFromList []
                , PV4.txInfoValidRange = PV4.POSIXTimeRange Nothing Nothing
                , PV4.txInfoTxCerts = []
                , PV4.txInfoTreasuryDonation = PV4.Lovelace 0
                , PV4.txInfoSubTxIx = Nothing
                , PV4.txInfoRequiredTopLevelGuards = PV4.unsafeFromList []
                , PV4.txInfoReferenceInputs = []
                , PV4.txInfoRedeemers =
                    PV4.unsafeFromList
                      [
                        ( PV4.Spending (transScriptHash scriptHash) txInRef
                        , PV4.Redeemer . PV4.dataToBuiltinData $ getPlutusData redeemer
                        )
                      ]
                , PV4.txInfoProposalProcedures = []
                , PV4.txInfoOutputs =
                    [ PV4.TxOut
                        addr1
                        (transCoinToValue coin1)
                        PV4.NoOutputDatum
                        Nothing
                    ]
                , PV4.txInfoMint = PV4.emptyMintValue
                , PV4.txInfoInputs =
                    [ PV4.TxInInfo
                        txInRef
                        ( PV4.TxOut
                            addr2
                            (transCoinToValue coin2)
                            PV4.NoOutputDatum
                            Nothing
                        )
                    ]
                , PV4.txInfoId = PV4.TxId $ transSafeHash txBodyHash
                , PV4.txInfoGuards = []
                , PV4.txInfoDirectDeposits = PV4.unsafeFromList []
                , PV4.txInfoData = PV4.unsafeFromList []
                , PV4.txInfoCurrentTreasuryAmount = Nothing
                , PV4.txInfoAccountBalanceIntervals =
                    PV4.AccountBalanceIntervals $ PV4.unsafeFromList []
                }
          Left failure -> expectationFailure $ "Failed to translate TxInfo: " <> show failure
  describe "PlutusV1-V3" $ do
    let plutusV1toV3 :: [SupportedLanguage era]
        plutusV1toV3 =
          [ SupportedLanguage SPlutusV1
          , SupportedLanguage SPlutusV2
          , SupportedLanguage SPlutusV3
          ]
    forM_ plutusV1toV3 $ \(SupportedLanguage slang) -> do
      it "UnsupportedScriptInSubTx" $ do
        let
          tx = mkBasicTx @era @SubTx mkBasicTxBody
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerSubTxInfo (TxIx 0)
          txInfoResult = unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
        txInfoResult
          `shouldBeLeft` inject (UnsupportedScriptInSubTx @era (plutusLanguage slang) (txIdTx tx))
      prop "DirectDepositsNotSupported" $ do
        accountAddr <- arbitrary
        coin <- arbitrary
        let
          dd = DirectDeposits (Map.singleton accountAddr coin)
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & directDepositsTxBodyL .~ dd
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          txInfoResult = unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
        pure $
          txInfoResult `shouldBeLeft` inject (DirectDepositsNotSupported @era dd)
      prop "AccountBalanceIntervalsNotSupported" $ \neAccountBalanceIntervals ->
        let
          abi = AccountBalanceIntervals $ NEM.toMap neAccountBalanceIntervals
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & accountBalanceIntervalsTxBodyL .~ abi
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          txInfoResult = unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult `shouldBeLeft` inject (AccountBalanceIntervalsNotSupported @era abi)
      prop "GuardScriptHashesNotSupported" $ \(scriptHash :: ScriptHash) ->
        let
          neScriptHashes = scriptHash :| []
          guards = OSet.fromList [ScriptHashObj scriptHash]
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & guardsTxBodyL .~ guards
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          txInfoResult = unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult `shouldBeLeft` inject (GuardScriptHashesNotSupported @era neScriptHashes)
      prop "RequiredTopLevelGuardsNotSupported" $ \neRequiredTopLevelGuards ->
        let
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & requiredTopLevelGuardsL .~ NEM.toMap neRequiredTopLevelGuards
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          txInfoResult = unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult
            `shouldBeLeft` inject (RequiredTopLevelGuardsNotSupported @era neRequiredTopLevelGuards)
