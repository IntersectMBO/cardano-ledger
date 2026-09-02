{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..), toAsPurpose)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
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
 )
import Cardano.Ledger.Dijkstra.State (UTxO (..))
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Plutus (
  Language (..),
  SLanguage (..),
  TxOutSource (..),
  getPlutusData,
  hashPlutusScript,
  plutusLanguage,
  transCoinToValue,
  transCred,
  transSafeHash,
  transScriptHash,
 )
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.Val as Val
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (..))
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Alonzo.Era (mkTestLedgerTxInfo)
import Test.Cardano.Ledger.Common
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
  , DijkstraEraTxBody era
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
  describe "PlutusV4" $ do
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
        ledgerTxInfo = mkLocalLedgerTxInfo utxo tx
      pure $
        (($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
          `shouldBeLeft` inject (PointerPresentInOutput @era (NES.singleton . TxOutFromOutput $ TxIx 0))
    prop "Collects all Ptr sources when multiple outputs have pointers" $ do
      pc0 <- arbitrary
      pc1 <- arbitrary
      pc2 <- arbitrary
      ptr0 <- arbitrary
      ptr2 <- arbitrary
      stakeCred <- arbitrary
      val0 <- arbitrary
      val1 <- arbitrary
      val2 <- arbitrary
      let
        txOuts =
          [ mkBasicTxOut (Addr Testnet pc0 (StakeRefPtr ptr0)) val0
          , mkBasicTxOut (Addr Testnet pc1 (StakeRefBase stakeCred)) val1
          , mkBasicTxOut (Addr Testnet pc2 (StakeRefPtr ptr2)) val2
          ]
        tx = mkBasicTx @era @TopTx $ mkBasicTxBody & outputsTxBodyL .~ txOuts
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
      pure $
        (($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
          `shouldBeLeft` inject
            ( PointerPresentInOutput @era . fromJust $
                NES.fromSet [TxOutFromOutput $ TxIx 0, TxOutFromOutput $ TxIx 2]
            )
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
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
      pure $
        (($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
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
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
      pure $
        (($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
          `shouldBeLeft` inject
            ( PointerPresentInOutput @era . fromJust $
                NES.fromSet [TxOutFromOutput $ TxIx 0, TxOutFromOutput $ TxIx 2]
            )
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
        ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
      pure $
        case ($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo) of
          Right (Right txInfo) ->
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
        let
          proxy = Proxy @PlutusV4
          script = Plutus.alwaysSucceedsNoDatum SPlutusV4
          scriptHash = hashPlutusScript script
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
          lti = mkLocalLedgerTxInfo utxo tx
          purpose = SpendingPurpose @era $ AsIxItem 0 txIn
          TxIn (TxId txIdHash) (TxIx txIx) = txIn
          TxId txBodyHash = txIdTx tx
          txInRef = PV4.TxOutRef (PV4.TxId $ transSafeHash txIdHash) (toInteger txIx)
          transStakeRef (StakeRefBase cred) = Just . PV4.AccountId $ transCred cred
          transStakeRef _ = Nothing
          addr1 = PV4.Address (transCred paymentCred1) (transStakeRef stakeRef1)
          addr2 = PV4.Address (transCred paymentCred2) (transStakeRef stakeRef2)
        pure $ case toPlutusTxInfo proxy lti of
          PlutusTxInfoResult (Right f) ->
            f (hoistPlutusPurpose toAsPurpose purpose)
              `shouldBeRight` PV4.TxInfo
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
          _ -> expectationFailure "Failed to translate TxInfo"
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
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
          txInfoResult =
            ($ SpendingPurpose AsPurpose)
              <$> unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
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
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
          txInfoResult =
            ($ SpendingPurpose AsPurpose)
              <$> unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
        pure $
          txInfoResult `shouldBeLeft` inject (DirectDepositsNotSupported @era dd)
      prop "AccountBalanceIntervalsNotSupported" $ \neAccountBalanceIntervals ->
        let
          abi = AccountBalanceIntervals $ NEM.toMap neAccountBalanceIntervals
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & accountBalanceIntervalsTxBodyL .~ abi
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
          txInfoResult =
            ($ SpendingPurpose AsPurpose)
              <$> unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult `shouldBeLeft` inject (AccountBalanceIntervalsNotSupported @era abi)
      prop "GuardScriptHashesNotSupported" $ \(scriptHash :: ScriptHash) ->
        let
          neScriptHashes = scriptHash :| []
          guards = OSet.fromList [ScriptHashObj scriptHash]
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & guardsTxBodyL .~ guards
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
          txInfoResult =
            ($ SpendingPurpose AsPurpose)
              <$> unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult `shouldBeLeft` inject (GuardScriptHashesNotSupported @era neScriptHashes)
      prop "RequiredTopLevelGuardsNotSupported" $ \neRequiredTopLevelGuards ->
        let
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & requiredTopLevelGuardsL .~ NEM.toMap neRequiredTopLevelGuards
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx
          txInfoResult =
            ($ SpendingPurpose AsPurpose)
              <$> unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult
            `shouldBeLeft` inject (RequiredTopLevelGuardsNotSupported @era neRequiredTopLevelGuards)
