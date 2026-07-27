{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  toPlutusTxCert,
  toPlutusTxInInfo,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TxOutSource (..))
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import Cardano.Ledger.BaseTypes (Exclusive (..), Globals (..), Inclusive (..), Inject (..), Network (..), ProtVer (..), StrictMaybe (..))
import Cardano.Ledger.Conway.TxCert (ConwayGovCert (..))
import qualified Cardano.Ledger.Conway.TxInfo as Conway
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  pattern GuardingPurpose,
 )
import Cardano.Ledger.Dijkstra.State (UTxO (..))
import Cardano.Ledger.Dijkstra.TxCert (DijkstraDelegCert (..), DijkstraTxCert (..))
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Cardano.Ledger.Plutus (
  Language (..),
  PlutusArgs (PlutusV4Args),
  SLanguage (..),
  plutusLanguage,
  transAccountAddress,
  transAccountId,
  transCoinToLovelace,
  transCred,
  transEpochNo,
  transKeyHash,
  transScriptHash,
 )
import Cardano.Ledger.State (StakePoolParams (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (Proxy))
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkCredential, mkKeyPair)
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

spec ::
  forall era.
  ( EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , EraPlutusTxInfo PlutusV4 era
  , Inject (DijkstraContextError era) (ContextError era)
  , DijkstraEraTxBody era
  , EraTx era
  , Arbitrary (Value era)
  ) =>
  Spec
spec = describe "TxInfo" $ do
  describe "PlutusV4" $ do
    let trans :: DijkstraTxCert DijkstraEra -> PV4.TxCert
        trans cert =
          either (error . show) id $
            toPlutusTxCert @'PlutusV4 @DijkstraEra Proxy (ProtVer (eraProtVerLow @era) 0) cert

    prop "TxCerts are correctly translated" $ \(cert :: DijkstraTxCert DijkstraEra) ->
      let pv4Cert = trans cert
       in case (cert, pv4Cert) of
            (DijkstraTxCertDeleg (DijkstraRegCert cred coin), PV4.TxCertRegAccount accId lovelace) ->
              accId === PV4.AccountId (transCred cred)
                .&&. lovelace === transCoinToLovelace coin
            (DijkstraTxCertDeleg (DijkstraUnRegCert cred coin), PV4.TxCertUnRegAccount accId lovelace) ->
              accId === PV4.AccountId (transCred cred)
                .&&. lovelace === transCoinToLovelace coin
            (DijkstraTxCertDeleg (DijkstraDelegCert cred delegatee), PV4.TxCertDelegAccount accId pDelegatee) ->
              accId === PV4.AccountId (transCred cred)
                .&&. pDelegatee === Conway.transDelegatee delegatee
            (DijkstraTxCertDeleg (DijkstraRegDelegCert cred delegatee coin), PV4.TxCertRegAccountDeleg accId pDelegatee lovelace) ->
              accId === PV4.AccountId (transCred cred)
                .&&. pDelegatee === Conway.transDelegatee delegatee
                .&&. lovelace === transCoinToLovelace coin
            (DijkstraTxCertPool (RegPool pp), PV4.TxCertPoolRegister pkh vrfHash) ->
              pkh === transKeyHash (sppId pp)
                .&&. vrfHash === PV1.PubKeyHash (PV1.toBuiltin (hashToBytes (unVRFVerKeyHash (sppVrf pp))))
            (DijkstraTxCertPool (RetirePool poolId epochNo), PV4.TxCertPoolRetire pkh epoch) ->
              pkh === transKeyHash poolId
                .&&. epoch === transEpochNo epochNo
            (DijkstraTxCertGov (ConwayAuthCommitteeHotKey coldCred hotCred), PV4.TxCertAuthHotCommittee pCold pHot) ->
              pCold === Conway.transColdCommitteeCred coldCred
                .&&. pHot === Conway.transHotCommitteeCred hotCred
            (DijkstraTxCertGov (ConwayResignCommitteeColdKey coldCred _anchor), PV4.TxCertResignColdCommittee pCold) ->
              pCold === Conway.transColdCommitteeCred coldCred
            (DijkstraTxCertGov (ConwayRegDRep drepCred deposit _anchor), PV4.TxCertRegDRep pDrep lovelace) ->
              pDrep === Conway.transDRepCred drepCred
                .&&. lovelace === transCoinToLovelace deposit
            (DijkstraTxCertGov (ConwayUnRegDRep drepCred deposit), PV4.TxCertUnRegDRep pDrep lovelace) ->
              pDrep === Conway.transDRepCred drepCred
                .&&. lovelace === transCoinToLovelace deposit
            (DijkstraTxCertGov (ConwayUpdateDRep drepCred _anchor), PV4.TxCertUpdateDRep pDrep) ->
              pDrep === Conway.transDRepCred drepCred
            _ ->
              counterexample ("Mismatched constructors: " <> show cert <> " -> " <> show pv4Cert) $
                property False

    prop "ScriptPurposes are correctly translated" $
      \(purpose :: PlutusPurpose AsIxItem DijkstraEra) (sh :: ScriptHash) ->
        let pv = ProtVer (eraProtVerLow @era) 0
            result = toPlutusScriptPurpose @'PlutusV4 @DijkstraEra Proxy pv sh purpose
            plutusSH = transScriptHash sh
         in case (purpose, result) of
              (SpendingPurpose (AsIxItem _ txIn), Right (PV4.Spending pSH pTxIn)) ->
                pSH === plutusSH
                  .&&. pTxIn === Conway.transTxIn txIn
              (MintingPurpose (AsIxItem _ policyId), Right (PV4.Minting pSH pCS)) ->
                pSH === plutusSH
                  .&&. pCS === Alonzo.transPolicyID policyId
              (CertifyingPurpose (AsIxItem ix txCert), Right (PV4.Certifying pSH pIx pCert)) ->
                pSH === plutusSH
                  .&&. pIx === toInteger ix
                  .&&. pCert === trans txCert
              (WithdrawingPurpose (AsIxItem _ acctAddr), Right (PV4.Withdrawing pSH pCred)) ->
                pSH === plutusSH
                  .&&. pCred === transAccountAddress acctAddr
              (VotingPurpose (AsIxItem _ voter), Right (PV4.Voting pSH pVoter)) ->
                pSH === plutusSH
                  .&&. pVoter === Conway.transVoter voter
              (ProposingPurpose (AsIxItem ix proposal), Right (PV4.Proposing pSH pIx pProposal)) ->
                pSH === plutusSH
                  .&&. pIx === toInteger ix
                  .&&. pProposal === Conway.transProposal (Proxy @'PlutusV4) proposal
              (GuardingPurpose (AsIxItem ix _guardSH), Right (PV4.Guarding pSH pIx)) ->
                pSH === plutusSH
                  .&&. pIx === toInteger ix
              (_, Left _) ->
                property True -- unsupported purpose, that's fine
              _ ->
                counterexample ("Mismatched: " <> show purpose <> " -> " <> show result) $
                  property False

    prop "TxInInfo is correctly translated" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
      pure $ case toPlutusTxInInfo @'PlutusV4 @DijkstraEra Proxy utxo txIn of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right (PV4.TxInInfo pTxOutRef pTxOut) -> do
          pTxOutRef `shouldBe` Conway.transTxIn txIn
          case Babbage.transTxOutV2 @DijkstraEra (TxOutFromInput txIn) txOut of
            Right expectedTxOut -> pTxOut `shouldBe` expectedTxOut
            Left err -> expectationFailure $ "TxOut translation failed: " <> show err

    prop "Direct deposits are translated" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      accountAddr <- arbitrary
      coin <- arbitrary
      let
        dd = DirectDeposits $ Map.singleton accountAddr coin
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & directDepositsTxBodyL .~ dd
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo ->
          PV4.txInfoDirectDeposits txInfo
            `shouldBe` Conway.transMap
              (transAccountId . aaId)
              transCoinToLovelace
              (Map.singleton accountAddr coin)

    prop "Account balance intervals are translated" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      accountId <- arbitrary
      interval <- arbitrary
      let
        abi = AccountBalanceIntervals $ Map.singleton accountId interval
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & accountBalanceIntervalsTxBodyL .~ abi
        expectedInterval = case interval of
          AccountBalanceLowerBound l ->
            PV4.AccountBalanceLowerBound $ transCoinToLovelace $ unInclusive l
          AccountBalanceUpperBound u ->
            PV4.AccountBalanceUpperBound $ transCoinToLovelace $ unExclusive u
          AccountBalanceBothBounds l u ->
            PV4.AccountBalanceBothBounds
              (transCoinToLovelace $ unInclusive l)
              (transCoinToLovelace $ unExclusive u)
          AccountBalanceExact c ->
            PV4.AccountBalanceExact $ transCoinToLovelace c
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo ->
          PV4.txInfoAccountBalanceIntervals txInfo
            `shouldBe` PV4.AccountBalanceIntervals
              (Conway.transMap transAccountId (const expectedInterval) (Map.singleton accountId interval))

    prop "Guard scripts are translated" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      guardCred <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & guardsTxBodyL .~ OSet.singleton guardCred
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo ->
          PV4.txInfoGuards txInfo `shouldBe` [transCred guardCred]

    prop "Required top-level guards are translated" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      guardCred <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & requiredTopLevelGuardsL .~ Map.singleton guardCred SNothing
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo ->
          PV4.txInfoRequiredTopLevelGuards txInfo
            `shouldBe` PV4.unsafeFromList [(transCred guardCred, Nothing)]

    prop "Withdrawals are translated with AccountId" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      accountAddr <- arbitrary
      coin <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & withdrawalsTxBodyL .~ Withdrawals (Map.singleton accountAddr coin)
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo ->
          PV4.txInfoWithdrawals txInfo
            `shouldBe` Conway.transMap
              (transAccountId . aaId)
              transCoinToLovelace
              (Map.singleton accountAddr coin)

    prop "Fee is set for top-level transactions" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      fee <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & feeTxBodyL .~ fee
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo -> do
          PV4.txInfoFee txInfo `shouldBe` transCoinToLovelace fee
          PV4.txInfoSubTxIx txInfo `shouldBe` Nothing

    prop "Treasury donation is translated" $ do
      paymentCred <- arbitrary
      val <- arbitrary
      txIn <- arbitrary
      donation <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & treasuryDonationTxBodyL .~ donation
      pure $ case translateTxInfo @era tx utxo of
        Left err -> expectationFailure $ "Translation failed: " <> show err
        Right txInfo ->
          PV4.txInfoTreasuryDonation txInfo `shouldBe` transCoinToLovelace donation

    prop "ScriptContext is correctly constructed" $
      \(purpose :: PlutusPurpose AsIxItem DijkstraEra) (sh :: ScriptHash) -> do
        paymentCred <- arbitrary
        val <- arbitrary
        txIn <- arbitrary
        let
          pv = ProtVer (eraProtVerLow @era) 0
          txOut = mkBasicTxOut (Addr Testnet paymentCred StakeRefNull) val
          utxo = UTxO [(txIn, txOut)]
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody
                & outputsTxBodyL .~ [txOut]
                & inputsTxBodyL .~ [txIn]
          redeemerData = Data $ PV1.I 0
        pure $ case translateTxInfo @era tx utxo of
          Left _ -> property True -- TxInfo translation failed, skip
          Right txInfo ->
            case toPlutusArgs @'PlutusV4 @DijkstraEra Proxy pv sh txInfo purpose Nothing Nothing redeemerData of
              Left _ -> property False -- toPlutusArgs should not fail
              Right (PlutusV4Args sc) ->
                PV4.scriptContextTxInfo sc === txInfo
                  .&&. PV4.scriptContextRedeemer sc === Babbage.transRedeemer redeemerData
                  .&&. PV4.scriptContextScriptHash sc === transScriptHash sh
                  .&&. checkScriptInfo purpose (PV4.scriptContextScriptInfo sc)

    it "GuardingScript includes sub-transaction infos" $ do
      let
        subTx = mkBasicTx @DijkstraEra @SubTx $ mkBasicTxBody @DijkstraEra @SubTx
        guardSH = ScriptHash $ mkDummyHash (0 :: Int)
        guardCred = ScriptHashObj guardSH
        txIn = mkTxInPartial (TxId $ unsafeMakeSafeHash $ mkDummyHash (1 :: Int)) 0
        txOut = mkBasicTxOut @DijkstraEra (Addr Testnet (mkCredential (mkKeyPair 0 :: KeyPair Payment)) StakeRefNull) (inject $ Coin 1)
        utxo = UTxO [(txIn, txOut)]
        tx =
          mkBasicTx @DijkstraEra @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
              & guardsTxBodyL .~ OSet.singleton guardCred
              & subTransactionsTxBodyL .~ OMap.singleton subTx
        guardPurpose = GuardingPurpose $ AsIxItem 0 guardSH
        redeemerData = Data $ PV1.I 0
        pv = ProtVer (eraProtVerLow @DijkstraEra) 0
      case translateTxInfo @DijkstraEra tx utxo of
        Left err -> expectationFailure $ "TxInfo translation failed: " <> show err
        Right txInfo ->
          case toPlutusArgs @'PlutusV4 @DijkstraEra Proxy pv guardSH txInfo guardPurpose Nothing Nothing redeemerData of
            Left err -> expectationFailure $ "toPlutusArgs failed: " <> show err
            Right (PlutusV4Args sc) ->
              case PV4.scriptContextScriptInfo sc of
                PV4.GuardingScript _ix (Just topTxInfo) ->
                  PV4.topTxInfoSubTransactions topTxInfo `shouldSatisfy` \subTxInfos ->
                    any (\si -> PV4.txInfoId si == Conway.transTxBodyId (subTx ^. bodyTxL)) subTxInfos
                PV4.GuardingScript _ix Nothing ->
                  expectationFailure "GuardingScript should have Just TopTxInfo"
                other ->
                  expectationFailure $ "Expected GuardingScript, got: " <> show other

    prop "Fails translation when Ptr present in outputs" $ do
      paymentCred <- arbitrary
      ptr <- arbitrary
      val <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred (StakeRefPtr ptr)) val
      txIn <- arbitrary
      paymentCred2 <- arbitrary
      stakeRef <- arbitrary
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
        ledgerTxInfo =
          LedgerTxInfo
            { ltiProtVer = ProtVer (eraProtVerLow @era) 0
            , ltiEpochInfo = epochInfo testGlobals
            , ltiSystemStart = systemStart testGlobals
            , ltiUTxO = utxo
            , ltiTx = tx
            , ltiMemoizedSubTransactions = mempty
            }
      pure $
        (fmap fst $ ($ SpendingPurpose AsPurpose) =<< unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
          `shouldBeLeft` inject (PointerPresentInOutput @era [txOut])

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
          ledgerTxInfo =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = epochInfo testGlobals
              , ltiSystemStart = systemStart testGlobals
              , ltiUTxO = mempty
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
          txInfoResult =
            fmap fst $ ($ SpendingPurpose AsPurpose)
              =<< unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
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
          ledgerTxInfo =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = epochInfo testGlobals
              , ltiSystemStart = systemStart testGlobals
              , ltiUTxO = mempty
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
          txInfoResult =
            fmap fst $ ($ SpendingPurpose AsPurpose)
              =<< unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
        pure $
          txInfoResult `shouldBeLeft` inject (DirectDepositsNotSupported @era dd)
      prop "AccountBalanceIntervalsNotSupported" $ \neAccountBalanceIntervals ->
        let
          abi = AccountBalanceIntervals $ NEM.toMap neAccountBalanceIntervals
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & accountBalanceIntervalsTxBodyL .~ abi
          ledgerTxInfo =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = epochInfo testGlobals
              , ltiSystemStart = systemStart testGlobals
              , ltiUTxO = mempty
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
          txInfoResult =
            fmap fst $ ($ SpendingPurpose AsPurpose)
              =<< unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult `shouldBeLeft` inject (AccountBalanceIntervalsNotSupported @era abi)
      prop "GuardScriptHashesNotSupported" $ \(scriptHash :: ScriptHash) ->
        let
          neScriptHashes = scriptHash :| []
          guards = OSet.fromList [ScriptHashObj scriptHash]
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & guardsTxBodyL .~ guards
          ledgerTxInfo =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = epochInfo testGlobals
              , ltiSystemStart = systemStart testGlobals
              , ltiUTxO = mempty
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
          txInfoResult =
            fmap fst $ ($ SpendingPurpose AsPurpose)
              =<< unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult `shouldBeLeft` inject (GuardScriptHashesNotSupported @era neScriptHashes)
      prop "RequiredTopLevelGuardsNotSupported" $ \neRequiredTopLevelGuards ->
        let
          tx =
            mkBasicTx @era @TopTx $
              mkBasicTxBody & requiredTopLevelGuardsL .~ NEM.toMap neRequiredTopLevelGuards
          ledgerTxInfo =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = epochInfo testGlobals
              , ltiSystemStart = systemStart testGlobals
              , ltiUTxO = mempty
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
          txInfoResult =
            fmap fst $ ($ SpendingPurpose AsPurpose)
              =<< unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult
            `shouldBeLeft` inject (RequiredTopLevelGuardsNotSupported @era neRequiredTopLevelGuards)

translateTxInfo ::
  forall era.
  EraPlutusTxInfo 'PlutusV4 era =>
  Tx TopTx era ->
  UTxO era ->
  Either (ContextError era) PV4.TxInfo
translateTxInfo tx utxo =
  let lti =
        LedgerTxInfo
          { ltiProtVer = ProtVer (eraProtVerLow @era) 0
          , ltiEpochInfo = epochInfo testGlobals
          , ltiSystemStart = systemStart testGlobals
          , ltiUTxO = utxo
          , ltiTx = tx
          , ltiMemoizedSubTransactions = mempty
          }
   in fmap fst $ ($ SpendingPurpose AsPurpose) =<< unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 lti)

-- | Check that ScriptInfo matches the purpose it was derived from.
checkScriptInfo :: PlutusPurpose AsIxItem DijkstraEra -> PV4.ScriptInfo -> Property
checkScriptInfo purpose scriptInfo =
  case (purpose, scriptInfo) of
    (SpendingPurpose _, PV4.SpendingScript {}) -> property True
    (MintingPurpose _, PV4.MintingScript {}) -> property True
    (CertifyingPurpose _, PV4.CertifyingScript {}) -> property True
    (WithdrawingPurpose _, PV4.WithdrawingScript {}) -> property True
    (VotingPurpose _, PV4.VotingScript {}) -> property True
    (ProposingPurpose _, PV4.ProposingScript {}) -> property True
    (GuardingPurpose _, PV4.GuardingScript _ _maybeTopTxInfo) ->
      property True
      -- counterexample "GuardingScript should have Just TopTxInfo"
      --   $ isJust maybeTopTxInfo
    _ ->
      counterexample
        ("ScriptInfo doesn't match purpose: " <> show scriptInfo)
        $ property False
