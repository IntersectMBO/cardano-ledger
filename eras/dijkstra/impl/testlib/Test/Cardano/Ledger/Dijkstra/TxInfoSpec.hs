{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Cardano.Ledger.Allegra.Scripts (invalidBeforeL, invalidHereAfterL)
import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusTxInfo (..),
  LedgerLevelTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  mkTxInfoResult,
  toPlutusTxInfoForPurpose,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), transDatums)
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..), toAsIx, toAsPurpose)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL, unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Inject (..),
  Network (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Conway.Governance (
  GovActionId,
  Vote,
  Voter,
  VotingProcedure (vProcVote),
  foldlVotingProcedures,
 )
import Cardano.Ledger.Conway.TxInfo (
  ConwayEraPlutusTxInfo,
  transMintValue,
  transTxBodyId,
  transTxId,
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceIntervals (..),
 )
import Cardano.Ledger.Dijkstra.State (EraUTxO, ScriptsNeeded, UTxO (..), getScriptsNeeded)
import Cardano.Ledger.Dijkstra.TxInfo (
  DijkstraContextError (..),
  transDirectDeposits,
  transGuardingTopTxInfo,
  transProposal,
  transTxInInfoV4,
  transTxOutV4,
  transValidityInterval,
  transVotingProcedures,
  transWithdrawals,
 )
import qualified Cardano.Ledger.Dijkstra.TxInfo as Dijkstra
import Cardano.Ledger.Mary.Value (AssetName, MaryValue, PolicyID, flattenMultiAsset)
import Cardano.Ledger.Plutus (
  Language (..),
  SLanguage (..),
  TxOutSource (..),
  getPlutusData,
  hashPlutusScript,
  plutusLanguage,
  transCoinToLovelace,
  transCoinToValue,
  transCred,
  transDatum,
  transSafeHash,
  transScriptHash,
  transTxIx,
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.Val as Val
import Control.Monad (zipWithM)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Either (isRight)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (isSJust, isSNothing)
import Data.Monoid (First (..), Sum (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.V3.MintValue as MintValue
import PlutusLedgerApi.V4 (TopTxInfoSimplified (..))
import qualified PlutusLedgerApi.V4 as PV4
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Eq as PlutusTx
import qualified PlutusTx.Functor as PlutusTx
import Test.Cardano.Ledger.Alonzo.Era (mkTestLedgerTxInfo)
import Test.Cardano.Ledger.Alonzo.TxInfoSpec (
  testEpochInfo,
  testSystemStart,
  validBetweenEpochInfo,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (genOrderedPair)
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  genBadValidityInterval,
  genGoodValidityInterval,
 )
import Test.Cardano.Ledger.Dijkstra.Translation.TranslatableGen (genTranslatableTxWithNestedTxs)
import qualified Test.Cardano.Ledger.Plutus.Examples as Plutus

spec :: forall era. (DijkstraEraImp era, ConwayEraPlutusTxInfo PlutusV4 era) => Spec
spec = describe "TxInfo" $ do
  let mkLocalLedgerTxInfo utxo tx =
        let ei = epochInfo testGlobals
            ss = systemStart testGlobals
         in mkTestLedgerTxInfo (ProtVer (eraProtVerLow @era) 0) ei ss utxo tx
  describe "Translation" $ do
    describe "transValidityInterval" $ do
      it "validity interval's upper bound is open" transVITimeUpperBoundIsOpen
      prop "translates successfully if bounds are valid" $ do
        (hi, lo) <- genOrderedPair
        validityInterval <- genGoodValidityInterval lo hi
        pure $
          Dijkstra.transValidityInterval
            (Proxy @era)
            (validBetweenEpochInfo lo hi)
            testSystemStart
            validityInterval
            `shouldSatisfy` isRight
      prop "fails to translate if the bounds are invalid" $ do
        (hi, lo) <- genOrderedPair
        validityInterval <- genBadValidityInterval lo hi
        pure $
          Dijkstra.transValidityInterval
            (Proxy @era)
            (validBetweenEpochInfo lo hi)
            testSystemStart
            validityInterval
            `shouldBeLeft` inject (TimeTranslationPastHorizon @era "out of valid range")
  describe "PlutusV4" $ do
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
          ledgerTxInfo = mkLocalLedgerTxInfo utxo tx $ LedgerTopTxInfo mempty
          purpose = SpendingPurpose @era $ AsIxItem 0 txIn
          TxIn (TxId txIdHash) (TxIx txIx) = txIn
          TxId txBodyHash = txIdTx tx
          txInRef = PV4.TxOutRef (PV4.TxId $ transSafeHash txIdHash) (toInteger txIx)
          transStakeRef (StakeRefBase cred) = Just . PV4.AccountId $ transCred cred
          transStakeRef _ = Nothing
          addr1 = PV4.Address (transCred paymentCred1) (transStakeRef stakeRef1)
          addr2 = PV4.Address (transCred paymentCred2) (transStakeRef stakeRef2)
        pure $ case toPlutusTxInfoForPurpose proxy ledgerTxInfo (hoistPlutusPurpose toAsPurpose purpose) of
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
      describe "transGuardingTopTxInfo" $ do
        prop
          "TopTxInfo construction matches a reference construction built from the ledger values"
          (propTopTxInfoMatchesReference @era)

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
              mkBasicTxBody & requiredTopLevelGuardsTxBodyL .~ NEM.toMap neRequiredTopLevelGuards
          ledgerTxInfo = mkLocalLedgerTxInfo mempty tx $ LedgerTopTxInfo mempty
          txInfoResult = unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
         in
          txInfoResult
            `shouldBeLeft` inject
              (RequiredTopLevelGuardsNotSupported @era $ NEM.keysSet neRequiredTopLevelGuards)

-- | The test checks that since protocol version 9 'transVITime' works correctly,
-- by returning open upper bound of the validaty interval.
transVITimeUpperBoundIsOpen :: Expectation
transVITimeUpperBoundIsOpen = do
  let interval = ValidityInterval SNothing (SJust (SlotNo 40))
  Dijkstra.transValidityInterval (Proxy @DijkstraEra) testEpochInfo testSystemStart interval
    `shouldBeRight` PV4.POSIXTimeRange Nothing (Just $ PV4.POSIXTime 40000)

-- | Compare the 'PV4.TopTxInfo' built by 'transGuardingTopTxInfo' against a
-- reference implementation built from the ledger values of the batch.
--
-- 'PV4.topTxInfoStartingAccountBalanceIntervals' is not checked: it is not
-- aggregated across the batch of transactions, but it is the translation of a
-- field that only exists on the top-level transaction. Therefore, the
-- translation test will be left to the unit tests of
-- 'PV4.txInfoAccountBalanceIntervals'.
propTopTxInfoMatchesReference ::
  forall era.
  ( DijkstraEraTxBody era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraTxWits era
  , Value era ~ MaryValue
  , ConwayEraPlutusTxInfo PlutusV4 era
  , Inject (AlonzoContextError era) (ContextError era)
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (DijkstraContextError era) (ContextError era)
  , Arbitrary (Tx TopTx era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) =>
  Property
propTopTxInfoMatchesReference =
  checkCoverage . forAll genBatchWithGuardingScriptHash $ \(topTx, utxo, guardingScriptHash) ->
    checkNestedTxsCoverage topTx utxo
      . checkGuardDatumsCoverage topTx guardingScriptHash
      $ do
        let mkLedgerTxInfo :: Tx l era -> LedgerLevelTxInfo l era -> LedgerTxInfo l era
            mkLedgerTxInfo =
              mkTestLedgerTxInfo (ProtVer (eraProtVerLow @era) 0) testEpochInfo testSystemStart utxo
            mkSubLedgerTxInfo txIx subTx = mkLedgerTxInfo subTx (LedgerSubTxInfo txIx)
            subTxsWithIx = zip [TxIx 0 ..] (OMap.elems (topTx ^. bodyTxL . subTransactionsTxBodyL))
            subTxInfoResults =
              Map.fromList
                [ (txIdTx subTx, mkTxInfoResult (mkSubLedgerTxInfo txIx subTx))
                | (txIx, subTx) <- subTxsWithIx
                ]
            ledgerTxInfo = mkLedgerTxInfo topTx (LedgerTopTxInfo subTxInfoResults)
        txInfo <- expectRight $ unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo)
        topTxInfo <- expectRight $ transGuardingTopTxInfo SPlutusV4 txInfo guardingScriptHash ledgerTxInfo
        expectedTopTxInfoSimplified <- expectRight $ mkReferenceTopTxInfoSimplified ledgerTxInfo
        PV4.topTxInfoSimplified topTxInfo `shouldMatchTopTxInfoSimplified` expectedTopTxInfoSimplified
        -- The sub-transactions are translated directly rather than looked up by 'TxId' in
        -- 'subTxInfoResults', so this checks the lookup and that the 'OMap' order is kept.
        expectedSubTxInfos <-
          expectRight $
            traverse
              (\(txIx, subTx) -> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 (mkSubLedgerTxInfo txIx subTx)))
              subTxsWithIx
        PV4.topTxInfoSubTransactions topTxInfo `shouldBe` expectedSubTxInfos
        shouldHaveUniqueAssocMapKeys (PV4.topTxInfoDatums topTxInfo)
        assocMapToMap (PV4.topTxInfoDatums topTxInfo)
          `shouldBe` mkReferenceGuardDatums guardingScriptHash topTx
  where
    -- A batch together with the guarding script hash whose 'PV4.TopTxInfo' is built. The hash
    -- is usually one of the script hashes in the required top-level guards of the batch, so
    -- that 'PV4.topTxInfoDatums' is not empty, and otherwise arbitrary.
    genBatchWithGuardingScriptHash :: Gen (Tx TopTx era, UTxO era, ScriptHash)
    genBatchWithGuardingScriptHash = do
      (topTx, utxo) <- genTranslatableTxWithNestedTxs @era
      let requiredGuardScriptHashes =
            [ sh
            | ScriptHashObj sh <-
                Set.toList $
                  Map.keysSet (topTx ^. bodyTxL . requiredTopLevelGuardsTxBodyL)
                    <> foldMap (Map.keysSet . (^. bodyTxL . requiredTopLevelGuardsTxBodyL)) (getSubTransactions topTx)
            ]
      guardingScriptHash <-
        case requiredGuardScriptHashes of
          [] -> arbitrary
          _ -> frequency [(9, elements requiredGuardScriptHashes), (1, arbitrary)]
      pure (topTx, utxo, guardingScriptHash)

    -- The datum of the guarding script in the required top-level guards of each transaction of
    -- the batch, keyed by the 'TxId' of that transaction.
    mkReferenceGuardDatums :: ScriptHash -> Tx TopTx era -> Map.Map PV4.TxId PV4.Datum
    mkReferenceGuardDatums guardingScriptHash topTx =
      Map.fromList
        [ (transTxId txId, transDatum datum)
        | (txId, guards) <-
            (txIdTx topTx, topTx ^. bodyTxL . requiredTopLevelGuardsTxBodyL)
              : [ (txIdTx subTx, subTx ^. bodyTxL . requiredTopLevelGuardsTxBodyL) | subTx <- getSubTransactions topTx
                ]
        , SJust datum <- [Map.findWithDefault SNothing (ScriptHashObj guardingScriptHash) guards]
        ]

    -- Require enough batches where the guarding script has a datum in the
    -- top-level transaction, and in a sub-transaction, since production looks
    -- these up separately, so that 'PV4.topTxInfoDatums' is tested by including
    -- datums from more than one transaction. Also require generated batch to
    -- sometimes have no transaction with a datum for the provided guarding script.
    checkGuardDatumsCoverage :: Testable prop => Tx TopTx era -> ScriptHash -> prop -> Property
    checkGuardDatumsCoverage topTx guardingScriptHash =
      let hasGuardDatum :: Tx l era -> Bool
          hasGuardDatum tx =
            isSJust $
              Map.findWithDefault
                SNothing
                (ScriptHashObj guardingScriptHash)
                (tx ^. bodyTxL . requiredTopLevelGuardsTxBodyL)
          datumCount = Map.size (mkReferenceGuardDatums guardingScriptHash topTx)
       in cover 5 (datumCount == 0) "Guarding script has no datum in any transaction"
            . cover 10 (hasGuardDatum topTx) "Guarding script has a datum in the top-level transaction"
            . cover
              10
              (any hasGuardDatum (getSubTransactions topTx))
              "Guarding script has a datum in a sub-transaction"
            . cover 2 (datumCount >= 2) "Guarding script has a datum in two or more transactions"

    -- Reports how many sub-transactions are generated, and require enough
    -- batches where the fields aggregated across the batch are set in more than
    -- one transaction, since that is where the aggregation strategies of
    -- 'transGuardingTopTxInfo' can disagree.
    checkNestedTxsCoverage :: Testable prop => Tx TopTx era -> UTxO era -> prop -> Property
    checkNestedTxsCoverage topTx utxo =
      let subTxs = getSubTransactions topTx
       in tabulate "Number of sub-transactions" [show (length subTxs)]
            . cover 5 (null subTxs) "Has no sub-transactions"
            . cover 15 (length subTxs >= 2) "Has at least two sub-transactions"
            . coverConcatenatedFields topTx
            . coverMints topTx
            . coverAccounts topTx
            . coverValidityIntervals topTx
            . coverGuards topTx
            . coverDatums topTx
            . coverRedeemers topTx utxo
            . coverVotes topTx
            . coverTreasury topTx

    -- Fields concatenated across the batch
    coverConcatenatedFields :: Testable p => Tx TopTx era -> p -> Property
    coverConcatenatedFields topTx =
      coverNonEmptyValueInNestedTxs topTx 25 "inputs" (^. bodyTxL . inputsTxBodyL)
        . coverNonEmptyValueInNestedTxs topTx 25 "reference inputs" (^. bodyTxL . referenceInputsTxBodyL)
        . coverSharedKeysInNestedTxs topTx 25 "reference inputs" (^. bodyTxL . referenceInputsTxBodyL)
        . coverNonEmptyValueInNestedTxs topTx 25 "outputs" (^. bodyTxL . outputsTxBodyL)
        . coverNonEmptyValueInNestedTxs topTx 25 "certs" (^. bodyTxL . certsTxBodyL)
        . coverNonEmptyValueInNestedTxs topTx 25 "proposals" (^. bodyTxL . proposalProceduresTxBodyL)

    coverMints :: Testable p => Tx TopTx era -> p -> Property
    coverMints topTx = do
      let
        mintedAssetIdsWith :: (Integer -> Bool) -> Tx l era -> Set (PolicyID, AssetName)
        mintedAssetIdsWith p tx =
          Set.fromList
            [ (pid, an)
            | (pid, an, q) <- flattenMultiAsset (tx ^. bodyTxL . mintTxBodyL)
            , p q
            ]
      coverSharedKeysInNestedTxs topTx 25 "minted asset ids" (mintedAssetIdsWith (> 0))
        . coverSharedKeysInNestedTxs topTx 25 "burned asset ids" (mintedAssetIdsWith (< 0))
        . cover
          10
          ( let mintedAssetIds = mconcat $ mapTxBatch topTx (mintedAssetIdsWith (> 0))
                burnedAssetIds = mconcat $ mapTxBatch topTx (mintedAssetIdsWith (< 0))
             in not (Set.disjoint mintedAssetIds burnedAssetIds)
          )
          "Same asset id minted in one tx and burned in another"
        . cover
          10
          ( let mintedAssetIdsPerTx = mapTxBatch topTx (mintedAssetIdsWith (> 0))
                burnedAssetIdsPerTx = mapTxBatch topTx (mintedAssetIdsWith (< 0))
             in or
                  [ mintedPolicyId == burnedPolicyId && mintedAssetName /= burnedAssetName
                  | (i, minted) <- zip [0 :: Int ..] mintedAssetIdsPerTx
                  , (j, burned) <- zip [0 ..] burnedAssetIdsPerTx
                  , i /= j
                  , (mintedPolicyId, mintedAssetName) <- Set.toList minted
                  , (burnedPolicyId, burnedAssetName) <- Set.toList burned
                  ]
          )
          "Same policy id, different asset names, minted in one tx and burned in another"

    -- Withdrawals and direct deposits
    coverAccounts :: Testable p => Tx TopTx era -> p -> Property
    coverAccounts topTx =
      coverNonEmptyValueInNestedTxs topTx 25 "withdrawals" (^. bodyTxL . withdrawalsTxBodyL)
        . coverSharedKeysInNestedTxs
          topTx
          25
          "withdrawals"
          (Map.keysSet . unWithdrawals . (^. bodyTxL . withdrawalsTxBodyL))
        . coverNonEmptyValueInNestedTxs topTx 25 "direct deposits" (^. bodyTxL . directDepositsTxBodyL)
        . coverSharedKeysInNestedTxs
          topTx
          25
          "direct deposits"
          (Map.keysSet . unDirectDeposits . (^. bodyTxL . directDepositsTxBodyL))

    coverValidityIntervals :: Testable p => Tx TopTx era -> p -> Property
    coverValidityIntervals topTx = do
      let subTxs = getSubTransactions topTx
          allTxVldtIntervals = mapTxBatch topTx (^. bodyTxL . vldtTxBodyL)
      coverNonEmptyValueInNestedTxs topTx 25 "validity intervals" (^. bodyTxL . vldtTxBodyL)
        . cover
          3
          ( all (isSNothing . invalidHereafter) allTxVldtIntervals
              && any (isSJust . invalidBefore) allTxVldtIntervals
          )
          "Validity interval with no upper bounds in tx batch, but at least one top or sub tx with a lower bound"
        . cover
          3
          ( all (isSNothing . invalidBefore) allTxVldtIntervals
              && any (isSJust . invalidHereafter) allTxVldtIntervals
          )
          "Validity interval with no lower bounds in tx batch, but at least one top or sub txs with an upper bound"
        . cover
          5
          ( any
              ( \subTx ->
                  subTx ^. bodyTxL . vldtTxBodyL . invalidBeforeL > topTx ^. bodyTxL . vldtTxBodyL . invalidBeforeL
              )
              subTxs
          )
          "Strictly greatest lower bound comes from a sub tx"
        . cover
          5
          (Set.size (Set.fromList [slot | SJust slot <- fmap invalidBefore allTxVldtIntervals]) >= 2)
          "Lower bound set in two or more transactions, with different slots"
        . cover
          5
          (Set.size (Set.fromList [slot | SJust slot <- fmap invalidHereafter allTxVldtIntervals]) >= 2)
          "Upper bound set in two or more transactions, with different slots"
        . cover
          5
          ( any
              ( \subTx ->
                  (subTx ^. bodyTxL . vldtTxBodyL . invalidHereAfterL)
                    `isUpperBoundBelow` (topTx ^. bodyTxL . vldtTxBodyL . invalidHereAfterL)
              )
              subTxs
          )
          "Strictly lowest upper bound comes from a sub tx"
        . cover
          1
          (mconcat allTxVldtIntervals `notElem` allTxVldtIntervals)
          "Greatest lower bound and smallest upper bound come from two different txs"

    -- Guards and required top-level guards
    coverGuards :: Testable p => Tx TopTx era -> p -> Property
    coverGuards topTx =
      coverNonEmptyValueInNestedTxs topTx 25 "guards" (^. bodyTxL . guardsTxBodyL)
        . coverSharedKeysInNestedTxs topTx 25 "guards" (OSet.toSet . (^. bodyTxL . guardsTxBodyL))
        . coverNonEmptyValueInNestedTxs
          topTx
          25
          "required top-level guards"
          (^. bodyTxL . requiredTopLevelGuardsTxBodyL)
        . coverSharedKeysInNestedTxs
          topTx
          25
          "required top-level guards"
          (Map.keysSet . (^. bodyTxL . requiredTopLevelGuardsTxBodyL))

    coverDatums :: Testable p => Tx TopTx era -> p -> Property
    coverDatums topTx =
      coverNonEmptyValueInNestedTxs topTx 25 "datums" (^. witsTxL . datsTxWitsL)
        . coverSharedKeysInNestedTxs topTx 25 "datums" (Map.keysSet . unTxDats . (^. witsTxL . datsTxWitsL))

    coverRedeemers :: Testable p => Tx TopTx era -> UTxO era -> p -> Property
    coverRedeemers topTx utxo =
      coverNonEmptyValueInNestedTxs topTx 25 "redeemers" (^. witsTxL . rdmrsTxWitsL)
        . coverSharedKeysInNestedTxs
          topTx
          15
          "redeemer script hashes"
          (Set.fromList . scriptHashesPerRedeemer utxo)
        . cover
          25
          ( any
              (\hs -> length hs > Set.size (Set.fromList hs))
              (mapTxBatch topTx (scriptHashesPerRedeemer utxo))
          )
          "Same script executed for two purposes in one transaction"

    coverVotes :: Testable p => Tx TopTx era -> p -> Property
    coverVotes topTx = do
      let
        subTxs = getSubTransactions topTx
        -- The `Vote` cast on each `(Voter, GovActionId)` pair by a transaction
        getTxVotes :: Tx l era -> Map.Map (Voter, GovActionId) Vote
        getTxVotes tx =
          foldlVotingProcedures
            (\acc voter gaid vp -> Map.insert (voter, gaid) (vProcVote vp) acc)
            mempty
            (tx ^. bodyTxL . votingProceduresTxBodyL)
        votesPerTx = mapTxBatch topTx getTxVotes
      coverNonEmptyValueInNestedTxs topTx 25 "votes" (^. bodyTxL . votingProceduresTxBodyL)
        . cover
          15
          ( any
              ((> 1) . Set.size)
              (Map.unionsWith (<>) (fmap (fmap Set.singleton) votesPerTx))
          )
          "Same voter and governance action with different votes in more than one transaction"
        . cover
          15
          ( or
              [ voter == voter' && gaid /= gaid'
              | (i, votes) <- zip [0 :: Int ..] votesPerTx
              , (j, votes') <- zip [0 ..] votesPerTx
              , i /= j
              , (voter, gaid) <- Map.keys votes
              , (voter', gaid') <- Map.keys votes'
              ]
          )
          "Same voter, different governance actions, across transactions"
        . cover
          15
          (any (or . Map.intersectionWith (/=) (getTxVotes topTx) . getTxVotes) subTxs)
          "Same voter and governance action in the top transaction and a sub transaction, with different votes"
        . cover
          15
          ( let subTxVoteOccurrences =
                  Map.unionsWith (+) [fmap (const (1 :: Int)) (getTxVotes subTx) | subTx <- subTxs]
             in any (\(k, n) -> n >= 2 && k `Map.notMember` getTxVotes topTx) $
                  Map.toList subTxVoteOccurrences
          )
          "Same voter and governance action in two or more sub transactions, but not in the top transaction"

    -- Current treasury amount and treasury donations
    coverTreasury :: Testable p => Tx TopTx era -> p -> Property
    coverTreasury topTx =
      let subTxs = getSubTransactions topTx
       in cover
            0.5
            ( isSNothing (topTx ^. bodyTxL . currentTreasuryValueTxBodyL)
                && all (\subTx -> isSNothing $ subTx ^. bodyTxL . currentTreasuryValueTxBodyL) subTxs
            )
            "Current treasury amount absent in top tx and sub txs"
            . cover
              5
              ( isSJust (topTx ^. bodyTxL . currentTreasuryValueTxBodyL)
                  && all (\subTx -> isSNothing $ subTx ^. bodyTxL . currentTreasuryValueTxBodyL) subTxs
              )
              "Current treasury amount present in top tx, but not in sub txs"
            . cover
              3
              ( isSNothing (topTx ^. bodyTxL . currentTreasuryValueTxBodyL)
                  && any (\subTx -> isSJust $ subTx ^. bodyTxL . currentTreasuryValueTxBodyL) subTxs
              )
              "Current treasury amount absent in top tx, but present in at least one sub txs"
            . cover
              5
              ( isSJust (topTx ^. bodyTxL . currentTreasuryValueTxBodyL)
                  && any (\subTx -> isSJust $ subTx ^. bodyTxL . currentTreasuryValueTxBodyL) subTxs
              )
              "Current treasury amount present in top tx, and present in at least one sub txs"
            . cover
              10
              ( Set.size
                  (Set.fromList [amount | SJust amount <- fmap (^. bodyTxL . currentTreasuryValueTxBodyL) subTxs])
                  >= 2
              )
              "Current treasury amount present in two or more sub txs, with different values"
            . coverNonEmptyValueInNestedTxs topTx 25 "treasury donations" (^. bodyTxL . treasuryDonationTxBodyL)

    -- Require at least @rate@ percent of the generated batches to have a non-empty
    -- value of the field extracted by @f@ in more than one transaction
    -- (top-level + sub-transactions).
    --
    -- This is particularly useful for tests which test the aggregation of
    -- values across transactions. In such a scenario, a transaction field set
    -- in only one transaction would never be combined with another. If that
    -- happens, then we won't be testing different aggregation strategies when
    -- constructing `TopTxInfoSimplified`.
    coverNonEmptyValueInNestedTxs ::
      (Testable p, Eq a, Monoid a) =>
      Tx TopTx era ->
      Double ->
      String ->
      (forall l. Tx l era -> a) ->
      p ->
      Property
    coverNonEmptyValueInNestedTxs topTx rate name f =
      cover rate (length (filter (/= mempty) (mapTxBatch topTx f)) >= 2) $
        "Non-empty " <> name <> " in more than one transaction"

    -- Require at least @rate@ percent of the generated batches to have a
    -- key, extracted by @f@, shared by at least two transactions of the
    -- transaction batch (top-level + sub-transactions), while no single
    -- transaction holds every key of the batch.
    coverSharedKeysInNestedTxs ::
      (Testable p, Ord a) =>
      Tx TopTx era ->
      Double ->
      String ->
      (forall l. Tx l era -> Set a) ->
      p ->
      Property
    coverSharedKeysInNestedTxs topTx rate name f =
      let keySetPerTx = mapTxBatch topTx f
          allKeys = Set.unions keySetPerTx
          hasSharedKey = sum (fmap Set.size keySetPerTx) > Set.size allKeys
          noTxHoldsAllKeys = notElem allKeys keySetPerTx
       in cover rate (hasSharedKey && noTxHoldsAllKeys) $
            "Keys of " <> name <> " are shared in more than one transaction, none holding them all"

    -- Construct a reference 'PV4.TopTxInfoSimplified' out of the 'LedgerTxInfo' of a top-level
    -- transaction. The idea is to compare this with the production code by using a different way of
    -- constructing each 'PV4.TopTxInfoSimplified' field.
    --
    -- The production implementation builds each field in one of two ways:
    --
    --   * "combine-then-translate": it combines the ledger values of every transaction of the
    --     batch monoidally, and then translates the result to the Plutus type (e.g. mints,
    --     withdrawals, validity range, votes, datums);
    --   * it reuses the already translated 'PV4.TxInfo' of every transaction of the batch, and
    --     concatenates the relevant field (e.g. ids, inputs, outputs, certificates, guards).
    --
    -- This reference implementation instead uses a "translate-then-combine"
    -- approach for every field: it translates the ledger values of each
    -- transaction directly to Plutus types, without reading any per-transaction
    -- 'PV4.TxInfo', and then combines the translated values. For the first kind
    -- of fields, this checks that combining before translating doesn't lose
    -- data or reorder the elements. For the second kind, this checks that the
    -- per-transaction 'PV4.TxInfo's are looked up and concatenated in batch
    -- order (sub-transactions in order, top-level transaction last).
    --
    -- Both approaches should yield the same result.
    mkReferenceTopTxInfoSimplified ::
      LedgerTxInfo TopTx era ->
      Either (ContextError era) PV4.TopTxInfoSimplified
    mkReferenceTopTxInfoSimplified ledgerTxInfo = do
      let proxyEra = Proxy @era
          proxyLang = Proxy @PlutusV4
          utxo = ltiUTxO ledgerTxInfo
          topTx = ltiTx ledgerTxInfo
      inputs <-
        foldMapBatchM
          topTx
          (\tx -> mapM (transTxInInfoV4 utxo) (Set.toList (tx ^. bodyTxL . inputsTxBodyL)))
      referenceInputs <-
        foldMapBatchM
          topTx
          (\tx -> mapM (transTxInInfoV4 utxo) (Set.toList (tx ^. bodyTxL . referenceInputsTxBodyL)))
      outputs <-
        foldMapBatchM
          topTx
          ( \tx ->
              zipWithM
                (transTxOutV4 . TxOutFromOutput)
                [minBound ..]
                (toList (tx ^. bodyTxL . outputsTxBodyL))
          )
      txCerts <-
        foldMapBatchM
          topTx
          ( \tx ->
              mapM (toPlutusTxCert proxyLang (ltiProtVer ledgerTxInfo)) (toList (tx ^. bodyTxL . certsTxBodyL))
          )
      validRangePerTx <-
        foldMapBatchM
          topTx
          ( \tx ->
              (: [])
                <$> transValidityInterval
                  proxyEra
                  (ltiEpochInfo ledgerTxInfo)
                  (ltiSystemStart ledgerTxInfo)
                  (tx ^. bodyTxL . vldtTxBodyL)
          )
      pure
        PV4.TopTxInfoSimplified
          { ttisIds = mapTxBatch topTx (transTxBodyId . (^. bodyTxL))
          , ttisInputs = inputs
          , ttisReferenceInputs = referenceInputs
          , ttisOutputs = outputs
          , ttisMints =
              MintValue.UnsafeMintValue $
                PV4.getValue $
                  foldMapBatch topTx (PV4.mintValueMinted . transMintValue . (^. bodyTxL . mintTxBodyL))
          , ttisBurns =
              MintValue.UnsafeMintValue
                . PlutusTx.fmap (PlutusTx.fmap negate)
                . PV4.getValue
                $ foldMapBatch topTx (PV4.mintValueBurned . transMintValue . (^. bodyTxL . mintTxBodyL))
          , ttisTxCerts = txCerts
          , ttisWithdrawals =
              unionsAssocMapWith (+) $
                mapTxBatch topTx (transWithdrawals . (^. bodyTxL . withdrawalsTxBodyL))
          , ttisDirectDeposits =
              unionsAssocMapWith (+) $
                mapTxBatch topTx (transDirectDeposits . (^. bodyTxL . directDepositsTxBodyL))
          , -- Intersect the translated intervals, i.e. the greatest
            -- lower bound and the smallest upper bound, with `Nothing` meaning unbounded.
            ttisValidRange =
              PV4.POSIXTimeRange
                (getMax <$> foldMap (fmap Max . PV4.fromInclusive) validRangePerTx)
                (getMin <$> foldMap (fmap Min . PV4.untilExclusive) validRangePerTx)
          , ttisGuards = foldMapBatch topTx (fmap transCred . toList . (^. bodyTxL . guardsTxBodyL))
          , ttisRequiredTopLevelGuards =
              Set.toList
                ( foldMapBatch
                    topTx
                    (\tx -> Set.map transCred $ Map.keysSet $ tx ^. bodyTxL . requiredTopLevelGuardsTxBodyL)
                )
          , -- The script hash of each redeemer whose purpose the transaction
            -- needs is looked up with 'getScriptsNeeded', instead of reading the
            -- translated `PV4.txInfoRedeemers`. Note that `ltiScriptHashesUsed`,
            -- which production reads, is also built from 'getScriptsNeeded', so
            -- this does not check 'getScriptsNeeded' itself. The generator
            -- provides the script of every redeemer, so the "provided" filter of
            -- production is not repeated here. The hashes are translated, and
            -- then deduplicated across the batch in the Plutus `ScriptHash`
            -- order.
            ttisRedeemerHashes =
              Set.toList $
                foldMapBatch topTx (Set.fromList . fmap transScriptHash . scriptHashesPerRedeemer utxo)
          , -- Union of the translated datums of each transaction.
            ttisData =
              PV4.unsafeFromList $
                Map.toList $
                  foldMapBatch topTx (Map.fromList . transDatums . (^. witsTxL . datsTxWitsL))
          , -- Each transaction's votes are translated, then merged
            -- per voter. For the same voter and governance action, a later
            -- transaction's vote replaces an earlier one, so the top-level
            -- transaction's votes win.
            ttisVotes =
              unionsAssocMapWith (AssocMap.unionWith (\_ new -> new)) $
                mapTxBatch topTx (transVotingProcedures . (^. bodyTxL . votingProceduresTxBodyL))
          , ttisProposalProcedures =
              foldMapBatch
                topTx
                (fmap (transProposal proxyLang) . toList . (^. bodyTxL . proposalProceduresTxBodyL))
          , -- The first transaction of the batch with a current treasury amount wins. Each
            -- amount is translated from the ledger value, instead of read from the per-tx
            -- 'PV4.TxInfo'.
            ttisCurrentTreasuryAmount =
              getFirst $
                foldMapBatch
                  topTx
                  ( First
                      . fmap transCoinToLovelace
                      . strictMaybeToMaybe
                      . (^. bodyTxL . currentTreasuryValueTxBodyL)
                  )
          , ttisTreasuryDonations =
              getSum
                (foldMapBatch topTx (\tx -> Sum $ transCoinToLovelace (tx ^. bodyTxL . treasuryDonationTxBodyL)))
          }

-- | Compare each 'PV4.TopTxInfoSimplified' field separately.
--
-- The `Eq` instances of the `PV4.Map`-based (represented as list of tuples)
-- fields should not be used because the instance converts to 'Data.Map' to do
-- the comparison. In doing so, we won't be able to catch issues with
-- unordered or duplicated keys in 'PV4.Map'. Therefore, we do equality checks
-- directly on each field.
shouldMatchTopTxInfoSimplified :: PV4.TopTxInfoSimplified -> PV4.TopTxInfoSimplified -> Expectation
shouldMatchTopTxInfoSimplified actual expected = do
  -- We deconstruct the data constructor here in order to get a GHC error if
  -- new fields are added. This also helps as a reminder for the programmer to
  -- add an assertion for the new field.
  let ttis@(PV4.TopTxInfoSimplified _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = actual
      PV4.TopTxInfoSimplified {..} = ttis
  ttisIds `shouldBe` PV4.ttisIds expected
  ttisInputs `shouldBe` PV4.ttisInputs expected
  ttisReferenceInputs `shouldBe` PV4.ttisReferenceInputs expected
  ttisOutputs `shouldBe` PV4.ttisOutputs expected
  shouldBeCanonicalMintValue (> 0) ttisMints
  mintValueToMap ttisMints `shouldBe` mintValueToMap (PV4.ttisMints expected)
  shouldBeCanonicalMintValue (< 0) ttisBurns
  mintValueToMap ttisBurns `shouldBe` mintValueToMap (PV4.ttisBurns expected)
  ttisTxCerts `shouldBe` PV4.ttisTxCerts expected
  shouldHaveUniqueAssocMapKeys ttisWithdrawals
  assocMapToMap ttisWithdrawals `shouldBe` assocMapToMap (PV4.ttisWithdrawals expected)
  shouldHaveUniqueAssocMapKeys ttisDirectDeposits
  assocMapToMap ttisDirectDeposits `shouldBe` assocMapToMap (PV4.ttisDirectDeposits expected)
  ttisValidRange `shouldBe` PV4.ttisValidRange expected
  ttisGuards `shouldBe` PV4.ttisGuards expected
  -- Sorting the actual list keeps its duplicates, while the expected list has none.
  -- We sort here manually because Ledger and Plutus codebases differ on the 'Ord'
  -- instance of 'Credential'.
  List.sort ttisRequiredTopLevelGuards `shouldBe` PV4.ttisRequiredTopLevelGuards expected
  ttisRedeemerHashes `shouldBe` PV4.ttisRedeemerHashes expected
  -- Exact order comparison depends on ledger and Plutus agreeing on hash
  -- order.
  AssocMap.toList ttisData `shouldBe` AssocMap.toList (PV4.ttisData expected)
  shouldHaveUniqueAssocMapKeys ttisVotes
  mapM_ shouldHaveUniqueAssocMapKeys (AssocMap.elems ttisVotes)
  fmap assocMapToMap (assocMapToMap ttisVotes)
    `shouldBe` fmap assocMapToMap (assocMapToMap (PV4.ttisVotes expected))
  ttisProposalProcedures `shouldBe` PV4.ttisProposalProcedures expected
  ttisCurrentTreasuryAmount `shouldBe` PV4.ttisCurrentTreasuryAmount expected
  ttisTreasuryDonations `shouldBe` PV4.ttisTreasuryDonations expected

-- | The sub-transactions of a top-level transaction, in 'OMap' order.
getSubTransactions :: (EraTx era, DijkstraEraTxBody era) => Tx TopTx era -> [Tx SubTx era]
getSubTransactions topTx = OMap.elems (topTx ^. bodyTxL . subTransactionsTxBodyL)

-- | The value extracted by @f@ from each transaction of the batch, in batch order: the
-- sub-transactions in 'OMap' order, then the top-level transaction last, mirroring
-- `foldMapBatch` in "Cardano.Ledger.Dijkstra.TxInfo". Sub-transactions and the top-level
-- transaction have different types, so @f@ must be polymorphic in the transaction level.
mapTxBatch ::
  (EraTx era, DijkstraEraTxBody era) => Tx TopTx era -> (forall l. Tx l era -> a) -> [a]
mapTxBatch topTx f = fmap f (getSubTransactions topTx) <> [f topTx]

-- | Combine the values extracted by @f@ from each transaction of the batch, in batch order.
foldMapBatch ::
  (EraTx era, DijkstraEraTxBody era, Monoid m) => Tx TopTx era -> (forall l. Tx l era -> m) -> m
foldMapBatch topTx f = mconcat (mapTxBatch topTx f)

-- | Same as 'foldMapBatch', but for per-transaction translations that can fail.
foldMapBatchM ::
  (EraTx era, DijkstraEraTxBody era, Monoid m) =>
  Tx TopTx era ->
  (forall l. Tx l era -> Either e m) ->
  Either e m
foldMapBatchM topTx f = mconcat <$> sequenceA (mapTxBatch topTx f)

-- | The script hash of each redeemer of a transaction whose purpose the transaction needs, so
-- the hash of a script used for two purposes appears twice.
scriptHashesPerRedeemer ::
  (EraUTxO era, AlonzoEraTxWits era, ScriptsNeeded era ~ AlonzoScriptsNeeded era) =>
  UTxO era ->
  Tx l era ->
  [ScriptHash]
scriptHashesPerRedeemer utxo tx =
  let AlonzoScriptsNeeded needed = getScriptsNeeded utxo (tx ^. bodyTxL)
      redeemers = tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL
   in [sh | (purpose, sh) <- needed, hoistPlutusPurpose toAsIx purpose `Map.member` redeemers]

-- | View a 'PV4.Map' as a 'Data.Map', ignoring the original order of its
-- entries and possible duplicates.
assocMapToMap :: Ord k => PV4.Map k v -> Map.Map k v
assocMapToMap = Map.fromList . AssocMap.toList

-- | Union a list of 'PV4.Map's, combining the values of a key present in more than one of them
-- with @combine@. The fold is from the left, so @combine@ receives the value of the earlier map
-- first.
unionsAssocMapWith :: PlutusTx.Eq k => (v -> v -> v) -> [PV4.Map k v] -> PV4.Map k v
unionsAssocMapWith combine = List.foldl' (AssocMap.unionWith combine) AssocMap.empty

-- | Convert 'MintValue.MintValue' to 'Data.Map'.
mintValueToMap :: MintValue.MintValue -> Map.Map PV4.CurrencySymbol (Map.Map PV4.TokenName Integer)
mintValueToMap = fmap assocMapToMap . assocMapToMap . MintValue.unMintValue

shouldHaveUniqueAssocMapKeys :: (HasCallStack, Ord k, Show k) => PV4.Map k v -> Expectation
shouldHaveUniqueAssocMapKeys m =
  let keys = AssocMap.keys m
   in List.nub keys `shouldBe` keys

-- | Checks that both nested 'PV4.Map' of 'MintValue.MintValue' have strictly
-- ascending keys (sorted and without duplicates), and every quantity satisfies
-- the predicate.
shouldBeCanonicalMintValue ::
  HasCallStack => (Integer -> Bool) -> MintValue.MintValue -> Expectation
shouldBeCanonicalMintValue isValidQuantity (MintValue.UnsafeMintValue m) = do
  shouldHaveStrictlyAscendingKeys m
  mapM_ shouldHaveStrictlyAscendingKeys (AssocMap.elems m)
  concatMap (filter (not . isValidQuantity) . AssocMap.elems) (AssocMap.elems m) `shouldBe` []

-- | Checks that 'PV4.Map' keys are sorted and without duplicates.
shouldHaveStrictlyAscendingKeys :: (HasCallStack, Ord k, Show k) => PV4.Map k v -> Expectation
shouldHaveStrictlyAscendingKeys m =
  let keys = AssocMap.keys m
   in keys `shouldBe` Set.toAscList (Set.fromList keys)

-- Whether the first upper bound of a validity interval is strictly below the second one,
-- where an absent upper bound means unbounded.
isUpperBoundBelow :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> Bool
isUpperBoundBelow (SJust slot) (SJust slot') = slot < slot'
isUpperBoundBelow (SJust _) SNothing = True
isUpperBoundBelow SNothing _ = False
