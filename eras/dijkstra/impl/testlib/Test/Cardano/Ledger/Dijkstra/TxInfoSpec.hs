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
  LedgerLevelTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  toPlutusTxInfoForPurpose,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (transDatums)
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
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Conway.Governance (GovActionId, Voter, foldlVotingProcedures)
import Cardano.Ledger.Conway.TxInfo (transMintValue, transTxBodyId)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceIntervals (..),
 )
import Cardano.Ledger.Dijkstra.State (UTxO (..))
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
  transSafeHash,
  transScriptHash,
  transTxIx,
 )
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.Val as Val
import Control.Monad (zipWithM)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import Data.Monoid (First (..), Sum (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..))
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.V3.MintValue as MintValue
import PlutusLedgerApi.V4 (TopTxInfoSimplified (..))
import qualified PlutusLedgerApi.V4 as PV4
import qualified PlutusTx.AssocMap as AssocMap
import Test.Cardano.Ledger.Alonzo.Era (mkTestLedgerTxInfo)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Translation.TranslatableGen (genTranslatableTxWithNestedTxs)
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
          "TopTxInfoSimplified construction matches when combining Plutus types"
          propTopTxInfoSimplifiedMatchesPlutusCombine

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

propTopTxInfoSimplifiedMatchesPlutusCombine :: ScriptHash -> Property
propTopTxInfoSimplifiedMatchesPlutusCombine guardingScriptHash = do
  forAll genTranslatableTxWithNestedTxs $ \(topTx, utxo) ->
    checkNestedTxsCoverage topTx $ do
      let testEpochInfo = epochInfo testGlobals
          testSystemStart = systemStart testGlobals
          mkSubLedgerTxInfo txIx subTx =
            mkTestLedgerTxInfo
              (ProtVer (eraProtVerLow @DijkstraEra) 0)
              testEpochInfo
              testSystemStart
              utxo
              subTx
              (LedgerSubTxInfo txIx)
          subTxInfoResults =
            Map.fromList
              [ (txIdTx subTx, mkTxInfoResult (mkSubLedgerTxInfo txIx subTx))
              | (txIx, subTx) <- zip [TxIx 0 ..] (OMap.elems (topTx ^. bodyTxL . subTransactionsTxBodyL))
              ]
          ledgerTxInfo =
            mkTestLedgerTxInfo
              (ProtVer (eraProtVerLow @DijkstraEra) 0)
              testEpochInfo
              testSystemStart
              utxo
              topTx
              (LedgerTopTxInfo subTxInfoResults)
      txInfo <- expectRight $ unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo)
      topTxInfo <- expectRight $ transGuardingTopTxInfo SPlutusV4 txInfo guardingScriptHash ledgerTxInfo
      expectedTopTxInfoSimplified <- expectRight $ referenceTopTxInfoSimplified ledgerTxInfo
      PV4.topTxInfoSimplified topTxInfo `shouldMatchTopTxInfoSimplified` expectedTopTxInfoSimplified

 where
  -- | Reports how many sub-transactions are generated, and require enough
  -- batches where the fields aggregated across the batch are set in more than
  -- one transaction, since that is where the aggregation strategies of
  -- 'transGuardingTopTxInfo' can disagree.
  checkNestedTxsCoverage :: Testable prop => Tx TopTx DijkstraEra -> prop -> Property
  checkNestedTxsCoverage topTx =
    checkCoverage
      . tabulate "Number of sub-transactions" [show (length subTxs)]
      . cover 50 (not (null subTxs)) "Has sub-transactions"
      . coverNonEmptyValueInNestedTxs "withdrawals" (^. bodyTxL . withdrawalsTxBodyL)
      . coverNonEmptyValueInNestedTxs "direct deposits" (^. bodyTxL . directDepositsTxBodyL)
      . coverNonEmptyValueInNestedTxs "validity intervals" (^. bodyTxL . vldtTxBodyL)
      . coverNonEmptyValueInNestedTxs "votes" (^. bodyTxL . votingProceduresTxBodyL)
      . coverNonEmptyValueInNestedTxs "datums" (^. witsTxL . datsTxWitsL)
      . coverNonEmptyValueInNestedTxs "required top-level guards" (^. bodyTxL . requiredTopLevelGuardsTxBodyL)
      -- TODO Mint and burn of the same assetid in different transactions
      . cover
          25
          (sum (fmap length votesPerTx) > length (mconcat votesPerTx))
          "Same voter and governance action in more than one transaction"
      -- TODO Add cover for withdrawals, direct deposits, required top-level
      -- guards, datums and mints to make sure keys occasionnaly overlap across
      -- transactions
      -- TODO Voter shared accross transactions with a different govactionid
      -- TODO Current treasury amount absent, only in sub-txs, only in the top tx, or in several
      -- TODO a lower bound only, an upper bound only, and the tighest bound coming from a sub-tx
      -- TODO Certs, proposals and guards non-empty in two or more txs
      -- TODO treasure donations non-zero in two or more txs
      -- TODO Nested txs share (voter, gaid) has a different translated Vote
    where
      subTxs = OMap.elems (topTx ^. bodyTxL . subTransactionsTxBodyL)
      -- The `(Voter, GovActionId)` pairs voted on by each transaction of the transaction batch (`TopTx` + `SubTx`s)
      votesPerTx :: [Set.Set (Voter, GovActionId)]
      votesPerTx =
        fmap
          (foldlVotingProcedures (\acc voter gaid _ -> Set.insert (voter, gaid) acc) mempty)
          (topTx ^. bodyTxL . votingProceduresTxBodyL : fmap (^. bodyTxL . votingProceduresTxBodyL) subTxs)
      -- | Require at least 50% of the generated batches to have a non-empty
      -- value of the field extracted by @f@ in more than one transaction
      -- (top-level + sub-transactions). A field set in a single transaction is
      -- never actually combined with a field of another transaction, so it
      -- cannot tell appart the aggregation strategies of the production
      -- implementationand of the reference implementation.
      coverNonEmptyValueInNestedTxs
        :: (Testable p, Eq a, Monoid a)
        => String
        -> (forall l. Tx l DijkstraEra -> a)
        -> p
        -> Property
      coverNonEmptyValueInNestedTxs name f =
        cover 50 (length (filter (/= mempty) (f topTx : map f subTxs)) >= 2) $
          "Non-empty " <> name <> " in more than one transaction"

      -- TODO coverSharedKeysInNestedTxs
      -- Require nested txs to share a key, while no single tx holds every key
      -- of the batch.

  -- | Reference 'PV4.TopTxInfoSimplified' constructed out of the 'LedgerTxInfo' of a top-level
  -- transaction. The idea is to compare this with the production code by using a different way of
  -- constructing each 'PV4.TopTxInfoSimplified' field.
  --
  -- The production implementation typically uses a "combine-then-translate"
  -- approach: it combines the ledger type monoidally, and then translate the
  -- result to the underlying Plutus type.
  --
  -- In this reference implementation, it is the other way around with a
  -- "translate-then-combine" approach: we translate the Ledger type to a Plutus
  -- type, and then combine the result monoidally.
  --
  -- Both approaches should yield the same result.
  referenceTopTxInfoSimplified ::
    LedgerTxInfo TopTx DijkstraEra -> Either (ContextError DijkstraEra) PV4.TopTxInfoSimplified
  referenceTopTxInfoSimplified ledgerTxInfo = do
    let proxyEra = Proxy @DijkstraEra
        proxyLang = Proxy @PlutusV4
        utxo = ltiUTxO ledgerTxInfo
        topTx = ltiTx ledgerTxInfo
        subTxs = topTx ^. bodyTxL . subTransactionsTxBodyL

        -- Sub-transactions are `Tx SubTx DijkstraEra` and the top tx is `Tx TopTx DijkstraEra` --
        -- different types -- so they can't share a plain list; `f` must stay polymorphic in the
        -- level, mirroring `foldMapBatch` in `Cardano.Ledger.Dijkstra.TxInfo`.
        foldMapBatch :: Monoid m => (forall level. Tx level DijkstraEra -> m) -> m
        foldMapBatch f = foldMap f subTxs <> f topTx

        -- Same as `foldMapBatch`, but for per-transaction translations that can fail.
        foldMapBatchM ::
          Monoid m =>
          (forall level. Tx level DijkstraEra -> Either (ContextError DijkstraEra) m) ->
          Either (ContextError DijkstraEra) m
        foldMapBatchM f = ((<>) . mconcat <$> traverse f (toList subTxs)) <*> f topTx

    inputs <-
      foldMapBatchM (\tx -> mapM (transTxInInfoV4 utxo) (Set.toList (tx ^. bodyTxL . inputsTxBodyL)))
    referenceInputs <-
      foldMapBatchM
        (\tx -> mapM (transTxInInfoV4 utxo) (Set.toList (tx ^. bodyTxL . referenceInputsTxBodyL)))
    outputs <-
      foldMapBatchM
        ( \tx ->
            zipWithM
              (transTxOutV4 . TxOutFromOutput)
              [minBound ..]
              (toList (tx ^. bodyTxL . outputsTxBodyL))
        )
    txCerts <-
      foldMapBatchM
        (\tx -> mapM (toPlutusTxCert proxyLang (ltiProtVer ledgerTxInfo)) (toList (tx ^. bodyTxL . certsTxBodyL)))
    validRangePerTx <-
      foldMapBatchM
        ( \tx ->
            (: [])
              <$> transValidityInterval proxyEra (ltiEpochInfo ledgerTxInfo) (ltiSystemStart ledgerTxInfo) (tx ^. bodyTxL . vldtTxBodyL)
        )
    pure
      PV4.TopTxInfoSimplified
        { ttisIds = foldMapBatch (\tx -> [transTxBodyId $ tx ^. bodyTxL])
        , ttisInputs = inputs
        , ttisReferenceInputs = referenceInputs
        , ttisOutputs = outputs
        , ttisMints =
            MintValue.UnsafeMintValue $
              PV4.getValue $
                foldMapBatch (PV4.mintValueMinted . transMintValue . (^. bodyTxL . mintTxBodyL))
        , ttisBurns =
            MintValue.UnsafeMintValue
              . AssocMap.mapWithKey (\_ -> AssocMap.mapWithKey (const negate))
              . PV4.getValue
              $ foldMapBatch (PV4.mintValueBurned . transMintValue . (^. bodyTxL . mintTxBodyL))
        , ttisTxCerts = txCerts
        , ttisWithdrawals =
            foldr
              (AssocMap.unionWith (+))
              AssocMap.empty
              ( foldMapBatch (\tx -> [transWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL]) ::
                  [PV4.Map PV4.Credential PV4.Lovelace]
              )
        , ttisDirectDeposits =
            foldr
              (AssocMap.unionWith (+))
              AssocMap.empty
              ( foldMapBatch (\tx -> [transDirectDeposits $ tx ^. bodyTxL . directDepositsTxBodyL]) ::
                  [PV4.Map PV4.Credential PV4.Lovelace]
              )
        , -- Intersect the translated intervals, i.e. the greatest
          -- lower bound and the smallest upper bound, with `Nothing` meaning unbounded.
          ttisValidRange =
            PV4.POSIXTimeRange
              (getMax <$> foldMap (fmap Max . PV4.fromInclusive) validRangePerTx)
              (getMin <$> foldMap (fmap Min . PV4.untilExclusive) validRangePerTx)
        , ttisGuards = foldMapBatch (fmap transCred . toList . (^. bodyTxL . guardsTxBodyL))
        , ttisRequiredTopLevelGuards =
            Set.toList
              ( foldMapBatch
                  (\tx -> Set.map transCred $ Map.keysSet $ tx ^. bodyTxL . requiredTopLevelGuardsTxBodyL)
              )
        , -- `genTranslatableTxWithNestedTxs` unconditionally clears every sub-/top-transaction's
          -- redeemers (`Redeemers Map.empty`), so `PV4.txInfoRedeemers` -- and therefore
          -- `ttisRedeemerHashes` -- is guaranteed empty in production too; there is no live
          -- redeemer data for this property to exercise.
          -- TODO: exercise this field, either by generating redeemers that point at real script
          -- needs (so `ltiScriptHashesUsed` resolves them), or with a unit test built on a guard
          -- script. It should also pin that production deduplicates the hashes across the batch.
          ttisRedeemerHashes = mempty
        , -- Uses "translate-then-combine": union of the translated datums of each transaction. A
          -- datum hash uniquely determines its datum, so the bias of the union does not matter.
          ttisData =
            PV4.unsafeFromList $
              Map.toList $
                foldMapBatch (Map.fromList . transDatums . (^. witsTxL . datsTxWitsL))
        , -- Uses "translate-then-combine": a later vote of a voter on the same governance action
          -- replaces an earlier one, so the top-level transaction's votes win.
          ttisVotes =
            -- TODO Rewrite similar to other property test which states that latest
            -- vote wins for duplicate keys
            PV4.unsafeFromList . Map.toList . fmap (PV4.unsafeFromList . Map.toList) $
              foldl
                (Map.unionWith (flip Map.union))
                Map.empty
                ( foldMapBatch
                    ( (: [])
                        . fmap assocMapToMap
                        . assocMapToMap
                        . transVotingProcedures
                        . (^. bodyTxL . votingProceduresTxBodyL)
                    )
                )
        , ttisProposalProcedures =
            foldMapBatch (fmap (transProposal proxyLang) . toList . (^. bodyTxL . proposalProceduresTxBodyL))
        , -- Uses "translate-then-combine" (combined with 'First'), like production, but translates
          -- each transaction's ledger value directly instead of reading the per-tx 'PV4.TxInfo'.
          ttisCurrentTreasuryAmount =
            getFirst $
              foldMapBatch
                ( First
                    . fmap transCoinToLovelace
                    . strictMaybeToMaybe
                    . (^. bodyTxL . currentTreasuryValueTxBodyL)
                )
        , -- Uses "translate-then-combine" strategy (combined with 'Sum') where production uses "combine-then-translate".
          ttisTreasuryDonations =
            getSum (foldMapBatch (\tx -> Sum $ transCoinToLovelace (tx ^. bodyTxL . treasuryDonationTxBodyL)))
        }

  -- | Compare each 'PV4.TopTxInfoSimplified' field separately.
  --
  -- The `Eq` instances of the `PV4.Map`-based (represented as list of tuples)
  -- fields should not be used because the instance converts to 'Data.Map' to do
  -- the comparison. In doing so, we won't be able to catch issues with
  -- unordered or duplicated keys in 'PV4.Map'. Therefore, we do equality checks
  -- directly on each field directly.
  shouldMatchTopTxInfoSimplified :: PV4.TopTxInfoSimplified -> PV4.TopTxInfoSimplified -> Expectation
  shouldMatchTopTxInfoSimplified actual expected = do
    -- We deconstruct the data constructor here in order to get a GHC warning if
    -- new fields are added.
    let (PV4.TopTxInfoSimplified
          ids
          inputs
          refInputs
          outputs
          mints
          burns
          certs
          withdrawals
          directDeposits
          validRange
          guards
          requiredTopLevelGuards
          redeemerHashes
          txData
          votes
          proposalProcedures
          currentTreasuryAmount
          treasuryDonations) = actual
    ids `shouldBe` ttisIds expected
    inputs `shouldBe` ttisInputs expected
    refInputs `shouldBe` ttisReferenceInputs expected
    outputs `shouldBe` ttisOutputs expected
    shouldBeCanonicalMintValue (> 0) mints
    mintValueToMap mints `shouldBe` mintValueToMap (ttisMints expected)
    shouldBeCanonicalMintValue (< 0) burns
    mintValueToMap burns `shouldBe` mintValueToMap (ttisBurns expected)
    certs `shouldBe` ttisTxCerts expected
    shouldHaveUniqueAssocMapKeys withdrawals
    assocMapToMap withdrawals `shouldBe` assocMapToMap (ttisWithdrawals expected)
    shouldHaveUniqueAssocMapKeys directDeposits
    assocMapToMap directDeposits `shouldBe` assocMapToMap (ttisDirectDeposits expected)
    validRange `shouldBe` ttisValidRange expected
    guards `shouldBe` ttisGuards expected
    -- Sorting the actual list keeps its duplicates, while the expected list has none.
    -- We sort here manually because Ledger and Plutus codebases differ on the 'Ord'
    -- instance of 'Credential'.
    List.sort requiredTopLevelGuards `shouldBe` ttisRequiredTopLevelGuards expected
    redeemerHashes `shouldBe` ttisRedeemerHashes expected
    -- Both sides are sorted by datum hash, so the order is compared as well
    AssocMap.toList txData `shouldBe` AssocMap.toList (ttisData expected)
    shouldHaveUniqueAssocMapKeys votes
    mapM_ shouldHaveUniqueAssocMapKeys (AssocMap.elems votes)
    fmap assocMapToMap (assocMapToMap votes)
      `shouldBe` fmap assocMapToMap (assocMapToMap (ttisVotes expected))
    proposalProcedures `shouldBe` ttisProposalProcedures expected
    currentTreasuryAmount `shouldBe` ttisCurrentTreasuryAmount expected
    treasuryDonations `shouldBe` ttisTreasuryDonations expected

-- | View a 'PV4.Map' as a 'Data.Map', ignoring the original order of its
-- entries and possible duplicates.
assocMapToMap :: Ord k => PV4.Map k v -> Map.Map k v
assocMapToMap = Map.fromList . AssocMap.toList

-- | Convert 'PV3.MintValue' to 'Data.Map'.
mintValueToMap :: MintValue.MintValue -> Map.Map PV4.CurrencySymbol (Map.Map PV4.TokenName Integer)
mintValueToMap = fmap assocMapToMap . assocMapToMap . MintValue.unMintValue

shouldHaveUniqueAssocMapKeys :: (HasCallStack, Ord k, Show k) => PV4.Map k v -> Expectation
shouldHaveUniqueAssocMapKeys m =
  let keys = AssocMap.keys m
   in List.nub keys `shouldBe` keys

-- | Checks that both nested 'PV4.Map' of 'PV3.MintValue' have strictly
-- ascending keys (sorted and without duplicates), and every quantity satisfies
-- the predicate.
shouldBeCanonicalMintValue :: HasCallStack => (Integer -> Bool) -> MintValue.MintValue -> Expectation
shouldBeCanonicalMintValue isValidQuantity (MintValue.UnsafeMintValue m) = do
  shouldHaveStrictlyAscendingKeys m
  mapM_ shouldHaveStrictlyAscendingKeys (AssocMap.elems m)
  concatMap (filter (not . isValidQuantity) . AssocMap.elems) (AssocMap.elems m) `shouldBe` []

-- | Checks that 'PV4.Map' keys are sorted and without duplicates.
shouldHaveStrictlyAscendingKeys :: (HasCallStack, Ord k, Show k) => PV4.Map k v -> Expectation
shouldHaveStrictlyAscendingKeys m =
  let keys = AssocMap.keys m
   in keys `shouldBe` Set.toAscList (Set.fromList keys)
