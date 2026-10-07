{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (CollectError (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  TransactionScriptFailure (ContextError, RedeemerPointsToUnknownScriptHash),
  evalTxExUnits,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Inject (..),
  Mismatch (..),
  Network (..),
  StrictMaybe (..),
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules (ConwayUtxosPredFailure (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra (evalDijkstraTxExUnits)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxowPredFailure (..))
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Plutus (
  Data (Data),
  ExUnits (..),
  Language (..),
  OrdExUnits (..),
  Plutus,
  SLanguage (..),
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.State (accountsL, accountsMapL)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import Lens.Micro.Mtl (use)
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsNoDatum,
  alwaysSucceedsNoDatum,
  alwaysSucceedsWithDatum,
 )

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOW" $ do
  describe "Receiving witnesses" $ do
    it "requires the protected payment key without requiring a guard" $ do
      key <- freshKeyHash @Payment
      let tx =
            mkBasicTx $
              mkBasicTxBody
                & outputsTxBodyL
                  .~ [mkCoinTxOut (AddrProtected Testnet (KeyHashObj key) StakeRefNull) (Coin 2_000_000)]
          removeReceivingWitness = pure . (witsTxL . addrTxWitsL %~ Set.filter ((/= asWitness key) . witVKeyHash))
      withPostFixup removeReceivingWitness $
        submitFailingTx
          tx
          [injectFailure $ Conway.MissingVKeyWitnessesUTXOW $ NES.singleton (asWitness key)]
      submitTx_ tx

    it "validates native Receiving with the existing guard environment" $ do
      key <- freshKeyHash
      sh <- impAddNativeScript (RequireGuard (KeyHashObj key))
      let tx =
            mkBasicTx $
              mkBasicTxBody
                & outputsTxBodyL
                  .~ [mkCoinTxOut (AddrProtected Testnet (ScriptHashObj sh) StakeRefNull) (Coin 2_000_000)]
      submitFailingTx tx [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton sh]
      submitTx_ (tx & bodyTxL . guardsTxBodyL .~ [KeyHashObj key])

  describe "RequireGuard native scripts" $ do
    it "Spending inputs locked by script requiring a keyhash guard" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      scriptHash <- impAddNativeScript (RequireGuard guardKeyHash)
      txIn <- produceScript scriptHash
      let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton scriptHash]
      submitTx_ $ tx & bodyTxL . guardsTxBodyL .~ [guardKeyHash]

    it "A native script required as guard needs to be witnessed " $ do
      let guardScript = RequireAllOf []
      let guardScriptHash = hashScript @era $ fromNativeScript guardScript
      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)
      txIn <- produceScript scriptHash
      let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton scriptHash]

      let txWithGuards = tx & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash]
      submitFailingTx
        txWithGuards
        [injectFailure $ Conway.MissingScriptWitnessesUTXOW $ NES.singleton guardScriptHash]
      submitTx_ $ txWithGuards & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]

    it "A failing native script required as guard results in a predicate failure" $ do
      let guardScriptFailing = RequireAnyOf []
      let guardScriptHash = hashScript @era $ fromNativeScript guardScriptFailing
      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegDepositTxCert (ScriptHashObj scriptHash) expectedDeposit]
              & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash]
              & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScriptFailing]
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton guardScriptHash]

    it "A redundant guard is ignored" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . guardsTxBodyL .~ [guardKeyHash]
      submitTx_ tx

    it "Nested RequiredGuard scripts" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      let guardScript = RequireGuard guardKeyHash
      let guardScriptHash = hashScript @era $ fromNativeScript guardScript
      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)
      txIn <- produceScript scriptHash
      let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton scriptHash]
      submitTx_ $
        tx
          & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash, guardKeyHash]
          & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]

  describe "Required top-level guards" $ do
    describe "MissingRequiredGuards" $ do
      it "A top-level required guard absent from the guards set is a predicate failure" $ do
        guardKeyHash <- KeyHashObj <$> freshKeyHash
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SNothing)]
        submitFailingTx
          tx
          [injectFailure $ MissingRequiredGuards $ NES.singleton guardKeyHash]
        submitTx_ $ tx & bodyTxL . guardsTxBodyL .~ [guardKeyHash]

      it "A guard required by a sub-transaction must be present in the top-level guards" $ do
        guardKeyHash <- KeyHashObj <$> freshKeyHash
        let subTx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SNothing)]
            tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTx
        submitFailingTx
          tx
          [injectFailure (MissingRequiredGuards (NES.singleton guardKeyHash))]

    describe "MalformedGuardDatums" $ do
      it "A key-hash guard carrying a datum is a predicate failure" $ do
        guardKeyHash <- KeyHashObj <$> freshKeyHash
        datum <- arbitrary @(Data era)
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardKeyHash]
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SJust datum)]
        submitFailingTx
          tx
          [injectFailure $ MalformedGuardDatums $ NES.singleton guardKeyHash]
        submitTx_ $ tx & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SNothing)]

      it "A native-script guard carrying a datum is a predicate failure" $ do
        datum <- arbitrary @(Data era)
        let guardScript = RequireAllOf []
            guardScriptHash = hashScript @era $ fromNativeScript guardScript
            guardCred = ScriptHashObj guardScriptHash
            tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardCred]
                & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardCred, SJust datum)]
        submitFailingTx
          tx
          [injectFailure $ MalformedGuardDatums $ NES.singleton guardCred]
        submitTx_ $ tx & bodyTxL . requiredTopLevelGuardsL .~ [(guardCred, SNothing)]

      it "A Plutus-script guard's datum presence is validated" $ do
        datum <- arbitrary @(Data era)
        let guardScript = alwaysSucceeds @'PlutusV3 3
            guardCred = ScriptHashObj (hashScript @era guardScript)
            malformed = injectFailure (MalformedGuardDatums (NES.singleton guardCred))
            mkTx mDatum =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardCred]
                & witsTxL . hashScriptTxWitsL .~ [guardScript]
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardCred, mDatum)]
            -- TODO replace with `submitFailingTx` once we have fixup support for plutus scripts
            hasMalformed tx = do
              result <- trySubmitTx tx
              pure $ case result of
                Left (predFailures, _) -> malformed `elem` predFailures
                Right _ -> False
        hasMalformed (mkTx SNothing) `shouldReturn` True
        hasMalformed (mkTx (SJust datum)) `shouldReturn` False

  describe "PlutusV4" $ do
    it "Extra redeemer for a key-locked certificate fails" $ do
      let plutus = alwaysSucceedsNoDatum SPlutusV4
      script <- fromPlutusScript <$> mkPlutusScript plutus
      refAddr <- freshKeyAddrNoPtr_
      txInitial <-
        impAnn "Sumbitting initial TX"
          . submitTx
          $ mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL
              .~ [ mkBasicTxOut (mkAddr (hashPlutusScript plutus) StakeRefNull) mempty
                 , mkBasicTxOut refAddr mempty & referenceScriptTxOutL .~ SJust script
                 ]
      stakeCred <- KeyHashObj <$> freshKeyHash
      deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      redeemerData <- arbitrary @(Data era)
      let prp = mkCertifyingPurpose $ AsIx 0
          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txInAt 0 txInitial]
              & bodyTxL . referenceInputsTxBodyL .~ [txInAt 1 txInitial]
              & bodyTxL . certsTxBodyL .~ [RegDepositTxCert stakeCred deposit]
      -- The extra redeemer resolves to an existing item that is not script-locked. UTXOW
      -- reports it as ExtraRedeemers and, unlike earlier Plutus versions, PlutusV4 TxInfo
      -- translation also fails with ScriptHashNotFoundForPurpose.
      submitFailingTx
        (tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert prp (redeemerData, ExUnits 0 0))
        [ injectFailure $ Alonzo.ExtraRedeemers [prp]
        , injectFailure $
            CollectErrors
              [ BadTranslation . inject $ ScriptHashNotFoundForPurpose prp
              ]
        ]
      submitTx_ tx

  describe "ExUnits" $
    forM_ (filter (>= PlutusV4) $ eraLanguages @era) $ \lang ->
      describe (show lang) $ withSLanguage lang $ \slang ->
        it "Attempt to calculate ExUnits with an invalid tx" $ do
          txIn <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum slang
          txFixed <- (mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn]) &) =<< asks iteFixup
          logToExpr txFixed

          let txBody = txFixed ^. bodyTxL
          goodPurpose <-
            expectJust . strictMaybeToMaybe . redeemerPointer txBody $ mkSpendingPurpose (AsItem txIn)
          -- Point the extra redeemer at the fee input, which is not locked by a script
          feeTxIn <- expectJust . Set.lookupMin . Set.delete txIn $ txBody ^. inputsTxBodyL
          badPurpose <-
            expectJust . strictMaybeToMaybe . redeemerPointer txBody $ mkSpendingPurpose (AsItem feeTxIn)
          redeemerData <- arbitrary @(Data era)
          let txBorked =
                txFixed
                  & witsTxL . rdmrsTxWitsL . unRedeemersL
                    %~ Map.insert badPurpose (redeemerData, ExUnits 5000 5000)
          logToExpr txBorked

          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          utxo <- getUTxO
          Globals {epochInfo, systemStart} <- use impGlobalsL
          let report = evalTxExUnits pp txBorked utxo epochInfo systemStart
          logToExpr report
          -- PlutusV4+ script purposes embed their script hash, so translating the dangling
          -- redeemer also fails the context of the valid one
          report
            `shouldBe` [ (badPurpose, Left $ RedeemerPointsToUnknownScriptHash badPurpose)
                       , (goodPurpose, Left . ContextError . inject $ ScriptHashNotFoundForPurpose badPurpose)
                       ]

  describe "Receiving batch evaluation" $ do
    it "estimates and evaluates receiving-only children independently" $ do
      child1 <- mkPlutusReceivingTx (alwaysSucceedsNoDatum SPlutusV4) (Coin 2_000_000)
      child2 <- mkPlutusReceivingTx (alwaysSucceedsNoDatum SPlutusV4) (Coin 3_000_000)
      let receivingPointer = ReceivingPurpose (AsIx 0)
          firstValue = (Data @era (P.I 2), ExUnits 2_000_000 200_000_000)
          secondValue = (Data @era (P.I 4), ExUnits 3_000_000 300_000_000)
          parentValue = (Data @era (P.I 6), ExUnits 1_000_000 100_000_000)
          authored child value = child & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.singleton receivingPointer value
      tx <- withSubTransactions [authored child1 firstValue, authored child2 secondValue]
      -- The same hash and raw index in three bodies still name three executions.
      let sh = hashPlutusScript (alwaysSucceedsNoDatum SPlutusV4)
          parentReceiving =
            tx
              & bodyTxL . outputsTxBodyL
                .~ [mkCoinTxOut (AddrProtected Testnet (ScriptHashObj sh) StakeRefNull) (Coin 2_000_000)]
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton receivingPointer parentValue
      fixed <- fixupTx parentReceiving
      pp <- getsPParams id
      utxo <- getUTxO
      Globals {epochInfo, systemStart} <- use impGlobalsL
      let report = evalDijkstraTxExUnits pp fixed utxo epochInfo systemStart
      let children = OMap.elems (fixed ^. bodyTxL . subTransactionsTxBodyL)
      Map.lookup receivingPointer (fixed ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
        `shouldBe` Just parentValue
      [Map.lookup receivingPointer (child ^. witsTxL . rdmrsTxWitsL . unRedeemersL) | child <- children]
        `shouldMatchList` [Just firstValue, Just secondValue]
      length children `shouldBe` 2
      Map.keys report
        `shouldMatchList` ((SNothing, receivingPointer) : [(SJust (txIdTx child), receivingPointer) | child <- children])
      forM_ (Map.elems report) $ \result -> result `shouldSatisfy` either (const False) (const True)
      withNoFixup (submitTx_ fixed)

    it "suppresses ordinary outputs when a child's Receiving fails" $ do
      child <- mkPlutusReceivingTx (alwaysFailsNoDatum SPlutusV4) (Coin 2_000_000)
      stakeKey <- freshKeyHash @Staking
      deposit <- getsPParams ppKeyDepositL
      let childWithCert = child & bodyTxL . certsTxBodyL .~ [RegDepositTxCert (KeyHashObj stakeKey) deposit]
      tx <- withSubTransactions [childWithCert]
      failed <- submitPhase2Invalid tx
      UTxO finalUtxo <- getUTxO
      let expectNoOutputs :: forall level. Tx level era -> ImpTestM era ()
          expectNoOutputs bodyTx =
            forM_ ([0 .. length (bodyTx ^. bodyTxL . outputsTxBodyL) - 1] :: [Int]) $ \index ->
              Map.member (txInAt index bodyTx) finalUtxo `shouldBe` False
      expectNoOutputs failed
      forM_ (OMap.elems (failed ^. bodyTxL . subTransactionsTxBodyL)) expectNoOutputs
      accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      Map.member (KeyHashObj stakeKey) accounts `shouldBe` False

  describe "Sub-transaction Plutus evaluation" $ do
    it "Evaluates every sub-transaction script during phase 2" $ do
      passingSubTx <- mkPlutusSpendingTx $ alwaysSucceedsNoDatum SPlutusV4
      failingSubTx <- mkPlutusSpendingTx $ alwaysFailsNoDatum SPlutusV4
      submitPhase2Invalid_ =<< withSubTransactions [passingSubTx, failingSubTx]

    -- See: https://github.com/IntersectMBO/formal-ledger-specifications/issues/723
    disableInConformanceIt "Enforces the transaction budget across sub-transactions" $ do
      subA <- mkPlutusSpendingTx $ alwaysSucceedsNoDatum SPlutusV4
      subB <- mkPlutusSpendingTx $ alwaysSucceedsNoDatum SPlutusV4
      tx <- withSubTransactions [subA, subB]
      let limit = ExUnits 1_500_000 200_000_000
      modifyPParams $ ppMaxTxExUnitsL .~ limit
      submitFailingTx
        tx
        [ injectFailure $
            Alonzo.ExUnitsTooBigUTxO $
              Mismatch
                { mismatchSupplied = OrdExUnits $ ExUnits 2_000_000 200_000_000
                , mismatchExpected = OrdExUnits limit
                }
        ]

-- Reference scripts avoid the unfinished PlutusV4 witness serialization.
mkPlutusSpendingTx ::
  forall era.
  DijkstraEraImp era =>
  Plutus 'PlutusV4 ->
  ImpTestM era (Tx SubTx era)
mkPlutusSpendingTx plutus = do
  script <- fromPlutusScript <$> mkPlutusScript plutus
  txIn <- produceScript $ hashPlutusScript plutus
  refAddr <- freshKeyAddrNoPtr_
  refTx <-
    submitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL . outputsTxBodyL
          .~ [mkBasicTxOut refAddr mempty & referenceScriptTxOutL .~ SJust script]
  redeemer <- arbitrary @(Data era)
  fixupPPHash $
    mkBasicTx mkBasicTxBody
      & bodyTxL . inputsTxBodyL .~ [txIn]
      & bodyTxL . referenceInputsTxBodyL .~ [txInAt 0 refTx]
      & witsTxL . rdmrsTxWitsL . unRedeemersL
        .~ Map.singleton (mkSpendingPurpose $ AsIx 0) (redeemer, ExUnits 1_000_000 100_000_000)

withSubTransactions ::
  DijkstraEraImp era =>
  [Tx SubTx era] ->
  ImpTestM era (Tx TopTx era)
withSubTransactions subTxs = do
  collateralAddr <- freshKeyAddrNoPtr_
  collateral <- sendCoinTo collateralAddr $ Coin 30_000_000
  pure $
    mkBasicTx mkBasicTxBody
      & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable subTxs
      & bodyTxL . collateralInputsTxBodyL .~ [collateral]

-- Explicit receiving-only child; a reference script from the original UTxO
-- supplies the validator, and each body's pointer is independently zero.
mkPlutusReceivingTx ::
  forall era.
  DijkstraEraImp era =>
  Plutus 'PlutusV4 ->
  Coin ->
  ImpTestM era (Tx SubTx era)
mkPlutusReceivingTx plutus amount = do
  script <- fromPlutusScript <$> mkPlutusScript plutus
  refAddr <- freshKeyAddrNoPtr_
  refTx <-
    submitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut refAddr mempty & referenceScriptTxOutL .~ SJust script]
  redeemer <- arbitrary @(Data era)
  fixupPPHash $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ [mkCoinTxOut (AddrProtected Testnet (ScriptHashObj (hashPlutusScript plutus)) StakeRefNull) amount]
      & bodyTxL . referenceInputsTxBodyL .~ [txInAt 0 refTx]
      & witsTxL . rdmrsTxWitsL . unRedeemersL
        .~ Map.singleton (ReceivingPurpose (AsIx 0)) (redeemer, ExUnits 1_000_000 100_000_000)
