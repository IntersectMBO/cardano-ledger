{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec (spec) where

import Cardano.Crypto.DSIGN (
  DSIGNAggregatable (createPossessionProofDSIGN),
  deriveVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Crypto.Leios (LeiosSigningKey)
import Cardano.Ledger.Alonzo.Plutus.Context (CollectError (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Globals (..), Inject (..), StrictMaybe (..))
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.Rules (ConwayUtxosPredFailure (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxowPredFailure (..))
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.TxCert (pattern RegBlsKeyTxCert)
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Dijkstra.TxWits (
  DijkstraEraTxWits (..),
  PoolVoteWitness (..),
  poolVoteSignContext,
 )
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Plutus (
  Data,
  ExUnits (..),
  Language (..),
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.State (BlsKey (..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import Lens.Micro.Mtl ((%=))
import Test.Cardano.Crypto.Leios.Gen (genLeiosSigningKey)
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum)

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOW" $ do
  describe "Pool-vote witnesses" $ do
    let voteTx spoKh gaId =
          mkBasicTx mkBasicTxBody
            & bodyTxL . votingProceduresTxBodyL
              .~ VotingProcedures
                ( Map.singleton
                    (StakePoolVoter spoKh)
                    (Map.singleton gaId (VotingProcedure VoteYes SNothing))
                )
        -- The BLS signature covers the final body hash, so it is attached (and
        -- the auto-signed cold-key witness dropped) after fixup.
        withPoolVoteWitness spoKh blsSk =
          withPostFixup $ \tx -> do
            let msg = hashToBytes . extractHash . hashAnnotated @_ @EraIndependentTxBody $ tx ^. bodyTxL
                wit = BlsPoolVoteWitness $ signDSIGN poolVoteSignContext msg blsSk
            pure $
              tx
                & witsTxL . poolVoteTxWitsL %~ Map.insert spoKh wit
                & witsTxL . addrTxWitsL %~ Set.filter ((/= asWitness spoKh) . witVKeyHash)
        setupPoolWithVotingKey = do
          spoKh <- freshKeyHash
          registerPool spoKh
          blsSk :: LeiosSigningKey <- liftGen genLeiosSigningKey
          let blsKey =
                BlsKey (deriveVerKeyDSIGN blsSk) (createPossessionProofDSIGN minSigPoPDST blsSk)
          submitTx_ $
            mkBasicTx (mkBasicTxBody & certsTxBodyL .~ [RegBlsKeyTxCert spoKh blsKey])
          pure (spoKh, blsSk)

    it "BLS witness authorizes an SPO vote without the cold key" $ do
      (spoKh, blsSk) <- setupPoolWithVotingKey
      gaId <- mkProposal InfoAction >>= submitProposal
      withPoolVoteWitness spoKh blsSk $ submitTx_ $ voteTx spoKh gaId
      gas <- getGovActionState gaId
      gasStakePoolVotes gas `shouldBe` Map.singleton spoKh VoteYes

    it "A pool-vote witness with an invalid signature is rejected" $ do
      (spoKh, blsSk) <- setupPoolWithVotingKey
      gaId <- mkProposal InfoAction >>= submitProposal
      let badWit =
            BlsPoolVoteWitness $
              signDSIGN poolVoteSignContext ("not-the-body-hash" :: BS.ByteString) blsSk
      withPostFixup
        ( \tx ->
            pure $
              tx
                & witsTxL . poolVoteTxWitsL %~ Map.insert spoKh badWit
                & witsTxL . addrTxWitsL %~ Set.filter ((/= asWitness spoKh) . witVKeyHash)
        )
        $ submitFailingTx
          (voteTx spoKh gaId)
          [injectFailure $ InvalidPoolVoteWitness $ NES.singleton spoKh]

    it "A pool-vote witness without a matching vote is rejected" $ do
      (spoKh, blsSk) <- setupPoolWithVotingKey
      withPoolVoteWitness spoKh blsSk $
        submitFailingTx
          (mkBasicTx mkBasicTxBody)
          [injectFailure $ ExtraneousPoolVoteWitness $ NES.singleton spoKh]

    it "A pool-vote witness without a registered voting key is rejected" $ do
      spoKh <- freshKeyHash
      registerPool spoKh
      blsSk :: LeiosSigningKey <- liftGen genLeiosSigningKey
      gaId <- mkProposal InfoAction >>= submitProposal
      withPoolVoteWitness spoKh blsSk $
        submitFailingTx
          (voteTx spoKh gaId)
          [injectFailure $ PoolVoteKeyNotRegistered $ NES.singleton spoKh]

    it "A pool-vote witness whose voting key aged out is rejected" $ do
      -- Shrink the KES setup so `maxKeyAgeEpochs` derives to 4 epochs
      -- instead of the mainnet-like 1862 of the Imp genesis.
      impGlobalsL %= \g -> g {maxKESEvo = 2, slotsPerKESPeriod = 4320}
      (spoKh, blsSk) <- setupPoolWithVotingKey
      passNEpochs 4
      gaId <- mkProposal InfoAction >>= submitProposal
      withPoolVoteWitness spoKh blsSk $
        submitFailingTx
          (voteTx spoKh gaId)
          [injectFailure $ PoolVoteKeyExpired $ NES.singleton spoKh]

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
