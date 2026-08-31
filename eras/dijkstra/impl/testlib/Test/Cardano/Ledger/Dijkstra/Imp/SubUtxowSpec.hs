{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubUtxowSpec (spec) where

import Cardano.Ledger.Address (bootstrapKeyHash)
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript (..))
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL, unTxDatsL)
import Cardano.Ledger.BaseTypes (Mismatch (..), SlotNo (..), StrictMaybe (..))
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionId,
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..), credKeyHashWitness)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraSubUtxowPredFailure (..))
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
  SLanguage (..),
  hashData,
  hashPlutusScript,
  plutusBinary,
  withSLanguage,
 )
import Cardano.Ledger.Plutus.Language (asSLanguage)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.State (StakePoolParams (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((%~), (&), (.~), (^.))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum, redeemerSameAsDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBUTXOW" $ do
  it "SubInvalidWitnessesUTXOW" $ do
    keyHash <- freshKeyHash @Payment
    keyPair <- getKeyPair $ asWitness keyHash
    txIn <- sendCoinTo (mkAddr keyHash StakeRefNull) mempty
    staleBodyHash <- arbitrary
    let subTx :: Tx SubTx era
        subTx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
        plantStaleWitness =
          pure . (witsTxL . addrTxWitsL .~ mkWitnessesVKey staleBodyHash [keyPair])
    withPostFixupSubTxs plantStaleWitness $
      submitFailingSubTx
        subTx
        [injectFailure . SubInvalidWitnessesUTXOW @era $ pure (vKey keyPair)]

  describe "SubMissingVKeyWitnessesUTXOW" $
    forM_ (missingVKeyWitnessSources @era) $ \(sourceName, mkSubTx) ->
      it sourceName $ do
        (subTx, keyHash) <- mkSubTx
        let dropWitness =
              pure
                . (witsTxL . addrTxWitsL %~ Set.filter ((/= keyHash) . witVKeyHash))
                . (witsTxL . bootAddrTxWitsL .~ mempty)
        withPostFixupSubTxs dropWitness $
          submitFailingSubTx
            subTx
            [injectFailure . SubMissingVKeyWitnessesUTXOW @era $ NES.singleton keyHash]

  describe "SubScriptWitnessNotValidatingUTXOW" $
    forM_ (failingNativeScriptPurposes @era) $ \(purposeName, mkSubTx) ->
      it purposeName $ do
        (subTx, scriptHash) <- mkSubTx
        submitFailingSubTx
          subTx
          [injectFailure . SubScriptWitnessNotValidatingUTXOW @era $ NES.singleton scriptHash]

  it "SubMissingTxMetadata" $ do
    auxData <- arbitrary @(TxAuxData era)
    let auxDataHash = hashTxAuxData auxData
        subTx :: Tx SubTx era
        subTx = mkBasicTx $ mkBasicTxBody & auxDataHashTxBodyL .~ SJust auxDataHash
    submitFailingSubTx subTx [injectFailure $ SubMissingTxMetadata @era auxDataHash]

  it "SubConflictingMetadataHash" $ do
    auxData <- arbitrary @(TxAuxData era)
    wrongAuxDataHash <- arbitrary @TxAuxDataHash
    let subTx :: Tx SubTx era
        subTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . auxDataHashTxBodyL .~ SJust wrongAuxDataHash
            & auxDataTxL .~ SJust auxData
    submitFailingSubTx
      subTx
      [ injectFailure . SubConflictingMetadataHash @era $
          Mismatch
            { mismatchSupplied = wrongAuxDataHash
            , mismatchExpected = hashTxAuxData auxData
            }
      ]

  it "SubMissingTxBodyMetadataHash" $ do
    auxData <- arbitrary @(TxAuxData era)
    let subTx :: Tx SubTx era
        subTx = mkBasicTx mkBasicTxBody & auxDataTxL .~ SJust auxData
        dropAuxDataHash =
          resetAddrTxWits . (bodyTxL . auxDataHashTxBodyL .~ SNothing)
    withPostFixupSubTxs dropAuxDataHash $
      submitFailingSubTx
        subTx
        [injectFailure . SubMissingTxBodyMetadataHash @era $ hashTxAuxData auxData]

  describe "SubExtraRedeemers" $ do
    it "for a native script, which takes no redeemer" $ do
      scriptHash <- impAddNativeScript $ RequireAllOf []
      txIn <- produceScript scriptHash
      redeemerData <- arbitrary
      let extraPurpose = mkSpendingPurpose $ AsIx 0
          subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
          addExtraRedeemer =
            resetAddrTxWits
              . ( witsTxL . rdmrsTxWitsL . unRedeemersL
                    %~ Map.insert extraPurpose (redeemerData, ExUnits 0 0)
                )
      withPostFixupSubTxs addExtraRedeemer $
        submitFailingSubTx subTx [injectFailure $ SubExtraRedeemers @era [extraPurpose]]

    it "at an index that points at no item" $ do
      keyHash <- freshKeyHash @Payment
      txIn <- sendCoinTo (mkAddr keyHash StakeRefNull) mempty
      redeemerData <- arbitrary
      let extraPurpose = mkSpendingPurpose $ AsIx 99
          subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
          addExtraRedeemer =
            resetAddrTxWits
              . ( witsTxL . rdmrsTxWitsL . unRedeemersL
                    %~ Map.insert extraPurpose (redeemerData, ExUnits 0 0)
                )
      withPostFixupSubTxs addExtraRedeemer $
        submitFailingSubTx subTx [injectFailure $ SubExtraRedeemers @era [extraPurpose]]

  describe "SubScriptIntegrityHashMismatch" $
    it "when no script requires an integrity hash" $ do
      badHash <- arbitrary
      let subTx :: Tx SubTx era
          subTx = mkBasicTx mkBasicTxBody
          supplyIntegrityHash =
            resetAddrTxWits . (bodyTxL . scriptIntegrityHashTxBodyL .~ SJust badHash)
      withPostFixupSubTxs supplyIntegrityHash $
        submitFailingSubTx
          subTx
          [ injectFailure $
              SubScriptIntegrityHashMismatch @era
                Mismatch {mismatchSupplied = SJust badHash, mismatchExpected = SNothing}
                SNothing
          ]

  describe "SubMalformedGuardDatums" $
    forM_ (malformedGuardDatumCases @era) $ \(caseName, mkGuard) ->
      it caseName $ do
        (guardCred, requiredGuards) <- mkGuard
        let subTx :: Tx SubTx era
            subTx = mkBasicTx $ mkBasicTxBody & requiredTopLevelGuardsL .~ requiredGuards
            topTx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardCred]
                & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTx
        submitFailingTx
          topTx
          [injectFailure . SubMalformedGuardDatums @era $ NES.singleton guardCred]

  describe "SubUnspendableUTxONoDatumHash, for languages that require a spending datum" $
    forM_ (filter (< PlutusV3) (eraLanguages @era)) $ \lang ->
      withSLanguage lang $ \slang ->
        it (show lang) $ do
          let scriptHash = hashPlutusScript $ redeemerSameAsDatum slang
          txIn <- impAnn "Produce a script output with no datum hash" $ do
            let addr = mkAddr scriptHash StakeRefNull
                tx =
                  mkBasicTx mkBasicTxBody
                    & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr mempty]
                resetTxOutDataHash =
                  bodyTxL . outputsTxBodyL
                    %~ ( \case
                           h :<| r -> (h & dataHashTxOutL .~ SNothing) :<| r
                           _ -> error "Expected non-empty outputs"
                       )
            txInAt 0
              <$> withPostFixup (resetAddrTxWits . resetTxOutDataHash) (submitTx tx)
          submitFailingSubTx
            (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn])
            [injectFailure . SubUnspendableUTxONoDatumHash @era $ NES.singleton txIn]

  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let redeemerSameAsDatumHash = hashPlutusScript $ redeemerSameAsDatum slang
            fixupResetAddrWits = fixupPPHash >=> resetAddrTxWits
            scriptSpendingSubTx txIn = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]

        it "SubMissingRequiredDatums" $ do
          txIn <- produceScript redeemerSameAsDatumHash
          let missingDatum = hashData @era (Data (P.I 3))
          withPostFixupSubTxs (fixupResetAddrWits . (witsTxL . datsTxWitsL .~ mempty)) $
            submitFailingSubTx
              (scriptSpendingSubTx txIn)
              [injectFailure $ SubMissingRequiredDatums @era (NES.singleton missingDatum) []]

        it "SubNotAllowedSupplementalDatums" $ do
          txIn <- produceScript redeemerSameAsDatumHash
          let extraDatum = Data (P.I 30)
              extraDatumHash = hashData @era extraDatum
              addExtraDatum =
                fixupResetAddrWits
                  . ( witsTxL . datsTxWitsL . unTxDatsL
                        %~ Map.insert extraDatumHash extraDatum
                    )
          withPostFixupSubTxs addExtraDatum $
            submitFailingSubTx
              (scriptSpendingSubTx txIn)
              [ injectFailure $
                  SubNotAllowedSupplementalDatums @era (NES.singleton extraDatumHash) []
              ]

        it "SubMissingRedeemers" $ do
          txIn <- produceScript redeemerSameAsDatumHash
          let missingRedeemer = mkSpendingPurpose $ AsItem txIn
          withPostFixupSubTxs (fixupResetAddrWits . (witsTxL . rdmrsTxWitsL .~ mempty)) $
            submitFailingSubTx
              (scriptSpendingSubTx txIn)
              [ injectFailure $
                  SubMissingRedeemers @era [(missingRedeemer, redeemerSameAsDatumHash)]
              ]

        it "SubExtraRedeemers" $ do
          txIn <- produceScript redeemerSameAsDatumHash
          redeemerData <- arbitrary
          let extraPurpose = mkMintingPurpose $ AsIx 2
              addExtraRedeemer =
                fixupResetAddrWits
                  . ( witsTxL . rdmrsTxWitsL . unRedeemersL
                        %~ Map.insert extraPurpose (redeemerData, ExUnits 0 0)
                    )
          withPostFixupSubTxs addExtraRedeemer $
            submitFailingSubTx
              (scriptSpendingSubTx txIn)
              [injectFailure $ SubExtraRedeemers @era [extraPurpose]]

        describe "SubScriptIntegrityHashMismatch" $ do
          let testHashMismatch badHash = do
                txIn <- produceScript redeemerSameAsDatumHash
                let topTx =
                      mkBasicTx mkBasicTxBody
                        & bodyTxL . subTransactionsTxBodyL
                          .~ OMap.singleton (scriptSpendingSubTx txIn)
                fixedUpTx <- fixupTx topTx
                let fixedUpSubTxs = OMap.elems $ fixedUpTx ^. bodyTxL . subTransactionsTxBodyL
                fixedUpSubTx <- case fixedUpSubTxs of
                  [subTx] -> pure subTx
                  _ -> assertFailure "Expected exactly one sub-transaction"
                let goodHash = fixedUpSubTx ^. bodyTxL . scriptIntegrityHashTxBodyL
                expectedIntegrity <- impComputeScriptIntegrity fixedUpSubTx
                badSubTx <-
                  resetAddrTxWits $
                    fixedUpSubTx & bodyTxL . scriptIntegrityHashTxBodyL .~ badHash
                badTopTx <-
                  resetAddrTxWits $
                    fixedUpTx & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton badSubTx
                withNoFixup $
                  submitFailingTx
                    badTopTx
                    [ injectFailure $
                        SubScriptIntegrityHashMismatch @era
                          Mismatch {mismatchSupplied = badHash, mismatchExpected = goodHash}
                          (originalBytes <$> expectedIntegrity)
                    ]
          it "the supplied hash is wrong" $ testHashMismatch . SJust =<< arbitrary
          it "the supplied hash is missing" $ testHashMismatch SNothing

        disableInConformanceIt "SubMalformedScriptWitnesses" $ do
          let scriptHash = hashPlutusScript $ asSLanguage slang malformedPlutus
          txIn <- produceScript scriptHash
          submitFailingSubTx
            (scriptSpendingSubTx txIn)
            [injectFailure . SubMalformedScriptWitnesses @era $ NES.singleton scriptHash]

        disableInConformanceIt "SubMalformedReferenceScripts" $ do
          script <- fromPlutusScript <$> mkPlutusScript (asSLanguage slang malformedPlutus)
          addr <- freshKeyAddr_
          let subTx :: Tx SubTx era
              subTx =
                mkBasicTx $
                  mkBasicTxBody
                    & outputsTxBodyL
                      .~ [mkBasicTxOut addr mempty & referenceScriptTxOutL .~ SJust script]
          submitFailingSubTx
            subTx
            [ injectFailure . SubMalformedReferenceScripts @era . NES.singleton $
                hashScript script
            ]

        it "SubInvalidMetadata" $ do
          let auxData :: TxAuxData era
              auxData =
                mkBasicTxAuxData
                  & plutusScriptsTxAuxDataL
                    .~ Map.singleton lang (pure . plutusBinary $ asSLanguage slang malformedPlutus)
              subTx :: Tx SubTx era
              subTx = mkBasicTx mkBasicTxBody & auxDataTxL .~ SJust auxData
          submitFailingSubTx subTx [injectFailure $ SubInvalidMetadata @era]

-- | Every distinct reason a sub-transaction requires a key witness, paired with a
-- sub-transaction that requires it and the key hash whose witness is to be withheld.
missingVKeyWitnessSources ::
  forall era.
  DijkstraEraImp era =>
  [(String, ImpTestM era (Tx SubTx era, KeyHash Witness))]
missingVKeyWitnessSources =
  [
    ( "spending a key hash locked input"
    , do
        keyHash <- freshKeyHash @Payment
        txIn <- sendCoinTo (mkAddr keyHash StakeRefNull) mempty
        pure (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn], asWitness keyHash)
    )
  ,
    ( "unregistering a staking credential"
    , do
        keyHash <- freshKeyHash
        void . registerStakeCredential $ KeyHashObj keyHash
        deposit <- getsPParams ppKeyDepositL
        pure
          ( mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL .~ [UnRegDepositTxCert (KeyHashObj keyHash) deposit]
          , asWitness keyHash
          )
    )
  ,
    ( "withdrawing from an account"
    , do
        keyHash <- freshKeyHash
        accountAddress <- registerStakeCredential $ KeyHashObj keyHash
        pure
          ( mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(accountAddress, mempty)]
          , asWitness keyHash
          )
    )
  ,
    ( "requiring a key hash guard"
    , do
        keyHash <- freshKeyHash
        pure
          ( mkBasicTx $ mkBasicTxBody & guardsTxBodyL .~ [KeyHashObj keyHash]
          , asWitness keyHash
          )
    )
  ,
    ( "spending a bootstrap address input"
    , do
        bootAddr <- freshBootstapAddress
        txIn <- sendCoinTo (AddrBootstrap bootAddr) mempty
        pure
          ( mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
          , asWitness $ bootstrapKeyHash bootAddr
          )
    )
  ,
    ( "registering a stake pool with an owner"
    , do
        poolKeyHash <- freshKeyHash
        ownerKeyHash <- freshKeyHash
        accountAddress <- registerStakeCredential . KeyHashObj =<< freshKeyHash
        poolParams <- freshPoolParams poolKeyHash accountAddress
        pure
          ( mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL
                  .~ [RegPoolTxCert poolParams {sppOwners = Set.singleton ownerKeyHash}]
          , asWitness ownerKeyHash
          )
    )
  ,
    ( "voting as a DRep"
    , do
        (drepCredential, _, _) <- setupSingleDRep 1_000_000
        govActionId <- submitGovAction InfoAction
        keyHash <- expectJust $ credKeyHashWitness drepCredential
        pure (voteSubTx (DRepVoter drepCredential) govActionId, keyHash)
    )
  ,
    ( "voting as a committee member"
    , do
        hotCredential <- NE.head <$> registerInitialCommittee
        govActionId <- submitGovAction InfoAction
        keyHash <- expectJust $ credKeyHashWitness hotCredential
        pure (voteSubTx (CommitteeVoter hotCredential) govActionId, keyHash)
    )
  ,
    ( "voting as a stake pool"
    , do
        poolKeyHash <- freshKeyHash
        registerPool poolKeyHash
        govActionId <- submitGovAction InfoAction
        pure (voteSubTx (StakePoolVoter poolKeyHash) govActionId, asWitness poolKeyHash)
    )
  ]

-- | A sub-transaction that casts a single yes vote.
voteSubTx :: DijkstraEraImp era => Voter -> GovActionId -> Tx SubTx era
voteSubTx voter govActionId =
  mkBasicTx $
    mkBasicTxBody
      & votingProceduresTxBodyL
        .~ VotingProcedures
          ( Map.singleton voter . Map.singleton govActionId $
              VotingProcedure {vProcVote = VoteYes, vProcAnchor = SNothing}
          )

-- | Every script purpose at which a sub-transaction can require a native script, paired
-- with a sub-transaction that needs a failing script for that purpose and the hash of that
-- script.
failingNativeScriptPurposes ::
  forall era.
  DijkstraEraImp era =>
  [(String, ImpTestM era (Tx SubTx era, ScriptHash))]
failingNativeScriptPurposes =
  [
    ( "spending"
    , do
        scriptHash <- unsatisfiableTimeLock
        txIn <- produceScript scriptHash
        pure (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn], scriptHash)
    )
  ,
    ( "certifying"
    , do
        scriptHash <- unsatisfiableTimeLock
        deposit <- getsPParams ppKeyDepositL
        pure
          ( mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL .~ [RegDepositTxCert (ScriptHashObj scriptHash) deposit]
          , scriptHash
          )
    )
  ,
    ( "guarding"
    , do
        scriptHash <- unsatisfiableTimeLock
        pure
          ( mkBasicTx $ mkBasicTxBody & guardsTxBodyL .~ [ScriptHashObj scriptHash]
          , scriptHash
          )
    )
  ,
    ( "minting"
    , do
        scriptHash <- unsatisfiableTimeLock
        subTx <- mkTokenMintingTx scriptHash
        pure (subTx, scriptHash)
    )
  ,
    ( "withdrawing"
    , do
        (scriptHash, accountAddress) <- registerLowerBoundTimeLockAccount
        pure
          ( mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(accountAddress, mempty)]
          , scriptHash
          )
    )
  ,
    ( "voting"
    , do
        scriptHash <- registerLowerBoundTimeLockDRep
        govActionId <- submitGovAction InfoAction
        pure (voteSubTx (DRepVoter (ScriptHashObj scriptHash)) govActionId, scriptHash)
    )
  ]

-- | A time lock that no sub-transaction built by these tests can satisfy, because none of
-- them sets a lower bound on its validity interval.
unsatisfiableTimeLock :: DijkstraEraImp era => ImpTestM era ScriptHash
unsatisfiableTimeLock = impAddNativeScript $ mkTimeStart (SlotNo maxBound)

-- | A time lock that is satisfied only by a transaction that declares a lower bound on its
-- validity interval. Registering the credential that it locks therefore succeeds, while a
-- sub-transaction, which declares no lower bound, fails to satisfy it.
lowerBoundTimeLock :: DijkstraEraImp era => ImpTestM era ScriptHash
lowerBoundTimeLock = impAddNativeScript $ mkTimeStart lowerBoundSlot

lowerBoundSlot :: SlotNo
lowerBoundSlot = SlotNo 1

registerLowerBoundTimeLockAccount ::
  DijkstraEraImp era => ImpTestM era (ScriptHash, AccountAddress)
registerLowerBoundTimeLockAccount = do
  scriptHash <- lowerBoundTimeLock
  deposit <- getsPParams ppKeyDepositL
  submitTx_ $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ [RegDepositTxCert (ScriptHashObj scriptHash) deposit]
      & bodyTxL . vldtTxBodyL .~ ValidityInterval (SJust lowerBoundSlot) SNothing
  accountAddress <- getAccountAddressFor $ ScriptHashObj scriptHash
  pure (scriptHash, accountAddress)

registerLowerBoundTimeLockDRep :: DijkstraEraImp era => ImpTestM era ScriptHash
registerLowerBoundTimeLockDRep = do
  scriptHash <- lowerBoundTimeLock
  deposit <- getsPParams ppDRepDepositL
  submitTx_ $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ [RegDRepTxCert (ScriptHashObj scriptHash) deposit SNothing]
      & bodyTxL . vldtTxBodyL .~ ValidityInterval (SJust lowerBoundSlot) SNothing
  pure scriptHash

-- | The ways a guard credential can carry the wrong datum presence, paired with the guard
-- credential and the @requiredTopLevelGuards@ entry that makes it malformed.
malformedGuardDatumCases ::
  forall era.
  DijkstraEraImp era =>
  [ ( String
    , ImpTestM era (Credential Guard, Map.Map (Credential Guard) (StrictMaybe (Data era)))
    )
  ]
malformedGuardDatumCases =
  [
    ( "a key hash guard carrying a datum"
    , do
        guardCred <- KeyHashObj <$> freshKeyHash
        datum <- arbitrary @(Data era)
        pure (guardCred, Map.singleton guardCred (SJust datum))
    )
  ,
    ( "a native script guard carrying a datum"
    , do
        guardCred <- ScriptHashObj <$> impAddNativeScript (RequireAllOf [])
        datum <- arbitrary @(Data era)
        pure (guardCred, Map.singleton guardCred (SJust datum))
    )
  ,
    ( "a Plutus script guard without a datum"
    , do
        plutusScript <- mkPlutusScript $ alwaysSucceedsNoDatum SPlutusV4
        let guardScript = fromPlutusScript plutusScript :: Script era
            guardCred = ScriptHashObj $ hashScript guardScript
        pure (guardCred, Map.singleton guardCred SNothing)
    )
  ]
