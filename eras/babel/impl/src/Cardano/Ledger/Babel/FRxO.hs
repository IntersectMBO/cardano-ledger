{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Babel.FRxO where

import Cardano.Ledger.Address (Addr (..), RewardAccount (..), bootstrapKeyHash)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxowPredFailure (
    MissingRequiredDatums,
    NotAllowedSupplementalDatums,
    UnspendableUTxONoDatumHash
  ),
 )
import Cardano.Ledger.Alonzo.TxWits (unTxDats)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoScriptsNeeded (AlonzoScriptsNeeded),
  getInputDataHashesTxBody,
  getMintingScriptsNeeded,
  getRewardingScriptsNeeded,
  getSpendingScriptsNeeded,
  zipAsIxItem,
 )
import Cardano.Ledger.Babbage.Tx (isTwoPhaseScriptAddressFromMap)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedures (unVotingProcedures),
 )
import Cardano.Ledger.Credential (Credential (..), credKeyHashWitness, credScriptHash)
import Cardano.Ledger.FRxO (FRxO (FRxO))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Plutus.Data (Datum (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Rules.ValidationMode (Test)
import Cardano.Ledger.Shelley.UTxO (txinLookup)
import Cardano.Ledger.TxIn (TxIn (TxIn))
import Cardano.Ledger.UTxO (
  EraUTxO,
  ScriptsProvided (ScriptsProvided),
  UTxO (..),
  getScriptHash,
  getScriptsProvided,
 )
import Control.Iterate.Exp ((➖))
import Control.SetAlgebra (Basic (..), eval, (◁))
import Data.Foldable (Foldable (..), sequenceA_, toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Validation (failureUnless)

-- | The unspent transaction outputs.
-- | Compute the transaction requests of a transaction.
-- TODO WG: Put this in the FRxO module (along with other helpers). Probably refactor so the actual logic is done on maps, then unwrap both UTxO and FRxO and call the functions you refactored.
txfrxo ::
  forall era.
  BabelEraTxBody era =>
  TxBody era ->
  FRxO era
txfrxo txBody =
  FRxO $
    Map.fromList
      [ (TxIn transId idx, out)
      | (out, idx) <-
          zip
            (toList $ txBody ^. requestsTxBodyL)
            [minBound ..]
      ]
  where
    transId = txIdTxBody txBody

txrequests :: BabelEraTxBody era => TxBody era -> SSeq.StrictSeq (TxOut era)
txrequests = (^. requestsTxBodyL)

txrequired :: BabelEraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
txrequired = (^. requiredTxsTxBodyL)

txfulfills :: BabelEraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
txfulfills = (^. fulfillsTxBodyL)

-- To Babel Fees implementers: These functions are NOT in the right place.
-- Given more time, I'd do something with the EraUTxO class.
getScriptsProvidedFrxo ::
  ( EraTx era
  , BabelEraTxBody era
  ) =>
  FRxO era ->
  Tx era ->
  ScriptsProvided era
getScriptsProvidedFrxo frxo tx = ScriptsProvided ans
  where
    txBody = tx ^. bodyTxL
    ins = txBody ^. fulfillsTxBodyL
    ans = getReferenceScriptsFrxo frxo ins

getReferenceScriptsFrxo ::
  BabbageEraTxOut era =>
  FRxO era ->
  Set (TxIn (EraCrypto era)) ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era)
getReferenceScriptsFrxo frxo ins = Map.fromList (getReferenceScriptsNonDistinctFrxo frxo ins)

getReferenceScriptsNonDistinctFrxo ::
  BabbageEraTxOut era =>
  FRxO era ->
  Set (TxIn (EraCrypto era)) ->
  [(ScriptHash (EraCrypto era), Script era)]
getReferenceScriptsNonDistinctFrxo (FRxO mp) inputs =
  [ (hashScript script, script)
  | txOut <- Map.elems (eval (inputs ◁ mp))
  , SJust script <- [txOut ^. referenceScriptTxOutL]
  ]

getWitsVKeyNeededFrxo ::
  (EraTx era, BabelEraTxBody era) =>
  CertState era ->
  UTxO era ->
  FRxO era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
getWitsVKeyNeededFrxo _ utxo frxo txBody =
  getWitsVKeyNeededNoGovFrxo utxo frxo txBody
    `Set.union` (txBody ^. reqSignerHashesTxBodyL)
    `Set.union` voterWitnesses txBody

voterWitnesses ::
  ConwayEraTxBody era =>
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
voterWitnesses txb =
  Map.foldrWithKey' accum mempty (unVotingProcedures (txb ^. votingProceduresTxBodyL))
  where
    accum voter _ khs =
      maybe khs (`Set.insert` khs) $
        case voter of
          CommitteeVoter cred -> credKeyHashWitness cred
          DRepVoter cred -> credKeyHashWitness cred
          StakePoolVoter poolId -> Just $ asWitness poolId

getWitsVKeyNeededNoGovFrxo ::
  forall era.
  EraTx era =>
  UTxO era ->
  FRxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
getWitsVKeyNeededNoGovFrxo utxo' _frxo' txBody =
  -- TODO WG what do I do?
  certAuthors
    `Set.union` inputAuthors
    `Set.union` owners
    `Set.union` wdrlAuthors
  where
    inputAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    inputAuthors = foldr' accum Set.empty (txBody ^. spendableInputsTxBodyF)
      where
        accum txin !ans =
          case txinLookup txin utxo' of
            Just txOut ->
              case txOut ^. addrTxOutL of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans
    wdrlAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    wdrlAuthors = Map.foldrWithKey' accum Set.empty (unWithdrawals (txBody ^. withdrawalsTxBodyL))
      where
        accum key _ !ans =
          case credKeyHashWitness (raCredential key) of
            Nothing -> ans
            Just vkeyWit -> Set.insert vkeyWit ans
    owners :: Set (KeyHash 'Witness (EraCrypto era))
    owners = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum (RegPoolTxCert pool) !ans =
          Set.union
            (Set.map asWitness (ppOwners pool))
            ans
        accum _cert ans = ans
    certAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    certAuthors = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum cert !ans =
          case getVKeyWitnessTxCert cert of
            Nothing -> ans
            Just vkeyWit -> Set.insert vkeyWit ans

getBabelScriptsNeededFrxo ::
  BabelEraTxBody era =>
  UTxO era ->
  FRxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getBabelScriptsNeededFrxo utxo frxo txBody =
  getSpendingScriptsNeeded utxo txBody
    <> getSpendingScriptsNeededFrxo frxo txBody
    <> getRewardingScriptsNeeded txBody
    <> certifyingScriptsNeeded
    <> getMintingScriptsNeeded txBody
    <> votingScriptsNeeded
    <> proposingScriptsNeeded
  where
    certifyingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (txBody ^. certsTxBodyL) $
            \asIxItem@(AsIxItem _ txCert) ->
              (CertifyingPurpose asIxItem,) <$> getScriptWitnessTxCert txCert

    votingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (Map.keys (unVotingProcedures (txBody ^. votingProceduresTxBodyL))) $
            \asIxItem@(AsIxItem _ voter) ->
              (VotingPurpose asIxItem,) <$> getVoterScriptHash voter
      where
        getVoterScriptHash = \case
          CommitteeVoter cred -> credScriptHash cred
          DRepVoter cred -> credScriptHash cred
          StakePoolVoter _ -> Nothing

    proposingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (txBody ^. proposalProceduresTxBodyL) $
            \asIxItem@(AsIxItem _ proposal) ->
              (ProposingPurpose asIxItem,) <$> getProposalScriptHash proposal
      where
        getProposalScriptHash ProposalProcedure {pProcGovAction} =
          case pProcGovAction of
            ParameterChange _ _ (SJust govPolicyHash) -> Just govPolicyHash
            TreasuryWithdrawals _ (SJust govPolicyHash) -> Just govPolicyHash
            _ -> Nothing

getSpendingScriptsNeededFrxo ::
  BabelEraTxBody era =>
  FRxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getSpendingScriptsNeededFrxo (FRxO frxo) txBody =
  AlonzoScriptsNeeded $
    catMaybes $
      zipAsIxItem (txBody ^. fulfillsTxBodyL) $
        \asIxItem@(AsIxItem _ txIn) -> do
          addr <- view addrTxOutL <$> Map.lookup txIn frxo
          hash <- getScriptHash addr
          return (SpendingPurpose asIxItem, hash)
{-# INLINEABLE getSpendingScriptsNeededFrxo #-}

missingRequiredDatumsFrxo ::
  forall era.
  (AlonzoEraTx era, BabelEraTxBody era, EraUTxO era) =>
  UTxO era ->
  FRxO era ->
  Tx era ->
  Test (AlonzoUtxowPredFailure era)
missingRequiredDatumsFrxo utxo frxo tx = do
  let txBody = tx ^. bodyTxL
      scriptsProvided = getScriptsProvided utxo tx <> getScriptsProvidedFrxo frxo tx
      (inputHashes, txInsNoDataHash) =
        getInputDataHashesTxBody utxo txBody scriptsProvided
          <> getFulfillDataHashesTxBody frxo txBody scriptsProvided
      txHashes = domain (unTxDats $ tx ^. witsTxL . datsTxWitsL)
      unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      allowedSupplementalDataHashes = getSupplementalDataHashes utxo frxo txBody
      supplimentalDatumHashes = eval (txHashes ➖ inputHashes)
      (okSupplimentalDHs, notOkSupplimentalDHs) =
        Set.partition (`Set.member` allowedSupplementalDataHashes) supplimentalDatumHashes
  sequenceA_
    [ failureUnless
        (Set.null txInsNoDataHash)
        (UnspendableUTxONoDatumHash txInsNoDataHash)
    , failureUnless
        (Set.null unmatchedDatumHashes)
        (MissingRequiredDatums unmatchedDatumHashes txHashes)
    , failureUnless
        (Set.null notOkSupplimentalDHs)
        (NotAllowedSupplementalDatums notOkSupplimentalDHs okSupplimentalDHs)
    ]

getFulfillDataHashesTxBody ::
  BabelEraTxBody era =>
  FRxO era ->
  TxBody era ->
  ScriptsProvided era ->
  (Set.Set (DataHash (EraCrypto era)), Set.Set (TxIn (EraCrypto era)))
getFulfillDataHashesTxBody (FRxO mp) txBody (ScriptsProvided scriptsProvided) =
  Map.foldlWithKey' accum (Set.empty, Set.empty) spendUTxO
  where
    fInputs = txBody ^. fulfillsTxBodyL
    spendUTxO = eval (fInputs ◁ mp)
    accum ans@(!hashSet, !inputSet) txIn txOut =
      let addr = txOut ^. addrTxOutL
          isTwoPhaseScriptAddress = isTwoPhaseScriptAddressFromMap scriptsProvided addr
       in case txOut ^. datumTxOutF of
            NoDatum
              | isTwoPhaseScriptAddress -> (hashSet, Set.insert txIn inputSet)
            DatumHash dataHash
              | isTwoPhaseScriptAddress -> (Set.insert dataHash hashSet, inputSet)
            -- Though it is somewhat odd to allow non-two-phase-scripts to include a datum,
            -- the Alonzo era already set the precedent with datum hashes, and several dapp
            -- developers see this as a helpful feature.
            _ -> ans

getSupplementalDataHashes ::
  BabbageEraTxBody era =>
  UTxO era ->
  FRxO era ->
  TxBody era ->
  Set.Set (DataHash (EraCrypto era))
getSupplementalDataHashes (UTxO utxo) (FRxO frxo) txBody =
  Set.fromList [dh | txOut <- outs, SJust dh <- [txOut ^. dataHashTxOutL]]
  where
    newOuts = map sizedValue $ toList $ txBody ^. allSizedOutputsTxBodyF
    referencedOuts =
      -- TODO WG this is DEFINITELY wrong
      Map.elems (Map.restrictKeys utxo (txBody ^. referenceInputsTxBodyL))
        <> Map.elems (Map.restrictKeys frxo (txBody ^. referenceInputsTxBodyL))
    outs = newOuts <> referencedOuts