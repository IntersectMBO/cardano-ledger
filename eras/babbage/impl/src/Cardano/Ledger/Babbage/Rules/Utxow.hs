{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Ledger.Babbage.Rules.Utxow where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash, hash)
import Cardano.Ledger.Alonzo.Data (DataHash)
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (language, scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Ledger (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoEvent)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoEvent (WrappedShelleyEraEvent), UtxowPredicateFail, hasExactSetOfRedeemers, missingRequiredDatums, ppViewHashesMatch, requiredSignersAreWitnessed, witsVKeyNeeded)
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose, ValidatedTx (..), wits)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..))
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo (TxDats (..), TxWitness (..), txdats')
import Cardano.Ledger.AuxiliaryData (ValidateAuxiliaryData)
import qualified Cardano.Ledger.Babbage.Collateral as Babbage (isTwoPhaseScriptAddress)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Utxo
  ( BabbageUTXO,
    BabbageUtxoPred (..),
  )
import Cardano.Ledger.Babbage.Rules.Utxos (ConcreteBabbage)
import Cardano.Ledger.Babbage.TxBody
  ( Datum (..),
    TxBody (..),
    TxOut (..),
    outputs',
    referenceInputs',
    spendInputs',
  )
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    quorum,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.SafeHash (HashAnnotated, hashAnnotated)
import Cardano.Ledger.Shelley.Constraints (UsesTxOut (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), witsFromTxWitnesses)
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEvent (UtxowEvent), LedgerPredicateFailure (UtxowFailure))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow
  ( UtxowEvent (UtxoEvent),
    validateNeededWitnesses,
  )
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (⊆), (➖))
import Control.State.Transition.Extended
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
  )
import qualified Data.Compact.SplitMap as SplitMap (foldlWithKey', lookup, (◁))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Validation (failureUnless)

-- ==================================================
-- Reuseable tests first used in the Babbage Era

{- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h) ∈ txouts tx ∪ utxo (refInputs tx)  -}
danglingWitnessDataHashes ::
  Era era =>
  Set.Set (DataHash (Crypto era)) ->
  (Alonzo.TxDats era) ->
  [TxOut era] ->
  Test (BabbageUtxoPred era)
danglingWitnessDataHashes inputHashes (Alonzo.TxDats m) outs =
  let hashesInUse = List.foldl' accum inputHashes outs
      accum ans (TxOut _ _ (DatumHash dhash) _) = Set.insert dhash ans
      accum ans _ = ans
   in failureUnless
        (eval (dom (m) ⊆ hashesInUse))
        (DanglingWitnessDataHash (eval (dom (m) ➖ hashesInUse)))

{-  ∀ s ∈ (txscripts txw utxo ∩ Scriptnative), validateScript s tx   -}
validateFailedBabbageScripts ::
  forall era.
  ( ValidateScript era,
    ExtendedUTxO era,
    Core.Script era ~ Script era
  ) =>
  Core.Tx era ->
  UTxO era ->
  Test (Shelley.UtxowPredicateFailure era)
validateFailedBabbageScripts tx utxo = do
  let failedScripts =
        Map.filterWithKey
          ( \hs script ->
              isNativeScript @era script
                && ( hashScript @era script /= hs
                       || not (validateScript @era script tx)
                   )
          )
          (txscripts utxo tx)
  failureUnless (Map.null failedScripts) $
    Shelley.ScriptWitnessNotValidatingUTXOW (Map.keysSet failedScripts)

-- ==============================================================
-- Here we define the transtion function, using reusable tests.
-- The tests are very generic and reusable, but the transition
-- function is very specific to the Babbage Era.

data BabbageUTXOW era

-- | A very specialized transitionRule function for the Babbage Era.
babbageUtxowTransition ::
  forall era.
  ( ValidateScript era,
    ValidateAuxiliaryData era (Crypto era),
    ExtendedUTxO era,
    STS (BabbageUTXOW era),
    -- Fix some Core types to the Babbage Era
    ConcreteBabbage era,
    -- Crypto magic
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (BabbageUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era
  ) =>
  TransitionRule (BabbageUTXOW era)
babbageUtxowTransition = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo@(UTxO mp) = _utxo u
      txbody = getField @"body" (tx :: Core.Tx era)
      txw = Alonzo.txdats' (wits tx)
      witsKeyHashes = witsFromTxWitnesses @era tx
      {- txwitscripts tx ∪ {hash s ↦ s | ( , , , s) ∈ utxo (spendInputs tx ∪ refInputs tx)} -}
      hashScriptMap = txscripts utxo tx
      {- { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a} -}
      (inputHashes, _) = inputDataHashes (txscripts utxo tx) tx utxo

  -- check scripts
  {- ∀s ∈ range(txscripts txw utxo ∩ Script^{ph1}), validateScript s tx -}
  runTest $ validateFailedBabbageScripts tx utxo -- CHANGED In BABBAGE txscripts depends on UTxO

  {-  { h | (_,h) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  let sNeeded = Set.fromList (map snd (Alonzo.scriptsNeeded utxo tx))
      sReceived = Map.keysSet hashScriptMap
  runTest $ Shelley.validateMissingScripts pp sNeeded sReceived

  {-  inputHashes  = dom(txdats txw)   -}
  runTest $ missingRequiredDatums hashScriptMap utxo tx txbody

  {- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h, ) ∈ txouts tx ∪ utxo (refInputs tx) } -}
  let outs = foldr (:) [] (outputs' txbody)
      allouts = Set.foldl' accum outs (referenceInputs' txbody)
      accum ans refinput = case SplitMap.lookup refinput mp of Just out -> out : ans; Nothing -> ans
  runTest $ danglingWitnessDataHashes inputHashes txw allouts

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers utxo tx txbody -- FIXME pass txscripts as parameter

  -- let txbodyHash = hashAnnotated @(Crypto era) txbody

  -- check VKey witnesses
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsVKeyNeeded genDelegs utxo tx witsKeyHashes

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded must include the reqSignerHashes in the union   -}
  {- reqSignerHashes txbody ⊆ witsKeyHashes -}
  runTestOnSignal $ requiredSignersAreWitnessed txbody witsKeyHashes

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    Shelley.validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $
    Shelley.validateMetadata pp tx

  {- languages tx utxo ⊆ dom(costmdls tx) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'. This check
  -- which appears in the spec, seems broken since costmdls is a projection of PPrams, not Tx

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx txbody pp utxo

  trans @(Core.EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

-- ================================

instance
  forall era.
  ( ValidateScript era,
    ValidateAuxiliaryData era (Crypto era),
    ExtendedUTxO era,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Fix some Core types to the Babbage Era
    Core.Tx era ~ ValidatedTx era,
    ConcreteBabbage era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (BabbageUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    Eq (PredicateFailure (Core.EraRule "UTXOS" era)),
    Show (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  STS (BabbageUTXOW era)
  where
  type State (BabbageUTXOW era) = UTxOState era
  type Signal (BabbageUTXOW era) = ValidatedTx era
  type Environment (BabbageUTXOW era) = UtxoEnv era
  type BaseM (BabbageUTXOW era) = ShelleyBase
  type PredicateFailure (BabbageUTXOW era) = BabbageUtxoPred era
  type Event (BabbageUTXOW era) = AlonzoEvent era
  transitionRules = [babbageUtxowTransition]
  initialRules = []

instance
  ( Era era,
    STS (BabbageUTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ BabbageUtxoPred era,
    Event (Core.EraRule "UTXO" era) ~ Alonzo.UtxoEvent era,
    BaseM (BabbageUTXOW era) ~ ShelleyBase,
    PredicateFailure (BabbageUTXOW era) ~ BabbageUtxoPred era,
    Event (BabbageUTXOW era) ~ AlonzoEvent era
  ) =>
  Embed (BabbageUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = id
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

{-
-- | In orer to reuse the AlonzoLEDGER STS instance we need to embed this UTXOW instance into it.
instance ( ConcreteBabbage era
         , Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era
         , State (Core.EraRule "UTXO" era) ~ UTxOState era
         , Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era
         , Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody)
         , Eq (PredicateFailure (Core.EraRule "UTXOS" era))
         , Show (PredicateFailure (Core.EraRule "UTXOS" era))
         , Embed (Core.EraRule "UTXO" era) (BabbageUTXOW era)
         , ValidateAuxiliaryData era (Crypto era)
         , ValidateScript era
      --   , PredicateFailure (Core.EraRule "UTXOW" era) ~ BabbageUtxoPred era
         , Event (Core.EraRule "UTXOW" era) ~ AlonzoEvent era
         ) =>
         Embed (BabbageUTXOW era) (AlonzoLEDGER era) where
  wrapFailed =  foo -- FromAlonzoUtxowFail . UtxowFailure
  wrapEvent = UtxowEvent

foo :: BabbageUtxoPred era -> LedgerPredicateFailure era
foo _ = undefined
-}
