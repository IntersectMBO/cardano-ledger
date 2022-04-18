{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babbage.Rules.Utxow where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Alonzo.Data
  ( Data (Data),
    DataHash,
    hashData,
  )
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoEvent)
import Cardano.Ledger.Alonzo.Rules.Utxow
  ( AlonzoEvent (WrappedShelleyEraEvent),
    hasExactSetOfRedeemers,
    missingRequiredDatums,
    ppViewHashesMatch,
    requiredSignersAreWitnessed,
    witsVKeyNeeded,
  )
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), validScript)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (TxWitness'))
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo (TxDats (..))
import Cardano.Ledger.AuxiliaryData (ValidateAuxiliaryData)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Utxo
  ( BabbageUTXO,
    BabbageUtxoPred (..),
  )
import Cardano.Ledger.Babbage.Rules.Utxos (ConcreteBabbage)
import Cardano.Ledger.Babbage.Scripts (refScripts)
import Cardano.Ledger.Babbage.TxBody (Datum (..), TxOut (..))
import Cardano.Ledger.BaseTypes
  ( ProtVer,
    ShelleyBase,
    quorum,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.API (TxIn)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), witsFromTxWitnesses)
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
import qualified Data.ByteString as BS
import Data.Foldable (sequenceA_)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Plutus.V1.Ledger.Api (Data (..))
import qualified Plutus.V1.Ledger.Api as Plutus
import Validation (failureUnless)

-- ==================================================
-- Reuseable tests first used in the Babbage Era

-- Int the Babbage Era with reference scripts, the needed
-- scripts only has to be a subset of the txscripts.
{-  { s | (_,s) ∈ scriptsNeeded utxo tx} - dom(refScripts tx utxo) = dom(txscripts txw)  -}
{-  sNeeded := scriptsNeeded utxo tx                                                     -}
{-  sReceived := Map.keysSet (getField @"scriptWits" tx)                                 -}
babbageMissingScripts ::
  forall era.
  Core.PParams era ->
  Set (ScriptHash (Crypto era)) ->
  Set (ScriptHash (Crypto era)) ->
  Set (ScriptHash (Crypto era)) ->
  Test (Shelley.UtxowPredicateFailure era)
babbageMissingScripts _ sNeeded sRefs sReceived =
  sequenceA_
    [ failureUnless (Set.null extra) $ Shelley.ExtraneousScriptWitnessesUTXOW extra,
      failureUnless (Set.null missing) $ Shelley.MissingScriptWitnessesUTXOW missing
    ]
  where
    -- FIXME what about the hard forks?
    neededNonRefs = sNeeded `Set.difference` sRefs
    missing = neededNonRefs `Set.difference` sReceived
    extra = sReceived `Set.difference` neededNonRefs

{- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h) ∈ txouts tx ∪ utxo (refInputs tx)  -}
danglingWitnessDataHashes ::
  Era era =>
  Set.Set (DataHash (Crypto era)) ->
  Alonzo.TxDats era ->
  [TxOut era] ->
  Test (BabbageUtxoPred era)
danglingWitnessDataHashes inputHashes (Alonzo.TxDats m) outs =
  let hashesInUse = List.foldl' accum inputHashes outs
      accum ans (TxOut _ _ (DatumHash dhash) _) = Set.insert dhash ans
      accum ans _ = ans
   in failureUnless
        (eval (dom m ⊆ hashesInUse))
        (DanglingWitnessDataHash (eval (dom m ➖ hashesInUse)))

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
validateFailedBabbageScripts tx utxo =
  let failedScripts =
        Map.filterWithKey
          ( \hs script ->
              let one = isNativeScript @era script
                  two = hashScript @era script /= hs -- TODO this is probably not needed. Only the script is transmitted on the wire, we compute the hash
                  three = not (validateScript @era script tx)
                  answer = one && (two || three)
               in answer
          )
          (txscripts utxo tx)
   in failureUnless
        (Map.null failedScripts)
        (Shelley.ScriptWitnessNotValidatingUTXOW $ Map.keysSet failedScripts)

{- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ ⋃ ( , ,d,s)∈txouts tx{s, d},
                       x ∈ Script ∪ Datum ⇒ isWellFormed x
-}
validateScriptsWellFormed ::
  forall era.
  ( ExtendedUTxO era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    Core.Script era ~ Script era
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Test (BabbageUtxoPred era)
validateScriptsWellFormed pp tx utxo =
  let invalidScripts = Map.filter (not . validScript (getField @"_protocolVersion" pp)) (txscripts utxo tx)
   in failureUnless
        (Map.null invalidScripts)
        (MalformedScripts $ Map.keysSet invalidScripts)

validateDatumsWellFormed ::
  forall era.
  ( ExtendedUTxO era,
    Era era
  ) =>
  Core.Tx era ->
  Test (BabbageUtxoPred era)
validateDatumsWellFormed tx =
  let invalidDatums = Set.map hashData $ Set.filter (\(Data d) -> not $ validateData' d) (txdata tx)
   in failureUnless
        (Set.null invalidDatums)
        (MalformedData invalidDatums)

validateData' ::
  Plutus.Data ->
  Bool
validateData' (Constr _ ds) = all validateData' ds
validateData' (Map ds) = all (\(x, y) -> validateData' x && validateData' y) ds
validateData' (List ds) = all validateData' ds
validateData' (I _) = True
validateData' (B bs) = BS.length bs <= 64

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
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  TransitionRule (BabbageUTXOW era)
babbageUtxowTransition = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u
      txbody = getField @"body" (tx :: Core.Tx era)
      witsKeyHashes = witsFromTxWitnesses @era tx
      hashScriptMap = txscripts utxo tx
      inputs = getField @"inputs" txbody

  -- check scripts
  {- ∀s ∈ range(txscripts txw utxo ∩ Script^{ph1}), validateScript s tx -}
  runTest $ validateFailedBabbageScripts tx utxo -- CHANGED In BABBAGE txscripts depends on UTxO

  {-  { h | (_,h) ∈ scriptsNeeded utxo tx} ⊆ dom(txscripts txw utxo)     -}
  let sNeeded = Set.fromList (map snd (Alonzo.scriptsNeeded utxo tx)) -- Script credentials
      sReceived = Map.keysSet $ case getField @"wits" tx of
        (TxWitness' _ _ scs _ _) -> scs
      sRefs = Map.keysSet $ refScripts inputs utxo
  runTest $ babbageMissingScripts pp sNeeded sRefs sReceived

  {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
  runTest $ missingRequiredDatums hashScriptMap utxo tx txbody

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers utxo tx txbody

  -- check VKey witnesses
  -- let txbodyHash = hashAnnotated @(Crypto era) txbody
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsVKeyNeeded genDelegs utxo tx witsKeyHashes
  -- TODO can we add the required signers to witsVKeyNeeded so we dont need the check below?

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded must include the reqSignerHashes in the union   -}
  {- reqSignerHashes txbody ⊆ witsKeyHashes -}
  runTestOnSignal $ requiredSignersAreWitnessed txbody witsKeyHashes

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ |genSig| ≥ Quorum  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    Shelley.validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $
    Shelley.validateMetadata pp tx

  {- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ (⋃ ( , ,d,s) ∈ txouts tx {s, d}),
                         x ∈ Script ∪ Datum ⇒ isWellFormed x
  -}
  runTest $ validateScriptsWellFormed pp tx utxo
  runTest $ validateDatumsWellFormed tx

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
