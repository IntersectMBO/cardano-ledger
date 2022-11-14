{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Utxow
  ( BabbageUTXOW,
    BabbageUtxowPredFailure (..),
    babbageMissingScripts,
    validateFailedBabbageScripts,
    validateScriptsWellFormed,
    babbageUtxowTransition,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxowEvent (WrappedShelleyEraEvent),
    AlonzoUtxowPredFailure (ShelleyInAlonzoUtxowPredFailure),
    hasExactSetOfRedeemers,
    missingRequiredDatums,
    ppViewHashesMatch,
    requiredSignersAreWitnessed,
    witsVKeyNeeded,
  )
import Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoUtxoEvent)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript, CostModels)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), validScript)
import Cardano.Ledger.Babbage.Era (BabbageUTXOW)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUTXO, BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.Tx (refScripts)
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody (..),
    BabbageEraTxOut (..),
    BabbageTxOut (..),
  )
import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase, quorum, strictMaybeToMaybe)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), witsFromTxWitnesses)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow
  ( ShelleyUtxowEvent (UtxoEvent),
    ShelleyUtxowPredFailure,
    validateNeededWitnesses,
  )
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Coders
  ( Decode (From, Invalid, SumD, Summands),
    Encode (Sum, To),
    decode,
    encode,
    (!>),
    (<!),
  )
import Data.Foldable (sequenceA_, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Records (HasField (..))
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (failureUnless)

data BabbageUtxowPredFailure era
  = AlonzoInBabbageUtxowPredFailure !(AlonzoUtxowPredFailure era)
  | -- | Embed UTXO rule failures
    UtxoFailure !(PredicateFailure (EraRule "UTXO" era))
  | -- | the set of malformed script witnesses
    MalformedScriptWitnesses
      !(Set (ScriptHash (Crypto era)))
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      !(Set (ScriptHash (Crypto era)))

deriving instance
  ( Era era,
    Show (ShelleyUtxowPredFailure era),
    Show (PredicateFailure (EraRule "UTXO" era)),
    Show (PredicateFailure (EraRule "UTXOS" era)),
    Show (Script era),
    Show (TxOut era),
    Show (TxBody era),
    Show (Value era)
  ) =>
  Show (BabbageUtxowPredFailure era)

deriving instance
  ( Era era,
    Eq (ShelleyUtxowPredFailure era),
    Eq (PredicateFailure (EraRule "UTXO" era)),
    Eq (PredicateFailure (EraRule "UTXOS" era)),
    Eq (TxOut era),
    Eq (Script era)
  ) =>
  Eq (BabbageUtxowPredFailure era)

instance Inject (AlonzoUtxowPredFailure era) (BabbageUtxowPredFailure era) where
  inject = AlonzoInBabbageUtxowPredFailure

instance Inject (ShelleyUtxowPredFailure era) (BabbageUtxowPredFailure era) where
  inject = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure

instance
  ( Era era,
    ToCBOR (TxOut era),
    ToCBOR (Value era),
    ToCBOR (PredicateFailure (EraRule "UTXOS" era)),
    ToCBOR (PredicateFailure (EraRule "UTXO" era)),
    ToCBOR (Script era),
    Typeable (AuxiliaryData era)
  ) =>
  ToCBOR (BabbageUtxowPredFailure era)
  where
  toCBOR = encode . work
    where
      work (AlonzoInBabbageUtxowPredFailure x) = Sum AlonzoInBabbageUtxowPredFailure 1 !> To x
      work (UtxoFailure x) = Sum UtxoFailure 2 !> To x
      work (MalformedScriptWitnesses x) = Sum MalformedScriptWitnesses 3 !> To x
      work (MalformedReferenceScripts x) = Sum MalformedReferenceScripts 4 !> To x

instance
  ( Era era,
    FromCBOR (TxOut era),
    FromCBOR (Value era),
    FromCBOR (PredicateFailure (EraRule "UTXOS" era)),
    FromCBOR (PredicateFailure (EraRule "UTXO" era)),
    Typeable (Script era),
    Typeable (AuxiliaryData era)
  ) =>
  FromCBOR (BabbageUtxowPredFailure era)
  where
  fromCBOR = decode (Summands "BabbageUtxowPred" work)
    where
      work 1 = SumD AlonzoInBabbageUtxowPredFailure <! From
      work 2 = SumD UtxoFailure <! From
      work 3 = SumD MalformedScriptWitnesses <! From
      work 4 = SumD MalformedReferenceScripts <! From
      work n = Invalid n

deriving via
  InspectHeapNamed "BabbageUtxowPred" (BabbageUtxowPredFailure era)
  instance
    NoThunks (BabbageUtxowPredFailure era)

-- ==================================================
-- Reuseable tests first used in the Babbage Era

-- Int the Babbage Era with reference scripts, the needed
-- scripts only has to be a subset of the txscripts.
{-  { s | (_,s) ∈ scriptsNeeded utxo tx} - dom(refScripts tx utxo) = dom(txscripts txw)  -}
{-  sNeeded := scriptsNeeded utxo tx                                                     -}
{-  sReceived := Map.keysSet (getField @"scriptWits" tx)                                 -}
babbageMissingScripts ::
  forall era.
  PParams era ->
  Set (ScriptHash (Crypto era)) ->
  Set (ScriptHash (Crypto era)) ->
  Set (ScriptHash (Crypto era)) ->
  Test (ShelleyUtxowPredFailure era)
babbageMissingScripts _ sNeeded sRefs sReceived =
  sequenceA_
    [ failureUnless (Set.null extra) $ Shelley.ExtraneousScriptWitnessesUTXOW extra,
      failureUnless (Set.null missing) $ Shelley.MissingScriptWitnessesUTXOW missing
    ]
  where
    neededNonRefs = sNeeded `Set.difference` sRefs
    missing = neededNonRefs `Set.difference` sReceived
    extra = sReceived `Set.difference` neededNonRefs

{-  ∀ s ∈ (txscripts txw utxo ∩ Scriptnative), validateScript s tx   -}
validateFailedBabbageScripts ::
  forall era.
  ( EraTx era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era
  ) =>
  Tx era ->
  UTxO era ->
  Set (ScriptHash (Crypto era)) ->
  Test (ShelleyUtxowPredFailure era)
validateFailedBabbageScripts tx utxo neededHashes =
  let phase1Map = getPhase1 (txscripts utxo tx)
      failedScripts =
        Map.filterWithKey
          ( \hs (script, phased) ->
              let needed = hs `Set.member` neededHashes
                  hashDisagrees = hashScript @era script /= hs
                  -- TODO this is probably not needed. Only the script is transmitted on the wire, we compute the hash
                  scriptDoesNotValidate = not (validateScript @era phased tx)
                  answer = needed && (hashDisagrees || scriptDoesNotValidate)
               in answer
          )
          phase1Map
   in failureUnless
        (Map.null failedScripts)
        (Shelley.ScriptWitnessNotValidatingUTXOW $ Map.keysSet failedScripts)

{- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ ⋃ ( , ,d,s)∈txouts tx{s, d},
                       x ∈ Script ∪ Datum ⇒ isWellFormed x
-}
validateScriptsWellFormed ::
  forall era.
  ( EraTx era,
    BabbageEraTxBody era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Script era ~ AlonzoScript era,
    TxOut era ~ BabbageTxOut era
  ) =>
  PParams era ->
  Tx era ->
  Test (BabbageUtxowPredFailure era)
validateScriptsWellFormed pp tx =
  sequenceA_
    [ failureUnless (Map.null invalidScriptWits) $ MalformedScriptWitnesses (Map.keysSet invalidScriptWits),
      failureUnless (null invalidRefScripts) $ MalformedReferenceScripts invalidRefScriptHashes
    ]
  where
    scriptWits = tx ^. witsTxL . scriptWitsL
    invalidScriptWits = Map.filter (not . validScript (getField @"_protocolVersion" pp)) scriptWits

    txBody = tx ^. bodyTxL
    normalOuts = toList $ txBody ^. outputsTxBodyL
    returnOut = txBody ^. collateralReturnTxBodyL
    outs = case returnOut of
      SNothing -> normalOuts
      SJust rOut -> rOut : normalOuts
    rScripts = mapMaybe (strictMaybeToMaybe . view referenceScriptTxOutL) outs
    invalidRefScripts = filter (not . validScript (getField @"_protocolVersion" pp)) rScripts
    invalidRefScriptHashes = Set.fromList $ map (hashScript @era) invalidRefScripts

-- ==============================================================
-- Here we define the transtion function, using reusable tests.
-- The tests are very generic and reusable, but the transition
-- function is very specific to the Babbage Era.

-- | A very specialized transitionRule function for the Babbage Era.
babbageUtxowTransition ::
  forall era.
  ( EraTx era,
    ExtendedUTxO era,
    Tx era ~ AlonzoTx era,
    Script era ~ AlonzoScript era,
    TxOut era ~ BabbageTxOut era,
    STS (BabbageUTXOW era),
    BabbageEraTxBody era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (BabbageUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ AlonzoTx era
  ) =>
  TransitionRule (BabbageUTXOW era)
babbageUtxowTransition = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u
      txBody = tx ^. bodyTxL
      witsKeyHashes = witsFromTxWitnesses @era tx
      hashScriptMap = txscripts utxo tx
      inputs = (txBody ^. referenceInputsTxBodyL) `Set.union` (txBody ^. inputsTxBodyL)

  -- check scripts
  {- neededHashes := {h | ( , h) ∈ scriptsNeeded utxo txb} -}
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sNeeded = Set.fromList (map snd (Alonzo.scriptsNeeded utxo tx)) -- Script credentials
  {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
  runTest $ validateFailedBabbageScripts tx utxo sNeeded -- CHANGED In BABBAGE txscripts depends on UTxO
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sReceived = Map.keysSet $ tx ^. witsTxL . scriptWitsL
      sRefs = Map.keysSet $ refScripts inputs utxo
  runTest $ babbageMissingScripts pp sNeeded sRefs sReceived

  {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
  runTest $ missingRequiredDatums hashScriptMap utxo tx txBody

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers utxo tx txBody

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
  runTestOnSignal $ requiredSignersAreWitnessed txBody witsKeyHashes

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
  runTest $ validateScriptsWellFormed pp tx
  -- Note that Datum validation is done during deserialization,
  -- as given by the decoders in the Plutus libraray

  {- languages tx utxo ⊆ dom(costmdls tx) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'. This check
  -- which appears in the spec, seems broken since costmdls is a projection of PPrams, not Tx

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx txBody pp utxo sNeeded

  trans @(EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

-- ================================

instance
  forall era.
  ( ExtendedUTxO era,
    EraTx era,
    BabbageEraTxBody era,
    TxOut era ~ BabbageTxOut era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Fix some Core types to the Babbage Era
    Tx era ~ AlonzoTx era,
    Script era ~ AlonzoScript era,
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (BabbageUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ AlonzoTx era,
    Eq (PredicateFailure (EraRule "UTXOS" era)),
    Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (BabbageUTXOW era)
  where
  type State (BabbageUTXOW era) = UTxOState era
  type Signal (BabbageUTXOW era) = AlonzoTx era
  type Environment (BabbageUTXOW era) = UtxoEnv era
  type BaseM (BabbageUTXOW era) = ShelleyBase
  type PredicateFailure (BabbageUTXOW era) = BabbageUtxowPredFailure era
  type Event (BabbageUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [babbageUtxowTransition]
  initialRules = []

instance
  ( Era era,
    STS (BabbageUTXO era),
    PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era,
    Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era,
    BaseM (BabbageUTXOW era) ~ ShelleyBase,
    PredicateFailure (BabbageUTXOW era) ~ BabbageUtxowPredFailure era,
    Event (BabbageUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabbageUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

instance Inject (BabbageUtxowPredFailure era) (BabbageUtxowPredFailure era) where
  inject = id
