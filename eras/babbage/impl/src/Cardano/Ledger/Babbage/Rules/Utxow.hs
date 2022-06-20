{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babbage.Rules.Utxow where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoEvent)
import Cardano.Ledger.Alonzo.Rules.Utxow
  ( AlonzoEvent (WrappedShelleyEraEvent),
    UtxowPredicateFail (WrappedShelleyEraFailure),
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
import Cardano.Ledger.AuxiliaryData (ValidateAuxiliaryData)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Utxo
  ( BabbageUTXO,
    BabbageUtxoPred (..),
  )
import Cardano.Ledger.Babbage.Rules.Utxos (ConcreteBabbage)
import Cardano.Ledger.Babbage.Scripts (refScripts)
import Cardano.Ledger.Babbage.TxBody (TxOut, txOutScript)
import Cardano.Ledger.BaseTypes
  ( ProtVer,
    ShelleyBase,
    quorum,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.API (TxIn)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), witsFromTxWitnesses)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow
  ( UtxowEvent (UtxoEvent),
    UtxowPredicateFailure,
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
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (failureUnless)

data BabbageUtxowPred era
  = FromAlonzoUtxowFail !(UtxowPredicateFail era)
  | -- | Embed UTXO rule failures
    UtxoFailure !(PredicateFailure (Core.EraRule "UTXO" era))
  | -- | the set of malformed script witnesses
    MalformedScriptWitnesses
      !(Set (ScriptHash (Crypto era)))
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      !(Set (ScriptHash (Crypto era)))

deriving instance
  ( Era era,
    Show (UtxowPredicateFailure era),
    Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Show (PredicateFailure (Core.EraRule "UTXOS" era)),
    Show (Core.Script era),
    Show (Core.TxOut era),
    Show (Core.TxBody era),
    Show (Core.Value era)
  ) =>
  Show (BabbageUtxowPred era)

deriving instance
  ( Era era,
    Eq (UtxowPredicateFailure era),
    Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Eq (PredicateFailure (Core.EraRule "UTXOS" era)),
    Eq (Core.TxOut era),
    Eq (Core.Script era)
  ) =>
  Eq (BabbageUtxowPred era)

instance Inject (UtxowPredicateFail era) (BabbageUtxowPred era) where
  inject = FromAlonzoUtxowFail

instance Inject (UtxowPredicateFailure era) (BabbageUtxowPred era) where
  inject = FromAlonzoUtxowFail . WrappedShelleyEraFailure

instance
  ( Era era,
    Typeable era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era)),
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    ToCBOR (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  ToCBOR (BabbageUtxowPred era)
  where
  toCBOR = encode . work
    where
      work (FromAlonzoUtxowFail x) = Sum FromAlonzoUtxowFail 1 !> To x
      work (UtxoFailure x) = Sum UtxoFailure 2 !> To x
      work (MalformedScriptWitnesses x) = Sum MalformedScriptWitnesses 3 !> To x
      work (MalformedReferenceScripts x) = Sum MalformedReferenceScripts 4 !> To x

instance
  ( Era era,
    Typeable era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era)),
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  FromCBOR (BabbageUtxowPred era)
  where
  fromCBOR = decode (Summands "BabbageUtxowPred" work)
    where
      work 1 = SumD FromAlonzoUtxowFail <! From
      work 2 = SumD UtxoFailure <! From
      work 3 = SumD MalformedScriptWitnesses <! From
      work 4 = SumD MalformedReferenceScripts <! From
      work n = Invalid n

deriving via
  InspectHeapNamed "BabbageUtxowPred" (BabbageUtxowPred era)
  instance
    NoThunks (BabbageUtxowPred era)

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
    neededNonRefs = sNeeded `Set.difference` sRefs
    missing = neededNonRefs `Set.difference` sReceived
    extra = sReceived `Set.difference` neededNonRefs

{-  ∀ s ∈ (txscripts txw utxo ∩ Scriptnative), validateScript s tx   -}
validateFailedBabbageScripts ::
  forall era.
  ( ValidateScript era,
    ExtendedUTxO era,
    Core.Script era ~ Script era
  ) =>
  Core.Tx era ->
  UTxO era ->
  Set (ScriptHash (Crypto era)) ->
  Test (Shelley.UtxowPredicateFailure era)
validateFailedBabbageScripts tx utxo neededHashes =
  let failedScripts =
        Map.filterWithKey
          ( \hs script ->
              let zero = hs `Set.member` neededHashes
                  one = isNativeScript @era script
                  two = hashScript @era script /= hs -- TODO this is probably not needed. Only the script is transmitted on the wire, we compute the hash
                  three = not (validateScript @era script tx)
                  answer = zero && one && (two || three)
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
  ( ValidateScript era,
    HasField "collateralReturn" (Core.TxBody era) (StrictMaybe (TxOut era)),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    Core.Script era ~ Script era,
    Core.TxOut era ~ TxOut era
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Test (BabbageUtxowPred era)
validateScriptsWellFormed pp tx =
  sequenceA_
    [ failureUnless (Map.null invalidScriptWits) $ MalformedScriptWitnesses (Map.keysSet invalidScriptWits),
      failureUnless (null invalidRefScripts) $ MalformedReferenceScripts invalidRefScriptHashes
    ]
  where
    scriptWits = getField @"scriptWits" tx
    invalidScriptWits = Map.filter (not . validScript (getField @"_protocolVersion" pp)) scriptWits

    txb = getField @"body" tx
    normalOuts = toList $ getField @"outputs" txb
    returnOut = getField @"collateralReturn" txb
    outs = case returnOut of
      SNothing -> normalOuts
      SJust rOut -> rOut : normalOuts
    rScripts = mapMaybe txOutScript outs
    invalidRefScripts = filter (not . validScript (getField @"_protocolVersion" pp)) rScripts
    invalidRefScriptHashes = Set.fromList $ map (hashScript @era) invalidRefScripts

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
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "referenceInputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
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
      inputs = getField @"referenceInputs" txbody `Set.union` getField @"inputs" txbody

  -- check scripts
  {- neededHashes := {h | ( , h) ∈ scriptsNeeded utxo txb} -}
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sNeeded = Set.fromList (map snd (Alonzo.scriptsNeeded utxo tx)) -- Script credentials
  {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
  runTest $ validateFailedBabbageScripts tx utxo sNeeded -- CHANGED In BABBAGE txscripts depends on UTxO
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sReceived = Map.keysSet $ case getField @"wits" tx of
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
  runTest $ validateScriptsWellFormed pp tx
  -- Note that Datum validation is done during deserialization,
  -- as given by the decoders in the Plutus libraray

  {- languages tx utxo ⊆ dom(costmdls tx) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'. This check
  -- which appears in the spec, seems broken since costmdls is a projection of PPrams, not Tx

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx txbody pp utxo sNeeded

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
  type PredicateFailure (BabbageUTXOW era) = BabbageUtxowPred era
  type Event (BabbageUTXOW era) = AlonzoEvent era
  transitionRules = [babbageUtxowTransition]
  initialRules = []

instance
  ( Era era,
    STS (BabbageUTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ BabbageUtxoPred era,
    Event (Core.EraRule "UTXO" era) ~ Alonzo.UtxoEvent era,
    BaseM (BabbageUTXOW era) ~ ShelleyBase,
    PredicateFailure (BabbageUTXOW era) ~ BabbageUtxowPred era,
    Event (BabbageUTXOW era) ~ AlonzoEvent era
  ) =>
  Embed (BabbageUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

instance Inject (BabbageUtxowPred era) (BabbageUtxowPred era) where
  inject = id
