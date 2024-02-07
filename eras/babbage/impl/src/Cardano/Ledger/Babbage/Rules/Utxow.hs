{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Utxow (
  BabbageUTXOW,
  BabbageUtxowPredFailure (..),
  babbageMissingScripts,
  validateFailedBabbageScripts,
  validateScriptsWellFormed,
  babbageUtxowTransition,
)
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure (..),
  hasExactSetOfRedeemers,
  missingRequiredDatums,
  ppViewHashesMatch,
 )
import Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoUtxoEvent)
import Cardano.Ledger.Alonzo.Scripts (validScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra, BabbageUTXOW)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUTXO, BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes (ShelleyBase, quorum, strictMaybeToMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (From, Invalid, SumD, Summands),
  Encode (Sum, To),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (certDState, dsGenDelegs)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Tx (witsFromTxWitnesses)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Foldable (sequenceA_, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (failureUnless)

data BabbageUtxowPredFailure era
  = AlonzoInBabbageUtxowPredFailure !(AlonzoUtxowPredFailure era) -- TODO: embed and translate
  | -- | Embed UTXO rule failures
    UtxoFailure !(PredicateFailure (EraRule "UTXO" era))
  | -- | the set of malformed script witnesses
    MalformedScriptWitnesses
      !(Set (ScriptHash (EraCrypto era)))
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      !(Set (ScriptHash (EraCrypto era)))
  deriving (Generic)

type instance EraRuleFailure "UTXOW" (BabbageEra c) = BabbageUtxowPredFailure (BabbageEra c)

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure (BabbageEra c) where
  injectFailure = id

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure (BabbageEra c) where
  injectFailure = AlonzoInBabbageUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (BabbageEra c) where
  injectFailure = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure (BabbageEra c) where
  injectFailure = UtxoFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure (BabbageEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (BabbageEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyPpupPredFailure (BabbageEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (BabbageEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure (BabbageEra c) where
  injectFailure = UtxoFailure . injectFailure

deriving instance
  ( AlonzoEraScript era
  , Show (ShelleyUtxowPredFailure era)
  , Show (PredicateFailure (EraRule "UTXO" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  , Show (TxOut era)
  , Show (TxCert era)
  , Show (Value era)
  ) =>
  Show (BabbageUtxowPredFailure era)

deriving instance
  ( AlonzoEraScript era
  , Eq (ShelleyUtxowPredFailure era)
  , Eq (PredicateFailure (EraRule "UTXO" era))
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (TxCert era)
  ) =>
  Eq (BabbageUtxowPredFailure era)

instance Inject (AlonzoUtxowPredFailure era) (BabbageUtxowPredFailure era) where
  inject = AlonzoInBabbageUtxowPredFailure

instance Inject (ShelleyUtxowPredFailure era) (BabbageUtxowPredFailure era) where
  inject = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure

instance
  ( AlonzoEraScript era
  , EncCBOR (TxOut era)
  , EncCBOR (TxCert era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (TxAuxData era)
  ) =>
  EncCBOR (BabbageUtxowPredFailure era)
  where
  encCBOR =
    encode . \case
      AlonzoInBabbageUtxowPredFailure x -> Sum AlonzoInBabbageUtxowPredFailure 1 !> To x
      UtxoFailure x -> Sum UtxoFailure 2 !> To x
      MalformedScriptWitnesses x -> Sum MalformedScriptWitnesses 3 !> To x
      MalformedReferenceScripts x -> Sum MalformedReferenceScripts 4 !> To x

instance
  ( AlonzoEraScript era
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (TxAuxData era)
  ) =>
  DecCBOR (BabbageUtxowPredFailure era)
  where
  decCBOR = decode $ Summands "BabbageUtxowPred" $ \case
    1 -> SumD AlonzoInBabbageUtxowPredFailure <! From
    2 -> SumD UtxoFailure <! From
    3 -> SumD MalformedScriptWitnesses <! From
    4 -> SumD MalformedReferenceScripts <! From
    n -> Invalid n

deriving via
  InspectHeapNamed "BabbageUtxowPred" (BabbageUtxowPredFailure era)
  instance
    NoThunks (BabbageUtxowPredFailure era)

instance
  ( AlonzoEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (BabbageUtxowPredFailure era)

-- ==================================================
-- Reuseable tests first used in the Babbage Era

-- Int the Babbage Era with reference scripts, the needed
-- scripts only has to be a subset of the txscripts.
{-  { s | (_,s) ∈ scriptsNeeded utxo tx} - dom(refScripts tx utxo) = dom(txscripts txw)  -}
{-  sNeeded := scriptsNeeded utxo tx                                                     -}
{-  sReceived := Map.keysSet (tx ^. witsTxL . scriptTxWitsL)                             -}
babbageMissingScripts ::
  forall era.
  PParams era ->
  Set (ScriptHash (EraCrypto era)) ->
  Set (ScriptHash (EraCrypto era)) ->
  Set (ScriptHash (EraCrypto era)) ->
  Test (ShelleyUtxowPredFailure era)
babbageMissingScripts _ sNeeded sRefs sReceived =
  sequenceA_
    [ failureUnless (Set.null extra) $ Shelley.ExtraneousScriptWitnessesUTXOW extra
    , failureUnless (Set.null missing) $ Shelley.MissingScriptWitnessesUTXOW missing
    ]
  where
    neededNonRefs = sNeeded `Set.difference` sRefs
    missing = neededNonRefs `Set.difference` sReceived
    extra = sReceived `Set.difference` neededNonRefs

{-  ∀ s ∈ (txscripts txw utxo ∩ Scriptnative), validateScript s tx   -}
validateFailedBabbageScripts ::
  EraTx era =>
  Tx era ->
  ScriptsProvided era ->
  Set (ScriptHash (EraCrypto era)) ->
  Test (ShelleyUtxowPredFailure era)
validateFailedBabbageScripts tx (ScriptsProvided scriptsProvided) neededHashes =
  let failedScripts =
        Map.filterWithKey
          ( \scriptHash script ->
              case getNativeScript script of
                Nothing -> False
                Just nativeScript ->
                  let scriptIsNeeded = scriptHash `Set.member` neededHashes
                      scriptDoesNotValidate = not (validateNativeScript tx nativeScript)
                   in scriptIsNeeded && scriptDoesNotValidate
          )
          scriptsProvided
   in failureUnless
        (Map.null failedScripts)
        (Shelley.ScriptWitnessNotValidatingUTXOW $ Map.keysSet failedScripts)

{- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ ⋃ ( , ,d,s)∈txouts tx{s, d},
                       x ∈ Script ∪ Datum ⇒ isWellFormed x
-}
validateScriptsWellFormed ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  ) =>
  PParams era ->
  Tx era ->
  Test (BabbageUtxowPredFailure era)
validateScriptsWellFormed pp tx =
  sequenceA_
    [ failureUnless (Map.null invalidScriptWits) $ MalformedScriptWitnesses (Map.keysSet invalidScriptWits)
    , failureUnless (null invalidRefScripts) $ MalformedReferenceScripts invalidRefScriptHashes
    ]
  where
    scriptWits = tx ^. witsTxL . scriptTxWitsL
    invalidScriptWits = Map.filter (not . validScript (pp ^. ppProtocolVersionL)) scriptWits

    txBody = tx ^. bodyTxL
    normalOuts = toList $ txBody ^. outputsTxBodyL
    returnOut = txBody ^. collateralReturnTxBodyL
    outs = case returnOut of
      SNothing -> normalOuts
      SJust rOut -> rOut : normalOuts
    rScripts = mapMaybe (strictMaybeToMaybe . view referenceScriptTxOutL) outs
    invalidRefScripts = filter (not . validScript (pp ^. ppProtocolVersionL)) rScripts
    invalidRefScriptHashes = Set.fromList $ map (hashScript @era) invalidRefScripts

-- ==============================================================
-- Here we define the transtion function, using reusable tests.
-- The tests are very generic and reusable, but the transition
-- function is very specific to the Babbage Era.

babbageUtxowMirTransition ::
  forall era.
  ( AlonzoEraTx era
  , ShelleyEraTxBody era
  , STS (BabbageUTXOW era)
  ) =>
  TransitionRule (BabbageUTXOW era)
babbageUtxowMirTransition = do
  TRC (UtxoEnv _ _ certState, st, tx) <- judgmentContext
  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ TxCert_mir} ≠ ∅  ⇒ |genSig| ≥ Quorum  -}
  let genDelegs = dsGenDelegs (certDState certState)
      witsKeyHashes = witsFromTxWitnesses tx
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    Shelley.validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx
  pure st

-- | UTXOW transition rule that is used in Babbage and Conway era.
babbageUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BabbageEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Inject (BabbageUtxowPredFailure era) (PredicateFailure (EraRule "UTXOW" era))
  , Inject (AlonzoUtxowPredFailure era) (PredicateFailure (EraRule "UTXOW" era))
  , Inject (ShelleyUtxowPredFailure era) (PredicateFailure (EraRule "UTXOW" era))
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (EraRule "UTXOW" era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , State (EraRule "UTXO" era) ~ UTxOState era
  ) =>
  TransitionRule (EraRule "UTXOW" era)
babbageUtxowTransition = do
  TRC (utxoEnv@(UtxoEnv _ pp certState), u, tx) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = utxosUtxo u
      txBody = tx ^. bodyTxL
      witsKeyHashes = witsFromTxWitnesses tx
      inputs = (txBody ^. referenceInputsTxBodyL) `Set.union` (txBody ^. inputsTxBodyL)

  -- check scripts
  {- neededHashes := {h | ( , h) ∈ scriptsNeeded utxo txb} -}
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let scriptsNeeded = getScriptsNeeded utxo txBody
      scriptsProvided = getScriptsProvided utxo tx
      scriptHashesNeeded = getScriptsHashesNeeded scriptsNeeded
  {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
  -- CHANGED In BABBAGE txscripts depends on UTxO
  runTest $ validateFailedBabbageScripts tx scriptsProvided scriptHashesNeeded

  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sReceived = Map.keysSet $ tx ^. witsTxL . scriptTxWitsL
      sRefs = Map.keysSet $ getReferenceScripts utxo inputs
  runTest $ babbageMissingScripts pp scriptHashesNeeded sRefs sReceived

  {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
  runTest $ missingRequiredDatums utxo tx

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

  -- check VKey witnesses
  -- let txbodyHash = hashAnnotated @(Crypto era) txbody
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsKeyHashes certState utxo txBody

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  {- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ (⋃ ( , ,d,s) ∈ txouts tx {s, d}),
                         x ∈ Script ∪ Datum ⇒ isWellFormed x
  -}
  runTest $ validateScriptsWellFormed pp tx
  -- Note that Datum validation is done during deserialization,
  -- as given by the decoders in the Plutus libraray

  {- languages tx utxo ⊆ dom(costmdls pp) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'.

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx pp scriptsProvided scriptHashesNeeded

  trans @(EraRule "UTXO" era) $ TRC (utxoEnv, u, tx)

-- ================================

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ShelleyEraTxBody era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BabbageEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraRule "UTXOW" era ~ BabbageUTXOW era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (BabbageUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (BabbageUTXOW era)
  where
  type State (BabbageUTXOW era) = UTxOState era
  type Signal (BabbageUTXOW era) = Tx era
  type Environment (BabbageUTXOW era) = UtxoEnv era
  type BaseM (BabbageUTXOW era) = ShelleyBase
  type PredicateFailure (BabbageUTXOW era) = BabbageUtxowPredFailure era
  type Event (BabbageUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [babbageUtxowMirTransition >> babbageUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (BabbageUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (BabbageUTXOW era) ~ ShelleyBase
  , PredicateFailure (BabbageUTXOW era) ~ BabbageUtxowPredFailure era
  , Event (BabbageUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabbageUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

instance Inject (BabbageUtxowPredFailure era) (BabbageUtxowPredFailure era) where
  inject = id
