{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Utxow (
  AlonzoUTXOW,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure (..),
  hasExactSetOfRedeemers,
  missingRequiredDatums,
  ppViewHashesMatch,
  requiredSignersAreWitnessed,
)
where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Alonzo.Era (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.PParams (getLanguageView)
import Cardano.Ledger.Alonzo.Plutus.TxInfo (ExtendedUTxO (..), languages)
import Cardano.Ledger.Alonzo.Rules.Utxo (
  AlonzoUTXO,
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  ScriptPurpose,
  hashScriptIntegrity,
  rdptr,
 )
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  ScriptIntegrityHash,
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  RdmrPtr,
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
  getInputDataHashesTxBody,
 )
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  StrictMaybe (..),
  quorum,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (
  UTxOState (..),
  witsFromTxWitnesses,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure (..),
  UtxoEnv (..),
  shelleyWitsVKeyNeeded,
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Tx (TxIn (..))
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (domain, eval, (⊆), (➖))
import Control.State.Transition.Extended
import Data.Foldable (sequenceA_)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Validation

-- =================================================

-- | The Predicate failure type in the Alonzo Era. It embeds the Predicate
--   failure type of the Shelley Era, as they share some failure modes.
data AlonzoUtxowPredFailure era
  = ShelleyInAlonzoUtxowPredFailure !(ShelleyUtxowPredFailure era)
  | -- | List of scripts for which no redeemers were supplied
    MissingRedeemers
      ![(ScriptPurpose era, ScriptHash (EraCrypto era))]
  | MissingRequiredDatums
      -- | Set of missing data hashes
      !(Set (DataHash (EraCrypto era)))
      -- | Set of received data hashes
      !(Set (DataHash (EraCrypto era)))
  | NotAllowedSupplementalDatums
      -- | Set of unallowed data hashes
      !(Set (DataHash (EraCrypto era)))
      -- | Set of acceptable supplemental data hashes
      !(Set (DataHash (EraCrypto era)))
  | PPViewHashesDontMatch
      -- | The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
      -- | Computed from the current Protocol Parameters
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  | -- | Set of witnesses which were needed and not supplied
    MissingRequiredSigners
      (Set (KeyHash 'Witness (EraCrypto era)))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (EraCrypto era)))
  | -- | List of redeemers not needed
    ExtraRedeemers
      ![RdmrPtr]
  deriving (Generic)

deriving instance
  ( Era era
  , Show (TxCert era)
  , Show (Script era)
  , Show (PredicateFailure (EraRule "UTXO" era)) -- The ShelleyUtxowPredFailure needs this to Show
  ) =>
  Show (AlonzoUtxowPredFailure era)

deriving instance
  ( Era era
  , Eq (TxCert era)
  , Eq (Script era)
  , Eq (PredicateFailure (EraRule "UTXO" era)) -- The ShelleyUtxowPredFailure needs this to Eq
  ) =>
  Eq (AlonzoUtxowPredFailure era)

instance
  ( Era era
  , NoThunks (TxCert era)
  , NoThunks (Script era)
  , NoThunks (PredicateFailure (EraRule "UTXO" era))
  ) =>
  NoThunks (AlonzoUtxowPredFailure era)

instance
  ( Era era
  , EncCBOR (TxCert era)
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (TxAuxData era)
  , EncCBOR (Script era)
  ) =>
  EncCBOR (AlonzoUtxowPredFailure era)
  where
  encCBOR =
    encode . \case
      ShelleyInAlonzoUtxowPredFailure x -> Sum ShelleyInAlonzoUtxowPredFailure 0 !> To x
      MissingRedeemers x -> Sum MissingRedeemers 1 !> To x
      MissingRequiredDatums x y -> Sum MissingRequiredDatums 2 !> To x !> To y
      NotAllowedSupplementalDatums x y -> Sum NotAllowedSupplementalDatums 3 !> To x !> To y
      PPViewHashesDontMatch x y -> Sum PPViewHashesDontMatch 4 !> To x !> To y
      MissingRequiredSigners x -> Sum MissingRequiredSigners 5 !> To x
      UnspendableUTxONoDatumHash x -> Sum UnspendableUTxONoDatumHash 6 !> To x
      ExtraRedeemers x -> Sum ExtraRedeemers 7 !> To x

newtype AlonzoUtxowEvent era
  = WrappedShelleyEraEvent (ShelleyUtxowEvent era)

instance
  ( Era era
  , DecCBOR (TxCert era)
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (Script era)
  , Typeable (TxAuxData era)
  ) =>
  DecCBOR (AlonzoUtxowPredFailure era)
  where
  decCBOR = decode (Summands "UtxowPredicateFail" decodePredFail)

decodePredFail ::
  ( Era era
  , DecCBOR (TxCert era)
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (Script era)
  , Typeable (TxAuxData era)
  ) =>
  Word ->
  Decode 'Open (AlonzoUtxowPredFailure era)
decodePredFail 0 = SumD ShelleyInAlonzoUtxowPredFailure <! From
decodePredFail 1 = SumD MissingRedeemers <! From
decodePredFail 2 = SumD MissingRequiredDatums <! From <! From
decodePredFail 3 = SumD NotAllowedSupplementalDatums <! From <! From
decodePredFail 4 = SumD PPViewHashesDontMatch <! From <! From
decodePredFail 5 = SumD MissingRequiredSigners <! From
decodePredFail 6 = SumD UnspendableUTxONoDatumHash <! From
decodePredFail 7 = SumD ExtraRedeemers <! From
decodePredFail n = Invalid n

-- =================

{- { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} ⊆ dom(txdats txw)   -}
{- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h, ) ∈ txouts tx ∪ utxo (refInputs tx) } -}
missingRequiredDatums ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  ) =>
  UTxO era ->
  Tx era ->
  Test (AlonzoUtxowPredFailure era)
missingRequiredDatums utxo tx = do
  let txBody = tx ^. bodyTxL
      scriptsProvided = getScriptsProvided utxo tx
      (inputHashes, txInsNoDataHash) = getInputDataHashesTxBody utxo txBody scriptsProvided
      txHashes = domain (unTxDats $ tx ^. witsTxL . datsTxWitsL)
      unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      allowedSupplementalDataHashes = getSupplementalDataHashes utxo txBody
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

-- ==================
{-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
hasExactSetOfRedeemers ::
  forall era.
  ( AlonzoEraTx era
  , Script era ~ AlonzoScript era
  ) =>
  Tx era ->
  ScriptsProvided era ->
  AlonzoScriptsNeeded era ->
  Test (AlonzoUtxowPredFailure era)
hasExactSetOfRedeemers tx (ScriptsProvided scriptsProvided) (AlonzoScriptsNeeded scriptsNeeded) = do
  let txBody = tx ^. bodyTxL
      redeemersNeeded =
        [ (rp, (sp, sh))
        | (sp, sh) <- scriptsNeeded
        , SJust rp <- [rdptr @era txBody sp]
        , Just script <- [Map.lookup sh scriptsProvided]
        , (not . isNativeScript) script
        ]
      (extraRdmrs, missingRdmrs) =
        extSymmetricDifference
          (Map.keys $ unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL)
          id
          redeemersNeeded
          fst
  sequenceA_
    [ failureUnless (null extraRdmrs) (ExtraRedeemers extraRdmrs)
    , failureUnless (null missingRdmrs) (MissingRedeemers (map snd missingRdmrs))
    ]

-- ======================
requiredSignersAreWitnessed ::
  AlonzoEraTxBody era =>
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  Test (AlonzoUtxowPredFailure era)
requiredSignersAreWitnessed txBody witsKeyHashes = do
  let reqSignerHashes' = txBody ^. reqSignerHashesTxBodyL
  failureUnless
    (eval (reqSignerHashes' ⊆ witsKeyHashes))
    (MissingRequiredSigners (eval $ reqSignerHashes' ➖ witsKeyHashes))

-- =======================
{-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
ppViewHashesMatch ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , Script era ~ AlonzoScript era
  ) =>
  Tx era ->
  PParams era ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era)) ->
  Test (AlonzoUtxowPredFailure era)
ppViewHashesMatch tx pp utxo sNeeded = do
  let langs = languages @era tx utxo sNeeded
      langViews = Set.map (getLanguageView pp) langs
      txWits = tx ^. witsTxL
      computedPPhash = hashScriptIntegrity langViews (txWits ^. rdmrsTxWitsL) (txWits ^. datsTxWitsL)
      bodyPPhash = tx ^. bodyTxL . scriptIntegrityHashTxBodyL
  failureUnless
    (bodyPPhash == computedPPhash)
    (PPViewHashesDontMatch bodyPPhash computedPPhash)

-- ==============================================================
-- Here we define the transtion function, using reusable tests.
-- The tests are very generic and resusabe, but the transition
-- function is very specific to the Alonzo Era.

-- | A very specialized transitionRule function for the Alonzo Era.
alonzoStyleWitness ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , ProtVerAtMost era 8
  ) =>
  TransitionRule (AlonzoUTXOW era)
alonzoStyleWitness = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = utxosUtxo u
      txBody = tx ^. bodyTxL
      witsKeyHashes = witsFromTxWitnesses @era tx
      scriptsProvided = getScriptsProvided utxo tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
  runTestOnSignal $ Shelley.validateFailedNativeScripts scriptsProvided tx

  {-  { h | (_,h) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  let scriptsNeeded = getScriptsNeeded utxo txBody
      scriptsHashesNeeded = getScriptsHashesNeeded scriptsNeeded
      shelleyScriptsNeeded = ShelleyScriptsNeeded scriptsHashesNeeded
  runTest $ Shelley.validateMissingScripts pp shelleyScriptsNeeded scriptsProvided

  {- inputHashes := { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} -}
  {-  inputHashes ⊆ dom(txdats txw)  -}
  runTest $ missingRequiredDatums utxo tx

  {- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h) ∈ txouts tx -}
  -- This is incorporated into missingRequiredDatums, see the
  -- (failure . UnspendableUTxONoDatumHash) path.

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

  -- check VKey witnesses
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txBodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  let needed = shelleyWitsVKeyNeeded utxo (tx ^. bodyTxL) genDelegs
  runTest $ validateNeededWitnesses @era witsKeyHashes needed

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded must include the reqSignerHashes in the union   -}
  {- reqSignerHashes txBody ⊆ witsKeyHashes -}
  runTestOnSignal $ requiredSignersAreWitnessed txBody witsKeyHashes

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ TxCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    Shelley.validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $
    Shelley.validateMetadata pp tx

  {- languages txw ⊆ dom(costmdls tx)  -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'. This check
  -- which appears in the spec, seems broken since costmdls is a projection of PPrams, not Tx

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx pp utxo scriptsHashesNeeded

  trans @(EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

-- ================================

extSymmetricDifference :: Ord k => [a] -> (a -> k) -> [b] -> (b -> k) -> ([a], [b])
extSymmetricDifference as fa bs fb = (extraA, extraB)
  where
    intersection = Set.fromList (map fa as) `Set.intersection` Set.fromList (map fb bs)
    extraA = filter (\x -> not $ fa x `Set.member` intersection) as
    extraB = filter (\x -> not $ fb x `Set.member` intersection) bs

-- ====================================
-- Make the STS instance

instance
  forall era.
  ( AlonzoEraTx era
  , EraTxAuxData era
  , ExtendedUTxO era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , Script era ~ AlonzoScript era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , ProtVerAtMost era 8
  ) =>
  STS (AlonzoUTXOW era)
  where
  type State (AlonzoUTXOW era) = UTxOState era
  type Signal (AlonzoUTXOW era) = Tx era
  type Environment (AlonzoUTXOW era) = UtxoEnv era
  type BaseM (AlonzoUTXOW era) = ShelleyBase
  type PredicateFailure (AlonzoUTXOW era) = AlonzoUtxowPredFailure era
  type Event (AlonzoUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [alonzoStyleWitness]
  initialRules = []

instance
  ( Era era
  , STS (AlonzoUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ AlonzoUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (AlonzoUTXOW era) ~ ShelleyBase
  , PredicateFailure (AlonzoUTXOW era) ~ AlonzoUtxowPredFailure era
  , Event (AlonzoUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (AlonzoUTXO era) (AlonzoUTXOW era)
  where
  wrapFailed = ShelleyInAlonzoUtxowPredFailure . UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

-- ==========================================================
-- inject instances

instance Inject (AlonzoUtxowPredFailure era) (AlonzoUtxowPredFailure era) where
  inject = id

instance Inject (ShelleyUtxowPredFailure era) (AlonzoUtxowPredFailure era) where
  inject = ShelleyInAlonzoUtxowPredFailure
