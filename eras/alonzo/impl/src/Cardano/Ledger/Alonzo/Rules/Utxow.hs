{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era (AlonzoEra, AlonzoUTXOW)
import Cardano.Ledger.Alonzo.PParams (getLanguageView)
import Cardano.Ledger.Alonzo.Rules.Utxo (
  AlonzoUTXO,
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxWits (
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
import Cardano.Ledger.CertState (certDState, dsGenDelegs)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure (..),
  UtxoEnv (..),
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Tx (witsFromTxWitnesses)
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (domain, eval, (⊆), (➖))
import Control.State.Transition.Extended
import Data.Foldable (sequenceA_)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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
      ![(PlutusPurpose AsItem era, ScriptHash (EraCrypto era))]
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
    MissingRequiredSigners -- TODO: remove once in Conway. It is now redundant. See #3972
      (Set (KeyHash 'Witness (EraCrypto era)))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (EraCrypto era)))
  | -- | List of redeemers not needed
    ExtraRedeemers
      ![PlutusPurpose AsIx era]
  deriving (Generic)

type instance EraRuleFailure "UTXOW" (AlonzoEra c) = AlonzoUtxowPredFailure (AlonzoEra c)

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure (AlonzoEra c)

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoUtxowPredFailure . UtxoFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoUtxowPredFailure . UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyPpupPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoUtxowPredFailure . UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoUtxowPredFailure . UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure (AlonzoEra c) where
  injectFailure = ShelleyInAlonzoUtxowPredFailure . UtxoFailure . injectFailure

deriving instance
  ( AlonzoEraScript era
  , Show (TxCert era)
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Show (AlonzoUtxowPredFailure era)

deriving instance
  ( AlonzoEraScript era
  , Eq (TxCert era)
  , Eq (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Eq (AlonzoUtxowPredFailure era)

instance
  ( AlonzoEraScript era
  , NoThunks (TxCert era)
  , NoThunks (PredicateFailure (EraRule "UTXO" era))
  ) =>
  NoThunks (AlonzoUtxowPredFailure era)

instance
  ( AlonzoEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (AlonzoUtxowPredFailure era)

instance
  ( AlonzoEraScript era
  , EncCBOR (TxCert era)
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (TxAuxData era)
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
  deriving (Generic)

deriving instance Eq (Event (EraRule "UTXO" era)) => Eq (AlonzoUtxowEvent era)

instance NFData (Event (EraRule "UTXO" era)) => NFData (AlonzoUtxowEvent era)

instance
  ( AlonzoEraScript era
  , DecCBOR (TxCert era)
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (TxAuxData era)
  ) =>
  DecCBOR (AlonzoUtxowPredFailure era)
  where
  decCBOR =
    decode $ Summands "UtxowPredicateFail" $ \case
      0 -> SumD ShelleyInAlonzoUtxowPredFailure <! From
      1 -> SumD MissingRedeemers <! From
      2 -> SumD MissingRequiredDatums <! From <! From
      3 -> SumD NotAllowedSupplementalDatums <! From <! From
      4 -> SumD PPViewHashesDontMatch <! From <! From
      5 -> SumD MissingRequiredSigners <! From
      6 -> SumD UnspendableUTxONoDatumHash <! From
      7 -> SumD ExtraRedeemers <! From
      n -> Invalid n

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
  AlonzoEraTx era =>
  Tx era ->
  ScriptsProvided era ->
  AlonzoScriptsNeeded era ->
  Test (AlonzoUtxowPredFailure era)
hasExactSetOfRedeemers tx (ScriptsProvided scriptsProvided) (AlonzoScriptsNeeded scriptsNeeded) = do
  let redeemersNeeded =
        [ (hoistPlutusPurpose toAsIx sp, (hoistPlutusPurpose toAsItem sp, sh))
        | (sp, sh) <- scriptsNeeded
        , Just script <- [Map.lookup sh scriptsProvided]
        , not (isNativeScript script)
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
{-# DEPRECATED requiredSignersAreWitnessed "As no longer used. `validateNeededWitnesses` now handles this check" #-}

-- =======================
{-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
ppViewHashesMatch ::
  forall era.
  AlonzoEraTx era =>
  Tx era ->
  PParams era ->
  ScriptsProvided era ->
  Set (ScriptHash (EraCrypto era)) ->
  Test (AlonzoUtxowPredFailure era)
ppViewHashesMatch tx pp (ScriptsProvided scriptsProvided) scriptsNeeded = do
  let scriptsUsed = Map.elems $ Map.restrictKeys scriptsProvided scriptsNeeded
      langs = Set.fromList $ plutusScriptLanguage <$> mapMaybe toPlutusScript scriptsUsed
      langViews = Set.map (getLanguageView pp) langs
      txWits = tx ^. witsTxL
      computedPPhash = hashScriptIntegrity langViews (txWits ^. rdmrsTxWitsL) (txWits ^. datsTxWitsL)
      bodyPPhash = tx ^. bodyTxL . scriptIntegrityHashTxBodyL
  failureUnless
    (bodyPPhash == computedPPhash)
    (PPViewHashesDontMatch bodyPPhash computedPPhash)

-- ==============================================================
-- Here we define the transtion function, using reusable tests.
-- The tests are very generic and reusabe, but the transition
-- function is very specific to the Alonzo Era.

-- | A very specialized transitionRule function for the Alonzo Era.
alonzoStyleWitness ::
  forall era.
  ( AlonzoEraTx era
  , ShelleyEraTxBody era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraRule "UTXOW" era ~ AlonzoUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  ) =>
  TransitionRule (EraRule "UTXOW" era)
alonzoStyleWitness = do
  TRC (utxoEnv@(UtxoEnv _ pp certState), u, tx) <- judgmentContext

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
  runTest $ Shelley.validateMissingScripts shelleyScriptsNeeded scriptsProvided

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
  runTest $ validateNeededWitnesses witsKeyHashes certState utxo txBody

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ TxCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  let genDelegs = dsGenDelegs (certDState certState)
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    Shelley.validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  {- languages txw ⊆ dom(costmdls pp)  -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a constructor of the predicate failure 'CollectError'.

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx pp scriptsProvided scriptsHashesNeeded

  trans @(EraRule "UTXO" era) $ TRC (utxoEnv, u, tx)

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
  , AlonzoEraUTxO era
  , ShelleyEraTxBody era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraRule "UTXOW" era ~ AlonzoUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  ) =>
  STS (AlonzoUTXOW era)
  where
  type State (AlonzoUTXOW era) = UTxOState era
  type Signal (AlonzoUTXOW era) = Tx era
  type Environment (AlonzoUTXOW era) = UtxoEnv era
  type BaseM (AlonzoUTXOW era) = ShelleyBase
  type PredicateFailure (AlonzoUTXOW era) = AlonzoUtxowPredFailure era
  type Event (AlonzoUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [alonzoStyleWitness @era]
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
