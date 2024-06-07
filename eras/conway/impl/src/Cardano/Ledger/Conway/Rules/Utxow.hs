{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Cardano.Ledger.Conway.Rules.Utxow (
  alonzoToConwayUtxowPredFailure,
  babbageToConwayUtxowPredFailure,
  ConwayUTXOW,
  ConwayUtxowPredFailure (..),
  conwayWitsVKeyNeeded,
  shelleyToConwayUtxowPredFailure,
)
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure,
  hasExactSetOfRedeemers,
  missingRequiredDatums,
  ppViewHashesMatch,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  -- babbageUtxowTransition,

  babbageMissingScripts,
  validateFailedBabbageScripts,
  validateScriptsWellFormed,
 )
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  BabbageUtxowPredFailure (..),
 )
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXO, ConwayUTXOW)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.UTxO (getConwayWitsVKeyNeeded)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  VKey,
  WitVKey (WitVKey),
  bwKey,
  verifyBootstrapWit,
  -- verifyBootstrapWitRequiredTxs,
 )
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.SafeHash (extractHash, hashAnnotated)
import Cardano.Ledger.Shelley.API (utxosUtxo)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  UtxoEnv (UtxoEnv),
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyUtxowPredFailure (..),
  UtxoEnv,
  validateMetadata,
 )
import Cardano.Ledger.Shelley.Tx (witsFromTxWitnesses)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO, verifyWitVKey)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  trans,
 )
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation

conwayWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
conwayWitsVKeyNeeded = getConwayWitsVKeyNeeded
{-# DEPRECATED conwayWitsVKeyNeeded "In favor of `getConwayWitsVKeyNeeded` or `getWitsVKeyNeeded`" #-}

-- ================================

-- | Predicate failure type for the Conway Era
data ConwayUtxowPredFailure era
  = UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | InvalidWitnessesUTXOW
      ![VKey 'Witness (EraCrypto era)]
  | -- | witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      !(Set (KeyHash 'Witness (EraCrypto era)))
  | -- | missing scripts
    MissingScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | -- | failed scripts
    ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | -- | hash of the full metadata
    MissingTxBodyMetadataHash
      !(AuxiliaryDataHash (EraCrypto era))
  | -- | hash of the metadata included in the transaction body
    MissingTxMetadata
      !(AuxiliaryDataHash (EraCrypto era))
  | ConflictingMetadataHash
      -- | hash of the metadata included in the transaction body
      !(AuxiliaryDataHash (EraCrypto era))
      -- | expected hash of the full metadata
      !(AuxiliaryDataHash (EraCrypto era))
  | -- | Contains out of range values (string`s too long)
    InvalidMetadata
  | -- | extraneous scripts
    ExtraneousScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | MissingRedeemers
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
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (EraCrypto era)))
  | -- | List of redeemers not needed
    ExtraRedeemers ![PlutusPurpose AsIx era]
  | -- | Embed UTXO rule failures
    MalformedScriptWitnesses
      !(Set (ScriptHash (EraCrypto era)))
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      !(Set (ScriptHash (EraCrypto era)))
  deriving (Generic)

type instance EraRuleFailure "UTXOW" (ConwayEra c) = ConwayUtxowPredFailure (ConwayEra c)

type instance EraRuleEvent "UTXOW" (ConwayEra c) = AlonzoUtxowEvent (ConwayEra c)

instance InjectRuleFailure "UTXOW" ConwayUtxowPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = babbageToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = alonzoToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

deriving instance
  ( ConwayEraScript era
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Show (ConwayUtxowPredFailure era)

deriving instance
  ( ConwayEraScript era
  , Eq (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Eq (ConwayUtxowPredFailure era)

deriving via
  InspectHeapNamed "ConwayUtxowPred" (ConwayUtxowPredFailure era)
  instance
    NoThunks (ConwayUtxowPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (ConwayUtxowPredFailure era)

--------------------------------------------------------------------------------
-- ConwayUTXOW STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ConwayEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentRequiredTxs)
  , EraRule "UTXOW" era ~ ConwayUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ConwayUTXOW era)
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ Shelley.UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (ConwayUTXOW era)
  where
  type State (ConwayUTXOW era) = Shelley.UTxOState era
  type Signal (ConwayUTXOW era) = Tx era
  type Environment (ConwayUTXOW era) = Shelley.UtxoEnv era
  type BaseM (ConwayUTXOW era) = ShelleyBase
  type PredicateFailure (ConwayUTXOW era) = ConwayUtxowPredFailure era
  type Event (ConwayUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [conwayUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (ConwayUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ ConwayUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (ConwayUTXOW era) ~ ShelleyBase
  , PredicateFailure (ConwayUTXOW era) ~ ConwayUtxowPredFailure era
  , Event (ConwayUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (ConwayUTXO era) (ConwayUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( ConwayEraScript era
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  EncCBOR (ConwayUtxowPredFailure era)
  where
  encCBOR =
    encode . \case
      UtxoFailure x -> Sum UtxoFailure 0 !> To x
      InvalidWitnessesUTXOW xs -> Sum InvalidWitnessesUTXOW 1 !> To xs
      MissingVKeyWitnessesUTXOW xs -> Sum MissingVKeyWitnessesUTXOW 2 !> To xs
      MissingScriptWitnessesUTXOW xs -> Sum MissingScriptWitnessesUTXOW 3 !> To xs
      ScriptWitnessNotValidatingUTXOW xs -> Sum ScriptWitnessNotValidatingUTXOW 4 !> To xs
      MissingTxBodyMetadataHash xs -> Sum MissingTxBodyMetadataHash 5 !> To xs
      MissingTxMetadata xs -> Sum MissingTxMetadata 6 !> To xs
      ConflictingMetadataHash a b -> Sum ConflictingMetadataHash 7 !> To a !> To b
      InvalidMetadata -> Sum InvalidMetadata 8
      ExtraneousScriptWitnessesUTXOW xs -> Sum ExtraneousScriptWitnessesUTXOW 9 !> To xs
      MissingRedeemers x -> Sum MissingRedeemers 10 !> To x
      MissingRequiredDatums x y -> Sum MissingRequiredDatums 11 !> To x !> To y
      NotAllowedSupplementalDatums x y -> Sum NotAllowedSupplementalDatums 12 !> To x !> To y
      PPViewHashesDontMatch x y -> Sum PPViewHashesDontMatch 13 !> To x !> To y
      UnspendableUTxONoDatumHash x -> Sum UnspendableUTxONoDatumHash 14 !> To x
      ExtraRedeemers x -> Sum ExtraRedeemers 15 !> To x
      MalformedScriptWitnesses x -> Sum MalformedScriptWitnesses 16 !> To x
      MalformedReferenceScripts x -> Sum MalformedReferenceScripts 17 !> To x

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  DecCBOR (ConwayUtxowPredFailure era)
  where
  decCBOR = decode . Summands "ConwayUtxowPred" $ \case
    0 -> SumD UtxoFailure <! From
    1 -> SumD InvalidWitnessesUTXOW <! From
    2 -> SumD MissingVKeyWitnessesUTXOW <! From
    3 -> SumD MissingScriptWitnessesUTXOW <! From
    4 -> SumD ScriptWitnessNotValidatingUTXOW <! From
    5 -> SumD MissingTxBodyMetadataHash <! From
    6 -> SumD MissingTxMetadata <! From
    7 -> SumD ConflictingMetadataHash <! From <! From
    8 -> SumD InvalidMetadata
    9 -> SumD ExtraneousScriptWitnessesUTXOW <! From
    10 -> SumD MissingRedeemers <! From
    11 -> SumD MissingRequiredDatums <! From <! From
    12 -> SumD NotAllowedSupplementalDatums <! From <! From
    13 -> SumD PPViewHashesDontMatch <! From <! From
    14 -> SumD UnspendableUTxONoDatumHash <! From
    15 -> SumD ExtraRedeemers <! From
    16 -> SumD MalformedScriptWitnesses <! From
    17 -> SumD MalformedReferenceScripts <! From
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

babbageToConwayUtxowPredFailure ::
  forall era.
  BabbageUtxowPredFailure era ->
  ConwayUtxowPredFailure era
babbageToConwayUtxowPredFailure = \case
  Babbage.AlonzoInBabbageUtxowPredFailure x -> alonzoToConwayUtxowPredFailure x
  Babbage.UtxoFailure x -> UtxoFailure x
  Babbage.MalformedScriptWitnesses xs -> MalformedScriptWitnesses xs
  Babbage.MalformedReferenceScripts xs -> MalformedReferenceScripts xs

alonzoToConwayUtxowPredFailure ::
  forall era.
  AlonzoUtxowPredFailure era ->
  ConwayUtxowPredFailure era
alonzoToConwayUtxowPredFailure = \case
  Alonzo.ShelleyInAlonzoUtxowPredFailure f -> shelleyToConwayUtxowPredFailure f
  Alonzo.MissingRedeemers rs -> MissingRedeemers rs
  Alonzo.MissingRequiredDatums mds rds -> MissingRequiredDatums mds rds
  Alonzo.NotAllowedSupplementalDatums uds ads -> NotAllowedSupplementalDatums uds ads
  Alonzo.PPViewHashesDontMatch a b -> PPViewHashesDontMatch a b
  Alonzo.MissingRequiredSigners _xs ->
    error "Impossible case. It will be removed once we are in Conway. See #3972"
  Alonzo.UnspendableUTxONoDatumHash ins -> UnspendableUTxONoDatumHash ins
  Alonzo.ExtraRedeemers xs -> ExtraRedeemers xs

shelleyToConwayUtxowPredFailure :: ShelleyUtxowPredFailure era -> ConwayUtxowPredFailure era
shelleyToConwayUtxowPredFailure = \case
  Shelley.InvalidWitnessesUTXOW xs -> InvalidWitnessesUTXOW xs
  Shelley.MissingVKeyWitnessesUTXOW xs -> MissingVKeyWitnessesUTXOW xs
  Shelley.MissingScriptWitnessesUTXOW xs -> MissingScriptWitnessesUTXOW xs
  Shelley.ScriptWitnessNotValidatingUTXOW xs -> ScriptWitnessNotValidatingUTXOW xs
  Shelley.UtxoFailure x -> UtxoFailure x
  Shelley.MIRInsufficientGenesisSigsUTXOW _xs ->
    error "Impossible: MIR has been removed in Conway"
  Shelley.MissingTxBodyMetadataHash x -> MissingTxBodyMetadataHash x
  Shelley.MissingTxMetadata x -> MissingTxMetadata x
  Shelley.ConflictingMetadataHash a b -> ConflictingMetadataHash a b
  Shelley.InvalidMetadata -> InvalidMetadata
  Shelley.ExtraneousScriptWitnessesUTXOW xs -> ExtraneousScriptWitnessesUTXOW xs

------------

-- | UTXOW transition rule that is used in Babbage and Conway era.
conwayUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BabbageEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , -- , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentRequiredTxs)
    Environment (EraRule "UTXOW" era) ~ Shelley.UtxoEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , State (EraRule "UTXOW" era) ~ Shelley.UTxOState era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (EraRule "UTXOW" era)
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , State (EraRule "UTXO" era) ~ Shelley.UTxOState era
  ) =>
  TransitionRule (EraRule "UTXOW" era)
conwayUtxowTransition = do
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

  -- TODO WG: This probably isn't exactly right, but it's close enough for now
  -- ∀[ (vk , σ) ∈ vkSigs ] isSigned vk (txidBytes (tx .Tx.body .TxBody.txid) + sumReqs (tx .requiredTxs)) σ
  -- check VKey witnesses
  -- let txbodyHash = hashAnnotated @(Crypto era) txbody
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ validateVerifiedWits tx

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

-- | Determine if the UTxO witnesses in a given transaction are correct.
validateVerifiedWits ::
  forall era.
  ( EraTx era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  -- , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentRequiredTxs)
  ) =>
  Tx era ->
  Test (ShelleyUtxowPredFailure era)
validateVerifiedWits tx =
  case failed of
    [] -> pure ()
    nonEmpty -> failure $ Shelley.InvalidWitnessesUTXOW nonEmpty
  where
    txBody = tx ^. bodyTxL
    txRequiredTxs = tx ^. requiredTxsTxL
    failed =
      failed' witVKeyVerifier
        <> failedBootstrap bootstrapWitVerifier
    (witVKeyVerifier, bootstrapWitVerifier) = case txRequiredTxs == mempty of
      True ->
        let txBodyHash = extractHash (hashAnnotated txBody)
         in (verifyWitVKey txBodyHash, verifyBootstrapWit txBodyHash)
      False ->
        let _a = extractHash (hashAnnotated txBody)
            _b = extractHash (hashAnnotated txRequiredTxs)
         in undefined
    -- let compositeHash = extractHash (hashAnnotated (txBody, txs))
    --  in (verifyWitVKeyRequiredTxs compositeHash, verifyBootstrapWitRequiredTxs compositeHash)
    wvkKey (WitVKey k _) = k

    failed' witnessVerification =
      wvkKey
        <$> filter
          (not . witnessVerification)
          (Set.toList $ tx ^. witsTxL . addrTxWitsL)
    failedBootstrap witnessVerification =
      bwKey
        <$> filter
          (not . witnessVerification)
          (Set.toList $ tx ^. witsTxL . bootAddrTxWitsL)