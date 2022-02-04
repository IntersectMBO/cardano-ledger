{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Utxow
  ( UTXOW,
    UtxowPredicateFailure (..),
    UtxowEvent (..),
    PredicateFailure,
    shelleyStyleWitness,
    ShelleyStyleWitnessNeeds,

    -- * Individual validation steps
    validateFailedScripts,
    validateMissingScripts,
    validateVerifiedWits,
    validateNeededWitnesses,
    validateMetadata,
    validateMIRInsufficientGenesisSigs,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash,
    ValidateAuxiliaryData (..),
    hashAuxiliaryData,
  )
import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase, StrictMaybe (..), invalidKey, quorum, (==>))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VKey,
    asWitness,
  )
import Cardano.Ledger.Rules.ValidationMode (runValidationStaticWith, runValidationWith)
import Cardano.Ledger.SafeHash (extractHash, hashAnnotated)
import Cardano.Ledger.Serialization
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Cardano.Ledger.Shelley.Address.Bootstrap (BootstrapWitness, bwKey, verifyBootstrapWit)
import Cardano.Ledger.Shelley.Delegation.Certificates (isInstantaneousRewards)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState
  ( UTxOState (..),
    WitHashes (..),
    diffWitHashes,
    nullWitHashes,
    witsFromTxWitnesses,
    witsVKeyNeeded,
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Utxo (UTXO, UtxoEnv (..), UtxoEvent, UtxoPredicateFailure)
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Shelley.Tx
  ( Tx,
    ValidateScript,
    WitVKey,
    hashScript,
    validateScript,
  )
import Cardano.Ledger.Shelley.TxBody (DCert, EraIndependentTxBody, Wdrl, WitVKey (..))
import Cardano.Ledger.Shelley.UTxO (UTxO, scriptsNeeded, verifyWitVKey)
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (∩))
import Control.State.Transition
  ( Embed,
    IRC (..),
    InitialRule,
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
    wrapEvent,
    wrapFailed,
  )
import Data.Foldable (sequenceA_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import NoThunks.Class (NoThunks (..))
import Validation

-- =========================================

data UTXOW era

data UtxowPredicateFailure era
  = InvalidWitnessesUTXOW
      ![VKey 'Witness (Crypto era)]
  | -- witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      !(WitHashes (Crypto era)) -- witnesses which were needed and not supplied
  | MissingScriptWitnessesUTXOW
      !(Set (ScriptHash (Crypto era))) -- missing scripts
  | ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash (Crypto era))) -- failed scripts
  | UtxoFailure (PredicateFailure (Core.EraRule "UTXO" era))
  | MIRInsufficientGenesisSigsUTXOW (Set (KeyHash 'Witness (Crypto era)))
  | MissingTxBodyMetadataHash
      !(AuxiliaryDataHash (Crypto era)) -- hash of the full metadata
  | MissingTxMetadata
      !(AuxiliaryDataHash (Crypto era)) -- hash of the metadata included in the transaction body
  | ConflictingMetadataHash
      !(AuxiliaryDataHash (Crypto era)) -- hash of the metadata included in the transaction body
      !(AuxiliaryDataHash (Crypto era)) -- hash of the full metadata
      -- Contains out of range values (strings too long)
  | InvalidMetadata
  | ExtraneousScriptWitnessesUTXOW
      !(Set (ScriptHash (Crypto era))) -- extraneous scripts
  deriving (Generic)

newtype UtxowEvent era
  = UtxoEvent (Event (Core.EraRule "UTXO" era))

instance
  ( NoThunks (PredicateFailure (Core.EraRule "UTXO" era)),
    Era era
  ) =>
  NoThunks (UtxowPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Era era
  ) =>
  Eq (UtxowPredicateFailure era)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Era era
  ) =>
  Show (UtxowPredicateFailure era)

instance
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  ToCBOR (UtxowPredicateFailure era)
  where
  toCBOR = \case
    InvalidWitnessesUTXOW wits ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeFoldable wits
    MissingVKeyWitnessesUTXOW (WitHashes missing) ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> encodeFoldable missing
    MissingScriptWitnessesUTXOW ss ->
      encodeListLen 2 <> toCBOR (2 :: Word8)
        <> encodeFoldable ss
    ScriptWitnessNotValidatingUTXOW ss ->
      encodeListLen 2 <> toCBOR (3 :: Word8)
        <> encodeFoldable ss
    (UtxoFailure a) ->
      encodeListLen 2 <> toCBOR (4 :: Word8)
        <> toCBOR a
    MIRInsufficientGenesisSigsUTXOW sigs ->
      encodeListLen 2 <> toCBOR (5 :: Word8)
        <> encodeFoldable sigs
    MissingTxBodyMetadataHash h ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR h
    MissingTxMetadata h ->
      encodeListLen 2 <> toCBOR (7 :: Word8) <> toCBOR h
    ConflictingMetadataHash bodyHash fullMDHash ->
      encodeListLen 3 <> toCBOR (8 :: Word8) <> toCBOR bodyHash <> toCBOR fullMDHash
    InvalidMetadata ->
      encodeListLen 1 <> toCBOR (9 :: Word8)
    ExtraneousScriptWitnessesUTXOW ss ->
      encodeListLen 2 <> toCBOR (10 :: Word8)
        <> encodeFoldable ss

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  FromCBOR (UtxowPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (UTXOW era)" $
    \case
      0 -> do
        wits <- decodeList fromCBOR
        pure (2, InvalidWitnessesUTXOW wits)
      1 -> do
        missing <- decodeSet fromCBOR
        pure (2, MissingVKeyWitnessesUTXOW $ WitHashes missing)
      2 -> do
        ss <- decodeSet fromCBOR
        pure (2, MissingScriptWitnessesUTXOW ss)
      3 -> do
        ss <- decodeSet fromCBOR
        pure (2, ScriptWitnessNotValidatingUTXOW ss)
      4 -> do
        a <- fromCBOR
        pure (2, UtxoFailure a)
      5 -> do
        s <- decodeSet fromCBOR
        pure (2, MIRInsufficientGenesisSigsUTXOW s)
      6 -> do
        h <- fromCBOR
        pure (2, MissingTxBodyMetadataHash h)
      7 -> do
        h <- fromCBOR
        pure (2, MissingTxMetadata h)
      8 -> do
        bodyHash <- fromCBOR
        fullMDHash <- fromCBOR
        pure (3, ConflictingMetadataHash bodyHash fullMDHash)
      9 -> pure (1, InvalidMetadata)
      10 -> do
        ss <- decodeSet fromCBOR
        pure (2, ExtraneousScriptWitnessesUTXOW ss)
      k -> invalidKey k

-- =================================================
--  State Transition System Instances

type ShelleyStyleWitnessNeeds era =
  ( HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "addrWits" (Core.Tx era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "bootWits" (Core.Tx era) (Set (BootstrapWitness (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    ValidateAuxiliaryData era (Crypto era),
    ValidateScript era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  )

initialLedgerStateUTXOW ::
  forall era.
  ( Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era
  ) =>
  InitialRule (UTXOW era)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakepools genDelegs) <- judgmentContext
  trans @(Core.EraRule "UTXO" era) $ IRC (UtxoEnv slots pp stakepools genDelegs)

-- | Function which collects VKey witnesses.
type CollectVKeyWitnesses era =
  UTxO era ->
  Core.Tx era ->
  GenDelegs (Crypto era) ->
  WitHashes (Crypto era)

-- | A generic Utxow witnessing function designed to be use across many Eras.
--   Note the 'embed' argument lifts from the simple Shelley (UtxowPredicateFailure) to
--   the PredicateFailure (type family) of the context of where it is called.
shelleyStyleWitness ::
  forall era utxow.
  ( Era era,
    BaseM (utxow era) ~ ShelleyBase,
    Embed (Core.EraRule "UTXO" era) (utxow era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Core.Tx era,
    Environment (utxow era) ~ UtxoEnv era,
    State (utxow era) ~ UTxOState era,
    Signal (utxow era) ~ Core.Tx era,
    STS (utxow era),
    ShelleyStyleWitnessNeeds era
  ) =>
  CollectVKeyWitnesses era ->
  (UtxowPredicateFailure era -> PredicateFailure (utxow era)) ->
  TransitionRule (utxow era)
shelleyStyleWitness collectVKeyWitnesses embed = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  auxdata := auxiliaryData tx   -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u
      witsKeyHashes = witsFromTxWitnesses @era tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}

  runValidationStaticWith embed $ validateFailedScripts tx

  {-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  runValidationWith embed $ validateMissingScripts pp utxo tx

  -- check VKey witnesses

  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runValidationStaticWith embed $ validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  let needed = collectVKeyWitnesses utxo tx genDelegs
  runValidationWith embed $ validateNeededWitnesses needed witsKeyHashes

  -- check metadata hash
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runValidationStaticWith embed $ validateMetadata pp tx

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  runValidationWith embed $
    validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  trans @(Core.EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

instance
  ( Era era,
    STS (UTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ UtxoPredicateFailure era,
    Event (Core.EraRule "UTXO" era) ~ UtxoEvent era
  ) =>
  Embed (UTXO era) (UTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( Era era,
    Core.Tx era ~ Tx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Core.Tx era,
    ShelleyStyleWitnessNeeds era
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = ShelleyBase
  type PredicateFailure (UTXOW era) = UtxowPredicateFailure era
  type Event _ = UtxowEvent era
  transitionRules = [shelleyStyleWitness witsVKeyNeeded id]
  initialRules = [initialLedgerStateUTXOW]

{-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
validateFailedScripts ::
  forall era.
  ValidateScript era =>
  Core.Tx era ->
  Validation (NonEmpty (UtxowPredicateFailure era)) ()
validateFailedScripts tx = do
  let failedScripts =
        Map.filterWithKey
          ( \hs validator ->
              hashScript @era validator /= hs || not (validateScript @era validator tx)
          )
          (getField @"scriptWits" tx)
  failureUnless (Map.null failedScripts) $
    ScriptWitnessNotValidatingUTXOW (Map.keysSet failedScripts)

{-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
validateMissingScripts ::
  forall era.
  ( ValidateScript era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  Core.PParams era ->
  UTxO era ->
  Core.Tx era ->
  Validation (NonEmpty (UtxowPredicateFailure era)) ()
validateMissingScripts pp utxo tx =
  let sNeeded = scriptsNeeded utxo tx
      sReceived = Map.keysSet (getField @"scriptWits" tx)
   in if HardForks.missingScriptsSymmetricDifference pp
        then
          sequenceA_
            [ failureUnless (sNeeded `Set.isSubsetOf` sReceived) $
                MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived),
              failureUnless (sReceived `Set.isSubsetOf` sNeeded) $
                ExtraneousScriptWitnessesUTXOW (sReceived `Set.difference` sNeeded)
            ]
        else
          failureUnless (sNeeded == sReceived) $
            MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived)

-- | Given a ledger state, determine if the UTxO witnesses in a given
--  transaction are correct.
validateVerifiedWits ::
  forall era.
  ( Era era,
    HasField "addrWits" (Core.Tx era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "bootWits" (Core.Tx era) (Set (BootstrapWitness (Crypto era))),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  Core.Tx era ->
  Validation (NonEmpty (UtxowPredicateFailure era)) ()
validateVerifiedWits tx =
  case failed <> failedBootstrap of
    [] -> pure ()
    nonEmpty -> failure $ InvalidWitnessesUTXOW nonEmpty
  where
    txbody = getField @"body" tx
    wvkKey (WitVKey k _) = k
    failed =
      wvkKey
        <$> filter
          (not . verifyWitVKey (extractHash (hashAnnotated @(Crypto era) txbody)))
          (Set.toList $ getField @"addrWits" tx)
    failedBootstrap =
      bwKey
        <$> filter
          (not . verifyBootstrapWit (extractHash (hashAnnotated @(Crypto era) txbody)))
          (Set.toList $ getField @"bootWits" tx)

validateNeededWitnesses ::
  WitHashes (Crypto era) ->
  WitHashes (Crypto era) ->
  Validation (NonEmpty (UtxowPredicateFailure era)) ()
validateNeededWitnesses needed witsKeyHashes =
  let missingWitnesses = diffWitHashes needed witsKeyHashes
   in failureUnless (nullWitHashes missingWitnesses) $
        MissingVKeyWitnessesUTXOW missingWitnesses

-- | check metadata hash
--   ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)
validateMetadata ::
  forall era.
  ( Era era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    ValidateAuxiliaryData era (Crypto era)
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Validation (NonEmpty (UtxowPredicateFailure era)) ()
validateMetadata pp tx =
  let txbody = getField @"body" tx
   in case (getField @"adHash" txbody, getField @"auxiliaryData" tx) of
        (SNothing, SNothing) -> pure ()
        (SJust mdh, SNothing) -> failure $ MissingTxMetadata mdh
        (SNothing, SJust md') ->
          failure $ MissingTxBodyMetadataHash (hashAuxiliaryData @era md')
        (SJust mdh, SJust md') ->
          sequenceA_
            [ failureUnless (hashAuxiliaryData @era md' == mdh) $
                ConflictingMetadataHash mdh (hashAuxiliaryData @era md'),
              -- check metadata value sizes
              when (SoftForks.validMetadata pp) $
                failureUnless (validateAuxiliaryData @era md') InvalidMetadata
            ]

-- | check genesis keys signatures for instantaneous rewards certificates
--
-- genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes
-- { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)
validateMIRInsufficientGenesisSigs ::
  ( HasField "body" (Core.Tx era) (Core.TxBody era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert crypto))
  ) =>
  GenDelegs (Crypto era) ->
  Word64 ->
  WitHashes (Crypto era) ->
  Core.Tx era ->
  Validation (NonEmpty (UtxowPredicateFailure era)) ()
validateMIRInsufficientGenesisSigs (GenDelegs genMapping) coreNodeQuorum witsKeyHashes tx =
  let genDelegates =
        Set.fromList $ asWitness . genDelegKeyHash <$> Map.elems genMapping
      WitHashes khAsSet = witsKeyHashes
      genSig = eval (genDelegates ∩ khAsSet)
      txBody = getField @"body" tx
      mirCerts =
        StrictSeq.forceToStrict
          . Seq.filter isInstantaneousRewards
          . StrictSeq.fromStrict
          $ getField @"certs" txBody
   in failureUnless
        (not (null mirCerts) ==> Set.size genSig >= fromIntegral coreNodeQuorum)
        $ MIRInsufficientGenesisSigsUTXOW genSig
