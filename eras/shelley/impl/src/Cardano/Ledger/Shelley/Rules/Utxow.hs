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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Utxow (
  ShelleyUTXOW,
  ShelleyUtxowPredFailure (..),
  ShelleyUtxowEvent (..),
  PredicateFailure,
  transitionRulesUTXOW,
  shelleyWitsVKeyNeeded,
  witsVKeyNeededGov,
  witsVKeyNeededNoGov,

  -- * Individual validation steps
  validateFailedNativeScripts,
  validateMissingScripts,
  validateVerifiedWits,
  validateMetadata,
  validateMIRInsufficientGenesisSigs,
  validateNeededWitnesses,
  propWits,
)
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (VerKeyDSIGN))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  StrictMaybe (..),
  invalidKey,
  maybeToStrictMaybe,
  quorum,
  (==>),
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeRecordSum, encodeListLen)
import Cardano.Ledger.CertState (CertState, certDState, dsGenDelegs)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (DSIGN))
import Cardano.Ledger.Keys (
  DSignable,
  GenDelegPair (..),
  GenDelegs (..),
  Hash,
  KeyHash,
  KeyRole (..),
  VKey,
  WitVKey (..),
  asWitness,
  bwKey,
  verifyBootstrapWit,
 )
import Cardano.Ledger.Rules.ValidationMode (
  Test,
  runTest,
  runTestOnSignal,
 )
import Cardano.Ledger.SafeHash (extractHash, hashAnnotated)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyUTXOW)
import Cardano.Ledger.Shelley.LedgerState.Types (UTxOState (..))
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (ProposedPPUpdates), Update (..))
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPpupPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (
  ShelleyUTXO,
  ShelleyUtxoPredFailure,
  UtxoEnv (..),
  UtxoEvent,
 )
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Shelley.Tx (witsFromTxWitnesses)
import Cardano.Ledger.Shelley.TxCert (isInstantaneousRewards)
import Cardano.Ledger.Shelley.UTxO (
  EraUTxO (..),
  ScriptsProvided (..),
  ShelleyScriptsNeeded (..),
  UTxO,
  getShelleyWitsVKeyNeededNoGov,
  verifyWitVKey,
 )
import Control.DeepSeq
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (∩), (◁))
import Control.State.Transition (
  Embed,
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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Validation

-- =========================================

data ShelleyUtxowPredFailure era
  = InvalidWitnessesUTXOW
      ![VKey 'Witness (EraCrypto era)]
  | -- witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      !(Set (KeyHash 'Witness (EraCrypto era))) -- witnesses which were needed and not supplied
  | MissingScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era))) -- missing scripts
  | ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash (EraCrypto era))) -- failed scripts
  | UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | MIRInsufficientGenesisSigsUTXOW (Set (KeyHash 'Witness (EraCrypto era)))
  | MissingTxBodyMetadataHash
      !(AuxiliaryDataHash (EraCrypto era)) -- hash of the full metadata
  | MissingTxMetadata
      !(AuxiliaryDataHash (EraCrypto era)) -- hash of the metadata included in the transaction body
  | ConflictingMetadataHash
      !(AuxiliaryDataHash (EraCrypto era)) -- hash of the metadata included in the transaction body
      !(AuxiliaryDataHash (EraCrypto era)) -- hash of the full metadata
      -- Contains out of range values (strings too long)
  | InvalidMetadata
  | ExtraneousScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era))) -- extraneous scripts
  deriving (Generic)

type instance EraRuleFailure "UTXOW" (ShelleyEra c) = ShelleyUtxowPredFailure (ShelleyEra c)

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (ShelleyEra c)

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (ShelleyEra c) where
  injectFailure = UtxoFailure

instance InjectRuleFailure "UTXOW" ShelleyPpupPredFailure (ShelleyEra c) where
  injectFailure = UtxoFailure . injectFailure

newtype ShelleyUtxowEvent era
  = UtxoEvent (Event (EraRule "UTXO" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "UTXO" era)) => Eq (ShelleyUtxowEvent era)

instance NFData (Event (EraRule "UTXO" era)) => NFData (ShelleyUtxowEvent era)

instance
  ( NoThunks (PredicateFailure (EraRule "UTXO" era))
  , Era era
  ) =>
  NoThunks (ShelleyUtxowPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "UTXO" era))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  , Era era
  ) =>
  NFData (ShelleyUtxowPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "UTXO" era))
  , Era era
  ) =>
  Eq (ShelleyUtxowPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "UTXO" era))
  , Era era
  ) =>
  Show (ShelleyUtxowPredFailure era)

instance
  ( Era era
  , Typeable (Script era)
  , Typeable (TxAuxData era)
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  EncCBOR (ShelleyUtxowPredFailure era)
  where
  encCBOR = \case
    InvalidWitnessesUTXOW wits ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR wits
    MissingVKeyWitnessesUTXOW missing ->
      encodeListLen 2 <> encCBOR (1 :: Word8) <> encCBOR missing
    MissingScriptWitnessesUTXOW ss ->
      encodeListLen 2
        <> encCBOR (2 :: Word8)
        <> encCBOR ss
    ScriptWitnessNotValidatingUTXOW ss ->
      encodeListLen 2
        <> encCBOR (3 :: Word8)
        <> encCBOR ss
    UtxoFailure a ->
      encodeListLen 2
        <> encCBOR (4 :: Word8)
        <> encCBOR a
    MIRInsufficientGenesisSigsUTXOW sigs ->
      encodeListLen 2
        <> encCBOR (5 :: Word8)
        <> encCBOR sigs
    MissingTxBodyMetadataHash h ->
      encodeListLen 2 <> encCBOR (6 :: Word8) <> encCBOR h
    MissingTxMetadata h ->
      encodeListLen 2 <> encCBOR (7 :: Word8) <> encCBOR h
    ConflictingMetadataHash bodyHash fullMDHash ->
      encodeListLen 3 <> encCBOR (8 :: Word8) <> encCBOR bodyHash <> encCBOR fullMDHash
    InvalidMetadata ->
      encodeListLen 1 <> encCBOR (9 :: Word8)
    ExtraneousScriptWitnessesUTXOW ss ->
      encodeListLen 2
        <> encCBOR (10 :: Word8)
        <> encCBOR ss

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , Typeable (Script era)
  , Typeable (TxAuxData era)
  ) =>
  DecCBOR (ShelleyUtxowPredFailure era)
  where
  decCBOR = decodeRecordSum "PredicateFailure (UTXOW era)" $
    \case
      0 -> do
        wits <- decCBOR
        pure (2, InvalidWitnessesUTXOW wits)
      1 -> do
        missing <- decCBOR
        pure (2, MissingVKeyWitnessesUTXOW missing)
      2 -> do
        ss <- decCBOR
        pure (2, MissingScriptWitnessesUTXOW ss)
      3 -> do
        ss <- decCBOR
        pure (2, ScriptWitnessNotValidatingUTXOW ss)
      4 -> do
        a <- decCBOR
        pure (2, UtxoFailure a)
      5 -> do
        s <- decCBOR
        pure (2, MIRInsufficientGenesisSigsUTXOW s)
      6 -> do
        h <- decCBOR
        pure (2, MissingTxBodyMetadataHash h)
      7 -> do
        h <- decCBOR
        pure (2, MissingTxMetadata h)
      8 -> do
        bodyHash <- decCBOR
        fullMDHash <- decCBOR
        pure (3, ConflictingMetadataHash bodyHash fullMDHash)
      9 -> pure (1, InvalidMetadata)
      10 -> do
        ss <- decCBOR
        pure (2, ExtraneousScriptWitnessesUTXOW ss)
      k -> invalidKey k

-- =================================================
--  State Transition System Instances

initialLedgerStateUTXOW ::
  forall era.
  ( Embed (EraRule "UTXO" era) (ShelleyUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  ) =>
  InitialRule (ShelleyUTXOW era)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp certState) <- judgmentContext
  trans @(EraRule "UTXO" era) $ IRC (UtxoEnv slots pp certState)

-- | A generic Utxow witnessing function designed to be used across many Eras.
--   Note the 'embed' argument lifts from the simple Shelley (ShelleyUtxowPredFailure) to
--   the PredicateFailure (type family) of the context of where it is called.
transitionRulesUTXOW ::
  forall era.
  ( EraUTxO era
  , ShelleyEraTxBody era
  , ScriptsNeeded era ~ ShelleyScriptsNeeded era
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Embed (EraRule "UTXO" era) (EraRule "UTXOW" era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , STS (EraRule "UTXOW" era)
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  ) =>
  TransitionRule (EraRule "UTXOW" era)
transitionRulesUTXOW = do
  (TRC (utxoEnv@(UtxoEnv _ pp certState), u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = utxosUtxo u
      witsKeyHashes = witsFromTxWitnesses tx
      scriptsProvided = getScriptsProvided utxo tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}

  runTestOnSignal $ validateFailedNativeScripts scriptsProvided tx

  {-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  let scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
  runTest $ validateMissingScripts scriptsNeeded scriptsProvided

  -- check VKey witnesses
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsKeyHashes certState utxo (tx ^. bodyTxL)

  -- check metadata hash
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $ validateMetadata pp tx

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ TxCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  let genDelegs = dsGenDelegs (certDState certState)
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  trans @(EraRule "UTXO" era) $ TRC (utxoEnv, u, tx)

instance
  ( Era era
  , STS (ShelleyUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ ShelleyUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ UtxoEvent era
  ) =>
  Embed (ShelleyUTXO era) (ShelleyUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( EraTx era
  , EraUTxO era
  , ShelleyEraTxBody era
  , ScriptsNeeded era ~ ShelleyScriptsNeeded era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ShelleyUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , EraRule "UTXOW" era ~ ShelleyUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , EraGov era
  ) =>
  STS (ShelleyUTXOW era)
  where
  type State (ShelleyUTXOW era) = UTxOState era
  type Signal (ShelleyUTXOW era) = Tx era
  type Environment (ShelleyUTXOW era) = UtxoEnv era
  type BaseM (ShelleyUTXOW era) = ShelleyBase
  type PredicateFailure (ShelleyUTXOW era) = ShelleyUtxowPredFailure era
  type Event (ShelleyUTXOW era) = ShelleyUtxowEvent era
  transitionRules = [transitionRulesUTXOW]
  initialRules = [initialLedgerStateUTXOW]

{-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
validateFailedNativeScripts ::
  EraTx era => ScriptsProvided era -> Tx era -> Test (ShelleyUtxowPredFailure era)
validateFailedNativeScripts (ScriptsProvided scriptsProvided) tx = do
  let failedScripts =
        Map.filter -- we keep around only non-validating native scripts
          (maybe False (not . validateNativeScript tx) . getNativeScript)
          scriptsProvided
  failureUnless (Map.null failedScripts) $
    ScriptWitnessNotValidatingUTXOW (Map.keysSet failedScripts)

{-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)    -}
{-  sNeeded := scriptsNeeded utxo tx                             -}
{-  sProvided := Map.keysSet (tx ^. witsTxL . scriptTxWitsL)     -}
validateMissingScripts ::
  ShelleyScriptsNeeded era ->
  ScriptsProvided era ->
  Test (ShelleyUtxowPredFailure era)
validateMissingScripts (ShelleyScriptsNeeded sNeeded) scriptsprovided =
  sequenceA_
    [ failureUnless (sNeeded `Set.isSubsetOf` sProvided) $
        MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sProvided)
    , failureUnless (sProvided `Set.isSubsetOf` sNeeded) $
        ExtraneousScriptWitnessesUTXOW (sProvided `Set.difference` sNeeded)
    ]
  where
    sProvided = Map.keysSet $ unScriptsProvided scriptsprovided

-- | Determine if the UTxO witnesses in a given transaction are correct.
validateVerifiedWits ::
  ( EraTx era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  ) =>
  Tx era ->
  Test (ShelleyUtxowPredFailure era)
validateVerifiedWits tx =
  case failed <> failedBootstrap of
    [] -> pure ()
    nonEmpty -> failure $ InvalidWitnessesUTXOW nonEmpty
  where
    txBody = tx ^. bodyTxL
    txBodyHash = extractHash (hashAnnotated txBody)
    wvkKey (WitVKey k _) = k
    failed =
      wvkKey
        <$> filter
          (not . verifyWitVKey txBodyHash)
          (Set.toList $ tx ^. witsTxL . addrTxWitsL)
    failedBootstrap =
      bwKey
        <$> filter
          (not . verifyBootstrapWit txBodyHash)
          (Set.toList $ tx ^. witsTxL . bootAddrTxWitsL)

-- | Verify that we provide at least all of the required witnesses
--
-- @witsVKeyNeeded utxo tx ⊆ witsKeyHashes@
validateNeededWitnesses ::
  EraUTxO era =>
  -- | Provided witness
  Set (KeyHash 'Witness (EraCrypto era)) ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Test (ShelleyUtxowPredFailure era)
validateNeededWitnesses witsKeyHashes certState utxo txBody =
  let needed = getWitsVKeyNeeded certState utxo txBody
      missingWitnesses = Set.difference needed witsKeyHashes
   in failureUnless (Set.null missingWitnesses) $
        MissingVKeyWitnessesUTXOW missingWitnesses

-- | Collect the set of hashes of keys that needs to sign a
--  given transaction. This set consists of the txin owners,
--  certificate authors, and withdrawal reward accounts.
witsVKeyNeededGov ::
  forall era.
  ShelleyEraTxBody era =>
  TxBody era ->
  GenDelegs (EraCrypto era) ->
  Set (KeyHash 'Witness (EraCrypto era))
witsVKeyNeededGov txBody genDelegs =
  asWitness `Set.map` proposedUpdatesWitnesses (txBody ^. updateTxBodyL) genDelegs
{-# DEPRECATED witsVKeyNeededGov "As unnecessary. Use `getWitsVKeyNeeded` instead" #-}

witsVKeyNeededNoGov ::
  forall era.
  EraTx era =>
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
witsVKeyNeededNoGov = getShelleyWitsVKeyNeededNoGov
{-# DEPRECATED witsVKeyNeededNoGov "Use `getShelleyWitsVKeyNeededNoGov` instead" #-}

shelleyWitsVKeyNeeded ::
  forall era.
  (EraTx era, ShelleyEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  GenDelegs (EraCrypto era) ->
  Set (KeyHash 'Witness (EraCrypto era))
shelleyWitsVKeyNeeded utxo txBody genDelegs =
  witsVKeyNeededNoGov utxo txBody
    `Set.union` witsVKeyNeededGov txBody genDelegs
{-# DEPRECATED shelleyWitsVKeyNeeded "Use `getShelleyWitsVKeyNeeded` instead" #-}

-- | check metadata hash
--   ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)
validateMetadata :: EraTx era => PParams era -> Tx era -> Test (ShelleyUtxowPredFailure era)
validateMetadata pp tx =
  let txBody = tx ^. bodyTxL
      pv = pp ^. ppProtocolVersionL
   in case (txBody ^. auxDataHashTxBodyL, tx ^. auxDataTxL) of
        (SNothing, SNothing) -> pure ()
        (SJust mdh, SNothing) -> failure $ MissingTxMetadata mdh
        (SNothing, SJust md') ->
          failure $ MissingTxBodyMetadataHash (hashTxAuxData md')
        (SJust mdh, SJust md') ->
          sequenceA_
            [ failureUnless (hashTxAuxData md' == mdh) $
                ConflictingMetadataHash mdh (hashTxAuxData md')
            , -- check metadata value sizes
              when (SoftForks.validMetadata pv) $
                failureUnless (validateTxAuxData pv md') InvalidMetadata
            ]

-- | check genesis keys signatures for instantaneous rewards certificates
--
-- genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes
-- { c ∈ txcerts txb ∩ TxCert_mir} ≠ ∅  ⇒ |genSig| ≥ Quorum
validateMIRInsufficientGenesisSigs ::
  ( EraTx era
  , ShelleyEraTxBody era
  ) =>
  GenDelegs (EraCrypto era) ->
  Word64 ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  Tx era ->
  Test (ShelleyUtxowPredFailure era)
validateMIRInsufficientGenesisSigs (GenDelegs genMapping) coreNodeQuorum witsKeyHashes tx =
  let genDelegates =
        Set.fromList $ asWitness . genDelegKeyHash <$> Map.elems genMapping
      khAsSet = witsKeyHashes
      genSig = eval (genDelegates ∩ khAsSet)
      txBody = tx ^. bodyTxL
      mirCerts =
        StrictSeq.forceToStrict
          . Seq.filter isInstantaneousRewards
          . StrictSeq.fromStrict
          $ txBody ^. certsTxBodyL
   in failureUnless
        (not (null mirCerts) ==> Set.size genSig >= fromIntegral coreNodeQuorum)
        $ MIRInsufficientGenesisSigsUTXOW genSig

-- | Deprecated.
proposedUpdatesWitnesses ::
  StrictMaybe (Update era) ->
  GenDelegs (EraCrypto era) ->
  Set (KeyHash 'Witness (EraCrypto era))
proposedUpdatesWitnesses SNothing _ = Set.empty
proposedUpdatesWitnesses (SJust (Update (ProposedPPUpdates pup) _)) (GenDelegs genDelegs) =
  Set.map asWitness . Set.fromList $ Map.elems updateKeys''
  where
    updateKeys' = eval (Map.keysSet pup ◁ genDelegs)
    updateKeys'' = Map.map genDelegKeyHash updateKeys'

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits ::
  Maybe (Update era) ->
  GenDelegs (EraCrypto era) ->
  Set (KeyHash 'Witness (EraCrypto era))
propWits mu = proposedUpdatesWitnesses (maybeToStrictMaybe mu)
{-# DEPRECATED
  propWits
  "This will become an internal function in the future. \
  \ Submit an issue if you still need it. "
  #-}
