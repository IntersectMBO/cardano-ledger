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
  ( ShelleyUTXOW,
    ShelleyUtxowPredFailure (..),
    ShelleyUtxowEvent (..),
    PredicateFailure,
    transitionRulesUTXOW,

    -- * Individual validation steps
    validateFailedScripts,
    validateMissingScripts,
    validateVerifiedWits,
    validateMetadata,
    validateMIRInsufficientGenesisSigs,
    validateNeededWitnesses,
    propWits,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes
  ( ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    invalidKey,
    quorum,
    strictMaybeToMaybe,
    (==>),
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
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
import Cardano.Ledger.Keys.Bootstrap (bwKey, verifyBootstrapWit)
import Cardano.Ledger.Rules.ValidationMode
  ( Inject (..),
    Test,
    runTest,
    runTestOnSignal,
  )
import Cardano.Ledger.SafeHash (extractHash, hashAnnotated)
import Cardano.Ledger.Serialization
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( delegCWitness,
    genesisCWitness,
    isInstantaneousRewards,
    poolCWitness,
    requiresVKeyWitness,
  )
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState.Types (UTxOState (..))
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (ProposedPPUpdates), Update (Update))
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUTXO, ShelleyUtxoPredFailure, UtxoEnv (..), UtxoEvent)
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    extractKeyHashWitnessSet,
    witsFromTxWitnesses,
  )
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    PoolCert (..),
    PoolParams (..),
    ShelleyEraTxBody (..),
    WitVKey (..),
    getRwdCred,
    unWdrl,
  )
import Cardano.Ledger.Shelley.UTxO (UTxO, scriptsNeeded, txinLookup, verifyWitVKey)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (∩), (◁))
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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Validation

-- =========================================

data ShelleyUTXOW era

data ShelleyUtxowPredFailure era
  = InvalidWitnessesUTXOW
      ![VKey 'Witness (Crypto era)]
  | -- witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      !(Set (KeyHash 'Witness (Crypto era))) -- witnesses which were needed and not supplied
  | MissingScriptWitnessesUTXOW
      !(Set (ScriptHash (Crypto era))) -- missing scripts
  | ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash (Crypto era))) -- failed scripts
  | UtxoFailure (PredicateFailure (EraRule "UTXO" era))
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

newtype ShelleyUtxowEvent era
  = UtxoEvent (Event (EraRule "UTXO" era))

instance
  ( NoThunks (PredicateFailure (EraRule "UTXO" era)),
    Era era
  ) =>
  NoThunks (ShelleyUtxowPredFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "UTXO" era)),
    Era era
  ) =>
  Eq (ShelleyUtxowPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "UTXO" era)),
    Era era
  ) =>
  Show (ShelleyUtxowPredFailure era)

instance
  ( Era era,
    Typeable (Script era),
    Typeable (AuxiliaryData era),
    ToCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  ToCBOR (ShelleyUtxowPredFailure era)
  where
  toCBOR = \case
    InvalidWitnessesUTXOW wits ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeFoldable wits
    MissingVKeyWitnessesUTXOW missing ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> encodeFoldable missing
    MissingScriptWitnessesUTXOW ss ->
      encodeListLen 2 <> toCBOR (2 :: Word8)
        <> encodeFoldable ss
    ScriptWitnessNotValidatingUTXOW ss ->
      encodeListLen 2 <> toCBOR (3 :: Word8)
        <> encodeFoldable ss
    UtxoFailure a ->
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
    FromCBOR (PredicateFailure (EraRule "UTXO" era)),
    Typeable (Script era),
    Typeable (AuxiliaryData era)
  ) =>
  FromCBOR (ShelleyUtxowPredFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (UTXOW era)" $
    \case
      0 -> do
        wits <- decodeList fromCBOR
        pure (2, InvalidWitnessesUTXOW wits)
      1 -> do
        missing <- decodeSet fromCBOR
        pure (2, MissingVKeyWitnessesUTXOW missing)
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

initialLedgerStateUTXOW ::
  forall era.
  ( Embed (EraRule "UTXO" era) (ShelleyUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era
  ) =>
  InitialRule (ShelleyUTXOW era)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakepools genDelegs) <- judgmentContext
  trans @(EraRule "UTXO" era) $ IRC (UtxoEnv slots pp stakepools genDelegs)

-- | A generic Utxow witnessing function designed to be used across many Eras.
--   Note the 'embed' argument lifts from the simple Shelley (ShelleyUtxowPredFailure) to
--   the PredicateFailure (type family) of the context of where it is called.
transitionRulesUTXOW ::
  forall era utxow.
  ( EraTx era,
    ShelleyEraTxBody era,
    BaseM (utxow era) ~ ShelleyBase,
    Embed (EraRule "UTXO" era) (utxow era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ Tx era,
    Environment (utxow era) ~ UtxoEnv era,
    State (utxow era) ~ UTxOState era,
    Signal (utxow era) ~ Tx era,
    PredicateFailure (utxow era) ~ ShelleyUtxowPredFailure era,
    STS (utxow era),
    HasField "_protocolVersion" (PParams era) ProtVer,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  TransitionRule (utxow era)
transitionRulesUTXOW = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u
      witsKeyHashes = witsFromTxWitnesses tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}

  runTestOnSignal $ validateFailedScripts tx

  {-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  runTest $ validateMissingScripts pp (scriptsNeeded utxo tx) (Map.keysSet (tx ^. witsTxL . scriptWitsL))

  -- check VKey witnesses
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsVKeyNeeded genDelegs utxo tx witsKeyHashes

  -- check metadata hash
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $ validateMetadata pp tx

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  trans @(EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

instance
  ( Era era,
    STS (ShelleyUTXO era),
    PredicateFailure (EraRule "UTXO" era) ~ ShelleyUtxoPredFailure era,
    Event (EraRule "UTXO" era) ~ UtxoEvent era
  ) =>
  Embed (ShelleyUTXO era) (ShelleyUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( EraTx era,
    ShelleyEraTxBody era,
    Tx era ~ ShelleyTx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    HasField "_protocolVersion" (PParams era) ProtVer,
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ShelleyUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ Tx era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  STS (ShelleyUTXOW era)
  where
  type State (ShelleyUTXOW era) = UTxOState era
  type Signal (ShelleyUTXOW era) = ShelleyTx era
  type Environment (ShelleyUTXOW era) = UtxoEnv era
  type BaseM (ShelleyUTXOW era) = ShelleyBase
  type PredicateFailure (ShelleyUTXOW era) = ShelleyUtxowPredFailure era
  type Event (ShelleyUTXOW era) = ShelleyUtxowEvent era
  transitionRules = [transitionRulesUTXOW]
  initialRules = [initialLedgerStateUTXOW]

{-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
validateFailedScripts ::
  forall era. EraTx era => Tx era -> Test (ShelleyUtxowPredFailure era)
validateFailedScripts tx = do
  let phase1Map = getPhase1 (tx ^. witsTxL . scriptWitsL)
      failedScripts =
        Map.filterWithKey
          ( \hs (core, phase) ->
              hashScript @era core /= hs || not (validateScript @era phase tx)
          )
          phase1Map
  failureUnless (Map.null failedScripts) $
    ScriptWitnessNotValidatingUTXOW (Map.keysSet failedScripts)

{-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)    -}
{-  sNeeded := scriptsNeeded utxo tx                             -}
{-  sReceived := Map.keysSet (getField @"scriptWits" tx)         -}
validateMissingScripts ::
  forall era.
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  PParams era ->
  Set (ScriptHash (Crypto era)) ->
  Set (ScriptHash (Crypto era)) ->
  Test (ShelleyUtxowPredFailure era)
validateMissingScripts pp sNeeded sReceived =
  if HardForks.missingScriptsSymmetricDifference pp
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
  ( EraTx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  Tx era ->
  Test (ShelleyUtxowPredFailure era)
validateVerifiedWits tx =
  case failed <> failedBootstrap of
    [] -> pure ()
    nonEmpty -> failure $ InvalidWitnessesUTXOW nonEmpty
  where
    txBody = tx ^. bodyTxL
    txBodyHash = extractHash (hashAnnotated @(Crypto era) txBody)
    wvkKey (WitVKey k _) = k
    failed =
      wvkKey
        <$> filter
          (not . verifyWitVKey txBodyHash)
          (Set.toList $ tx ^. witsTxL . addrWitsL)
    failedBootstrap =
      bwKey
        <$> filter
          (not . verifyBootstrapWit txBodyHash)
          (Set.toList $ tx ^. witsTxL . bootAddrWitsL)

{-
validateNeededWitnesses ::
  ( Era era,
    HasField "wdrls" (TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (TxBody era) (StrictMaybe (Update era))
  ) =>
  GenDelegs (Crypto era) ->
  UTxO era ->
  Tx era ->
  Set (KeyHash 'Witness (Crypto era)) ->
  Test (ShelleyUtxowPredFailure era)
validateNeededWitnesses genDelegs utxo tx witsKeyHashes =
  let needed = witsVKeyNeeded utxo tx genDelegs
      missingWitnesses = diffWitHashes needed witsKeyHashes
   in failureUnless (nullWitHashes missingWitnesses) $
        MissingVKeyWitnessesUTXOW missingWitnesses
-}

-- How to compute the set of witnessed needed (witsvkeyneeded) varies
-- from Era to Era, so we parameterise over that function in this test.
-- That allows it to be used in many Eras.
validateNeededWitnesses ::
  (UTxO era -> Tx era -> GenDelegs (Crypto era) -> Set (KeyHash 'Witness (Crypto era))) ->
  GenDelegs (Crypto era) ->
  UTxO era ->
  Tx era ->
  Set (KeyHash 'Witness (Crypto era)) ->
  Test (ShelleyUtxowPredFailure era)
validateNeededWitnesses witsvkeyneeded genDelegs utxo tx witsKeyHashes =
  let needed = witsvkeyneeded utxo tx genDelegs
      missingWitnesses = Set.difference needed witsKeyHashes
   in failureUnless (Set.null missingWitnesses) $
        MissingVKeyWitnessesUTXOW missingWitnesses

-- | Collect the set of hashes of keys that needs to sign a
--  given transaction. This set consists of the txin owners,
--  certificate authors, and withdrawal reward accounts.
witsVKeyNeeded ::
  forall era.
  ( EraTx era,
    ShelleyEraTxBody era
  ) =>
  UTxO era ->
  Tx era ->
  GenDelegs (Crypto era) ->
  Set (KeyHash 'Witness (Crypto era))
witsVKeyNeeded utxo' tx genDelegs =
  certAuthors
    `Set.union` inputAuthors
    `Set.union` owners
    `Set.union` wdrlAuthors
    `Set.union` updateKeys
  where
    txBody = tx ^. bodyTxL
    inputAuthors :: Set (KeyHash 'Witness (Crypto era))
    inputAuthors = foldr accum Set.empty (txBody ^. inputsTxBodyL)
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just txOut ->
              case txOut ^. addrTxOutL of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness (Crypto era))
    wdrlAuthors = Map.foldrWithKey accum Set.empty (unWdrl (txBody ^. wdrlsTxBodyL))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (Crypto era))
    owners = foldr accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum (DCertPool (RegPool pool)) ans =
          Set.union
            (Set.map asWitness (_poolOwners pool))
            ans
        accum _cert ans = ans
    cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
    cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
    cwitness c = error $ show c ++ " does not have a witness"
    -- key reg requires no witness but this is already filtered outby requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness (Crypto era))
    certAuthors = foldr accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness (Crypto era))
    updateKeys =
      asWitness
        `Set.map` propWits
          (strictMaybeToMaybe $ txBody ^. updateTxBodyL)
          genDelegs

-- | check metadata hash
--   ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)
validateMetadata ::
  forall era.
  ( EraTx era,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  PParams era ->
  Tx era ->
  Test (ShelleyUtxowPredFailure era)
validateMetadata pp tx =
  let txBody = tx ^. bodyTxL
      pv = getField @"_protocolVersion" pp
   in case (txBody ^. auxDataHashTxBodyL, tx ^. auxDataTxL) of
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
                failureUnless (validateAuxiliaryData @era pv md') InvalidMetadata
            ]

-- | check genesis keys signatures for instantaneous rewards certificates
--
-- genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes
-- { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ |genSig| ≥ Quorum
validateMIRInsufficientGenesisSigs ::
  ( EraTx era,
    ShelleyEraTxBody era
  ) =>
  GenDelegs (Crypto era) ->
  Word64 ->
  Set (KeyHash 'Witness (Crypto era)) ->
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

-- ===================================================
-- Inject Instances

instance Inject (ShelleyUtxowPredFailure era) (ShelleyUtxowPredFailure era) where
  inject = id

instance
  PredicateFailure (EraRule "UTXO" era) ~ ShelleyUtxoPredFailure era =>
  Inject (ShelleyUtxoPredFailure era) (ShelleyUtxowPredFailure era)
  where
  inject = UtxoFailure

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits ::
  Maybe (Update era) ->
  GenDelegs (Crypto era) ->
  Set (KeyHash 'Witness (Crypto era))
propWits Nothing _ = Set.empty
propWits (Just (Update (ProposedPPUpdates pup) _)) (GenDelegs genDelegs) =
  Set.map asWitness . Set.fromList $ Map.elems updateKeys
  where
    updateKeys' = eval (Map.keysSet pup ◁ genDelegs)
    updateKeys = Map.map genDelegKeyHash updateKeys'
