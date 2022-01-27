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
    ShelleyUtxowNeeds,
    initialLedgerStateUTXOW,
    genericShelleyUtxow,
    UtxowDeltaS (..),
    shelleyUtxowDelta,
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
import Cardano.Ledger.Era (Era (Crypto), ValidateScript (hashScript, isNativeScript, validateScript))
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
import Cardano.Ledger.Rules.ValidationMode (failBecauseS, (?!#), (?!#:))
import Cardano.Ledger.Serialization
  ( decodeList,
    decodeRecordSum,
    decodeSet,
    encodeFoldable,
  )
import Cardano.Ledger.Shelley.Address.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Shelley.Delegation.Certificates (isInstantaneousRewards)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState
  ( UTxOState (..),
    WitHashes (..),
    diffWitHashes,
    nullWitHashes,
    verifiedWits,
    witsFromTxWitnesses,
    witsVKeyNeeded,
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Utxo (UTXO, UtxoEnv (..), UtxoEvent, UtxoPredicateFailure)
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Shelley.Tx
  ( Tx,
    WitVKey,
    WitnessSet,
  )
import Cardano.Ledger.Shelley.TxBody (DCert, EraIndependentTxBody, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO, scriptsNeeded)
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
    (?!),
    (?!:),
  )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import NoThunks.Class (NoThunks (..))

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

type ShelleyUtxowNeeds era =
  ( HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))), --scriptsNeeded
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))), --scriptsNeeded
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)), --scriptsNeeded
    HasField "addrWits" (Core.Tx era) (Set (WitVKey 'Witness (Crypto era))), -- witsFromTxWitnesses
    HasField "bootWits" (Core.Tx era) (Set (BootstrapWitness (Crypto era))), -- witsFromTxWitnesses
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)), -- shelleyUtxowDelta
    HasField "_protocolVersion" (Core.PParams era) ProtVer, -- missingScriptsSymmetricDifference
    Ord (Core.Script era), -- We need to make Sets of Scripts                -- shelleyUtxowDelta
    ValidateAuxiliaryData era (Crypto era), -- hashAuxiliaryData
    ValidateScript era, -- isNativeScript
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody) -- verifiedWits
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
  ( -- Fix Core.Witnesses to the Shelley Era
    Core.Witnesses era ~ WitnessSet era,
    Core.Tx era ~ Tx era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Core.Tx era,
    PredicateFailure (UTXOW era) ~ UtxowPredicateFailure era,
    -- Supply the HasField and Validate instances for Shelley
    ShelleyUtxowNeeds era
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = ShelleyBase
  type PredicateFailure (UTXOW era) = UtxowPredicateFailure era
  type Event _ = UtxowEvent era
  transitionRules = [genericShelleyUtxow shelleyUtxowDelta]
  initialRules = [initialLedgerStateUTXOW]

-- =====================================================
-- Writing resuable checks from the Shelley era

data UtxowDeltaS utxow era = UtxowDeltaS
  { witsVKeyNeeded' ::
      UTxO era ->
      Core.Tx era ->
      GenDelegs (Crypto era) ->
      WitHashes (Crypto era),
    embed' :: UtxowPredicateFailure era -> PredicateFailure (utxow era),
    txScripts' :: Core.Tx era -> UTxO era -> Set (Core.Script era),
    referenceInputs' :: Core.Tx era -> Set (TxIn (Crypto era))
  }

shelleyUtxowDelta ::
  ( Era era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Core.Witnesses era ~ WitnessSet era,
    Ord (Core.Script era)
  ) =>
  UtxowDeltaS UTXOW era
shelleyUtxowDelta = UtxowDeltaS witsVKeyNeeded id txscripts (const Set.empty)
  where
    txscripts tx _utxo = Set.fromList (Map.elems (getField @"scriptWits" (getField @"wits" tx)))

genericShelleyUtxow ::
  forall era utxow.
  ( Era era,
    -- (utxow era) has an STS instance
    STS (utxow era), -- Needed to use liftSTS
    BaseM (utxow era) ~ ShelleyBase,
    Environment (utxow era) ~ UtxoEnv era,
    State (utxow era) ~ UTxOState era,
    Signal (utxow era) ~ Core.Tx era,
    -- Utxow calls Utxo, so we need some consistency constraints
    Embed (Core.EraRule "UTXO" era) (utxow era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Core.Tx era,
    --
    ShelleyUtxowNeeds era
  ) =>
  UtxowDeltaS utxow era ->
  TransitionRule (utxow era)
genericShelleyUtxow delta = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  auxdata := auxiliaryData tx   -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let txbody = getField @"body" tx
      utxo = _utxo u
      witsKeyHashes = witsFromTxWitnesses @era tx
      auxdata = getField @"auxiliaryData" tx
      liftfail = embed' delta
      alltxscripts = txScripts' delta tx utxo

  -- check scripts
  {-  ∀ s ∈ (txscripts tx utxo) ∩ Scriptnative), runNativeScript s tx   -}
  let -- This consistent hash check is implicit in validateScript
      -- in the specifcation but is separate in the code.
      inconsistent = Map.foldlWithKey' accum [] (getField @"scriptWits" tx)
        where
          accum ans hs script =
            if hashScript @era script == hs then ans else hs : ans
      invalidphase1 = Set.foldl' accum [] alltxscripts
        where
          accum ans s =
            if isNativeScript @era s && not (validateScript @era s tx)
              then hashScript @era s : ans
              else ans
  case inconsistent ++ invalidphase1 of
    [] -> pure ()
    fs -> failBecauseS $ liftfail $ ScriptWitnessNotValidatingUTXOW $ Set.fromList $ fs

  {-  { s | (_,s) ∈ scriptsNeeded utxo tx} = { hashScript h | h ∈ txscripts tx utxo } -}
  let sNeeded = scriptsNeeded utxo tx
      sReceived = Set.map (hashScript @era) alltxscripts
  if HardForks.missingScriptsSymmetricDifference pp
    then do
      sNeeded `Set.isSubsetOf` sReceived
        ?! liftfail (MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived))
      sReceived `Set.isSubsetOf` sNeeded
        ?! liftfail (ExtraneousScriptWitnessesUTXOW (sReceived `Set.difference` sNeeded))
    else
      sNeeded == sReceived
        ?! liftfail (MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived))

  -- check VKey witnesses

  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  verifiedWits @era tx ?!#: (liftfail . InvalidWitnessesUTXOW)

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  let needed = (witsVKeyNeeded' delta) utxo tx genDelegs
      missingWitnesses = diffWitHashes needed witsKeyHashes
      haveNeededWitnesses =
        if nullWitHashes missingWitnesses
          then Right ()
          else Left missingWitnesses
  haveNeededWitnesses ?!: (liftfail . MissingVKeyWitnessesUTXOW)

  -- check metadata hash
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  case (getField @"adHash" txbody, auxdata) of
    (SNothing, SNothing) -> pure ()
    (SJust mdh, SNothing) -> failBecauseS $ liftfail (MissingTxMetadata mdh)
    (SNothing, SJust md') ->
      failBecauseS $
        liftfail (MissingTxBodyMetadataHash (hashAuxiliaryData @era md'))
    (SJust mdh, SJust md') -> do
      hashAuxiliaryData @era md' == mdh
        ?!# liftfail (ConflictingMetadataHash mdh (hashAuxiliaryData @era md'))

      -- check metadata value sizes
      when (SoftForks.validMetadata pp) $
        validateAuxiliaryData @era md' ?!# liftfail InvalidMetadata

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  let genDelegates =
        Set.fromList $
          asWitness . genDelegKeyHash
            <$> Map.elems genMapping
      (WitHashes khAsSet) = witsKeyHashes
      genSig = eval (genDelegates ∩ khAsSet)
      mirCerts =
        StrictSeq.forceToStrict
          . Seq.filter isInstantaneousRewards
          . StrictSeq.fromStrict
          $ getField @"certs" txbody
      GenDelegs genMapping = genDelegs

  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  ( not (null mirCerts)
      ==> Set.size genSig >= fromIntegral coreNodeQuorum
    )
    ?! liftfail (MIRInsufficientGenesisSigsUTXOW genSig)

  trans @(Core.EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)
