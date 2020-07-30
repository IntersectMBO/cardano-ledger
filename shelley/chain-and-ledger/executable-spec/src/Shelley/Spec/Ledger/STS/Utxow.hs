{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Utxow
  ( UTXOW,
    PredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Prelude (NoUnexpectedThunks (..), asks)
import Control.Iterate.SetAlgebra (eval, (∩))
import Control.State.Transition
  ( Embed,
    IRC (..),
    InitialRule,
    STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    liftSTS,
    trans,
    wrapFailed,
    (?!),
    (?!:),
  )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address (BootstrapVerKey)
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    StrictMaybe (..),
    invalidKey,
    quorum,
    (==>),
  )
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.Certificates (isInstantaneousRewards)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VKey,
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( UTxOState (..),
    WitHashes (..),
    diffWitHashes,
    nullWitHashes,
    verifiedWits,
    witsFromWitnessSet,
    witsVKeyNeeded,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash, hashMetaData)
import Shelley.Spec.Ledger.STS.Utxo (UTXO, UtxoEnv (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (decodeList, decodeRecordSum, decodeSet, encodeFoldable)
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    hashScript,
    txwitsScript,
    validateScript,
  )
import Shelley.Spec.Ledger.TxData (TxBody (..))
import Shelley.Spec.Ledger.UTxO (scriptsNeeded)

data UTXOW crypto

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  STS (UTXOW crypto)
  where
  type State (UTXOW crypto) = UTxOState crypto
  type Signal (UTXOW crypto) = Tx crypto
  type Environment (UTXOW crypto) = UtxoEnv crypto
  type BaseM (UTXOW crypto) = ShelleyBase
  data PredicateFailure (UTXOW crypto)
    = InvalidWitnessesUTXOW
        ![VKey 'Witness crypto]
        ![BootstrapVerKey]
    | -- witnesses which failed in verifiedWits function
      MissingVKeyWitnessesUTXOW
        !(WitHashes crypto) -- witnesses which were needed and not supplied
    | MissingScriptWitnessesUTXOW
        !(Set (ScriptHash crypto)) -- missing scripts
    | ScriptWitnessNotValidatingUTXOW
        !(Set (ScriptHash crypto)) -- failed scripts
    | UtxoFailure (PredicateFailure (UTXO crypto))
    | MIRInsufficientGenesisSigsUTXOW (Set (KeyHash 'Witness crypto))
    | MissingTxBodyMetaDataHash
        !(MetaDataHash crypto) -- hash of the full metadata
    | MissingTxMetaData
        !(MetaDataHash crypto) -- hash of the metadata included in the transaction body
    | ConflictingMetaDataHash
        !(MetaDataHash crypto) -- hash of the metadata included in the transaction body
        !(MetaDataHash crypto) -- hash of the full metadata
    deriving (Eq, Generic, Show)

  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (UTXOW crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (UTXOW crypto))
  where
  toCBOR = \case
    InvalidWitnessesUTXOW wits bootstrapWits ->
      encodeListLen 3 <> toCBOR (0 :: Word8) <> encodeFoldable wits <> encodeFoldable bootstrapWits
    MissingVKeyWitnessesUTXOW (WitHashes missing bootstrapMissing) ->
      encodeListLen 3 <> toCBOR (1 :: Word8) <> encodeFoldable missing <> encodeFoldable bootstrapMissing
    MissingScriptWitnessesUTXOW ss -> encodeListLen 2 <> toCBOR (2 :: Word8) <> encodeFoldable ss
    ScriptWitnessNotValidatingUTXOW ss -> encodeListLen 2 <> toCBOR (3 :: Word8) <> encodeFoldable ss
    (UtxoFailure a) ->
      encodeListLen 2 <> toCBOR (4 :: Word8)
        <> toCBOR a
    MIRInsufficientGenesisSigsUTXOW sigs -> encodeListLen 2 <> toCBOR (5 :: Word8) <> encodeFoldable sigs
    MissingTxBodyMetaDataHash h ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR h
    MissingTxMetaData h ->
      encodeListLen 2 <> toCBOR (7 :: Word8) <> toCBOR h
    ConflictingMetaDataHash bodyHash fullMDHash ->
      encodeListLen 3 <> toCBOR (8 :: Word8) <> toCBOR bodyHash <> toCBOR fullMDHash

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (UTXOW crypto))
  where
  fromCBOR = decodeRecordSum "PredicateFailure (UTXOW crypto)" $
    \case
      0 -> do
        wits <- decodeList fromCBOR
        bootstrapWits <- decodeList fromCBOR
        pure (3, InvalidWitnessesUTXOW wits bootstrapWits)
      1 -> do
        missing <- decodeSet fromCBOR
        bootstrapMissing <- decodeSet fromCBOR
        pure (3, MissingVKeyWitnessesUTXOW $ WitHashes missing bootstrapMissing)
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
        pure (2, MissingTxBodyMetaDataHash h)
      7 -> do
        h <- fromCBOR
        pure (2, MissingTxMetaData h)
      8 -> do
        bodyHash <- fromCBOR
        fullMDHash <- fromCBOR
        pure (3, ConflictingMetaDataHash bodyHash fullMDHash)
      k -> invalidKey k

initialLedgerStateUTXOW ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  InitialRule (UTXOW crypto)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakepools genDelegs) <- judgmentContext
  trans @(UTXO crypto) $ IRC (UtxoEnv slots pp stakepools genDelegs)

utxoWitnessed ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  TransitionRule (UTXOW crypto)
utxoWitnessed =
  judgmentContext
    >>= \(TRC (UtxoEnv slot pp stakepools genDelegs, u, tx@(Tx txbody wits md))) -> do
      let utxo = _utxo u
      let witsKeyHashes = witsFromWitnessSet wits

      -- check multi-signature scripts
      let failedScripts =
            filter
              (\(hs, validator) -> hashScript validator /= hs || not (validateScript validator tx))
              (Map.toList $ txwitsScript tx)
      case failedScripts of
        [] -> pure ()
        fs -> failBecause $ ScriptWitnessNotValidatingUTXOW $ Set.fromList $ fmap fst fs

      let sNeeded = scriptsNeeded utxo tx
          sReceived = Map.keysSet (txwitsScript tx)
      sNeeded == sReceived ?! MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived)

      -- check VKey witnesses
      verifiedWits tx ?!: uncurry InvalidWitnessesUTXOW

      let needed = witsVKeyNeeded utxo tx genDelegs
          missingWitnesses = diffWitHashes needed witsKeyHashes
          haveNeededWitnesses = case nullWitHashes missingWitnesses of
            True -> Right ()
            False -> Left missingWitnesses
      haveNeededWitnesses ?!: MissingVKeyWitnessesUTXOW

      -- check metadata hash
      case (_mdHash txbody, md) of
        (SNothing, SNothing) -> pure ()
        (SJust mdh, SNothing) -> failBecause $ MissingTxMetaData mdh
        (SNothing, SJust md') -> failBecause $ MissingTxBodyMetaDataHash (hashMetaData md')
        (SJust mdh, SJust md') ->
          hashMetaData md' == mdh ?! ConflictingMetaDataHash mdh (hashMetaData md')

      -- check genesis keys signatures for instantaneous rewards certificates
      let genDelegates = Set.fromList $ fmap (asWitness . genDelegKeyHash) $ Map.elems genMapping
          WitHashes {properWitHashes = khAsSet} = witsKeyHashes
          genSig = eval (genDelegates ∩ khAsSet)
          mirCerts =
            StrictSeq.toStrict
              . Seq.filter isInstantaneousRewards
              . StrictSeq.getSeq
              $ _certs txbody
          GenDelegs genMapping = genDelegs

      coreNodeQuorum <- liftSTS $ asks quorum
      ( (not $ null mirCerts)
          ==> Set.size genSig >= fromIntegral coreNodeQuorum
        )
        ?! MIRInsufficientGenesisSigsUTXOW genSig

      trans @(UTXO crypto) $
        TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (UTXO crypto) (UTXOW crypto)
  where
  wrapFailed = UtxoFailure
