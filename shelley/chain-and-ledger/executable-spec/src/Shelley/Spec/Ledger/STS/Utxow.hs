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

import Byron.Spec.Ledger.Core (dom, (∩))
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord,
    encodeListLen,
    matchSize,
  )
import Cardano.Prelude (NoUnexpectedThunks (..), asks)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
  ( (==>),
    ShelleyBase,
    StrictMaybe (..),
    intervalValue,
    invalidKey,
    quorum,
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates (isInstantaneousRewards)
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState (UTxOState (..), verifiedWits, witsVKeyNeeded)
import Shelley.Spec.Ledger.MetaData (hashMetaData)
import Shelley.Spec.Ledger.PParams (_d)
import Shelley.Spec.Ledger.STS.Utxo
import Shelley.Spec.Ledger.Serialization (decodeList, decodeSet, encodeFoldable)
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxData
import Shelley.Spec.Ledger.UTxO

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
    = InvalidWitnessesUTXOW [VKey 'Witness crypto]
    | MissingVKeyWitnessesUTXOW (Set (KeyHash 'Witness crypto))
    | MissingScriptWitnessesUTXOW
    | ScriptWitnessNotValidatingUTXOW
    | UtxoFailure (PredicateFailure (UTXO crypto))
    | MIRInsufficientGenesisSigsUTXOW (Set (KeyHash 'Witness crypto))
    | MIRImpossibleInDecentralizedNetUTXOW
    | BadMetaDataHashUTXOW
    deriving (Eq, Generic, Show)

  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (UTXOW crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (UTXOW crypto))
  where
  toCBOR = \case
    InvalidWitnessesUTXOW wits ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeFoldable wits
    MissingVKeyWitnessesUTXOW missing ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> encodeFoldable missing
    MissingScriptWitnessesUTXOW -> encodeListLen 1 <> toCBOR (2 :: Word8)
    ScriptWitnessNotValidatingUTXOW -> encodeListLen 1 <> toCBOR (3 :: Word8)
    (UtxoFailure a) ->
      encodeListLen 2 <> toCBOR (4 :: Word8)
        <> toCBOR a
    MIRInsufficientGenesisSigsUTXOW sigs -> encodeListLen 2 <> toCBOR (5 :: Word8) <> encodeFoldable sigs
    MIRImpossibleInDecentralizedNetUTXOW -> encodeListLen 1 <> toCBOR (6 :: Word8)
    BadMetaDataHashUTXOW -> encodeListLen 1 <> toCBOR (7 :: Word8)

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (UTXOW crypto))
  where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "InvalidWitnessesUTXOW" 2 n
        wits <- decodeList fromCBOR
        pure $ InvalidWitnessesUTXOW wits
      1 -> do
        matchSize "MissingVKeyWitnessesUTXOW" 2 n
        missing <- decodeSet fromCBOR
        pure $ MissingVKeyWitnessesUTXOW missing
      2 -> matchSize "MissingScriptWitnessesUTXOW" 1 n >> pure MissingScriptWitnessesUTXOW
      3 ->
        matchSize "ScriptWitnessNotValidatingUTXOW" 1 n
          >> pure ScriptWitnessNotValidatingUTXOW
      4 -> do
        matchSize "UtxoFailure" 2 n
        a <- fromCBOR
        pure $ UtxoFailure a
      5 -> do
        matchSize "MIRInsufficientGenesisSigsUTXOW" 2 n
        s <- fromCBOR
        pure $ MIRInsufficientGenesisSigsUTXOW s
      6 ->
        matchSize "MIRImpossibleInDecentralizedNetUTXOW" 1 n
          >> pure MIRImpossibleInDecentralizedNetUTXOW
      7 ->
        matchSize "BadMetaDataHashUTXOW" 1 n
          >> pure BadMetaDataHashUTXOW
      k -> invalidKey k

initialLedgerStateUTXOW ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  InitialRule (UTXOW crypto)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakeCreds stakepools genDelegs) <- judgmentContext
  trans @(UTXO crypto) $ IRC (UtxoEnv slots pp stakeCreds stakepools genDelegs)

utxoWitnessed ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  TransitionRule (UTXOW crypto)
utxoWitnessed =
  judgmentContext
    >>= \(TRC (UtxoEnv slot pp stakeCreds stakepools genDelegs, u, tx@(Tx txbody wits _ md))) -> do
      let utxo = _utxo u
      let witsKeyHashes = Set.map witKeyHash wits

      -- check multi-signature scripts
      all
        ( \(hs, validator) ->
            hashScript validator == hs
              && validateScript validator tx
        )
        (Map.toList $ txwitsScript tx)
        ?! ScriptWitnessNotValidatingUTXOW

      scriptsNeeded utxo tx == Map.keysSet (txwitsScript tx)
        ?! MissingScriptWitnessesUTXOW

      -- check VKey witnesses
      verifiedWits tx ?!: InvalidWitnessesUTXOW

      let needed = witsVKeyNeeded utxo tx genDelegs
          missingWitnesses = needed `Set.difference` witsKeyHashes
          haveNeededWitnesses = case missingWitnesses == Set.empty of
            True -> Right ()
            False -> Left missingWitnesses
      haveNeededWitnesses ?!: MissingVKeyWitnessesUTXOW

      -- check metadata hash
      case (_mdHash txbody) of
        SNothing -> md == SNothing ?! BadMetaDataHashUTXOW
        SJust mdh -> case md of
          SNothing -> failBecause BadMetaDataHashUTXOW
          SJust md' -> hashMetaData md' == mdh ?! BadMetaDataHashUTXOW

      -- check genesis keys signatures for instantaneous rewards certificates
      let genSig = (Set.map asWitness $ dom genMapping) ∩ Set.map witKeyHash wits
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
      ( (not $ null mirCerts)
          ==> (0 < intervalValue (_d pp))
        )
        ?! MIRImpossibleInDecentralizedNetUTXOW

      trans @(UTXO crypto) $
        TRC (UtxoEnv slot pp stakeCreds stakepools genDelegs, u, tx)

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (UTXO crypto) (UTXOW crypto)
  where
  wrapFailed = UtxoFailure
