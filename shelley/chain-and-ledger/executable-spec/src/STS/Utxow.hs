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

module STS.Utxow
  ( UTXOW
  , PredicateFailure(..)
  )
where

import           BaseTypes (Globals, ShelleyBase, intervalValue, invalidKey, quorum, (==>))
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..), asks)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace (..), envGen, sigGen)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           Delegation.Certificates (isInstantaneousRewards)
import           GHC.Generics (Generic)
import           Hedgehog (Gen)
import           Keys
import           Ledger.Core (dom, (∩))
import           LedgerState (UTxOState (..), verifiedWits, witsVKeyNeeded)
import           MetaData (hashMetaData)
import           PParams (_d)
import           STS.Utxo
import           Tx
import           TxData
import           UTxO
import           Validation (Validity (..))

data UTXOW crypto

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => STS (UTXOW crypto)
 where
  type State (UTXOW crypto) = UTxOState crypto
  type Signal (UTXOW crypto) = Tx crypto
  type Environment (UTXOW crypto) = UtxoEnv crypto
  type BaseM (UTXOW crypto) = ShelleyBase
  data PredicateFailure (UTXOW crypto)
    = InvalidWitnessesUTXOW
    | MissingVKeyWitnessesUTXOW
    | MissingScriptWitnessesUTXOW
    | ScriptWitnessNotValidatingUTXOW
    | UtxoFailure (PredicateFailure (UTXO crypto))
    | MIRInsufficientGenesisSigsUTXOW
    | MIRImpossibleInDecentralizedNetUTXOW
    | BadMetaDataHashUTXOW
    deriving (Eq, Generic, Show)

  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

instance NoUnexpectedThunks (PredicateFailure (UTXOW crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (UTXOW crypto))
 where
   toCBOR = \case
      InvalidWitnessesUTXOW                -> encodeListLen 1 <> toCBOR (0 :: Word8)
      MissingVKeyWitnessesUTXOW            -> encodeListLen 1 <> toCBOR (1 :: Word8)
      MissingScriptWitnessesUTXOW          -> encodeListLen 1 <> toCBOR (2 :: Word8)
      ScriptWitnessNotValidatingUTXOW      -> encodeListLen 1 <> toCBOR (3 :: Word8)
      (UtxoFailure a)                      -> encodeListLen 2 <> toCBOR (4 :: Word8)
                                                <> toCBOR a
      MIRInsufficientGenesisSigsUTXOW      -> encodeListLen 1 <> toCBOR (5 :: Word8)
      MIRImpossibleInDecentralizedNetUTXOW -> encodeListLen 1 <> toCBOR (6 :: Word8)
      BadMetaDataHashUTXOW                 -> encodeListLen 1 <> toCBOR (7 :: Word8)

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (UTXOW crypto))
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "InvalidWitnessesUTXOW" 1 n >> pure InvalidWitnessesUTXOW
      1 -> matchSize "MissingVKeyWitnessesUTXOW" 1 n >> pure MissingVKeyWitnessesUTXOW
      2 -> matchSize "MissingScriptWitnessesUTXOW" 1 n >> pure MissingScriptWitnessesUTXOW
      3 -> matchSize "ScriptWitnessNotValidatingUTXOW" 1 n >>
             pure ScriptWitnessNotValidatingUTXOW
      4 -> do
        matchSize "UtxoFailure" 2 n
        a <- fromCBOR
        pure $ UtxoFailure a
      5 -> matchSize "MIRInsufficientGenesisSigsUTXOW" 1 n >>
             pure MIRInsufficientGenesisSigsUTXOW
      6 -> matchSize "MIRImpossibleInDecentralizedNetUTXOW" 1 n >>
             pure MIRImpossibleInDecentralizedNetUTXOW
      7 -> matchSize "BadMetaDataHashUTXOW" 1 n >>
             pure BadMetaDataHashUTXOW
      k -> invalidKey k

initialLedgerStateUTXOW
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
   => InitialRule (UTXOW crypto)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakeCreds stakepools genDelegs) <- judgmentContext
  trans @(UTXO crypto) $ IRC (UtxoEnv slots pp stakeCreds stakepools genDelegs)

utxoWitnessed
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
   => TransitionRule (UTXOW crypto)
utxoWitnessed = do
  TRC (UtxoEnv slot pp stakeCreds stakepools genDelegs, u, tx@(Tx txbody wits _ md))
    <- judgmentContext

  let utxo = _utxo u
  let witsKeyHashes = Set.map witKeyHash wits

  -- check multi-signature scripts
  all (\(hs, validator) -> hashScript validator == hs
      && validateScript validator tx) (Map.toList $ txwitsScript tx)
    ?! ScriptWitnessNotValidatingUTXOW

  scriptsNeeded utxo tx == Map.keysSet (txwitsScript tx)
    ?! MissingScriptWitnessesUTXOW

  -- check VKey witnesses
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW

  let needed = witsVKeyNeeded utxo tx genDelegs
  needed `Set.isSubsetOf` witsKeyHashes  ?! MissingVKeyWitnessesUTXOW

  -- check metadata hash
  case (_mdHash txbody) of
    Nothing  -> md == Nothing ?! BadMetaDataHashUTXOW
    Just mdh -> case md of
                  Nothing  -> failBecause BadMetaDataHashUTXOW
                  Just md' -> hashMetaData md' == mdh ?! BadMetaDataHashUTXOW

  -- check genesis keys signatures for instantaneous rewards certificates
  let genSig = (Set.map undiscriminateKeyHash $ dom genMapping) ∩ Set.map witKeyHash wits
      mirCerts = Seq.filter isInstantaneousRewards $ _certs txbody
      GenDelegs genMapping = genDelegs

  coreNodeQuorum <- liftSTS $ asks quorum
  (    (not $ null mirCerts)
   ==> Set.size genSig >= fromIntegral coreNodeQuorum)
      ?! MIRInsufficientGenesisSigsUTXOW
  (    (not $ null mirCerts)
   ==> (0 < intervalValue (_d pp)))
    ?! MIRImpossibleInDecentralizedNetUTXOW

  trans @(UTXO crypto)
    $ TRC (UtxoEnv slot pp stakeCreds stakepools genDelegs, u, tx)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (UTXO crypto) (UTXOW crypto)
 where
  wrapFailed = UtxoFailure

instance
    ( Crypto crypto
    , Signable (DSIGN crypto) (TxBody crypto)
    )
  => HasTrace (UTXOW crypto) where
  envGen _ = undefined :: Gen (UtxoEnv crypto)
  sigGen _ _ = undefined :: Gen (Tx crypto)

  type BaseEnv (UTXOW crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
