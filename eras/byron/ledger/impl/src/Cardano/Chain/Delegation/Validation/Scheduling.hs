{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Delegation.Validation.Scheduling (
  -- * Scheduling
  Environment (..),
  State (..),
  Error (..),
  ScheduledDelegation (..),
  scheduleCertificate,
)
where

import Cardano.Chain.Common (BlockCount, KeyHash, hashKey)
import Cardano.Chain.Delegation.Certificate (ACertificate)
import qualified Cardano.Chain.Delegation.Certificate as Certificate
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting (
  EpochNumber,
  SlotNumber (..),
  addSlotCount,
 )
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Ledger.Binary (
  Annotated (..),
  DecCBOR (..),
  Decoder,
  DecoderError (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  cborError,
  decodeListLen,
  decodeWord8,
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  matchSize,
  toByronCBOR,
 )
import Cardano.Prelude hiding (State, cborError)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic :: !(Annotated ProtocolMagicId ByteString)
  , allowedDelegators :: !(Set KeyHash)
  , currentEpoch :: !EpochNumber
  , currentSlot :: !SlotNumber
  , k :: !BlockCount
  }
  deriving (Eq, Show, Generic, NFData)

data State = State
  { scheduledDelegations :: !(Seq ScheduledDelegation)
  , keyEpochDelegations :: !(Set (EpochNumber, KeyHash))
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance ToCBOR State where
  toCBOR = toByronCBOR

instance FromCBOR State where
  fromCBOR = fromByronCBOR

instance DecCBOR State where
  decCBOR = do
    enforceSize "State" 2
    State
      <$> (Seq.fromList <$> decCBOR)
      <*> decCBOR

instance EncCBOR State where
  encCBOR s =
    encodeListLen 2
      <> encCBOR (toList (scheduledDelegations s))
      <> encCBOR (keyEpochDelegations s)

data ScheduledDelegation = ScheduledDelegation
  { sdSlot :: !SlotNumber
  , sdDelegator :: !KeyHash
  , sdDelegate :: !KeyHash
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance ToCBOR ScheduledDelegation where
  toCBOR = toByronCBOR

instance FromCBOR ScheduledDelegation where
  fromCBOR = fromByronCBOR

instance DecCBOR ScheduledDelegation where
  decCBOR = do
    enforceSize "ScheduledDelegation" 3
    ScheduledDelegation
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR

instance EncCBOR ScheduledDelegation where
  encCBOR sd =
    encodeListLen 3
      <> encCBOR (sdSlot sd)
      <> encCBOR (sdDelegator sd)
      <> encCBOR (sdDelegate sd)

data Error
  = -- | The delegation certificate has an invalid signature
    InvalidCertificate
  | -- | This delegator has already delegated for the given epoch
    MultipleDelegationsForEpoch EpochNumber KeyHash
  | -- | This delegator has already delgated in this slot
    MultipleDelegationsForSlot SlotNumber KeyHash
  | -- | This delegator is not one of the allowed genesis keys
    NonGenesisDelegator KeyHash
  | -- | This delegation is for a past or for a too future epoch
    WrongEpoch EpochNumber EpochNumber
  deriving (Eq, Show)

instance ToCBOR Error where
  toCBOR = toByronCBOR

instance FromCBOR Error where
  fromCBOR = fromByronCBOR

instance EncCBOR Error where
  encCBOR err = case err of
    InvalidCertificate ->
      encodeListLen 1
        <> encCBOR (0 :: Word8)
    MultipleDelegationsForEpoch epochNumber keyHash ->
      encodeListLen 3
        <> encCBOR (1 :: Word8)
        <> encCBOR epochNumber
        <> encCBOR keyHash
    MultipleDelegationsForSlot slotNumber keyHash ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR slotNumber
        <> encCBOR keyHash
    NonGenesisDelegator keyHash ->
      encodeListLen 2
        <> encCBOR (3 :: Word8)
        <> encCBOR keyHash
    WrongEpoch currentEpoch delegationEpoch ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR currentEpoch
        <> encCBOR delegationEpoch

instance DecCBOR Error where
  decCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "Scheduling.Error" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 1 >> pure InvalidCertificate
      1 -> checkSize 3 >> MultipleDelegationsForEpoch <$> decCBOR <*> decCBOR
      2 -> checkSize 3 >> MultipleDelegationsForSlot <$> decCBOR <*> decCBOR
      3 -> checkSize 2 >> NonGenesisDelegator <$> decCBOR
      4 -> checkSize 3 >> WrongEpoch <$> decCBOR <*> decCBOR
      _ -> cborError $ DecoderErrorUnknownTag "Scheduling.Error" tag

-- | Update the delegation 'State' with a 'Certificate' if it passes
--   all the validation rules. This is an implementation of the delegation
--   scheduling inference rule from the ledger specification.
scheduleCertificate ::
  MonadError Error m =>
  Environment ->
  State ->
  ACertificate ByteString ->
  m State
scheduleCertificate env st cert = do
  -- Check that the delegator is a genesis key
  delegatorHash
    `Set.member` allowedDelegators
    `orThrowError` NonGenesisDelegator delegatorHash

  -- Check that the delegation epoch refers to the current or to the next epoch
  currentEpoch
    <= delegationEpoch
    && delegationEpoch
    <= currentEpoch
    + 1
    `orThrowError` WrongEpoch currentEpoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  (delegationEpoch, delegatorHash)
    `Set.notMember` keyEpochDelegations
    `orThrowError` MultipleDelegationsForEpoch delegationEpoch delegatorHash

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot scheduledDelegations)
    `orThrowError` MultipleDelegationsForSlot currentSlot delegatorHash

  -- Check that the delegation certificate is valid
  Certificate.isValid protocolMagic cert `orThrowError` InvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $
    State
      { scheduledDelegations = scheduledDelegations |> delegation
      , keyEpochDelegations =
          Set.insert
            (delegationEpoch, delegatorHash)
            keyEpochDelegations
      }
  where
    Environment {protocolMagic, allowedDelegators, currentEpoch, currentSlot, k} =
      env

    State {scheduledDelegations, keyEpochDelegations} = st

    delegatorHash = hashKey $ Certificate.issuerVK cert
    delegateHash = hashKey $ Certificate.delegateVK cert

    delegationEpoch = Certificate.epoch cert

    activationSlot = addSlotCount (kSlotSecurityParam k) currentSlot

    delegatesThisSlot sd =
      sdSlot sd == activationSlot && sdDelegator sd == delegatorHash

    delegation = ScheduledDelegation activationSlot delegatorHash delegateHash
