{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Delegation.Validation.Scheduling
  ( -- * Scheduling
    Environment (..),
    State (..),
    Error (..),
    ScheduledDelegation (..),
    scheduleCertificate,
  )
where

import Cardano.Binary
  ( Annotated (..),
    Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    enforceSize,
    matchSize,
  )
import Cardano.Chain.Common (BlockCount, KeyHash, hashKey)
import Cardano.Chain.Delegation.Certificate (ACertificate)
import qualified Cardano.Chain.Delegation.Certificate as Certificate
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting
  ( EpochNumber,
    SlotNumber (..),
    addSlotCount,
  )
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Prelude hiding (State)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic :: !(Annotated ProtocolMagicId ByteString),
    allowedDelegators :: !(Set KeyHash),
    currentEpoch :: !EpochNumber,
    currentSlot :: !SlotNumber,
    k :: !BlockCount
  }
  deriving (Eq, Show, Generic, NFData)

data State = State
  { scheduledDelegations :: !(Seq ScheduledDelegation),
    keyEpochDelegations :: !(Set (EpochNumber, KeyHash))
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance FromCBOR State where
  fromCBOR = do
    enforceSize "State" 2
    State
      <$> (Seq.fromList <$> fromCBOR)
      <*> fromCBOR

instance ToCBOR State where
  toCBOR s =
    encodeListLen 2
      <> toCBOR (toList (scheduledDelegations s))
      <> toCBOR (keyEpochDelegations s)

data ScheduledDelegation = ScheduledDelegation
  { sdSlot :: !SlotNumber,
    sdDelegator :: !KeyHash,
    sdDelegate :: !KeyHash
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance FromCBOR ScheduledDelegation where
  fromCBOR = do
    enforceSize "ScheduledDelegation" 3
    ScheduledDelegation
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance ToCBOR ScheduledDelegation where
  toCBOR sd =
    encodeListLen 3
      <> toCBOR (sdSlot sd)
      <> toCBOR (sdDelegator sd)
      <> toCBOR (sdDelegate sd)

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
  toCBOR err = case err of
    InvalidCertificate ->
      encodeListLen 1
        <> toCBOR (0 :: Word8)
    MultipleDelegationsForEpoch epochNumber keyHash ->
      encodeListLen 3
        <> toCBOR (1 :: Word8)
        <> toCBOR epochNumber
        <> toCBOR keyHash
    MultipleDelegationsForSlot slotNumber keyHash ->
      encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> toCBOR slotNumber
        <> toCBOR keyHash
    NonGenesisDelegator keyHash ->
      encodeListLen 2
        <> toCBOR (3 :: Word8)
        <> toCBOR keyHash
    WrongEpoch currentEpoch delegationEpoch ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR currentEpoch
        <> toCBOR delegationEpoch

instance FromCBOR Error where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: Int -> Decoder s ()
        checkSize size = matchSize "Scheduling.Error" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 1 >> pure InvalidCertificate
      1 -> checkSize 3 >> MultipleDelegationsForEpoch <$> fromCBOR <*> fromCBOR
      2 -> checkSize 3 >> MultipleDelegationsForSlot <$> fromCBOR <*> fromCBOR
      3 -> checkSize 2 >> NonGenesisDelegator <$> fromCBOR
      4 -> checkSize 3 >> WrongEpoch <$> fromCBOR <*> fromCBOR
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
  delegatorHash `Set.member` allowedDelegators
    `orThrowError` NonGenesisDelegator delegatorHash

  -- Check that the delegation epoch refers to the current or to the next epoch
  currentEpoch <= delegationEpoch && delegationEpoch <= currentEpoch + 1
    `orThrowError` WrongEpoch currentEpoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  (delegationEpoch, delegatorHash) `Set.notMember` keyEpochDelegations
    `orThrowError` MultipleDelegationsForEpoch delegationEpoch delegatorHash

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot scheduledDelegations)
    `orThrowError` MultipleDelegationsForSlot currentSlot delegatorHash

  -- Check that the delegation certificate is valid
  Certificate.isValid protocolMagic cert `orThrowError` InvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $
    State
      { scheduledDelegations = scheduledDelegations |> delegation,
        keyEpochDelegations =
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
