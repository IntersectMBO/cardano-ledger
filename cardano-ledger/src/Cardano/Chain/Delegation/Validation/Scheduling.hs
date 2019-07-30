{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Delegation.Validation.Scheduling
  (
  -- * Scheduling
    Environment(..)
  , State(..)
  , Error(..)
  , ScheduledDelegation(..)
  , scheduleCertificate
  )
where

import Cardano.Prelude hiding (State)

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Cardano.Binary
  ( Annotated(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common (BlockCount, KeyHash, hashKey)
import Cardano.Chain.Delegation.Certificate (ACertificate)
import qualified Cardano.Chain.Delegation.Certificate as Certificate
import Cardano.Chain.ProtocolConstants (kSlotSecurityParam)
import Cardano.Chain.Slotting
  ( EpochNumber
  , SlotNumber(..)
  , addSlotCount
  )
import Cardano.Crypto (ProtocolMagicId)


--------------------------------------------------------------------------------
-- Scheduling
--------------------------------------------------------------------------------

data Environment = Environment
  { protocolMagic     :: !(Annotated ProtocolMagicId ByteString)
  , allowedDelegators :: !(Set KeyHash)
  , currentEpoch      :: !EpochNumber
  , currentSlot       :: !SlotNumber
  , k                 :: !BlockCount
  } deriving (Eq, Show, Generic, NFData)

data State = State
  { scheduledDelegations :: !(Seq ScheduledDelegation)
  , keyEpochDelegations  :: !(Set (EpochNumber, KeyHash))
  } deriving (Eq, Show, Generic, NFData)

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
  { sdSlot      :: !SlotNumber
  , sdDelegator :: !KeyHash
  , sdDelegate  :: !KeyHash
  } deriving (Eq, Show, Generic, NFData)

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

  = InvalidCertificate
  -- ^ The delegation certificate has an invalid signature

  | MultipleDelegationsForEpoch EpochNumber KeyHash
  -- ^ This delegator has already delegated for the given epoch

  | MultipleDelegationsForSlot SlotNumber KeyHash
  -- ^ This delegator has already delgated in this slot

  | NonGenesisDelegator KeyHash
  -- ^ This delegator is not one of the allowed genesis keys

  | PastEpoch EpochNumber EpochNumber
  -- ^ This delegation is for a past epoch

  deriving (Eq, Show)


-- | Update the delegation 'State' with a 'Certificate' if it passes
--   all the validation rules. This is an implementation of the delegation
--   scheduling inference rule from the ledger specification.
scheduleCertificate
  :: MonadError Error m
  => Environment
  -> State
  -> ACertificate ByteString
  -> m State
scheduleCertificate env st cert = do
  -- Check that the delegator is a genesis key
  delegatorHash `Set.member` allowedDelegators
    `orThrowError` NonGenesisDelegator delegatorHash

  -- Check that the delegation epoch is greater than or equal to the current one
  currentEpoch <= delegationEpoch
    `orThrowError` PastEpoch currentEpoch delegationEpoch

  -- Check that the delegator hasn't already delegated in 'delegationEpoch'
  (delegationEpoch, delegatorHash) `Set.notMember` keyEpochDelegations
    `orThrowError` MultipleDelegationsForEpoch delegationEpoch delegatorHash

  -- Check that the delegator hasn't issued a certificate in this slot
  isNothing (Seq.findIndexL delegatesThisSlot scheduledDelegations)
    `orThrowError` MultipleDelegationsForSlot currentSlot delegatorHash

  -- Check that the delegation certificate is valid
  Certificate.isValid protocolMagic cert `orThrowError` InvalidCertificate

  -- Schedule the new delegation and register the epoch/delegator pair
  pure $ State
    { scheduledDelegations = scheduledDelegations |> delegation
    , keyEpochDelegations  = Set.insert
      (delegationEpoch, delegatorHash)
      keyEpochDelegations
    }
 where
  Environment { protocolMagic, allowedDelegators, currentEpoch, currentSlot, k }
    = env

  State { scheduledDelegations, keyEpochDelegations } = st

  delegatorHash = hashKey $ Certificate.issuerVK cert
  delegateHash = hashKey $ Certificate.delegateVK cert

  delegationEpoch = Certificate.epoch cert

  activationSlot  = addSlotCount (kSlotSecurityParam k) currentSlot

  delegatesThisSlot sd =
    sdSlot sd == activationSlot && sdDelegator sd == delegatorHash

  delegation = ScheduledDelegation activationSlot delegatorHash delegateHash
