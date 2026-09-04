{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The Leios voting committee: the pools entitled to vote on endorser blocks
-- for one epoch (CIP-0164). The committee type itself is cardano-base's
-- 'LeiosCommittee'; this module only re-exports it, gives it an empty value,
-- and teaches it to serialize as part of the ledger state. Selection lives in
-- "Cardano.Ledger.State.SnapShots", next to the snapshot it is derived from.
module Cardano.Ledger.State.LeiosCommittee (
  LeiosCommittee (..),
  LeiosSeat (..),
  emptyLeiosCommittee,
) where

import Cardano.Crypto.Leios (LeiosCommittee (..), LeiosSeat (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeFixedSized,
  decodeStrictMaybe,
  encodeFixedSized,
  encodeStrictMaybe,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardanoimport Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.Ledger.State.StakePool (BlsKey (..))
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Vector.Strict as V

-- | The committee with no seats: the value for eras before Leios and for a
-- committee size of zero.
emptyLeiosCommittee :: LeiosCommittee
emptyLeiosCommittee = UnsafeLeiosCommittee V.empty

-- Orphans: the committee is part of the ledger state, but its type belongs to
-- cardano-base, which has no reason to know how we serialize it.

instance EncCBOR LeiosSeat where
  encCBOR (LeiosSeat weight vkey) =
    encode $
      Rec LeiosSeat
        !> To weight
        !> E (encodeStrictMaybe encodeFixedSized) vkey

instance DecCBOR LeiosSeat where
  decCBOR =
    decode $
      RecD LeiosSeat
        <! From
        <! D (decodeStrictMaybe decodeFixedSized)

-- | Decoding goes straight to the constructor rather than through
-- 'mkLeiosCommittee': that takes proofs of possession, which a seated committee
-- no longer carries — they were verified when it was selected.
instance EncCBOR LeiosCommittee where
  encCBOR = encCBOR . V.toList . leiosCommitteeSeats

instance DecCBOR LeiosCommittee where
  decCBOR = UnsafeLeiosCommittee . V.fromList <$> decCBOR

instance ToJSON LeiosSeat where
  toJSON (LeiosSeat weight vkey) =
    object
      [ "seatWeight" .= weight
      , "seatVKey" .= case vkey of
          SNothing -> Nothing
          SJust vk -> Just (show vk)
      ]

instance ToJSON LeiosCommittee where
  toJSON = toJSON . V.toList . leiosCommitteeSeats
