{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The Leios voting committee: the pools entitled to vote on endorser blocks
-- for one epoch (CIP-0164). The committee type itself is cardano-base's
-- 'LeiosCommittee'; this module re-exports it, gives it an empty value, teaches
-- it to serialize as part of the ledger state, and selects it from the
-- per-epoch stake standing of the pools ('LeiosCandidate').
module Cardano.Ledger.State.LeiosCommittee (
  LeiosCommittee (..),
  LeiosSeat (..),
  emptyLeiosCommittee,
  LeiosCandidate (..),
  selectLeiosCommittee,
) where

import Cardano.Crypto.Leios (LeiosCommittee (..), LeiosSeat (..), Weight, mkLeiosCommittee)
import Cardano.Ledger.BaseTypes (EpochInterval, StrictMaybe (..), addEpochInterval)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeFixedSized,
  decodeStrictMaybe,
  encodeFixedSized,
  encodeStrictMaybe,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Ledger.State.StakePool (BlsKey (..), BlsKeyState (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Function ((&))
import Data.Ord (Down (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Strict as VS
import Data.Word (Word16)

-- | The committee with no seats: the value for eras before Leios and for a
-- committee size of zero.
emptyLeiosCommittee :: LeiosCommittee
emptyLeiosCommittee = UnsafeLeiosCommittee VS.empty

-- | A stake pool standing for committee selection. Keeps 'selectLeiosCommittee'
-- independent of the snapshot the candidates are projected from.
data LeiosCandidate = LeiosCandidate
  { lcPoolId :: !(KeyHash StakePool)
  -- ^ Tie-breaker: pools of equal stake are seated by ascending id.
  , lcStake :: !(CompactForm Coin)
  -- ^ Ranking key: pools are seated in descending stake.
  , lcWeight :: !Weight
  -- ^ The seat's voting weight: the pool's share of the active stake.
  , lcKey :: !(StrictMaybe BlsKeyState)
  -- ^ The registered voting key with its registration epoch, if the pool has one.
  }
  deriving (Show, Eq)

-- | Seat the @committeeSize@ pools with the most stake, largest first, ties
-- broken by ascending pool id. A pool with no registered key, one whose key has
-- aged out (CIP-0164: honoured for @maxKeyAge@ epochs after its registration,
-- judged against the epoch this committee is selected for), or one whose proof
-- of possession does not verify, is seated keyless. A size of zero yields the
-- empty committee without inspecting the candidates, so pre-Dijkstra snapshots
-- carry it for free even when forced.
selectLeiosCommittee ::
  EpochNo -> EpochInterval -> Word16 -> Vector LeiosCandidate -> LeiosCommittee
selectLeiosCommittee _ _ 0 _ = emptyLeiosCommittee
selectLeiosCommittee epochNo maxKeyAge committeeSize candidates =
  candidates
    & sortByStake
    & V.take size
    & V.map toSeat
    & V.convert
    & mkLeiosCommittee
  where
    -- Only the top @size@ need to be in order, so partial-sort them in place
    -- and leave the rest untouched instead of ordering the whole vector.
    sortByStake = V.modify (\mv -> Intro.partialSortBy higherStake mv size)

    size = min (fromIntegral committeeSize) (V.length candidates)

    higherStake a b =
      compare (Down (lcStake a), lcPoolId a) (Down (lcStake b), lcPoolId b)

    toSeat c = (toTuple <$> honoured c, lcWeight c)

    -- The key is offered to the committee only while it is still honoured; an
    -- aged-out key leaves the pool seated but keyless.
    honoured c = do
      bks <- lcKey c
      if epochNo < addEpochInterval (bksRegisteredIn bks) maxKeyAge
        then SJust (bksKey bks)
        else SNothing

    toTuple (BlsKey vk pop) = (vk, pop)

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
  encCBOR = encCBOR . VS.toList . leiosCommitteeSeats

instance DecCBOR LeiosCommittee where
  decCBOR = UnsafeLeiosCommittee . VS.fromList <$> decCBOR

instance ToJSON LeiosSeat where
  toJSON (LeiosSeat weight vkey) =
    object
      [ "seatWeight" .= weight
      , "seatVKey" .= case vkey of
          SNothing -> Nothing
          SJust vk -> Just (show vk)
      ]

instance ToJSON LeiosCommittee where
  toJSON = toJSON . VS.toList . leiosCommitteeSeats
