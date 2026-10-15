{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Leios (CIP-0164) parts of the ledger state.
--
-- The voting committee: the pools entitled to vote on endorser blocks for one
-- epoch. The committee type itself is cardano-base's 'LeiosCommittee'; this
-- module re-exports it, gives it an empty value, teaches it to serialize as part
-- of the ledger state, and selects it from the per-epoch stake standing of the
-- pools ('LeiosCandidate').
--
-- The last endorser block announcement made by a block header ('EbAnnouncement').
module Cardano.Ledger.State.Leios (
  LeiosCommittee (..),
  LeiosSeat (..),
  emptyLeiosCommittee,
  LeiosCandidate (..),
  selectLeiosCommittee,
  leiosCommitteeToJSON,
  EbAnnouncement (..),
) where

import Cardano.Crypto.Leios (LeiosCommittee (..), LeiosSeat (..), Weight, mkLeiosCommittee)
import Cardano.Ledger.BaseTypes (
  EpochInterval,
  KeyValuePairs (..),
  StrictMaybe (..),
  ToKeyValuePairs (..),
  addEpochInterval,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Block (EbReferencesAnnouncement)
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Hashes (EraIndependentBlockHeader, SafeHash)
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.Ledger.Slot (EpochNo, SlotNo)
import Cardano.Ledger.State.StakePool (BlsKey (..), BlsKeyState (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), Value, object, toJSON, (.=))
import Data.Function ((&))
import Data.Ord (Down (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Strict as VS
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

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

    size = min (fromIntegral @Word16 @Int committeeSize) (V.length candidates)

    higherStake a b =
      compare (Down (lcStake a), lcPoolId a) (Down (lcStake b), lcPoolId b)

    toSeat c = (honouredKey c, lcWeight c)

    -- The key is offered to the committee only while it is still honoured; an
    -- aged-out key leaves the pool seated but keyless.
    honouredKey c = do
      bks <- lcKey c
      if epochNo < addEpochInterval (bksRegisteredIn bks) maxKeyAge
        then let BlsKey vk pop = bksKey bks in SJust (vk, pop)
        else SNothing

-- | Render a 'LeiosCommittee' as JSON for ledger purposes.
leiosCommitteeToJSON :: LeiosCommittee -> Value
leiosCommitteeToJSON =
  toJSON . map leiosSeatToJSON . VS.toList . leiosCommitteeSeats
  where
    leiosSeatToJSON (LeiosSeat weight vkey) =
      object
        [ "seatWeight" .= weight
        , "seatVKey" .= show (strictMaybeToMaybe vkey)
        ]

-- | Endorser Block (EB) references announcement together with the block header that made it.
data EbAnnouncement = EbAnnouncement
  { ebAnnouncement :: !EbReferencesAnnouncement
  -- ^ Announcement of the Endorser Block references
  , ebAnnouncementSlotNo :: !SlotNo
  -- ^ Slot number of the block whose header made the announcement
  , ebAnnouncedInBlockHeader :: !(SafeHash EraIndependentBlockHeader)
  -- ^ Hash of the block header that made the announcement
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks, NFData)
  deriving (ToJSON) via KeyValuePairs EbAnnouncement

instance EncCBOR EbAnnouncement where
  encCBOR (EbAnnouncement a s h) =
    encodeListLen 3
      <> encCBOR a
      <> encCBOR s
      <> encCBOR h

instance DecCBOR EbAnnouncement where
  decCBOR =
    decodeRecordNamed "EbAnnouncement" (const 3) $
      EbAnnouncement
        <$> decCBOR
        <*> decCBOR
        <*> decCBOR

instance ToKeyValuePairs EbAnnouncement where
  toKeyValuePairs (EbAnnouncement a s h) =
    [ "announcement" .= a
    , "slotNo" .= s
    , "announcedInBlockHeader" .= h
    ]
