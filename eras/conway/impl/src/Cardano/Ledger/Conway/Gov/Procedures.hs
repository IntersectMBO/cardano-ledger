{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Gov.Procedures (
  GovProcedures (..),
  VotingProcedures (..),
  VotingProcedure (..),
  ProposalProcedure (..),
  Anchor (..),
  AnchorDataHash,
  Vote (..),
  Voter (..),
  Committee (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  govActionIdToText,
  indexedGovProps,
  -- Lenses
  pProcDepositL,
) where

import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Address (RewardAcnt)
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  AnchorDataHash,
  ProtVer,
  UnitInterval,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeEnumBounded,
  decodeMapByKey,
  decodeNullStrictMaybe,
  encodeEnum,
  encodeListLen,
  encodeNullStrictMaybe,
  encodeWord8,
  invalidKey,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  decodeRecordSum,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), EraPParams, PParamsUpdate)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.Shelley.Gov (Constitution)
import Cardano.Ledger.Shelley.RewardProvenance ()
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Slotting.Slot (EpochNo)
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.Aeson (KeyValue (..), ToJSON (..), ToJSONKey (..), object, pairs)
import Data.Aeson.Types (toJSONKeyText)
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq (..))
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

newtype GovActionIx = GovActionIx Word32
  deriving
    ( Generic
    , Eq
    , Ord
    , Show
    , NFData
    , NoThunks
    , EncCBOR
    , DecCBOR
    , ToJSON
    )

data GovActionId c = GovActionId
  { gaidTxId :: !(TxId c)
  , gaidGovActionIx :: !GovActionIx
  }
  deriving (Generic, Eq, Ord, Show)

instance Crypto c => DecCBOR (GovActionId c) where
  decCBOR =
    decode $
      RecD GovActionId
        <! From
        <! From

instance Crypto c => EncCBOR (GovActionId c) where
  encCBOR GovActionId {..} =
    encode $
      Rec GovActionId
        !> To gaidTxId
        !> To gaidGovActionIx

instance NoThunks (GovActionId c)

instance Crypto c => NFData (GovActionId c)

instance Crypto c => ToJSON (GovActionId c) where
  toJSON = object . toGovActionIdPairs
  toEncoding = pairs . mconcat . toGovActionIdPairs

toGovActionIdPairs :: (KeyValue a, Crypto c) => GovActionId c -> [a]
toGovActionIdPairs gaid@(GovActionId _ _) =
  let GovActionId {..} = gaid
   in [ "txId" .= gaidTxId
      , "govActionIx" .= gaidGovActionIx
      ]

instance Crypto c => ToJSONKey (GovActionId c) where
  toJSONKey = toJSONKeyText govActionIdToText

govActionIdToText :: GovActionId c -> Text.Text
govActionIdToText (GovActionId (TxId txidHash) (GovActionIx ix)) =
  hashToTextAsHex (extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

data Voter c
  = CommitteeVoter !(Credential 'HotCommitteeRole c)
  | DRepVoter !(Credential 'DRepRole c)
  | StakePoolVoter !(KeyHash 'StakePool c)
  deriving (Generic, Eq, Ord, Show)

instance Crypto c => ToJSON (Voter c)

-- TODO: Make it nicer, eg. "DRep-ScriptHash<0xdeadbeef>..."
-- Will also need a ToJSONKey instance for Credential.
instance Crypto c => ToJSONKey (Voter c)

instance Crypto c => DecCBOR (Voter c) where
  decCBOR = decodeRecordSum "Voter" $ \case
    0 -> (2,) . CommitteeVoter . KeyHashObj <$> decCBOR
    1 -> (2,) . CommitteeVoter . ScriptHashObj <$> decCBOR
    2 -> (2,) . DRepVoter . KeyHashObj <$> decCBOR
    3 -> (2,) . DRepVoter . ScriptHashObj <$> decCBOR
    4 -> (2,) . StakePoolVoter <$> decCBOR
    5 -> fail "Script objects are not allowed for StakePool votes"
    t -> invalidKey t

instance Crypto c => EncCBOR (Voter c) where
  encCBOR = \case
    CommitteeVoter (KeyHashObj keyHash) ->
      encodeListLen 2 <> encodeWord8 0 <> encCBOR keyHash
    CommitteeVoter (ScriptHashObj scriptHash) ->
      encodeListLen 2 <> encodeWord8 1 <> encCBOR scriptHash
    DRepVoter (KeyHashObj keyHash) ->
      encodeListLen 2 <> encodeWord8 2 <> encCBOR keyHash
    DRepVoter (ScriptHashObj scriptHash) ->
      encodeListLen 2 <> encodeWord8 3 <> encCBOR scriptHash
    StakePoolVoter keyHash ->
      encodeListLen 2 <> encodeWord8 4 <> encCBOR keyHash

instance NoThunks (Voter c)

instance NFData (Voter c)

data Vote
  = VoteNo
  | VoteYes
  | Abstain
  deriving (Generic, Eq, Show, Enum, Bounded)

instance ToJSON Vote

instance NoThunks Vote

instance NFData Vote

instance DecCBOR Vote where
  decCBOR = decodeEnumBounded

instance EncCBOR Vote where
  encCBOR = encodeEnum

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures ::
      Map (Voter (EraCrypto era)) (Map (GovActionId (EraCrypto era)) (VotingProcedure era))
  }
  deriving stock (Generic, Eq, Show)
  deriving newtype (NoThunks, EncCBOR, ToJSON)

deriving newtype instance Era era => NFData (VotingProcedures era)

instance Era era => DecCBOR (VotingProcedures era) where
  decCBOR =
    fmap VotingProcedures $ decodeMapByKey decCBOR $ \voter -> do
      subMap <- decCBOR
      when (null subMap) $
        fail $
          "VotingProcedures require votes, but Voter: " <> show voter <> " didn't have any"
      pure subMap
  {-# INLINE decCBOR #-}

data VotingProcedure era = VotingProcedure
  { vProcVote :: !Vote
  , vProcAnchor :: !(StrictMaybe (Anchor (EraCrypto era)))
  }
  deriving (Generic, Eq, Show)

instance NoThunks (VotingProcedure era)

instance Crypto (EraCrypto era) => NFData (VotingProcedure era)

instance Era era => DecCBOR (VotingProcedure era) where
  decCBOR =
    decode $
      RecD VotingProcedure
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

instance Era era => EncCBOR (VotingProcedure era) where
  encCBOR VotingProcedure {..} =
    encode $
      Rec (VotingProcedure @era)
        !> To vProcVote
        !> E (encodeNullStrictMaybe encCBOR) vProcAnchor

instance EraPParams era => ToJSON (VotingProcedure era) where
  toJSON = object . toVotingProcedurePairs
  toEncoding = pairs . mconcat . toVotingProcedurePairs

toVotingProcedurePairs :: (KeyValue a, EraPParams era) => VotingProcedure era -> [a]
toVotingProcedurePairs vProc@(VotingProcedure _ _) =
  let VotingProcedure {..} = vProc
   in [ "anchor" .= vProcAnchor
      , "decision" .= vProcVote
      ]

data GovProcedures era = GovProcedures
  { gpVotingProcedures :: !(VotingProcedures era)
  , gpProposalProcedures :: !(Seq (ProposalProcedure era))
  }
  deriving (Eq, Generic)

-- | Attaches indices to a sequence of proposal procedures. The indices grow
-- from left to right.
indexedGovProps ::
  Seq (ProposalProcedure era) ->
  Seq (GovActionIx, ProposalProcedure era)
indexedGovProps = enumerateProps 0
  where
    enumerateProps _ Empty = Empty
    enumerateProps !n (x :<| xs) = (GovActionIx n, x) :<| enumerateProps (succ n) xs

instance EraPParams era => NoThunks (GovProcedures era)

instance EraPParams era => NFData (GovProcedures era)

deriving instance EraPParams era => Show (GovProcedures era)

data ProposalProcedure era = ProposalProcedure
  { pProcDeposit :: !Coin
  , pProcReturnAddr :: !(RewardAcnt (EraCrypto era))
  , pProcGovAction :: !(GovAction era)
  , pProcAnchor :: !(Anchor (EraCrypto era))
  }
  deriving (Generic, Eq, Show)

pProcDepositL :: Lens' (ProposalProcedure era) Coin
pProcDepositL = lens pProcDeposit (\p x -> p {pProcDeposit = x})

instance EraPParams era => NoThunks (ProposalProcedure era)

instance EraPParams era => NFData (ProposalProcedure era)

instance EraPParams era => DecCBOR (ProposalProcedure era) where
  decCBOR =
    decode $
      RecD ProposalProcedure
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (ProposalProcedure era) where
  encCBOR ProposalProcedure {..} =
    encode $
      Rec (ProposalProcedure @era)
        !> To pProcDeposit
        !> To pProcReturnAddr
        !> To pProcGovAction
        !> To pProcAnchor

data Committee era = Committee
  { committeeMembers :: !(Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
  -- ^ Committee members with epoch number when each of them expires
  , committeeQuorum :: !UnitInterval
  -- ^ Quorum of the committee that is necessary for a successful vote
  }
  deriving (Eq, Show, Generic)

instance Era era => NoThunks (Committee era)

instance Era era => NFData (Committee era)

instance Era era => DecCBOR (Committee era) where
  decCBOR =
    decode $
      RecD Committee
        <! From
        <! From

instance Era era => EncCBOR (Committee era) where
  encCBOR Committee {committeeMembers, committeeQuorum} =
    encode $
      Rec (Committee @era)
        !> To committeeMembers
        !> To committeeQuorum

instance EraPParams era => ToJSON (Committee era) where
  toJSON = object . toCommitteePairs
  toEncoding = pairs . mconcat . toCommitteePairs

toCommitteePairs :: (KeyValue a, EraPParams era) => Committee era -> [a]
toCommitteePairs committee@(Committee _ _) =
  let Committee {..} = committee
   in [ "members" .= committeeMembers
      , "quorum" .= committeeQuorum
      ]

data GovAction era
  = ParameterChange !(PParamsUpdate era)
  | HardForkInitiation !ProtVer
  | TreasuryWithdrawals !(Map (RewardAcnt (EraCrypto era)) Coin)
  | NoConfidence
  | NewCommittee
      -- | Old committee
      !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
      -- | New Committee
      !(Committee era)
  | NewConstitution !(Constitution era)
  | InfoAction
  deriving (Generic)

deriving instance EraPParams era => Eq (GovAction era)

deriving instance EraPParams era => Show (GovAction era)

instance EraPParams era => NoThunks (GovAction era)

instance EraPParams era => NFData (GovAction era)

instance EraPParams era => ToJSON (GovAction era)

instance EraPParams era => DecCBOR (GovAction era) where
  decCBOR =
    decode $
      Summands "GovAction" dec
    where
      dec 0 = SumD ParameterChange <! From
      dec 1 = SumD HardForkInitiation <! From
      dec 2 = SumD TreasuryWithdrawals <! From
      dec 3 = SumD NoConfidence
      dec 4 = SumD NewCommittee <! From <! From
      dec 5 = SumD NewConstitution <! From
      dec 6 = SumD InfoAction
      dec k = Invalid k

instance EraPParams era => EncCBOR (GovAction era) where
  encCBOR x = encode (enc x)
    where
      enc (ParameterChange ppup) = Sum ParameterChange 0 !> To ppup
      enc (HardForkInitiation pv) = Sum HardForkInitiation 1 !> To pv
      enc (TreasuryWithdrawals ws) = Sum TreasuryWithdrawals 2 !> To ws
      enc NoConfidence = Sum NoConfidence 3
      enc (NewCommittee old new) = Sum NewCommittee 4 !> To old !> To new
      enc (NewConstitution c) = Sum NewConstitution 5 !> To c
      enc InfoAction = Sum InfoAction 6
