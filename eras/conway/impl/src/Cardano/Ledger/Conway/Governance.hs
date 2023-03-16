{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

module Cardano.Ledger.Conway.Governance (
  EraGovernance (..),
  ConwayTallyState (..),
  EnactState (..),
  RatifyState (..),
  ConwayGovernance (..),
  -- Lenses
  cgTallyL,
  cgRatifyL,
  cgVoterRolesL,
  GovernanceAction (..),
  GovernanceActionState (..),
  GovernanceActionIx (..),
  GovernanceActionId (..),
  govActionIdToText,
  VoterRole (..),
  Vote (..),
  VotingProcedure (..),
  ProposalProcedure (..),
  GovernanceProcedure (..),
  Anchor (..),
  AnchorDataHash,
) where

import Cardano.Crypto.Hash.Class (hashToTextAsHex)
import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..), StrictMaybe, Url)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeEnumBounded,
  decodeNullStrictMaybe,
  encodeEnum,
  encodeNullStrictMaybe,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.TxIn (TxId (..))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

data GovernanceAction era
  = ParameterChange !(PParamsUpdate era)
  | HardForkInitiation !ProtVer
  | TreasuryWithdrawals !(Map (Credential 'Staking (EraCrypto era)) Coin)
  | NoConfidence
  | NewCommittee !(Set (KeyHash 'Voting (EraCrypto era))) !Rational
  | NewConstitution !(SafeHash (EraCrypto era) ByteString)
  deriving (Generic)

deriving instance EraPParams era => Eq (GovernanceAction era)

deriving instance EraPParams era => Show (GovernanceAction era)

instance EraPParams era => NoThunks (GovernanceAction era)

instance EraPParams era => NFData (GovernanceAction era)

instance EraPParams era => ToJSON (GovernanceAction era)

instance EraPParams era => DecCBOR (GovernanceAction era) where
  decCBOR =
    decode $
      Summands "GovernanceAction" dec
    where
      dec 0 = SumD ParameterChange <! From
      dec 1 = SumD HardForkInitiation <! From
      dec 2 = SumD TreasuryWithdrawals <! From
      dec 3 = SumD NoConfidence
      dec 4 = SumD NewCommittee <! From <! From
      dec 5 = SumD NewConstitution <! From
      dec k = Invalid k

instance EraPParams era => EncCBOR (GovernanceAction era) where
  encCBOR x = encode (enc x)
    where
      enc (ParameterChange ppup) = Sum ParameterChange 0 !> To ppup
      enc (HardForkInitiation pv) = Sum HardForkInitiation 1 !> To pv
      enc (TreasuryWithdrawals ws) = Sum TreasuryWithdrawals 2 !> To ws
      enc NoConfidence = Sum NoConfidence 3
      enc (NewCommittee mems quorum) = Sum NewCommittee 4 !> To mems !> To quorum
      enc (NewConstitution h) = Sum NewConstitution 5 !> To h

newtype GovernanceActionIx = GovernanceActionIx Word64
  deriving (Generic, Eq, Ord, Show, Num, Enum, NFData, NoThunks, EncCBOR, DecCBOR, ToJSON)

data GovernanceActionId c = GovernanceActionId
  { gaidTxId :: !(TxId c)
  , gaidGovActionIx :: !GovernanceActionIx
  }
  deriving (Generic, Eq, Ord, Show)

instance Crypto c => DecCBOR (GovernanceActionId c) where
  decCBOR =
    decode $
      RecD GovernanceActionId
        <! From
        <! From

instance Crypto c => EncCBOR (GovernanceActionId c) where
  encCBOR GovernanceActionId {..} =
    encode $
      Rec GovernanceActionId
        !> To gaidTxId
        !> To gaidGovActionIx

instance NoThunks (GovernanceActionId c)

instance Crypto c => NFData (GovernanceActionId c)

instance Crypto c => ToJSON (GovernanceActionId c) where
  toJSON = object . toGovernanceActionIdPairs
  toEncoding = pairs . mconcat . toGovernanceActionIdPairs

toGovernanceActionIdPairs :: (KeyValue a, Crypto c) => GovernanceActionId c -> [a]
toGovernanceActionIdPairs gaid@(GovernanceActionId _ _) =
  let GovernanceActionId {..} = gaid
   in [ "txId" .= gaidTxId
      , "govActionIx" .= gaidGovActionIx
      ]

instance Crypto c => ToJSONKey (GovernanceActionId c) where
  toJSONKey = toJSONKeyText govActionIdToText

govActionIdToText :: GovernanceActionId c -> Text.Text
govActionIdToText (GovernanceActionId (TxId txidHash) (GovernanceActionIx ix)) =
  hashToTextAsHex (extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

data AnchorDataHash

data Anchor c = Anchor
  { anchorUrl :: !Url
  , anchorDataHash :: !(SafeHash c AnchorDataHash)
  }
  deriving (Eq, Show, Generic)

instance NoThunks (Anchor c)

instance Crypto c => NFData (Anchor c) where
  rnf = rwhnf

instance Crypto c => DecCBOR (Anchor c) where
  decCBOR =
    decode $
      RecD Anchor
        <! From
        <! From

instance Crypto c => EncCBOR (Anchor c) where
  encCBOR Anchor {..} =
    encode $
      Rec Anchor
        !> To anchorUrl
        !> To anchorDataHash

instance Crypto c => ToJSON (Anchor c) where
  toJSON = object . toAnchorPairs
  toEncoding = pairs . mconcat . toAnchorPairs

toAnchorPairs :: (KeyValue a, Crypto c) => Anchor c -> [a]
toAnchorPairs vote@(Anchor _ _) =
  let Anchor {..} = vote
   in [ "url" .= anchorUrl
      , "dataHash" .= anchorDataHash
      ]

data VotingProcedure era = VotingProcedure
  { vProcGovActionId :: !(GovernanceActionId (EraCrypto era))
  , vProcRole :: !VoterRole
  , vProcRoleKeyHash :: !(Credential 'Voting (EraCrypto era))
  , vProcVote :: !Vote
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
        <! From
        <! From
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

instance Era era => EncCBOR (VotingProcedure era) where
  encCBOR VotingProcedure {..} =
    encode $
      Rec (VotingProcedure @era)
        !> To vProcGovActionId
        !> To vProcRole
        !> To vProcRoleKeyHash
        !> To vProcVote
        !> E (encodeNullStrictMaybe encCBOR) vProcAnchor

instance EraPParams era => ToJSON (VotingProcedure era) where
  toJSON = object . toVotingProcedurePairs
  toEncoding = pairs . mconcat . toVotingProcedurePairs

toVotingProcedurePairs :: (KeyValue a, EraPParams era) => VotingProcedure era -> [a]
toVotingProcedurePairs vProc@(VotingProcedure _ _ _ _ _) =
  let VotingProcedure {..} = vProc
   in [ "govActionId" .= vProcGovActionId
      , "role" .= vProcRole
      , "roleKeyHash" .= vProcRoleKeyHash
      , "anchor" .= vProcAnchor
      , "decision" .= vProcVote
      ]

data ProposalProcedure era = ProposalProcedure
  { pProcDeposit :: !Coin
  , pProcReturnAddr :: !(KeyHash 'Staking (EraCrypto era))
  , pProcGovernanceAction :: !(GovernanceAction era)
  , pProcAnchor :: !(StrictMaybe (Anchor (EraCrypto era)))
  }
  deriving (Generic, Eq, Show)

instance EraPParams era => NoThunks (ProposalProcedure era)

instance EraPParams era => NFData (ProposalProcedure era)

instance EraPParams era => DecCBOR (ProposalProcedure era) where
  decCBOR =
    decode $
      RecD ProposalProcedure
        <! From
        <! From
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

instance EraPParams era => EncCBOR (ProposalProcedure era) where
  encCBOR ProposalProcedure {..} =
    encode $
      Rec (ProposalProcedure @era)
        !> To pProcDeposit
        !> To pProcReturnAddr
        !> To pProcGovernanceAction
        !> E (encodeNullStrictMaybe encCBOR) pProcAnchor

data GovernanceProcedure era
  = GovernanceVotingProcedure !(VotingProcedure era)
  | GovernanceProposalProcedure !(ProposalProcedure era)
  deriving (Eq, Generic)

instance EraPParams era => DecCBOR (GovernanceProcedure era) where
  decCBOR = decode $ Summands "GovernanceProcedure" dec
    where
      dec 0 = SumD GovernanceVotingProcedure <! From
      dec 1 = SumD GovernanceProposalProcedure <! From
      dec n = Invalid n

instance EraPParams era => EncCBOR (GovernanceProcedure era) where
  encCBOR (GovernanceVotingProcedure vProc) =
    encode @_ @(GovernanceProcedure era) $
      Sum GovernanceVotingProcedure 0 !> To vProc
  encCBOR (GovernanceProposalProcedure pProc) =
    encode @_ @(GovernanceProcedure era) $
      Sum GovernanceProposalProcedure 1 !> To pProc

instance EraPParams era => NoThunks (GovernanceProcedure era)

instance EraPParams era => NFData (GovernanceProcedure era)

deriving instance EraPParams era => Show (GovernanceProcedure era)

data VoterRole
  = ConstitutionalCommittee
  | DRep
  | SPO
  deriving (Generic, Eq, Ord, Show, Enum, Bounded)

instance ToJSON VoterRole

instance DecCBOR VoterRole where
  decCBOR = decodeEnumBounded

instance EncCBOR VoterRole where
  encCBOR = encodeEnum

instance NoThunks VoterRole

instance NFData VoterRole

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

data GovernanceActionState era = GovernanceActionState
  { gasVotes :: !(Map (VoterRole, Credential 'Voting (EraCrypto era)) Vote)
  , gasDeposit :: !Coin
  , gasReturnAddr :: !(KeyHash 'Staking (EraCrypto era))
  , gasAction :: !(GovernanceAction era)
  , gasProposedIn :: !EpochNo
  }
  deriving (Generic)

instance EraPParams era => ToJSON (GovernanceActionState era) where
  toJSON = object . toGovernanceActionStatePairs
  toEncoding = pairs . mconcat . toGovernanceActionStatePairs

toGovernanceActionStatePairs :: (KeyValue a, EraPParams era) => GovernanceActionState era -> [a]
toGovernanceActionStatePairs gas@(GovernanceActionState _ _ _ _ _) =
  let GovernanceActionState {..} = gas
   in [ "votes" .= gasVotes
      , "deposit" .= gasDeposit
      , "returnAddr" .= gasReturnAddr
      , "action" .= gasAction
      , "proposedIn" .= gasProposedIn
      ]

deriving instance EraPParams era => Eq (GovernanceActionState era)

deriving instance EraPParams era => Show (GovernanceActionState era)

instance EraPParams era => NoThunks (GovernanceActionState era)

instance EraPParams era => NFData (GovernanceActionState era)

instance (Era era, EraPParams era) => DecCBOR (GovernanceActionState era) where
  decCBOR =
    decode $
      RecD GovernanceActionState
        <! From
        <! From
        <! From
        <! From
        <! From

instance (Era era, EraPParams era) => EncCBOR (GovernanceActionState era) where
  encCBOR GovernanceActionState {..} =
    encode $
      Rec GovernanceActionState
        !> To gasVotes
        !> To gasDeposit
        !> To gasReturnAddr
        !> To gasAction
        !> To gasProposedIn

newtype ConwayTallyState era = ConwayTallyState
  { unConwayTallyState :: Map (GovernanceActionId (EraCrypto era)) (GovernanceActionState era)
  }
  deriving (Generic, NFData)

deriving instance EraPParams era => Eq (ConwayTallyState era)

deriving instance EraPParams era => Show (ConwayTallyState era)

deriving instance EraPParams era => ToJSON (ConwayTallyState era)

instance EraPParams era => NoThunks (ConwayTallyState era)

instance Default (ConwayTallyState era) where
  def = ConwayTallyState mempty

deriving instance EraPParams era => EncCBOR (ConwayTallyState era)

deriving instance EraPParams era => DecCBOR (ConwayTallyState era)

instance EraPParams era => ToCBOR (ConwayTallyState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayTallyState era) where
  fromCBOR = fromEraCBOR @era

data EnactState era = EnactState
  { ensCommittee :: !(StrictMaybe (Set (KeyHash 'Voting (EraCrypto era)), Rational))
  -- ^ Constitutional Committee
  , ensConstitution :: !(SafeHash (EraCrypto era) ByteString)
  -- ^ Hash of the Constitution
  , ensProtVer :: !ProtVer
  , ensPParams :: !(PParams era)
  , ensTreasury :: !Coin
  , ensWithdrawals :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  }
  deriving (Generic)

instance EraPParams era => ToJSON (EnactState era) where
  toJSON = object . toEnactStatePairs
  toEncoding = pairs . mconcat . toEnactStatePairs

toEnactStatePairs :: (KeyValue a, EraPParams era) => EnactState era -> [a]
toEnactStatePairs cg@(EnactState _ _ _ _ _ _) =
  let EnactState {..} = cg
   in [ "committee" .= ensCommittee
      , "constitution" .= ensConstitution
      , "protVer" .= ensProtVer
      , "pparams" .= ensPParams
      , "treasury" .= ensTreasury
      , "withdrawals" .= ensWithdrawals
      ]

deriving instance Eq (PParams era) => Eq (EnactState era)

deriving instance Show (PParams era) => Show (EnactState era)

instance EraPParams era => Default (EnactState era) where
  def =
    EnactState
      def
      def
      (ProtVer (eraProtVerLow @era) 0)
      def
      (Coin 0)
      def

instance EraPParams era => DecCBOR (EnactState era) where
  decCBOR =
    decode $
      RecD EnactState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (EnactState era) where
  encCBOR EnactState {..} =
    encode $
      Rec EnactState
        !> To ensCommittee
        !> To ensConstitution
        !> To ensProtVer
        !> To ensPParams
        !> To ensTreasury
        !> To ensWithdrawals

instance EraPParams era => NFData (EnactState era)

instance EraPParams era => NoThunks (EnactState era)

data RatifyState era = RatifyState
  { rsEnactState :: !(EnactState era)
  , rsFuture :: !(StrictSeq (GovernanceActionId (EraCrypto era), GovernanceActionState era))
  }
  deriving (Generic, Eq, Show)

instance EraPParams era => Default (RatifyState era)

instance EraPParams era => DecCBOR (RatifyState era) where
  decCBOR =
    decode $
      RecD RatifyState
        <! From
        <! From

instance EraPParams era => EncCBOR (RatifyState era) where
  encCBOR RatifyState {..} =
    encode $
      Rec RatifyState
        !> To rsEnactState
        !> To rsFuture

instance EraPParams era => NFData (RatifyState era)

instance EraPParams era => NoThunks (RatifyState era)

instance EraPParams era => ToJSON (RatifyState era) where
  toJSON = object . toRatifyStatePairs
  toEncoding = pairs . mconcat . toRatifyStatePairs

toRatifyStatePairs :: (KeyValue a, EraPParams era) => RatifyState era -> [a]
toRatifyStatePairs cg@(RatifyState _ _) =
  let RatifyState {..} = cg
   in [ "enactState" .= rsEnactState
      , "future" .= rsFuture
      ]

data ConwayGovernance era = ConwayGovernance
  { cgTally :: !(ConwayTallyState era)
  , cgRatify :: !(RatifyState era)
  , cgVoterRoles :: !(Map (Credential 'Voting (EraCrypto era)) VoterRole)
  }
  deriving (Generic, Eq, Show)

cgTallyL :: Lens' (ConwayGovernance era) (ConwayTallyState era)
cgTallyL = lens cgTally (\x y -> x {cgTally = y})

cgRatifyL :: Lens' (ConwayGovernance era) (RatifyState era)
cgRatifyL = lens cgRatify (\x y -> x {cgRatify = y})

cgVoterRolesL :: Lens' (ConwayGovernance era) (Map (Credential 'Voting (EraCrypto era)) VoterRole)
cgVoterRolesL = lens cgVoterRoles (\x y -> x {cgVoterRoles = y})

instance EraPParams era => DecCBOR (ConwayGovernance era) where
  decCBOR =
    decode $
      RecD ConwayGovernance
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (ConwayGovernance era) where
  encCBOR ConwayGovernance {..} =
    encode $
      Rec ConwayGovernance
        !> To cgTally
        !> To cgRatify
        !> To cgVoterRoles

instance EraPParams era => Default (ConwayGovernance era)

instance EraPParams era => NFData (ConwayGovernance era)

instance EraPParams era => NoThunks (ConwayGovernance era)

instance EraPParams era => ToJSON (ConwayGovernance era) where
  toJSON = object . toConwayGovernancePairs
  toEncoding = pairs . mconcat . toConwayGovernancePairs

toConwayGovernancePairs :: (KeyValue a, EraPParams era) => ConwayGovernance era -> [a]
toConwayGovernancePairs cg@(ConwayGovernance _ _ _) =
  let ConwayGovernance {..} = cg
   in [ "tally" .= cgTally
      , "ratify" .= cgRatify
      , "voterRoles" .= cgVoterRoles
      ]

instance Crypto c => EraGovernance (ConwayEra c) where
  type GovernanceState (ConwayEra c) = ConwayGovernance (ConwayEra c)
