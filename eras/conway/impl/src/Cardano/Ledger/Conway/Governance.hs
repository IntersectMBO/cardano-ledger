{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Governance (
  ConwayTallyState (..),
  GovernanceActionInfo (..),
  GovernanceAction (..),
  GovernanceActionState (..),
  GovernanceActionIx (..),
  GovernanceActionId (..),
  Vote (..),
  VoterRole (..),
  VoteDecision (..),
  makeGovAction,
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeEnumBounded,
  encodeEnum,
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
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.TxBody (Url)
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data GovernanceActionInfo era = GovernanceActionInfo
  { gaiDepositAmount :: !Coin
  , gaiRewardAddress :: !(KeyHash 'Staking (EraCrypto era))
  , gaiMetadataURL :: !Url
  , gaiMetadataHash :: !(SafeHash (EraCrypto era) ByteString)
  , gaiAction :: !(GovernanceAction era)
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (GovernanceActionInfo era)

deriving instance EraPParams era => Show (GovernanceActionInfo era)

instance EraPParams era => NoThunks (GovernanceActionInfo era)

instance EraPParams era => NFData (GovernanceActionInfo era)

instance EraPParams era => FromCBOR (GovernanceActionInfo era) where
  fromCBOR =
    decode $
      RecD GovernanceActionInfo
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => ToCBOR (GovernanceActionInfo era) where
  toCBOR GovernanceActionInfo {..} =
    encode $
      Rec GovernanceActionInfo
        !> To gaiDepositAmount
        !> To gaiRewardAddress
        !> To gaiMetadataURL
        !> To gaiMetadataHash
        !> To gaiAction

data GovernanceAction era
  = ParameterChange !(PParamsUpdate era)
  | HardForkInitiation !ProtVer
  | TreasuryWithdrawals !(Map (Credential 'Staking (EraCrypto era)) Coin)
  deriving (Generic)

deriving instance EraPParams era => Eq (GovernanceAction era)

deriving instance EraPParams era => Show (GovernanceAction era)

instance EraPParams era => NoThunks (GovernanceAction era)

instance EraPParams era => NFData (GovernanceAction era)

instance EraPParams era => FromCBOR (GovernanceAction era) where
  fromCBOR =
    decode $
      Summands "GovernanceAction" dec
    where
      dec 0 = ParameterChange <$> From
      dec 1 = HardForkInitiation <$> From
      dec 2 = TreasuryWithdrawals <$> From
      dec k = Invalid k

instance EraPParams era => ToCBOR (GovernanceAction era) where
  toCBOR x = encode (enc x)
    where
      enc (ParameterChange ppup) = Sum (toCBOR ppup) 0
      enc (HardForkInitiation pv) = Sum (toCBOR pv) 1
      enc (TreasuryWithdrawals ws) = Sum (toCBOR ws) 2

newtype GovernanceActionIx = GovernanceActionIx Word64
  deriving (Generic, Eq, Ord, Show, Num, Enum)

instance NoThunks GovernanceActionIx

instance NFData GovernanceActionIx

deriving newtype instance FromCBOR GovernanceActionIx

deriving newtype instance ToCBOR GovernanceActionIx

data GovernanceActionId c = GovernanceActionId
  { gaidTxId :: !(TxId c)
  , gaidGovActionIx :: !GovernanceActionIx
  }
  deriving (Generic, Eq, Ord, Show)

instance Crypto c => FromCBOR (GovernanceActionId c) where
  fromCBOR =
    decode $
      RecD GovernanceActionId
        <! From
        <! From

instance NoThunks (GovernanceActionId c)

instance Crypto c => NFData (GovernanceActionId c)

data Vote era = Vote
  { voteGovActionId :: !(GovernanceActionId (EraCrypto era))
  , voteRole :: !VoterRole
  , voteRoleKeyHash :: !(KeyHash 'Voting (EraCrypto era))
  , voteMetadataURL :: !Url
  , voteMetadataHash :: !(SafeHash (EraCrypto era) ByteString)
  , voteDecision :: !VoteDecision
  }
  deriving (Generic, Eq, Show)

instance NoThunks (Vote era)

instance Crypto (EraCrypto era) => NFData (Vote era)

instance
  ( Era era
  , Crypto (EraCrypto era)
  ) =>
  FromCBOR (Vote era)
  where
  fromCBOR =
    decode $
      RecD Vote
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance Crypto c => ToCBOR (GovernanceActionId c) where
  toCBOR GovernanceActionId {..} =
    encode $
      Rec GovernanceActionId
        !> To gaidTxId
        !> To gaidGovActionIx

instance Era era => ToCBOR (Vote era) where
  toCBOR Vote {..} =
    encode $
      Rec (Vote @era)
        !> To voteGovActionId
        !> To voteRole
        !> To voteRoleKeyHash
        !> To voteMetadataURL
        !> To voteMetadataHash
        !> To voteDecision

data VoterRole
  = ConstitutionalCommittee
  | DRep
  | SPO
  deriving (Generic, Eq, Ord, Show, Enum, Bounded)

instance FromCBOR VoterRole where
  fromCBOR = decodeEnumBounded

instance ToCBOR VoterRole where
  toCBOR = encodeEnum

instance NoThunks VoterRole

instance NFData VoterRole

data VoteDecision
  = No
  | Yes
  | Abstain
  deriving (Generic, Eq, Show, Enum, Bounded)

instance NoThunks VoteDecision

instance NFData VoteDecision

instance FromCBOR VoteDecision where
  fromCBOR = decodeEnumBounded

instance ToCBOR VoteDecision where
  toCBOR = encodeEnum

data GovernanceActionState era = GovernanceActionState
  { gasVotes :: !(Map (VoterRole, KeyHash 'Voting (EraCrypto era)) (Vote era))
  , gasDeposit :: !Coin
  , gasReturnAddr :: !(KeyHash 'Staking (EraCrypto era))
  , gasAction :: !(GovernanceAction era)
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (GovernanceActionState era)

instance EraPParams era => NoThunks (GovernanceActionState era)

instance EraPParams era => NFData (GovernanceActionState era)

deriving instance EraPParams era => Show (GovernanceActionState era)

newtype ConwayTallyState era
  = ConwayTallyState
      (Map (GovernanceActionId (EraCrypto era)) (GovernanceActionState era))
  deriving (Generic, NFData)

deriving instance EraPParams era => Eq (ConwayTallyState era)

deriving instance EraPParams era => Show (ConwayTallyState era)

instance EraPParams era => NoThunks (ConwayTallyState era)

instance Era era => ToCBOR (ConwayTallyState era) where
  toCBOR = error "Unimplemented"

instance Era era => FromCBOR (ConwayTallyState era) where
  fromCBOR = error "Unimplemented"

instance Era era => EncCBOR (ConwayTallyState era) where
  encCBOR = encEraToCBOR @era

instance Era era => DecCBOR (ConwayTallyState era) where
  decCBOR = decEraFromCBOR @era

instance Default (ConwayTallyState era) where
  def = ConwayTallyState mempty

makeGovAction :: GovernanceActionInfo era -> GovernanceActionState era
makeGovAction GovernanceActionInfo {..} =
  GovernanceActionState
    { gasVotes = mempty
    , gasDeposit = gaiDepositAmount
    , gasReturnAddr = gaiRewardAddress
    , gasAction = gaiAction
    }
