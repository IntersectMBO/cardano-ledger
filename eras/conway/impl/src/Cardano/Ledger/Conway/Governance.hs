{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance (
  EraGovernance (..),
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
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeEnumBounded, encodeEnum)
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
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Governance
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

instance EraPParams era => DecCBOR (GovernanceActionInfo era) where
  decCBOR =
    decode $
      RecD GovernanceActionInfo
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (GovernanceActionInfo era) where
  encCBOR GovernanceActionInfo {..} =
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

instance EraPParams era => DecCBOR (GovernanceAction era) where
  decCBOR =
    decode $
      Summands "GovernanceAction" dec
    where
      dec 0 = ParameterChange <$> From
      dec 1 = HardForkInitiation <$> From
      dec 2 = TreasuryWithdrawals <$> From
      dec k = Invalid k

instance EraPParams era => EncCBOR (GovernanceAction era) where
  encCBOR x = encode (enc x)
    where
      enc (ParameterChange ppup) = Sum (encCBOR ppup) 0
      enc (HardForkInitiation pv) = Sum (encCBOR pv) 1
      enc (TreasuryWithdrawals ws) = Sum (encCBOR ws) 2

newtype GovernanceActionIx = GovernanceActionIx Word64
  deriving (Generic, Eq, Ord, Show, Num, Enum)

instance NoThunks GovernanceActionIx

instance NFData GovernanceActionIx

deriving newtype instance DecCBOR GovernanceActionIx

deriving newtype instance EncCBOR GovernanceActionIx

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
  DecCBOR (Vote era)
  where
  decCBOR =
    decode $
      RecD Vote
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance Crypto c => EncCBOR (GovernanceActionId c) where
  encCBOR GovernanceActionId {..} =
    encode $
      Rec GovernanceActionId
        !> To gaidTxId
        !> To gaidGovActionIx

instance Era era => EncCBOR (Vote era) where
  encCBOR Vote {..} =
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

instance DecCBOR VoterRole where
  decCBOR = decodeEnumBounded

instance EncCBOR VoterRole where
  encCBOR = encodeEnum

instance NoThunks VoterRole

instance NFData VoterRole

data VoteDecision
  = No
  | Yes
  | Abstain
  deriving (Generic, Eq, Show, Enum, Bounded)

instance NoThunks VoteDecision

instance NFData VoteDecision

instance DecCBOR VoteDecision where
  decCBOR = decodeEnumBounded

instance EncCBOR VoteDecision where
  encCBOR = encodeEnum

data GovernanceActionState era = GovernanceActionState
  { gasVotes :: !(Map (VoterRole, KeyHash 'Voting (EraCrypto era)) (Vote era))
  , gasDeposit :: !Coin
  , gasReturnAddr :: !(KeyHash 'Staking (EraCrypto era))
  , gasAction :: !(GovernanceAction era)
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (GovernanceActionState era)

instance EraPParams era => NoThunks (GovernanceActionState era)

deriving instance EraPParams era => Show (GovernanceActionState era)

instance EraPParams era => NFData (GovernanceActionState era)

newtype ConwayTallyState era
  = ConwayTallyState
      (Map (GovernanceActionId (EraCrypto era)) (GovernanceActionState era))
  deriving (Generic)

deriving instance EraPParams era => Eq (ConwayTallyState era)

deriving instance EraPParams era => Show (ConwayTallyState era)

deriving instance EraPParams era => NFData (ConwayTallyState era)

instance EraPParams era => NoThunks (ConwayTallyState era)

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

instance EraPParams era => EncCBOR (ConwayTallyState era) where
  encCBOR = undefined

instance EraPParams era => DecCBOR (ConwayTallyState era) where
  decCBOR = undefined

instance Crypto c => EraGovernance (ConwayEra c) where
  type GovernanceState (ConwayEra c) = ConwayTallyState (ConwayEra c)
  emptyGovernanceState = ConwayTallyState mempty
