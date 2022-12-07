{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Core
  ( ConwayEraTxBody (..),
    GovernanceActionInfo (..),
    GovernanceAction (..),
    GovernanceActionIx (..),
    GovernanceActionId (..),
    Vote (..),
    VoterRole (..),
    VoteDecision (..),
  )
where

import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, Era (..), EraTxBody (..))
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders
  ( Decode (..),
    Encode (..),
    decode,
    encode,
    (!>),
    (<!),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.TxBody (Url)
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)

class BabbageEraTxBody era => ConwayEraTxBody era where
  govActionsTxBodyL :: Lens' (TxBody era) (StrictSeq (GovernanceActionInfo era))
  votesTxBodyL :: Lens' (TxBody era) (StrictSeq (Vote era))

----- PLACEHOLDERS -----

data GovernanceActionInfo era = GovernanceActionInfo
  { gaiDepositAmount :: !Coin,
    gaiRewardAddress :: !(KeyHash 'Staking (EraCrypto era)),
    gaiMetadataURL :: !Url,
    gaiMetadataHash :: !(SafeHash (EraCrypto era) ByteString),
    gaiAction :: !(GovernanceAction era)
  }
  deriving (Generic)

deriving instance Eq (PParamsUpdate era) => Eq (GovernanceActionInfo era)

deriving instance Show (PParamsUpdate era) => Show (GovernanceActionInfo era)

instance NoThunks (PParamsUpdate era) => NoThunks (GovernanceActionInfo era)

instance NFData (PParamsUpdate era) => NFData (GovernanceActionInfo era)

instance
  ( Era era,
    FromCBOR (PParamsUpdate era)
  ) =>
  FromCBOR (GovernanceActionInfo era)
  where
  fromCBOR =
    decode $
      RecD GovernanceActionInfo
        <! From
        <! From
        <! From
        <! From
        <! From

instance
  ( Era era,
    ToCBOR (PParamsUpdate era)
  ) =>
  ToCBOR (GovernanceActionInfo era)
  where
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

deriving instance Eq (PParamsUpdate era) => Eq (GovernanceAction era)

deriving instance Show (PParamsUpdate era) => Show (GovernanceAction era)

instance NoThunks (PParamsUpdate era) => NoThunks (GovernanceAction era)

instance NFData (PParamsUpdate era) => NFData (GovernanceAction era)

instance
  ( Era era,
    FromCBOR (PParamsUpdate era)
  ) =>
  FromCBOR (GovernanceAction era)
  where
  fromCBOR =
    decode $
      Summands "GovernanceAction" dec
    where
      dec 0 = ParameterChange <$> From
      dec 1 = HardForkInitiation <$> From
      dec 2 = TreasuryWithdrawals <$> From
      dec k = Invalid k

instance
  ( Era era,
    ToCBOR (PParamsUpdate era)
  ) =>
  ToCBOR (GovernanceAction era)
  where
  toCBOR x = encode (enc x)
    where
      enc (ParameterChange ppup) = Sum (toCBOR ppup) 0
      enc (HardForkInitiation pv) = Sum (toCBOR pv) 1
      enc (TreasuryWithdrawals ws) = Sum (toCBOR ws) 2

newtype GovernanceActionIx = GovernanceActionIx Word64
  deriving (Generic, Eq, Show)

instance NoThunks GovernanceActionIx

instance NFData GovernanceActionIx

deriving newtype instance FromCBOR GovernanceActionIx

deriving newtype instance ToCBOR GovernanceActionIx

data GovernanceActionId c = GovernanceActionId
  { gaidTxId :: !(TxId c),
    gaidGovActionIx :: !(GovernanceActionIx)
  }
  deriving (Generic, Eq, Show)

instance Crypto c => FromCBOR (GovernanceActionId c) where
  fromCBOR =
    decode $
      RecD GovernanceActionId
        <! From
        <! From

instance NoThunks (GovernanceActionId c)

instance Crypto c => NFData (GovernanceActionId c)

data Vote era = Vote
  { voteGovActionId :: !(GovernanceActionId (EraCrypto era)),
    voteRole :: !VoterRole,
    voteRoleKeyHash :: !(KeyHash 'Voting (EraCrypto era)),
    voteMetadataURL :: !Url,
    voteMetadataHash :: !(SafeHash (EraCrypto era) ByteString),
    voteDecision :: !VoteDecision
  }
  deriving (Generic, Eq, Show)

instance NoThunks (Vote era)

instance Crypto (EraCrypto era) => NFData (Vote era)

instance
  ( Era era,
    Crypto (EraCrypto era)
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
  deriving (Generic, Eq, Show, Enum)

instance FromCBOR VoterRole where
  fromCBOR =
    decode $
      Summands "VoterRole" dec
    where
      dec 0 = SumD ConstitutionalCommittee
      dec 1 = SumD DRep
      dec 2 = SumD SPO
      dec k = Invalid k

instance ToCBOR VoterRole where
  toCBOR x =
    encode $
      Sum x $ case x of
        ConstitutionalCommittee -> 0
        DRep -> 1
        SPO -> 2

instance NoThunks VoterRole

instance NFData VoterRole

data VoteDecision
  = No
  | Yes
  | Abstain
  deriving (Generic, Eq, Show, Enum)

instance NoThunks VoteDecision

instance NFData VoteDecision

instance FromCBOR VoteDecision where
  fromCBOR =
    decode $
      Summands "VoteDecision" dec
    where
      dec 0 = SumD No
      dec 1 = SumD Yes
      dec 2 = SumD Abstain
      dec k = Invalid k

instance ToCBOR VoteDecision where
  toCBOR x =
    encode $
      Sum x $ case x of
        No -> 0
        Yes -> 1
        Abstain -> 2
