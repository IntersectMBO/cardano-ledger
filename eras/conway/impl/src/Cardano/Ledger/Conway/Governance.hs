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
  EnactState (..),
  RatifyState (..),
  ConwayGovernance (..),
  GovernanceAction (..),
  GovernanceActionState (..),
  GovernanceActionIx (..),
  GovernanceActionId (..),
  VoterRole (..),
  VoteDecision (..),
  -- Lenses
  cgTallyL,
  cgRatifyL,
  cgVoterRolesL,
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..), StrictMaybe)
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
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
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

instance Crypto c => EncCBOR (GovernanceActionId c) where
  encCBOR GovernanceActionId {..} =
    encode $
      Rec GovernanceActionId
        !> To gaidTxId
        !> To gaidGovActionIx

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
  { gasVotes :: !(Map (VoterRole, Credential 'Voting (EraCrypto era)) VoteDecision)
  , gasDeposit :: !Coin
  , gasReturnAddr :: !(KeyHash 'Staking (EraCrypto era))
  , gasAction :: !(GovernanceAction era)
  , gasProposedIn :: !EpochNo
  }
  deriving (Generic)

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

newtype ConwayTallyState era = ConwayTallyState {unConwayTallyState :: Map (GovernanceActionId (EraCrypto era)) (GovernanceActionState era)}
  deriving (Generic, NFData)

deriving instance EraPParams era => Eq (ConwayTallyState era)

deriving instance EraPParams era => Show (ConwayTallyState era)

instance EraPParams era => NoThunks (ConwayTallyState era)

instance Default (ConwayTallyState era) where
  def = ConwayTallyState mempty

data EnactState era = EnactState
  { ensCC :: !(StrictMaybe (Set (KeyHash 'Voting (EraCrypto era)), Rational))
  , ensConstitution :: !(SafeHash (EraCrypto era) ByteString)
  , ensProtVer :: !ProtVer
  , ensPParams :: !(PParams era)
  }
  deriving (Generic)

instance Era era => EncCBOR (ConwayTallyState era) where
  encCBOR _ =
    -- FIXME: implement the actual encoder. Has to put this ugly thing here instead of an
    -- error because consensus already has golden tests for Conway
    encCBOR (mempty :: Map () ())

instance Era era => DecCBOR (ConwayTallyState era) where
  decCBOR = do
    _ :: Map () () <- decCBOR
    -- FIXME: implement the actual decoder. Has to put this ugly thing here instead of an
    -- error because consensus already has golden tests for Conway
    pure (ConwayTallyState mempty)

instance Era era => ToCBOR (ConwayTallyState era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (ConwayTallyState era) where
  fromCBOR = fromEraCBOR @era

deriving instance Eq (PParams era) => Eq (EnactState era)

deriving instance Show (PParams era) => Show (EnactState era)

instance (Era era, EraPParams era) => Default (EnactState era) where
  def =
    EnactState
      def
      def
      (ProtVer (eraProtVerLow @era) 0)
      def

instance (Era era, EraPParams era) => DecCBOR (EnactState era) where
  decCBOR =
    decode $
      RecD EnactState
        <! From
        <! From
        <! From
        <! From

instance (Era era, EraPParams era) => EncCBOR (EnactState era) where
  encCBOR EnactState {..} =
    encode $
      Rec EnactState
        !> To ensCC
        !> To ensConstitution
        !> To ensProtVer
        !> To ensPParams

instance EraPParams era => NFData (EnactState era)

instance EraPParams era => NoThunks (EnactState era)

data RatifyState era = RatifyState
  { rsES :: !(EnactState era)
  , rsFuture ::
      !( StrictSeq
          (GovernanceActionId (EraCrypto era), GovernanceActionState era)
       )
  }
  deriving (Generic, Eq, Show)

instance EraPParams era => Default (RatifyState era)

instance (Era era, EraPParams era) => DecCBOR (RatifyState era) where
  decCBOR =
    decode $
      RecD RatifyState
        <! From
        <! From

instance (Era era, EraPParams era) => EncCBOR (RatifyState era) where
  encCBOR RatifyState {..} =
    encode $
      Rec RatifyState
        !> To rsES
        !> To rsFuture

instance EraPParams era => NFData (RatifyState era)

instance EraPParams era => NoThunks (RatifyState era)

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

instance
  ( Crypto c
  , EraPParams (ConwayEra c)
  ) =>
  EraGovernance (ConwayEra c)
  where
  type GovernanceState (ConwayEra c) = ConwayGovernance (ConwayEra c)
