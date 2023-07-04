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
  GovernanceAction (..),
  GovernanceActionState (..),
  GovernanceActionIx (..),
  GovernanceActionId (..),
  govActionIdToText,
  Voter (..),
  Vote (..),
  VotingProcedure (..),
  ProposalProcedure (..),
  GovernanceProcedures (..),
  Anchor (..),
  AnchorDataHash,
  ensConstitutionL,
  rsEnactStateL,
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..), StrictMaybe)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
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
import Cardano.Ledger.Conway.Governance.Procedures (
  Anchor (..),
  AnchorDataHash,
  GovernanceAction (..),
  GovernanceActionId (..),
  GovernanceActionIx (..),
  GovernanceProcedures (..),
  ProposalProcedure (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  govActionIdToText,
 )
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Governance
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks)

data GovernanceActionState era = GovernanceActionState
  { gasCommitteeVotes :: !(Map (Credential 'CommitteeHotKey (EraCrypto era)) Vote)
  , gasDRepVotes :: !(Map (Credential 'Voting (EraCrypto era)) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash 'StakePool (EraCrypto era)) Vote)
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
toGovernanceActionStatePairs gas@(GovernanceActionState _ _ _ _ _ _ _) =
  let GovernanceActionState {..} = gas
   in [ "committeeVotes" .= gasCommitteeVotes
      , "dRepVotes" .= gasDRepVotes
      , "stakePoolVotes" .= gasStakePoolVotes
      , "deposit" .= gasDeposit
      , "returnAddr" .= gasReturnAddr
      , "action" .= gasAction
      , "proposedIn" .= gasProposedIn
      ]

deriving instance EraPParams era => Eq (GovernanceActionState era)

deriving instance EraPParams era => Show (GovernanceActionState era)

instance EraPParams era => NoThunks (GovernanceActionState era)

instance EraPParams era => NFData (GovernanceActionState era)

instance EraPParams era => DecCBOR (GovernanceActionState era) where
  decCBOR =
    decode $
      RecD GovernanceActionState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (GovernanceActionState era) where
  encCBOR GovernanceActionState {..} =
    encode $
      Rec GovernanceActionState
        !> To gasCommitteeVotes
        !> To gasDRepVotes
        !> To gasStakePoolVotes
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

ensConstitutionL :: Lens' (EnactState era) (SafeHash (EraCrypto era) ByteString)
ensConstitutionL = lens ensConstitution (\x y -> x {ensConstitution = y})

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

instance EraPParams era => ToCBOR (EnactState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (EnactState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => NFData (EnactState era)

instance EraPParams era => NoThunks (EnactState era)

data RatifyState era = RatifyState
  { rsEnactState :: !(EnactState era)
  , rsFuture :: !(StrictSeq (GovernanceActionId (EraCrypto era), GovernanceActionState era))
  }
  deriving (Generic, Eq, Show)

rsEnactStateL :: Lens' (RatifyState era) (EnactState era)
rsEnactStateL = lens rsEnactState (\x y -> x {rsEnactState = y})

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

instance EraPParams era => ToCBOR (RatifyState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (RatifyState era) where
  fromCBOR = fromEraCBOR @era

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
  }
  deriving (Generic, Eq, Show)

cgTallyL :: Lens' (ConwayGovernance era) (ConwayTallyState era)
cgTallyL = lens cgTally (\x y -> x {cgTally = y})

cgRatifyL :: Lens' (ConwayGovernance era) (RatifyState era)
cgRatifyL = lens cgRatify (\x y -> x {cgRatify = y})

instance EraPParams era => DecCBOR (ConwayGovernance era) where
  decCBOR =
    decode $
      RecD ConwayGovernance
        <! From
        <! From

instance EraPParams era => EncCBOR (ConwayGovernance era) where
  encCBOR ConwayGovernance {..} =
    encode $
      Rec ConwayGovernance
        !> To cgTally
        !> To cgRatify

instance EraPParams era => ToCBOR (ConwayGovernance era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovernance era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => Default (ConwayGovernance era)

instance EraPParams era => NFData (ConwayGovernance era)

instance EraPParams era => NoThunks (ConwayGovernance era)

instance EraPParams era => ToJSON (ConwayGovernance era) where
  toJSON = object . toConwayGovernancePairs
  toEncoding = pairs . mconcat . toConwayGovernancePairs

toConwayGovernancePairs :: (KeyValue a, EraPParams era) => ConwayGovernance era -> [a]
toConwayGovernancePairs cg@(ConwayGovernance _ _) =
  let ConwayGovernance {..} = cg
   in [ "tally" .= cgTally
      , "ratify" .= cgRatify
      ]

instance Crypto c => EraGovernance (ConwayEra c) where
  type GovernanceState (ConwayEra c) = ConwayGovernance (ConwayEra c)
  getConstitutionHash g = Just $ g ^. cgRatifyL . rsEnactStateL . ensConstitutionL
