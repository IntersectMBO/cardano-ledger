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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance (
  EraGov (..),
  GovActionsState (..),
  EnactState (..),
  RatifyState (..),
  ConwayGovState (..),
  Committee (..),
  GovAction (..),
  GovActionState (..),
  GovActionIx (..),
  GovActionId (..),
  GovActionPurpose (..),
  PrevGovActionId (..),
  govActionIdToText,
  Voter (..),
  Vote (..),
  VotingProcedure (..),
  VotingProcedures (..),
  ProposalProcedure (..),
  GovProcedures (..),
  Anchor (..),
  AnchorData (..),
  indexedGovProps,
  Constitution (..),
  ConwayEraGov (..),
  -- Lenses
  cgGovActionsStateL,
  cgRatifyL,
  ensConstitutionL,
  rsEnactStateL,
  curPParamsConwayGovStateL,
  prevPParamsConwayGovStateL,
  constitutionScriptL,
  constitutionAnchorL,
) where

import Cardano.Ledger.Address (RewardAcnt)
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
  AnchorData (..),
  Committee (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionPurpose (..),
  GovProcedures (..),
  PrevGovActionId (..),
  ProposalProcedure (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  govActionIdToText,
  indexedGovProps,
 )
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.Governance
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data GovActionState era = GovActionState
  { gasCommitteeVotes :: !(Map (Credential 'HotCommitteeRole (EraCrypto era)) Vote)
  , gasDRepVotes :: !(Map (Credential 'DRepRole (EraCrypto era)) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash 'StakePool (EraCrypto era)) Vote)
  , gasDeposit :: !Coin
  , gasReturnAddr :: !(RewardAcnt (EraCrypto era))
  , gasAction :: !(GovAction era)
  , gasProposedIn :: !EpochNo
  }
  deriving (Generic)

instance EraPParams era => ToJSON (GovActionState era) where
  toJSON = object . toGovActionStatePairs
  toEncoding = pairs . mconcat . toGovActionStatePairs

toGovActionStatePairs :: (KeyValue a, EraPParams era) => GovActionState era -> [a]
toGovActionStatePairs gas@(GovActionState _ _ _ _ _ _ _) =
  let GovActionState {..} = gas
   in [ "committeeVotes" .= gasCommitteeVotes
      , "dRepVotes" .= gasDRepVotes
      , "stakePoolVotes" .= gasStakePoolVotes
      , "deposit" .= gasDeposit
      , "returnAddr" .= gasReturnAddr
      , "action" .= gasAction
      , "proposedIn" .= gasProposedIn
      ]

deriving instance EraPParams era => Eq (GovActionState era)

deriving instance EraPParams era => Show (GovActionState era)

instance EraPParams era => NoThunks (GovActionState era)

instance EraPParams era => NFData (GovActionState era)

instance EraPParams era => DecCBOR (GovActionState era) where
  decCBOR =
    decode $
      RecD GovActionState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (GovActionState era) where
  encCBOR GovActionState {..} =
    encode $
      Rec GovActionState
        !> To gasCommitteeVotes
        !> To gasDRepVotes
        !> To gasStakePoolVotes
        !> To gasDeposit
        !> To gasReturnAddr
        !> To gasAction
        !> To gasProposedIn

newtype GovActionsState era = GovActionsState
  { unGovActionsState :: Map (GovActionId (EraCrypto era)) (GovActionState era)
  }
  deriving (Generic, NFData)

deriving instance EraPParams era => Eq (GovActionsState era)

deriving instance EraPParams era => Show (GovActionsState era)

deriving instance EraPParams era => ToJSON (GovActionsState era)

instance EraPParams era => NoThunks (GovActionsState era)

instance Default (GovActionsState era) where
  def = GovActionsState mempty

deriving instance EraPParams era => EncCBOR (GovActionsState era)

deriving instance EraPParams era => DecCBOR (GovActionsState era)

instance EraPParams era => ToCBOR (GovActionsState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (GovActionsState era) where
  fromCBOR = fromEraCBOR @era

data EnactState era = EnactState
  { ensCommittee :: !(StrictMaybe (Committee era))
  -- ^ Constitutional Committee
  , ensConstitution :: !(Constitution era)
  -- ^ Hash of the Constitution
  , ensProtVer :: !ProtVer
  , ensPParams :: !(PParams era)
  , ensPrevPParams :: !(PParams era)
  , ensTreasury :: !Coin
  , ensWithdrawals :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  }
  deriving (Generic)

ensConstitutionL :: Lens' (EnactState era) (Constitution era)
ensConstitutionL = lens ensConstitution (\x y -> x {ensConstitution = y})

ensCurPParamsL :: Lens' (EnactState era) (PParams era)
ensCurPParamsL = lens ensPParams (\es x -> es {ensPParams = x})

ensPrevPParamsL :: Lens' (EnactState era) (PParams era)
ensPrevPParamsL = lens ensPrevPParams (\es x -> es {ensPrevPParams = x})

instance EraPParams era => ToJSON (EnactState era) where
  toJSON = object . toEnactStatePairs
  toEncoding = pairs . mconcat . toEnactStatePairs

toEnactStatePairs :: (KeyValue a, EraPParams era) => EnactState era -> [a]
toEnactStatePairs cg@(EnactState _ _ _ _ _ _ _) =
  let EnactState {..} = cg
   in [ "committee" .= ensCommittee
      , "constitution" .= ensConstitution
      , "protVer" .= ensProtVer
      , "pparams" .= ensPParams
      , "prevPParams" .= ensPParams
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
        <! From

instance EraPParams era => EncCBOR (EnactState era) where
  encCBOR EnactState {..} =
    encode $
      Rec EnactState
        !> To ensCommittee
        !> To ensConstitution
        !> To ensProtVer
        !> To ensPParams
        !> To ensPrevPParams
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
  , rsFuture ::
      !( StrictSeq
          (GovActionId (EraCrypto era), GovActionState era)
       )
  , rsRemoved ::
      !( StrictSeq
          (GovActionId (EraCrypto era), GovActionState era)
       )
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
        <! From

instance EraPParams era => EncCBOR (RatifyState era) where
  encCBOR RatifyState {..} =
    encode $
      Rec RatifyState
        !> To rsEnactState
        !> To rsFuture
        !> To rsRemoved

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
toRatifyStatePairs cg@(RatifyState _ _ _) =
  let RatifyState {..} = cg
   in [ "enactState" .= rsEnactState
      , "future" .= rsFuture
      , "removed" .= rsRemoved
      ]

data ConwayGovState era = ConwayGovState
  { cgGovActionsState :: !(GovActionsState era)
  , cgRatify :: !(RatifyState era)
  }
  deriving (Generic, Eq, Show)

cgGovActionsStateL :: Lens' (ConwayGovState era) (GovActionsState era)
cgGovActionsStateL = lens cgGovActionsState (\x y -> x {cgGovActionsState = y})

cgRatifyL :: Lens' (ConwayGovState era) (RatifyState era)
cgRatifyL = lens cgRatify (\x y -> x {cgRatify = y})

curPParamsConwayGovStateL :: Lens' (ConwayGovState era) (PParams era)
curPParamsConwayGovStateL = cgRatifyL . rsEnactStateL . ensCurPParamsL

prevPParamsConwayGovStateL :: Lens' (ConwayGovState era) (PParams era)
prevPParamsConwayGovStateL = cgRatifyL . rsEnactStateL . ensPrevPParamsL

instance EraPParams era => DecCBOR (ConwayGovState era) where
  decCBOR =
    decode $
      RecD ConwayGovState
        <! From
        <! From

instance EraPParams era => EncCBOR (ConwayGovState era) where
  encCBOR ConwayGovState {..} =
    encode $
      Rec ConwayGovState
        !> To cgGovActionsState
        !> To cgRatify

instance EraPParams era => ToCBOR (ConwayGovState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => Default (ConwayGovState era)

instance EraPParams era => NFData (ConwayGovState era)

instance EraPParams era => NoThunks (ConwayGovState era)

instance EraPParams era => ToJSON (ConwayGovState era) where
  toJSON = object . toConwayGovPairs
  toEncoding = pairs . mconcat . toConwayGovPairs

toConwayGovPairs :: (KeyValue a, EraPParams era) => ConwayGovState era -> [a]
toConwayGovPairs cg@(ConwayGovState _ _) =
  let ConwayGovState {..} = cg
   in [ "gov" .= cgGovActionsState
      , "ratify" .= cgRatify
      ]

instance EraPParams (ConwayEra c) => EraGov (ConwayEra c) where
  type GovState (ConwayEra c) = ConwayGovState (ConwayEra c)

  getConstitution g = Just $ g ^. cgRatifyL . rsEnactStateL . ensConstitutionL

  curPParamsGovStateL = curPParamsConwayGovStateL

  prevPParamsGovStateL = prevPParamsConwayGovStateL

class EraGov era => ConwayEraGov era where
  constitutionGovStateL :: Lens' (GovState era) (Constitution era)

instance Crypto c => ConwayEraGov (ConwayEra c) where
  constitutionGovStateL = cgRatifyL . rsEnactStateL . ensConstitutionL
