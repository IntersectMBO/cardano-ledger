{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.State.VState (
  VState (..),
  vsDRepsL,
  vsCommitteeStateL,
  vsNumDormantEpochsL,
  vsActualDRepExpiry,
  lookupDepositVState,
)
where

import Cardano.Ledger.BaseTypes (binOpEpochNo)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decNoShareCBOR,
  decodeMap,
  interns,
  internsFromSet,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))

-- | The state that tracks the voting entities (DReps and Constitutional Committee
-- members). In the formal ledger specification this type is called @GState@
data VState era = VState
  { vsDReps :: !(Map (Credential 'DRepRole) DRepState)
  , vsCommitteeState :: !(CommitteeState era)
  , vsNumDormantEpochs :: !EpochNo
  -- ^ Number of contiguous epochs in which there are exactly zero
  -- active governance proposals to vote on. It is incremented in every
  -- EPOCH rule if the number of active governance proposals to vote on
  -- continues to be zero. It is reset to zero when a new governance
  -- action is successfully proposed. We need this counter in order to
  -- bump DRep expiries through dormant periods when DReps do not have
  -- an opportunity to vote on anything.
  }
  deriving (Show, Eq, Generic)

-- | Function that looks up the deposit for currently registered DRep
lookupDepositVState :: VState era -> Credential 'DRepRole -> Maybe Coin
lookupDepositVState vstate = fmap drepDeposit . flip Map.lookup (vstate ^. vsDRepsL)

instance Default (VState era) where
  def = VState def def (EpochNo 0)

instance NoThunks (VState era)

instance NFData (VState era)

instance Era era => DecShareCBOR (VState era) where
  type
    Share (VState era) =
      ( Interns (Credential 'Staking)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  getShare VState {vsDReps, vsCommitteeState} =
    (internsFromSet (foldMap drepDelegs vsDReps), fst (getShare vsDReps), getShare vsCommitteeState)
  decShareCBOR (cs, cd, _) =
    decode $
      RecD VState
        <! D (decodeMap (interns cd <$> decCBOR) (decShareCBOR cs))
        <! D decNoShareCBOR
        <! From

instance Era era => DecCBOR (VState era) where
  decCBOR = decNoShareCBOR

instance Era era => EncCBOR (VState era) where
  encCBOR VState {..} =
    encode $
      Rec (VState @era)
        !> To vsDReps
        !> To vsCommitteeState
        !> To vsNumDormantEpochs

instance ToJSON (VState era) where
  toJSON = object . toVStatePair
  toEncoding = pairs . mconcat . toVStatePair

toVStatePair :: KeyValue e a => VState era -> [a]
toVStatePair vs@(VState _ _ _) =
  let VState {..} = vs
   in [ "dreps" .= vsDReps
      , "committeeState" .= vsCommitteeState
      , "numDormantEpochs" .= vsNumDormantEpochs
      ]

vsDRepsL :: Lens' (VState era) (Map (Credential 'DRepRole) DRepState)
vsDRepsL = lens vsDReps (\vs u -> vs {vsDReps = u})

vsCommitteeStateL :: Lens' (VState era) (CommitteeState era)
vsCommitteeStateL = lens vsCommitteeState (\vs u -> vs {vsCommitteeState = u})

vsNumDormantEpochsL :: Lens' (VState era) EpochNo
vsNumDormantEpochsL = lens vsNumDormantEpochs (\vs u -> vs {vsNumDormantEpochs = u})

vsActualDRepExpiry :: Credential 'DRepRole -> VState era -> Maybe EpochNo
vsActualDRepExpiry cred vs =
  binOpEpochNo (+) (vsNumDormantEpochs vs) . drepExpiry <$> Map.lookup cred (vsDReps vs)
