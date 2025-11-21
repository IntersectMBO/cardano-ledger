{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
  unDelegReDelegDRep,
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..), binOpEpochNo)
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
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.State.Account
import Cardano.Ledger.Conway.State.DRep (DRep (DRepCredential), DRepState, drepDelegs, drepDelegsL, drepDeposit, drepExpiry)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), (.=))
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- | The state that tracks the voting entities (DReps and Constitutional Committee
-- members). In the formal ledger specification this type is called @GState@
data VState era = VState
  { vsDReps :: !(Map (Credential DRepRole) DRepState)
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
  deriving (ToJSON) via KeyValuePairs (VState era)

-- | Function that looks up the deposit for currently registered DRep
lookupDepositVState :: VState era -> Credential DRepRole -> Maybe Coin
lookupDepositVState vstate = fmap (fromCompact . drepDeposit) . flip Map.lookup (vstate ^. vsDRepsL)

instance Default (VState era) where
  def = VState def def (EpochNo 0)

instance NoThunks (VState era)

instance NFData (VState era)

instance Era era => DecShareCBOR (VState era) where
  type
    Share (VState era) =
      ( Interns (Credential Staking)
      , Interns (Credential DRepRole)
      , Interns (Credential HotCommitteeRole)
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

instance ToKeyValuePairs (VState era) where
  toKeyValuePairs vs@(VState _ _ _) =
    let VState {..} = vs
     in [ "dreps" .= vsDReps
        , "committeeState" .= vsCommitteeState
        , "numDormantEpochs" .= vsNumDormantEpochs
        ]

-- | Reverses DRep delegation.
-- To be called when a stake credential is unregistered or its delegation target changes.
-- If the new delegation matches the previous one, this is a noop.
unDelegReDelegDRep ::
  ConwayEraAccounts era =>
  Credential Staking ->
  -- | Account that is losing its current delegation and/or acquiring a new one
  AccountState era ->
  -- | Potential new delegation. In case when stake credential unregisters this must be `Nothing`.
  Maybe DRep ->
  VState era ->
  VState era
unDelegReDelegDRep stakeCred accountState mNewDRep =
  fromMaybe (vsDRepsL %~ addNewDelegation) $ do
    dRep@(DRepCredential dRepCred) <- accountState ^. dRepDelegationAccountStateL
    pure $
      -- There is no need to update set of delegations if delegation is unchanged
      if Just dRep == mNewDRep
        then id
        else
          vsDRepsL %~ addNewDelegation . Map.adjust (drepDelegsL %~ Set.delete stakeCred) dRepCred
  where
    addNewDelegation =
      case mNewDRep of
        Just (DRepCredential dRepCred) ->
          Map.adjust (drepDelegsL %~ Set.insert stakeCred) dRepCred
        _ -> id

vsDRepsL :: Lens' (VState era) (Map (Credential DRepRole) DRepState)
vsDRepsL = lens vsDReps (\vs u -> vs {vsDReps = u})

vsCommitteeStateL :: Lens' (VState era) (CommitteeState era)
vsCommitteeStateL = lens vsCommitteeState (\vs u -> vs {vsCommitteeState = u})

vsNumDormantEpochsL :: Lens' (VState era) EpochNo
vsNumDormantEpochsL = lens vsNumDormantEpochs (\vs u -> vs {vsNumDormantEpochs = u})

vsActualDRepExpiry :: Credential DRepRole -> VState era -> Maybe EpochNo
vsActualDRepExpiry cred vs =
  binOpEpochNo (+) (vsNumDormantEpochs vs) . drepExpiry <$> Map.lookup cred (vsDReps vs)
