{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Core
  ( ConwayEraTxBody (..),
    GovernanceActionInfo (..),
    GovernanceAction (..),
    Vote (..),
    VoteDecision (..),
  )
where

import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, Era (..), EraTxBody (..))
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Control.DeepSeq (NFData)
import Data.Sequence.Strict (StrictSeq)
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)
import Cardano.Ledger.Coin (Coin(..))
import Cardano.Ledger.Keys (KeyHash(..), KeyRole (..))
import Cardano.Ledger.Shelley.TxBody (Url)
import Cardano.Ledger.SafeHash (SafeHash)
import Data.ByteString (ByteString)
import Cardano.Ledger.Core (EraPParams(..))
import Cardano.Ledger.BaseTypes (TxIx(..))

class BabbageEraTxBody era => ConwayEraTxBody era where
  govActionsL :: Lens' (TxBody era) (StrictSeq (GovernanceActionInfo era))
  votesL :: Lens' (TxBody era) (StrictSeq (Vote era))

----- PLACEHOLDERS -----

data GovernanceActionInfo era = GovernanceActionInfo
  { gaDepositAmount :: Coin,
    gaRewardAddress :: KeyHash 'Staking (EraCrypto era),
    gaMetadataURL :: Url,
    gaMetadataHash :: SafeHash (EraCrypto era) ByteString,
    gaAction :: GovernanceAction era
  }
  deriving (Generic)

deriving instance Eq (PParamsUpdate era) => Eq (GovernanceActionInfo era)

deriving instance Show (PParamsUpdate era) => Show (GovernanceActionInfo era)

instance NoThunks (PParamsUpdate era) => NoThunks (GovernanceActionInfo era)

instance NFData (PParamsUpdate era) => NFData (GovernanceActionInfo era)

instance Era era => FromCBOR (GovernanceActionInfo era) where
  fromCBOR = undefined

instance Era era => ToCBOR (GovernanceActionInfo era) where
  toCBOR = undefined

data GovernanceAction era
  = ParameterChange (PParamsUpdate era)
  | HardForkInitiation
  | TreasuryWithdrawals
  deriving (Generic)

deriving instance Eq (PParamsUpdate era) => Eq (GovernanceAction era)

deriving instance Show (PParamsUpdate era) => Show (GovernanceAction era)

instance NoThunks (PParamsUpdate era) => NoThunks (GovernanceAction era)

instance NFData (PParamsUpdate era) => NFData (GovernanceAction era)

data Vote era = Vote
  { voteGovActionID :: TxIx,
    voteRole :: VoterRole,
    voteRoleKeyHash :: KeyHash 'Voting (EraCrypto era),
    voteMetadataURL :: Url,
    voteMetadataHash :: SafeHash (EraCrypto era) ByteString,
    voteDecision :: VoteDecision
  }
  deriving (Generic, Eq)

data VoterRole
  = ConstitutionalCommittee
  | DRep
  | SPO
  deriving (Generic, Eq)

instance NoThunks VoterRole

instance NFData VoterRole

data VoteDecision
  = Yes
  | No
  | Abstain
  deriving (Generic, Eq)

instance NoThunks VoteDecision

instance NFData VoteDecision

instance NoThunks (Vote era)

instance NFData (Vote era)

instance Show (Vote era) where
  show = undefined

instance Era era => FromCBOR (Vote era) where
  fromCBOR = undefined

instance Era era => ToCBOR (Vote era) where
  toCBOR = undefined
