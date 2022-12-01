{-# LANGUAGE DeriveGeneric #-}

module Cardano.Ledger.Conway.Core
  ( ConwayEraTxBody (..),
    GovernanceAction,
    Vote,
  )
where

import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, Era, EraTxBody (..))
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Control.DeepSeq (NFData)
import Data.Sequence.Strict (StrictSeq)
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)

class BabbageEraTxBody era => ConwayEraTxBody era where
  govActionsL :: Lens' (TxBody era) (StrictSeq (GovernanceAction era))
  votesL :: Lens' (TxBody era) (StrictSeq (Vote era))

----- PLACEHOLDERS -----

data GovernanceAction era = GovernanceAction
  deriving (Generic, Eq)

instance NoThunks (GovernanceAction era)

instance NFData (GovernanceAction era)

instance Show (GovernanceAction era) where
  show = undefined

instance Era era => FromCBOR (GovernanceAction era) where
  fromCBOR = undefined

instance Era era => ToCBOR (GovernanceAction era) where
  toCBOR = undefined

data Vote era = Vote
  deriving (Generic, Eq)

instance NoThunks (Vote era)

instance NFData (Vote era)

instance Show (Vote era) where
  show = undefined

instance Era era => FromCBOR (Vote era) where
  fromCBOR = undefined

instance Era era => ToCBOR (Vote era) where
  toCBOR = undefined
