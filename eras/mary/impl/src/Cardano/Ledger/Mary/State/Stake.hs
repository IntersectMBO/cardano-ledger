{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.State.Stake () where

import Cardano.Ledger.Mary.Core ()
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.State (
  EraStake (..),
  ShelleyInstantStake,
  addShelleyInstantStake,
  deleteShelleyInstantStake,
  resolveShelleyInstantStake,
  shelleyInstantStakeCredentialsL,
 )

instance EraStake MaryEra where
  type InstantStake MaryEra = ShelleyInstantStake MaryEra
  instantStakeCredentialsL = shelleyInstantStakeCredentialsL
  addInstantStake = addShelleyInstantStake
  deleteInstantStake = deleteShelleyInstantStake
  resolveInstantStake = resolveShelleyInstantStake
