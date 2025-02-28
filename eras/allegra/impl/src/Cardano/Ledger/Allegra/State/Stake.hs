{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.State.Stake () where

import Cardano.Ledger.Allegra.Core ()
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Shelley.State (
  EraStake (..),
  ShelleyInstantStake,
  addShelleyInstantStake,
  deleteShelleyInstantStake,
  resolveShelleyInstantStake,
  shelleyInstantStakeCredentialsL,
 )

instance EraStake AllegraEra where
  type InstantStake AllegraEra = ShelleyInstantStake AllegraEra
  instantStakeCredentialsL = shelleyInstantStakeCredentialsL
  addInstantStake = addShelleyInstantStake
  deleteInstantStake = deleteShelleyInstantStake
  resolveInstantStake = resolveShelleyInstantStake
