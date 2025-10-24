-- | This module is deprecated and will be removed in a future version.
-- Please import these types from "Cardano.Ledger.State" instead.
--
-- The contents of this module have been moved to "Cardano.Ledger.State.StakePool"
-- to provide better separation between pool registration parameters and pool state
-- management. All types are now re-exported through "Cardano.Ledger.State" for
-- convenient access.
module Cardano.Ledger.PoolParams
  {-# DEPRECATED "Use `Cardano.Ledger.State` instead." #-} (
  StakePoolParams (..),
  PoolMetadata (..),
  StakePoolRelay (..),
  SizeOfPoolRelays (..),
  SizeOfPoolOwners (..),
) where

import Cardano.Ledger.State.StakePool (
  PoolMetadata (..),
  SizeOfPoolOwners (..),
  SizeOfPoolRelays (..),
  StakePoolParams (..),
  StakePoolRelay (..),
 )
