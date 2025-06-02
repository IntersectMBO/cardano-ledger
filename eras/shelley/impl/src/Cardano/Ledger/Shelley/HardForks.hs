{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.HardForks (
  bootstrapPhase,
  forgoRewardPrefilter,
  translateUpperBoundForPlutusScripts,
  forgoPointerAddressResolution,
  disallowUnelectedCommitteeFromVoting,
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)

-- | Starting with protocol version 7, the reward calculation no longer
-- filters out unregistered stake addresses at the moment the calculation begins.
-- See the Shelley Ledger Errata 17.2.
forgoRewardPrefilter ::
  ProtVer ->
  Bool
forgoRewardPrefilter pv = pvMajor pv > natVersion @6

-- | Starting with protocol version 9, we translate the upper bound of validity interval
-- correctly for Plutus scripts.
--
-- TODO - After mainnet has successfully moved to protocol version 9, we can check
-- to see if the semantic difference here has even been exercised.
-- (We probably also need to check preprod and potentially preview.)
-- If it has not been exercised by version 9, we can safely remove this check
-- and always use the correct semantics (which cleans up the code).
translateUpperBoundForPlutusScripts ::
  ProtVer ->
  Bool
translateUpperBoundForPlutusScripts pv = pvMajor pv > natVersion @8

-- | Starting with protocol version 9, we no longer resolve pointer addresses.
forgoPointerAddressResolution ::
  ProtVer ->
  Bool
forgoPointerAddressResolution pv = pvMajor pv > natVersion @8

-- | Bootstrap phase
bootstrapPhase ::
  ProtVer ->
  Bool
bootstrapPhase pv = pvMajor pv == natVersion @9

-- | Starting with protocol version 11, we do not allow unelected committee
-- members to submit votes.
disallowUnelectedCommitteeFromVoting ::
  ProtVer ->
  Bool
disallowUnelectedCommitteeFromVoting pv = pvMajor pv > natVersion @10
