{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.HardForks (
  aggregatedRewards,
  allowMIRTransfer,
  validatePoolRewardAccountNetID,
  forgoRewardPrefilter,
  translateUpperBoundForPlutusScripts,
  forgoPointerAddressResolution,
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)

aggregatedRewards ::
  ProtVer ->
  Bool
aggregatedRewards pv = pvMajor pv > natVersion @2

-- | Starting with protocol version 5, the MIR certs will also be
-- able to transfer funds between the reserves and the treasury.
-- Additionally, the semantics for the pervious functionality will
-- change a bit. Before version 5 redundancies in the instantaneous
-- reward mapping were handled by overriding. Now they are handled
-- by adding the values and allowing for negatives updates, provided
-- the sum for each key remains positive.
allowMIRTransfer ::
  ProtVer ->
  Bool
allowMIRTransfer pv = pvMajor pv > natVersion @4

-- | Starting with protocol version 5, we will validate the network ID
-- for the reward account listed in stake pool registration certificates.
validatePoolRewardAccountNetID ::
  ProtVer ->
  Bool
validatePoolRewardAccountNetID pv = pvMajor pv > natVersion @4

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
