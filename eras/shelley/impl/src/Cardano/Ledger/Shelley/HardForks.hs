{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Shelley.HardForks
  ( aggregatedRewards,
    allowMIRTransfer,
    validatePoolRewardAccountNetID,
    allowScriptStakeCredsToEarnRewards,
    translateTimeForPlutusScripts,
    missingScriptsSymmetricDifference,
    forgoRewardPrefilter,
    allowOutsideForecastTTL,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer (..))

aggregatedRewards ::
  ProtVer ->
  Bool
aggregatedRewards pv = pvMajor pv > 2

-- | Starting with protocol version 5, the MIR certs will also be
-- able to transfer funds between the reserves and the treasury.
-- Additionally, the semantics for the pervious functionality will
-- change a bit. Before version 5 redundancies in the instantaneous
-- reward mapving were handled by overriding. Now they are handled
-- by adding the values and allowing for negatives updates, provided
-- the sum for each key remains positive.
allowMIRTransfer ::
  ProtVer ->
  Bool
allowMIRTransfer pv = pvMajor pv > 4

-- | Starting with protocol version 5, we will validate the network ID
-- for the reward account listed in stake pool registration certificates.
validatePoolRewardAccountNetID ::
  ProtVer ->
  Bool
validatePoolRewardAccountNetID pv = pvMajor pv > 4

-- | Starting with protocol version 5, Stake Credentials bound by scripts
-- will be eligibile for staking rewards.
allowScriptStakeCredsToEarnRewards ::
  ProtVer ->
  Bool
allowScriptStakeCredsToEarnRewards pv = pvMajor pv > 4

-- | Starting with protocol version 6, we translate slots to time correctly for
-- Plutus scripts.
translateTimeForPlutusScripts ::
  ProtVer ->
  Bool
translateTimeForPlutusScripts pv = pvMajor pv > 5

-- | Starting with protocol version 7, the UTXO rule predicate failure
-- MissingScriptWitnessesUTXOW will not be used for extraneous scripts
missingScriptsSymmetricDifference ::
  ProtVer ->
  Bool
missingScriptsSymmetricDifference pv = pvMajor pv > 6

-- | Starting with protocol version 7, the reward calculation no longer
-- filters out unregistered stake addresses at the moment the calculation begins.
-- See the Shelley Ledger Errata 17.2.
forgoRewardPrefilter ::
  ProtVer ->
  Bool
forgoRewardPrefilter pv = pvMajor pv > 6

-- | In versions 5 and 6, we allow the ttl field to lie outside the stability
-- window.
allowOutsideForecastTTL ::
  ProtVer ->
  Bool
allowOutsideForecastTTL pv =
  let mv = pvMajor pv
   in mv == 5 || mv == 6
