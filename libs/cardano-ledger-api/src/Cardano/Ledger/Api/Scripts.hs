{-# LANGUAGE PatternSynonyms #-}

module Cardano.Ledger.Api.Scripts (
  module Cardano.Ledger.Api.Scripts.Data,
  EraScript (Script, NativeScript),
  ScriptHash,
  scriptPrefixTag,
  upgradeScript,
  hashScript,
  getNativeScript,
  validateNativeScript,
  isNativeScript,
  ValidityInterval (..),

  -- * Alonzo
  AlonzoEraScript (
    PlutusScript,
    PlutusPurpose,
    toSpendingPurpose,
    toMintingPurpose,
    toCertifyingPurpose,
    toRewardingPurpose
  ),
  isPlutusScript,
  pattern SpendingPurpose,
  pattern MintingPurpose,
  pattern CertifyingPurpose,
  pattern RewardingPurpose,
  CostModels,

  -- * Conway
  ConwayEraScript (
    toVotingPurpose,
    toProposingPurpose
  ),
  pattern VotingPurpose,
  pattern ProposingPurpose,
)
where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  CostModels,
  isPlutusScript,
  pattern CertifyingPurpose,
  pattern MintingPurpose,
  pattern RewardingPurpose,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Api.Scripts.Data
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
  pattern ProposingPurpose,
  pattern VotingPurpose,
 )
import Cardano.Ledger.Core (EraScript (..), hashScript, isNativeScript, validateNativeScript)
import Cardano.Ledger.Hashes (ScriptHash)
