{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

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
#if __GLASGOW_HASKELL__ >= 914
  data SpendingPurpose,
  data MintingPurpose,
  data CertifyingPurpose,
  data RewardingPurpose,
#else
  pattern SpendingPurpose,
  pattern MintingPurpose,
  pattern CertifyingPurpose,
  pattern RewardingPurpose,
#endif
  CostModels,

  -- * Conway
  ConwayEraScript (
    toVotingPurpose,
    toProposingPurpose
  ),
#if __GLASGOW_HASKELL__ >= 914
  data VotingPurpose,
  data ProposingPurpose,
#else
  pattern VotingPurpose,
  pattern ProposingPurpose,
#endif
) where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  CostModels,
  isPlutusScript,
#if __GLASGOW_HASKELL__ >= 914
  data CertifyingPurpose,
  data MintingPurpose,
  data RewardingPurpose,
  data SpendingPurpose,
#else
  pattern CertifyingPurpose,
  pattern MintingPurpose,
  pattern RewardingPurpose,
  pattern SpendingPurpose,
#endif
 )
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Api.Scripts.Data
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
#if __GLASGOW_HASKELL__ >= 914
  data ProposingPurpose,
  data VotingPurpose,
#else
  pattern ProposingPurpose,
  pattern VotingPurpose,
#endif
 )
import Cardano.Ledger.Core (EraScript (..), hashScript, isNativeScript, validateNativeScript)
import Cardano.Ledger.Hashes (ScriptHash)
