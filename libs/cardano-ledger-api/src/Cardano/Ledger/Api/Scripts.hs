{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

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

  -- * Any era
  AnyEraScript (..),
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  pattern AnyEraSpendingPurpose,
  pattern AnyEraMintingPurpose,
  pattern AnyEraCertifyingPurpose,
  pattern AnyEraRewardingPurpose,
  pattern AnyEraVotingPurpose,
  pattern AnyEraProposingPurpose,
  pattern AnyEraGuardingPurpose,
#else
  data AnyEraSpendingPurpose,
  data AnyEraMintingPurpose,
  data AnyEraCertifyingPurpose,
  data AnyEraRewardingPurpose,
  data AnyEraVotingPurpose,
  data AnyEraProposingPurpose,
  data AnyEraGuardingPurpose,
#endif
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
#if __GLASGOW_HASKELL__ < 914
  pattern SpendingPurpose,
  pattern MintingPurpose,
  pattern CertifyingPurpose,
  pattern RewardingPurpose,
#else
  data SpendingPurpose,
  data MintingPurpose,
  data CertifyingPurpose,
  data RewardingPurpose,
#endif
  CostModels,

  -- * Conway
  ConwayEraScript (
    toVotingPurpose,
    toProposingPurpose
  ),
#if __GLASGOW_HASKELL__ < 914
  pattern VotingPurpose,
  pattern ProposingPurpose,
#else
  data VotingPurpose,
  data ProposingPurpose,
#endif

  -- * Dijkstra
  DijkstraEraScript (
    toGuardingPurpose
  ),
#if __GLASGOW_HASKELL__ < 914
  pattern GuardingPurpose,
#else
  data GuardingPurpose,
#endif
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  CostModels,
  isPlutusScript,
#if __GLASGOW_HASKELL__ < 914
  pattern CertifyingPurpose,
  pattern MintingPurpose,
  pattern RewardingPurpose,
  pattern SpendingPurpose,
#else
  data CertifyingPurpose,
  data MintingPurpose,
  data RewardingPurpose,
  data SpendingPurpose,
#endif
 )
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Scripts.Data
import Cardano.Ledger.Conway.Governance (ProposalProcedure, Voter)
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
#if __GLASGOW_HASKELL__ < 914
  pattern ProposingPurpose,
  pattern VotingPurpose,
#else
  data ProposingPurpose,
  data VotingPurpose,
#endif
 )
import Cardano.Ledger.Core (
  EraScript (..),
  TxCert,
  hashScript,
  isNativeScript,
  validateNativeScript,
 )
import Cardano.Ledger.Dijkstra.Scripts (
  DijkstraEraScript (..),
#if __GLASGOW_HASKELL__ < 914
  pattern GuardingPurpose,
#else
  data GuardingPurpose,
#endif
 )
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.Plutus (Language)
import Cardano.Ledger.TxIn (TxIn)
import Data.Word (Word32)

class EraScript era => AnyEraScript era where
  anyEraMaxLanguage :: Maybe Language
  default anyEraMaxLanguage ::
    AlonzoEraScript era => Maybe Language
  anyEraMaxLanguage = Just (eraMaxLanguage @era)

  anyEraToPlutusScript :: Script era -> Maybe (PlutusScript era)
  default anyEraToPlutusScript ::
    AlonzoEraScript era => Script era -> Maybe (PlutusScript era)
  anyEraToPlutusScript = toPlutusScript

  anyEraToSpendingPurpose :: PlutusPurpose f era -> Maybe (f Word32 TxIn)
  default anyEraToSpendingPurpose ::
    AlonzoEraScript era => PlutusPurpose f era -> Maybe (f Word32 TxIn)
  anyEraToSpendingPurpose = toSpendingPurpose

  anyEraToMintingPurpose :: PlutusPurpose f era -> Maybe (f Word32 PolicyID)
  default anyEraToMintingPurpose ::
    AlonzoEraScript era => PlutusPurpose f era -> Maybe (f Word32 PolicyID)
  anyEraToMintingPurpose = toMintingPurpose

  anyEraToCertifyingPurpose :: PlutusPurpose f era -> Maybe (f Word32 (TxCert era))
  default anyEraToCertifyingPurpose ::
    AlonzoEraScript era => PlutusPurpose f era -> Maybe (f Word32 (TxCert era))
  anyEraToCertifyingPurpose = toCertifyingPurpose

  anyEraToRewardingPurpose :: PlutusPurpose f era -> Maybe (f Word32 RewardAccount)
  default anyEraToRewardingPurpose ::
    AlonzoEraScript era => PlutusPurpose f era -> Maybe (f Word32 RewardAccount)
  anyEraToRewardingPurpose = toRewardingPurpose

  anyEraToVotingPurpose :: PlutusPurpose f era -> Maybe (f Word32 Voter)
  default anyEraToVotingPurpose ::
    ConwayEraScript era => PlutusPurpose f era -> Maybe (f Word32 Voter)
  anyEraToVotingPurpose = toVotingPurpose

  anyEraToProposingPurpose :: PlutusPurpose f era -> Maybe (f Word32 (ProposalProcedure era))
  default anyEraToProposingPurpose ::
    ConwayEraScript era => PlutusPurpose f era -> Maybe (f Word32 (ProposalProcedure era))
  anyEraToProposingPurpose = toProposingPurpose

  anyEraToGuardingPurpose :: PlutusPurpose f era -> Maybe (f Word32 ScriptHash)
  default anyEraToGuardingPurpose ::
    DijkstraEraScript era => PlutusPurpose f era -> Maybe (f Word32 ScriptHash)
  anyEraToGuardingPurpose = toGuardingPurpose

instance AnyEraScript ShelleyEra where
  anyEraMaxLanguage = Nothing
  anyEraToPlutusScript = const Nothing
  anyEraToSpendingPurpose = const Nothing
  anyEraToMintingPurpose = const Nothing
  anyEraToCertifyingPurpose = const Nothing
  anyEraToRewardingPurpose = const Nothing
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript AllegraEra where
  anyEraMaxLanguage = Nothing
  anyEraToPlutusScript = const Nothing
  anyEraToSpendingPurpose = const Nothing
  anyEraToMintingPurpose = const Nothing
  anyEraToCertifyingPurpose = const Nothing
  anyEraToRewardingPurpose = const Nothing
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript MaryEra where
  anyEraMaxLanguage = Nothing
  anyEraToPlutusScript = const Nothing
  anyEraToSpendingPurpose = const Nothing
  anyEraToMintingPurpose = const Nothing
  anyEraToCertifyingPurpose = const Nothing
  anyEraToRewardingPurpose = const Nothing
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript AlonzoEra where
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript BabbageEra where
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript ConwayEra where
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript DijkstraEra

pattern AnyEraSpendingPurpose ::
  AnyEraScript era => f Word32 TxIn -> PlutusPurpose f era
pattern AnyEraSpendingPurpose c <- (anyEraToSpendingPurpose -> Just c)

pattern AnyEraMintingPurpose ::
  AnyEraScript era => f Word32 PolicyID -> PlutusPurpose f era
pattern AnyEraMintingPurpose c <- (anyEraToMintingPurpose -> Just c)

pattern AnyEraCertifyingPurpose ::
  AnyEraScript era => f Word32 (TxCert era) -> PlutusPurpose f era
pattern AnyEraCertifyingPurpose c <- (anyEraToCertifyingPurpose -> Just c)

pattern AnyEraRewardingPurpose ::
  AnyEraScript era => f Word32 RewardAccount -> PlutusPurpose f era
pattern AnyEraRewardingPurpose c <- (anyEraToRewardingPurpose -> Just c)

pattern AnyEraVotingPurpose ::
  AnyEraScript era => f Word32 Voter -> PlutusPurpose f era
pattern AnyEraVotingPurpose c <- (anyEraToVotingPurpose -> Just c)

pattern AnyEraProposingPurpose ::
  AnyEraScript era => f Word32 (ProposalProcedure era) -> PlutusPurpose f era
pattern AnyEraProposingPurpose c <- (anyEraToProposingPurpose -> Just c)

pattern AnyEraGuardingPurpose ::
  AnyEraScript era => f Word32 ScriptHash -> PlutusPurpose f era
pattern AnyEraGuardingPurpose c <- (anyEraToGuardingPurpose -> Just c)

{-# COMPLETE
  AnyEraSpendingPurpose
  , AnyEraMintingPurpose
  , AnyEraCertifyingPurpose
  , AnyEraRewardingPurpose
  , AnyEraVotingPurpose
  , AnyEraProposingPurpose
  , AnyEraGuardingPurpose
  #-}
