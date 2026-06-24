{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
  pattern AnyEraSpendingPurpose,
  pattern AnyEraMintingPurpose,
  pattern AnyEraCertifyingPurpose,
  pattern AnyEraWithdrawingPurpose,
  pattern AnyEraRewardingPurpose,
  pattern AnyEraVotingPurpose,
  pattern AnyEraProposingPurpose,
  pattern AnyEraGuardingPurpose,

  -- * Alonzo
  AlonzoEraScript (
    PlutusScript,
    PlutusPurpose,
    toSpendingPurpose,
    toMintingPurpose,
    toCertifyingPurpose,
    toWithdrawingPurpose,
    toRewardingPurpose
  ),
  isPlutusScript,
  pattern SpendingPurpose,
  pattern MintingPurpose,
  pattern CertifyingPurpose,
  pattern WithdrawingPurpose,
  pattern RewardingPurpose,
  CostModels,

  -- * Conway
  ConwayEraScript (
    toVotingPurpose,
    toProposingPurpose
  ),
  pattern VotingPurpose,
  pattern ProposingPurpose,

  -- * Dijkstra
  DijkstraEraScript (
    toGuardingPurpose
  ),
  pattern GuardingPurpose,
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
) where

import Cardano.Ledger.Address (AccountAddress)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  CostModels,
  isPlutusScript,
  pattern CertifyingPurpose,
  pattern MintingPurpose,
  pattern RewardingPurpose,
  pattern SpendingPurpose,
  pattern WithdrawingPurpose,
 )
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Scripts.Data
import Cardano.Ledger.Conway.Governance (ProposalProcedure, Voter)
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (..),
  pattern ProposingPurpose,
  pattern VotingPurpose,
 )
import Cardano.Ledger.Core (
  EraScript (..),
  TxCert,
  hashScript,
  isNativeScript,
  validateNativeScript,
 )
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraEraScript (..),
  pattern GuardingPurpose,
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

  anyEraToWithdrawingPurpose :: PlutusPurpose f era -> Maybe (f Word32 AccountAddress)
  default anyEraToWithdrawingPurpose ::
    AlonzoEraScript era => PlutusPurpose f era -> Maybe (f Word32 AccountAddress)
  anyEraToWithdrawingPurpose = toWithdrawingPurpose

  anyEraToRewardingPurpose :: PlutusPurpose f era -> Maybe (f Word32 AccountAddress)
  anyEraToRewardingPurpose = anyEraToWithdrawingPurpose

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

{-# DEPRECATED anyEraToRewardingPurpose "In favor of `anyEraToWithdrawingPurpose`" #-}

instance AnyEraScript ShelleyEra where
  anyEraMaxLanguage = Nothing
  anyEraToPlutusScript = const Nothing
  anyEraToSpendingPurpose = const Nothing
  anyEraToMintingPurpose = const Nothing
  anyEraToCertifyingPurpose = const Nothing
  anyEraToWithdrawingPurpose = const Nothing
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript AllegraEra where
  anyEraMaxLanguage = Nothing
  anyEraToPlutusScript = const Nothing
  anyEraToSpendingPurpose = const Nothing
  anyEraToMintingPurpose = const Nothing
  anyEraToCertifyingPurpose = const Nothing
  anyEraToWithdrawingPurpose = const Nothing
  anyEraToVotingPurpose = const Nothing
  anyEraToProposingPurpose = const Nothing
  anyEraToGuardingPurpose = const Nothing

instance AnyEraScript MaryEra where
  anyEraMaxLanguage = Nothing
  anyEraToPlutusScript = const Nothing
  anyEraToSpendingPurpose = const Nothing
  anyEraToMintingPurpose = const Nothing
  anyEraToCertifyingPurpose = const Nothing
  anyEraToWithdrawingPurpose = const Nothing
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

pattern AnyEraWithdrawingPurpose ::
  AnyEraScript era => f Word32 AccountAddress -> PlutusPurpose f era
pattern AnyEraWithdrawingPurpose c <- (anyEraToWithdrawingPurpose -> Just c)

pattern AnyEraRewardingPurpose ::
  AnyEraScript era => f Word32 AccountAddress -> PlutusPurpose f era
pattern AnyEraRewardingPurpose c <- (anyEraToWithdrawingPurpose -> Just c)
{-# DEPRECATED AnyEraRewardingPurpose "In favor of `AnyEraWithdrawingPurpose`" #-}

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
  , AnyEraWithdrawingPurpose
  , AnyEraVotingPurpose
  , AnyEraProposingPurpose
  , AnyEraGuardingPurpose
  #-}
