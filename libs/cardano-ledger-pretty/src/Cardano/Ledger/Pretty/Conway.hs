{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pretty.Conway
  ( ppConwayTxBody,
  )
where

import Cardano.Ledger.Babbage.TxBody
  ( AllegraEraTxBody (..),
    AlonzoEraTxBody (..),
    BabbageEraTxBody (..),
    MaryEraTxBody (..),
    ShelleyEraTxBody (..),
  )
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert (..), transDCert)
import Cardano.Ledger.Conway.Governance
  ( GovernanceAction (..),
    GovernanceActionId (..),
    GovernanceActionInfo (..),
    GovernanceActionIx (..),
    Vote (..),
    VoteDecision (..),
    VoterRole (..),
  )
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Core (EraPParams (..), EraTxBody (..), Value)
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppAuxiliaryDataHash,
    ppCoin,
    ppConstitutionalDelegCert,
    ppDelegCert,
    ppKeyHash,
    ppNetwork,
    ppPoolCert,
    ppRecord,
    ppSafeHash,
    ppSet,
    ppSexp,
    ppStrictMaybe,
    ppStrictSeq,
    ppTxId,
    ppTxIn,
    ppTxOut,
    ppUrl,
    ppWdrl,
    ppWord64,
  )
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppValidityInterval)
import Lens.Micro ((^.))

instance
  ( ConwayEraTxBody era,
    PrettyA (Value era),
    TxBody era ~ ConwayTxBody era,
    PrettyA (PParamsUpdate era)
  ) =>
  PrettyA (ConwayTxBody era)
  where
  prettyA = ppConwayTxBody

ppConwayDCert :: ConwayDCert c -> PDoc
ppConwayDCert (ConwayDCertDeleg dc) = ppSexp "ConwayDCertDeleg" [ppDelegCert dc]
ppConwayDCert (ConwayDCertPool pc) = ppSexp "ConwayDCertPool" [ppPoolCert pc]
ppConwayDCert (ConwayDCertConstitutional gdc) = ppSexp "ConwayDCertConstitutional" [ppConstitutionalDelegCert gdc]

ppConwayTxBody ::
  forall era.
  ( ConwayEraTxBody era,
    PrettyA (Value era),
    TxBody era ~ ConwayTxBody era,
    PrettyA (GovernanceActionInfo era)
  ) =>
  ConwayTxBody era ->
  PDoc
ppConwayTxBody txb =
  ppRecord
    "TxBody (Conway)"
    [ ("spending inputs", ppSet ppTxIn $ txb ^. inputsTxBodyL),
      ("collateral inputs", ppSet ppTxIn $ txb ^. collateralInputsTxBodyL),
      ("reference inputs", ppSet ppTxIn $ txb ^. referenceInputsTxBodyL),
      ("outputs", ppStrictSeq (ppTxOut @era) (txb ^. outputsTxBodyL)),
      ("collateral return", ppStrictMaybe (ppTxOut @era) (txb ^. collateralReturnTxBodyL)),
      ("total collateral", ppStrictMaybe ppCoin $ txb ^. totalCollateralTxBodyL),
      ("certificates", ppStrictSeq ppConwayDCert $ txb ^. conwayCertsTxBodyL),
      ("withdrawals", ppWdrl $ txb ^. wdrlsTxBodyL),
      ("transaction fee", ppCoin $ txb ^. feeTxBodyL),
      ("validity interval", ppValidityInterval $ txb ^. vldtTxBodyL),
      ("required signer hashes", ppSet ppKeyHash $ txb ^. reqSignerHashesTxBodyL),
      ("mint", ppMultiAsset $ txb ^. mintTxBodyL),
      ("script integrity hash", ppStrictMaybe ppSafeHash $ txb ^. scriptIntegrityHashTxBodyL),
      ("auxiliary data hash", ppStrictMaybe ppAuxiliaryDataHash $ txb ^. auxDataHashTxBodyL),
      ("network id", ppStrictMaybe ppNetwork $ txb ^. networkIdTxBodyL),
      ("governance actions", ppStrictSeq prettyA $ txb ^. govActionsTxBodyL),
      ("votes", ppStrictSeq prettyA $ txb ^. votesTxBodyL)
    ]

ppGovernanceActionIx :: GovernanceActionIx -> PDoc
ppGovernanceActionIx (GovernanceActionIx idx) = ppWord64 idx

ppGovernanceActionId :: GovernanceActionId era -> PDoc
ppGovernanceActionId GovernanceActionId {..} =
  ppRecord
    "GovernanceActionId"
    [ ("transaction id", ppTxId gaidTxId),
      ("governance action index", ppGovernanceActionIx gaidGovActionIx)
    ]

ppVoterRole :: VoterRole -> PDoc
ppVoterRole ConstitutionalCommittee = "constitutional committee"
ppVoterRole DRep = "DRep"
ppVoterRole SPO = "SPO"

ppVoteDecision :: VoteDecision -> PDoc
ppVoteDecision No = "no"
ppVoteDecision Yes = "yes"
ppVoteDecision Abstain = "abstain"

instance PrettyA (Vote era) where
  prettyA Vote {..} =
    ppRecord
      "Vote"
      [ ("governance action ID", ppGovernanceActionId voteGovActionId),
        ("voter role", ppVoterRole voteRole),
        ("vote role key hash", ppKeyHash voteRoleKeyHash),
        ("vote metadata URL", ppUrl voteMetadataURL),
        ("vote metadata hash", ppSafeHash voteMetadataHash),
        ("vote decision", ppVoteDecision voteDecision)
      ]

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceAction era) where
  prettyA (ParameterChange ppup) =
    ppRecord "ParameterChange" $
      [("protocol parameters update", prettyA ppup)]
  prettyA (HardForkInitiation pv) =
    ppRecord "HardForkInitiation" $
      [("protocol version", prettyA pv)]
  prettyA (TreasuryWithdrawals ws) =
    ppRecord "TreasuryWithdrawals" $
      [("withdrawals map", prettyA ws)]

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceActionInfo era) where
  prettyA GovernanceActionInfo {..} =
    ppRecord
      "GovernanceActionInfo"
      [ ("deposit amount", ppCoin gaiDepositAmount),
        ("reward address", ppKeyHash gaiRewardAddress),
        ("metadata URL", ppUrl gaiMetadataURL),
        ("metadata hash", ppSafeHash gaiMetadataHash),
        ("governance action", prettyA gaiAction)
      ]

instance forall c. PrettyA (ConwayDCert c) where
  prettyA = prettyA . transDCert
