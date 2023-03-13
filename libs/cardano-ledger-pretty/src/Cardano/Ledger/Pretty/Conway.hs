{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

module Cardano.Ledger.Pretty.Conway (
  ppConwayTxBody,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert (..), transDCert)
import Cardano.Ledger.Conway.Governance (
  ConwayGovernance (..),
  ConwayTallyState (..),
  GovernanceAction (..),
  GovernanceActionId (..),
  GovernanceActionIx (..),
  GovernanceActionState (..),
  VoteDecision (..),
  VoterRole (..),
 )
import Cardano.Ledger.Conway.Rules (
  ConwayLedgerPredFailure (..),
  ConwayTallyPredFailure,
  EnactState (..),
  GovernanceMetadata,
  GovernanceProcedure,
  PredicateFailure,
  RatifyState (..),
 )
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Pretty (
  PDoc,
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
  ppString,
  ppTxIn,
  ppWithdrawals,
 )
import Cardano.Ledger.Pretty.Babbage (ppBabbagePParams, ppBabbagePParamsUpdate)
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppValidityInterval)
import Lens.Micro ((^.))
import Prettyprinter (viaShow)

instance
  ( ConwayEraTxBody era
  , PrettyA (TxOut era)
  , TxBody era ~ ConwayTxBody era
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
  ( ConwayEraTxBody era
  , PrettyA (TxOut era)
  , TxBody era ~ ConwayTxBody era
  ) =>
  ConwayTxBody era ->
  PDoc
ppConwayTxBody txb =
  ppRecord
    "TxBody (Conway)"
    [ ("spending inputs", ppSet ppTxIn $ txb ^. inputsTxBodyL)
    , ("collateral inputs", ppSet ppTxIn $ txb ^. collateralInputsTxBodyL)
    , ("reference inputs", ppSet ppTxIn $ txb ^. referenceInputsTxBodyL)
    , ("outputs", ppStrictSeq prettyA (txb ^. outputsTxBodyL))
    , ("collateral return", ppStrictMaybe prettyA (txb ^. collateralReturnTxBodyL))
    , ("total collateral", ppStrictMaybe ppCoin $ txb ^. totalCollateralTxBodyL)
    , ("certificates", ppStrictSeq ppConwayDCert $ txb ^. conwayCertsTxBodyL)
    , ("withdrawals", ppWithdrawals $ txb ^. withdrawalsTxBodyL)
    , ("transaction fee", ppCoin $ txb ^. feeTxBodyL)
    , ("validity interval", ppValidityInterval $ txb ^. vldtTxBodyL)
    , ("required signer hashes", ppSet ppKeyHash $ txb ^. reqSignerHashesTxBodyL)
    , ("mint", ppMultiAsset $ txb ^. mintTxBodyL)
    , ("script integrity hash", ppStrictMaybe ppSafeHash $ txb ^. scriptIntegrityHashTxBodyL)
    , ("auxiliary data hash", ppStrictMaybe ppAuxiliaryDataHash $ txb ^. auxDataHashTxBodyL)
    , ("network id", ppStrictMaybe ppNetwork $ txb ^. networkIdTxBodyL)
    , ("governance procedures", ppStrictSeq prettyA $ txb ^. govProcsTxBodyL)
    ]

instance PrettyA VoteDecision where
  prettyA = viaShow

instance EraPParams era => PrettyA (GovernanceProcedure era) where
  prettyA = viaShow

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceAction era) where
  prettyA (ParameterChange ppup) =
    ppRecord
      "ParameterChange"
      [("protocol parameters update", prettyA ppup)]
  prettyA (HardForkInitiation pv) =
    ppRecord
      "HardForkInitiation"
      [("protocol version", prettyA pv)]
  prettyA (TreasuryWithdrawals ws) =
    ppRecord
      "TreasuryWithdrawals"
      [("withdrawals map", prettyA ws)]
  prettyA NoConfidence =
    ppRecord "NoConfidence" []
  prettyA (NewCommittee ms q) =
    ppRecord
      "NewCommittee"
      [ ("members", prettyA ms)
      , ("quorum", prettyA q)
      ]
  prettyA (NewConstitution c) =
    ppRecord
      "NewConstitution"
      [("hash", prettyA c)]

instance forall c. PrettyA (ConwayDCert c) where
  prettyA = prettyA . transDCert

instance Crypto c => PrettyA (PParams (ConwayEra c)) where
  prettyA = ppBabbagePParams

instance Crypto c => PrettyA (PParamsUpdate (ConwayEra c)) where
  prettyA = ppBabbagePParamsUpdate

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , PrettyA (PredicateFailure (EraRule "DELEGS" era))
  , PrettyA (PredicateFailure (EraRule "TALLY" era))
  ) =>
  PrettyA (ConwayLedgerPredFailure era)
  where
  prettyA (ConwayUtxowFailure x) = prettyA x
  prettyA (ConwayDelegsFailure x) = prettyA x
  prettyA (ConwayTallyFailure x) = prettyA x

instance PrettyA (ConwayTallyPredFailure era) where
  prettyA = viaShow

instance PrettyA (PParamsUpdate era) => PrettyA (ConwayTallyState era) where
  prettyA (ConwayTallyState x) = prettyA x

instance PrettyA (GovernanceActionId era) where
  prettyA gaid@(GovernanceActionId _ _) =
    let GovernanceActionId {..} = gaid
     in ppRecord
          "GovernanceActionId"
          [ ("Transaction ID", prettyA gaidTxId)
          , ("Governance Action Index", prettyA gaidGovActionIx)
          ]

instance PrettyA GovernanceActionIx where
  prettyA (GovernanceActionIx x) = prettyA x

instance PrettyA VoterRole where
  prettyA = ppString . show

instance PrettyA (GovernanceMetadata era) where
  prettyA = viaShow

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceActionState era) where
  prettyA gas@(GovernanceActionState _ _ _ _ _) =
    let GovernanceActionState {..} = gas
     in ppRecord
          "GovernanceActionState"
          [ ("Votes", prettyA gasVotes)
          , ("Deposit", prettyA gasDeposit)
          , ("Return Address", prettyA gasReturnAddr)
          , ("Action", prettyA gasAction)
          , ("Proposed In", prettyA gasProposedIn)
          ]

instance PrettyA (PParams era) => PrettyA (EnactState era) where
  prettyA ens@(EnactState _ _ _ _) =
    let EnactState {..} = ens
     in ppRecord
          "EnactState"
          [ ("Constitutional Committee", prettyA ensCommittee)
          , ("PParams", prettyA ensPParams)
          , ("ProtVer", prettyA ensProtVer)
          , ("Constitution", prettyA ensConstitution)
          ]

instance
  ( PrettyA (PParamsUpdate era)
  , PrettyA (PParams era)
  ) =>
  PrettyA (RatifyState era)
  where
  prettyA rs@(RatifyState _ _) =
    let RatifyState {..} = rs
     in ppRecord
          "RatifyState"
          [ ("EnactState", prettyA rsEnactState)
          , ("Future", prettyA rsFuture)
          ]

instance
  ( PrettyA (PParamsUpdate era)
  , PrettyA (PParams era)
  ) =>
  PrettyA (ConwayGovernance era)
  where
  prettyA cg@(ConwayGovernance _ _ _) =
    let ConwayGovernance {..} = cg
     in ppRecord
          "ConwayGovernance"
          [ ("Tally", prettyA cgTally)
          , ("Ratify", prettyA cgRatify)
          , ("VoterRoles", prettyA cgVoterRoles)
          ]
