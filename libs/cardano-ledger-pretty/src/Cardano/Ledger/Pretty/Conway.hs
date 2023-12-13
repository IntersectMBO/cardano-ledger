{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pretty.Conway (
  ppConwayTxBody,
) where

import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits, Prices)
import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochNo, NonNegativeInterval, UnitInterval)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
  Committee (..),
  ConwayGovState (..),
  DRepPulsingState (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovProcedures (..),
  PrevGovActionId (..),
  PrevGovActionIds (..),
  PrevGovActionIdsChildren,
  ProposalProcedure (..),
  Proposals,
  PulsingSnapshot (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  finishDRepPulser,
  proposalsActions,
 )
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure (..),
  ConwayGovCertEnv (..),
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayLedgerPredFailure (..),
  EnactSignal (..),
  EnactState (..),
  GovEnv (..),
  GovRuleState (..),
  PredicateFailure,
 )
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayGovCert (..),
  ConwayTxCert (..),
  Delegatee (..),
 )
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Pretty (
  PDoc,
  PrettyA (..),
  ppAuxiliaryDataHash,
  ppCoin,
  ppKeyHash,
  ppList,
  ppMap,
  ppNetwork,
  ppOSet,
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
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.Shelley.Rules (PoolEnv (..))
import Data.Foldable
import Data.Text (Text)
import Lens.Micro ((^.))
import Numeric.Natural (Natural)
import Prettyprinter (viaShow)

instance
  ( ConwayEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , TxBody era ~ ConwayTxBody era
  ) =>
  PrettyA (ConwayTxBody era)
  where
  prettyA = ppConwayTxBody

instance PrettyA (Delegatee era) where
  prettyA (DelegStake skh) =
    ppRecord
      "DelegStake"
      [ ("Staking Key Hash", prettyA skh)
      ]
  prettyA (DelegVote vc) =
    ppRecord
      "DelegVote"
      [ ("Voting Credential", prettyA vc)
      ]
  prettyA (DelegStakeVote skh vc) =
    ppRecord
      "DelegStakeVote"
      [ ("Staking Key Hash", prettyA skh)
      , ("Voting Credential", prettyA vc)
      ]

instance PrettyA (ConwayDelegCert c) where
  prettyA (ConwayRegCert c deposit) =
    ppRecord
      "ConwayRegCert"
      [ ("StakeCredential", prettyA c)
      , ("Deposit", prettyA deposit)
      ]
  prettyA (ConwayUnRegCert c deposit) =
    ppRecord
      "ConwayUnRegCert"
      [ ("StakeCredential", prettyA c)
      , ("Deposit", prettyA deposit)
      ]
  prettyA (ConwayDelegCert stakeCredential delegatee) =
    ppRecord
      "ConwayDeleg"
      [ ("Stake Credential", prettyA stakeCredential)
      , ("Delegatee", prettyA delegatee)
      ]
  prettyA (ConwayRegDelegCert stakeCredential delegatee deposit) =
    ppRecord
      "ConwayRegDeleg"
      [ ("Stake Credential", prettyA stakeCredential)
      , ("Delegatee", prettyA delegatee)
      , ("Deposit", prettyA deposit)
      ]

instance PrettyA (ConwayGovCert c) where
  prettyA = ppConwayGovCert

instance PrettyA (PParams c) => PrettyA (ConwayGovCertEnv c) where
  prettyA (ConwayGovCertEnv pp ce) = ppSexp "ConwayGovCertEnv" [prettyA pp, prettyA ce]

instance PrettyA (PParams c) => PrettyA (PoolEnv c) where
  prettyA (PoolEnv sn pp) = ppSexp "PoolEnv" [prettyA sn, prettyA pp]

ppConwayTxCert :: ConwayTxCert era -> PDoc
ppConwayTxCert = \case
  ConwayTxCertDeleg dc -> ppSexp "ConwayTxCertDeleg" [prettyA dc]
  ConwayTxCertPool pc -> ppSexp "ConwayTxCertPool" [ppPoolCert pc]
  ConwayTxCertGov gdc -> ppSexp "ConwayTxCertGov" [ppConwayGovCert gdc]

ppConwayGovCert :: ConwayGovCert c -> PDoc
ppConwayGovCert = \case
  ConwayRegDRep cred deposit mAnchor ->
    ppSexp "ConwayRegDRep" [prettyA cred, prettyA deposit, prettyA mAnchor]
  ConwayUnRegDRep cred deposit ->
    ppSexp "ConwayUnRegDRep" [prettyA cred, prettyA deposit]
  ConwayUpdateDRep cred mAnchor ->
    ppSexp "ConwayUpdateDRep" [prettyA cred, prettyA mAnchor]
  ConwayAuthCommitteeHotKey coldKey hotKey ->
    ppSexp "ConwayAuthCommitteeHotKey" [prettyA coldKey, prettyA hotKey]
  ConwayResignCommitteeColdKey coldKey a ->
    ppSexp "ConwayResignCommitteeColdKey" [prettyA coldKey, prettyA a]

ppConwayTxBody ::
  forall era.
  ( ConwayEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
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
    , ("withdrawals", ppWithdrawals $ txb ^. withdrawalsTxBodyL)
    , ("transaction fee", ppCoin $ txb ^. feeTxBodyL)
    , ("validity interval", prettyA $ txb ^. vldtTxBodyL)
    , ("required signer hashes", ppSet ppKeyHash $ txb ^. reqSignerHashesTxBodyL)
    , ("mint", prettyA $ txb ^. mintTxBodyL)
    , ("script integrity hash", ppStrictMaybe ppSafeHash $ txb ^. scriptIntegrityHashTxBodyL)
    , ("auxiliary data hash", ppStrictMaybe ppAuxiliaryDataHash $ txb ^. auxDataHashTxBodyL)
    , ("network id", ppStrictMaybe ppNetwork $ txb ^. networkIdTxBodyL)
    , ("voting procedures", prettyA $ unVotingProcedures (txb ^. votingProceduresTxBodyL))
    , ("proposal procedures", ppOSet prettyA $ txb ^. proposalProceduresTxBodyL)
    ]

instance PrettyA Vote where
  prettyA = viaShow

instance EraPParams era => PrettyA (VotingProcedure era) where
  prettyA VotingProcedure {..} =
    ppRecord
      "VotingProcedure"
      [ ("Vote", prettyA vProcVote)
      , ("Anchor", prettyA vProcAnchor)
      ]

instance EraPParams era => PrettyA (VotingProcedures era) where
  prettyA VotingProcedures {..} = ppMap prettyA prettyA unVotingProcedures

instance (EraPParams era, PrettyA (PParamsUpdate era)) => PrettyA (ProposalProcedure era) where
  prettyA ProposalProcedure {..} =
    ppRecord
      "ProposalProcedure"
      [ ("Deposit", prettyA pProcDeposit)
      , ("ReturnAddr", prettyA pProcReturnAddr)
      , ("GovAction", prettyA pProcGovAction)
      , ("Anchor", prettyA pProcAnchor)
      ]

instance (EraPParams era, PrettyA (PParamsUpdate era)) => PrettyA (GovProcedures era) where
  prettyA GovProcedures {..} =
    ppRecord
      "GovProcedures"
      [ ("VotingProcedures", prettyA gpVotingProcedures)
      , ("ProposalProcedures", ppList prettyA (toList gpProposalProcedures))
      ]

instance PrettyA (PrevGovActionId p c) where
  prettyA = viaShow

instance PrettyA (PrevGovActionIdsChildren era) where
  prettyA = viaShow

instance PrettyA (PParamsUpdate era) => PrettyA (GovAction era) where
  prettyA (ParameterChange pgaid ppup) =
    ppRecord
      "ParameterChange"
      [ ("previous governance action id", prettyA pgaid)
      , ("protocol parameters update", prettyA ppup)
      ]
  prettyA (HardForkInitiation pgaid pv) =
    ppRecord
      "HardForkInitiation"
      [ ("previous governance action id", prettyA pgaid)
      , ("protocol version", prettyA pv)
      ]
  prettyA (TreasuryWithdrawals ws) =
    ppRecord
      "TreasuryWithdrawals"
      [("withdrawals map", prettyA ws)]
  prettyA (NoConfidence pgaid) =
    ppRecord "NoConfidence" [("previous governance action id", prettyA pgaid)]
  prettyA (UpdateCommittee pgaid toRemove toAdd qrm) =
    ppRecord
      "UpdateCommittee"
      [ ("previous governance action id", prettyA pgaid)
      , ("membersToRemove", prettyA toRemove)
      , ("membersToAdd", prettyA toAdd)
      , ("quorum", prettyA qrm)
      ]
  prettyA (NewConstitution pgaid Constitution {..}) =
    ppRecord
      "NewConstitution"
      [ ("previous governance action id", prettyA pgaid)
      , ("anchor", prettyA constitutionAnchor)
      , ("script", prettyA constitutionScript)
      ]
  prettyA InfoAction =
    ppRecord "InfoAction" []

instance PrettyA (Committee era) where
  prettyA Committee {..} =
    ppRecord
      "Committee"
      [ ("members", prettyA committeeMembers)
      , ("quorum", prettyA committeeQuorum)
      ]

instance forall c. PrettyA (ConwayTxCert c) where
  prettyA = ppConwayTxCert

ppConwayPParams ::
  forall era f.
  ( PrettyA (HKD f UnitInterval)
  , PrettyA (HKD f Prices)
  , PrettyA (HKD f PoolVotingThresholds)
  , PrettyA (HKD f Coin)
  , PrettyA (HKD f Natural)
  , PrettyA (HKD f ExUnits)
  , PrettyA (HKD f EpochNo)
  , PrettyA (HKD f EpochInterval)
  , PrettyA (HKD f DRepVotingThresholds)
  , PrettyA (HKD f CostModels)
  , PrettyA (HKD f CoinPerByte)
  , PrettyA (HKD f NonNegativeInterval)
  , ConwayEraPParams era
  , HKDFunctor f
  ) =>
  Text ->
  PParamsHKD f era ->
  PDoc
ppConwayPParams n pp =
  ppRecord
    n
    [ ("Tau", prettyA $ pp ^. hkdTauL @era @f)
    , ("Rho", prettyA $ pp ^. hkdRhoL @era @f)
    , ("Prices", prettyA $ pp ^. hkdPricesL @era @f)
    , ("PoolVotingThresholds", prettyA $ pp ^. hkdPoolVotingThresholdsL @era @f)
    , ("PoolDeposit", prettyA $ pp ^. hkdPoolDepositL @era @f)
    , ("NOpt", prettyA $ pp ^. hkdNOptL @era @f)
    , ("MinPoolCost", prettyA $ pp ^. hkdMinPoolCostL @era @f)
    , ("MinFeeB", prettyA $ pp ^. hkdMinFeeBL @era @f)
    , ("MinFeeA", prettyA $ pp ^. hkdMinFeeAL @era @f)
    , ("CommitteeMinSize", prettyA $ pp ^. hkdCommitteeMinSizeL @era @f)
    , ("MaxValSize", prettyA $ pp ^. hkdMaxValSizeL @era @f)
    , ("MaxTxSize", prettyA $ pp ^. hkdMaxTxSizeL @era @f)
    , ("MaxTxExUnits", prettyA $ pp ^. hkdMaxTxExUnitsL @era @f)
    , ("MaxCollateralInputs", prettyA $ pp ^. hkdMaxCollateralInputsL @era @f)
    , ("MaxBlockExUnits", prettyA $ pp ^. hkdMaxBlockExUnitsL @era @f)
    , ("MaxBHSize", prettyA $ pp ^. hkdMaxBHSizeL @era @f)
    , ("MaxBBSize", prettyA $ pp ^. hkdMaxBBSizeL @era @f)
    , ("KeyDeposit", prettyA $ pp ^. hkdKeyDepositL @era @f)
    , ("GovActionLifetime", prettyA $ pp ^. hkdGovActionLifetimeL @era @f)
    , ("GovActionDeposit", prettyA $ pp ^. hkdGovActionDepositL @era @f)
    , ("EMax", prettyA $ pp ^. hkdEMaxL @era @f)
    , ("DRepVotingThresholds", prettyA $ pp ^. hkdDRepVotingThresholdsL @era @f)
    , ("DRepDeposit", prettyA $ pp ^. hkdDRepDepositL @era @f)
    , ("DRepActivity", prettyA $ pp ^. hkdDRepActivityL @era @f)
    , ("CostModels", prettyA $ pp ^. hkdCostModelsL @era @f)
    , ("CommitteeMaxTermLength", prettyA $ pp ^. hkdCommitteeMaxTermLengthL @era @f)
    , ("CollateralPercentage", prettyA $ pp ^. hkdCollateralPercentageL @era @f)
    , ("CoinsPerUTxOByte", prettyA $ pp ^. hkdCoinsPerUTxOByteL @era @f)
    , ("A0", prettyA $ pp ^. hkdA0L @era @f)
    ]

instance Crypto c => PrettyA (PParams (ConwayEra c)) where
  prettyA (PParams x) = ppConwayPParams "PParams" x

instance Crypto c => PrettyA (PParamsUpdate (ConwayEra c)) where
  prettyA (PParamsUpdate x) = ppConwayPParams "PParamsUpdate" x

instance PrettyA PoolVotingThresholds where
  prettyA x@(PoolVotingThresholds _ _ _ _) =
    let PoolVotingThresholds {..} = x
     in ppRecord
          "PoolVotingThresholds"
          [ ("MotionNoConfidence", prettyA pvtMotionNoConfidence)
          , ("HardForkInitiation", prettyA pvtHardForkInitiation)
          , ("CommitteeNormal", prettyA pvtCommitteeNormal)
          , ("CommitteeNoConfidence", prettyA pvtCommitteeNoConfidence)
          ]

instance PrettyA DRepVotingThresholds where
  prettyA x@(DRepVotingThresholds _ _ _ _ _ _ _ _ _ _) =
    let DRepVotingThresholds {..} = x
     in ppRecord
          "DRepVotingThresholds"
          [ ("UpdateToConstitution", prettyA dvtUpdateToConstitution)
          , ("TreasuryWithdrawal", prettyA dvtTreasuryWithdrawal)
          , ("PPTechnicalGroup", prettyA dvtPPTechnicalGroup)
          , ("PPNetworkGroup", prettyA dvtPPNetworkGroup)
          , ("PPGovGroup", prettyA dvtPPGovGroup)
          , ("PPEconomicGroup", prettyA dvtPPEconomicGroup)
          , ("MotionNoConfidence", prettyA dvtMotionNoConfidence)
          , ("HardForkInitiation", prettyA dvtHardForkInitiation)
          , ("CommitteeNormal", prettyA dvtCommitteeNormal)
          , ("CommitteeNoConfidence", prettyA dvtCommitteeNoConfidence)
          ]

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , PrettyA (PredicateFailure (EraRule "CERTS" era))
  , PrettyA (PredicateFailure (EraRule "GOV" era))
  ) =>
  PrettyA (ConwayLedgerPredFailure era)
  where
  prettyA (ConwayUtxowFailure x) = prettyA x
  prettyA (ConwayCertsFailure x) = prettyA x
  prettyA (ConwayGovFailure x) = prettyA x
  prettyA (ConwayWdrlNotDelegatedToDRep x) =
    ppSexp "ConwayWdrlNotDelegatedToDRep" [prettyA x]
  prettyA (ConwayTreasuryValueMismatch x y) =
    ppSexp "ConwayTreasuryValueMismatch" [prettyA x, prettyA y]

instance EraPParams era => PrettyA (ConwayGovPredFailure era) where
  prettyA = viaShow

instance PrettyA (PParamsUpdate era) => PrettyA (Proposals era) where
  prettyA = prettyA . proposalsActions

instance PrettyA (PParamsUpdate era) => PrettyA (GovRuleState era) where
  prettyA grs@(GovRuleState _ _) =
    let GovRuleState {..} = grs
     in ppRecord
          "GovRuleState"
          [ ("grsPrevGovActionIdsChildren", prettyA grsPrevGovActionIdsChildren)
          , ("grsProposals", prettyA grsProposals)
          ]

instance PrettyA (GovActionId era) where
  prettyA gaid@(GovActionId _ _) =
    let GovActionId {..} = gaid
     in ppRecord
          "GovActionId"
          [ ("Transaction ID", prettyA gaidTxId)
          , ("Gov Action Index", prettyA gaidGovActionIx)
          ]

instance PrettyA GovActionIx where
  prettyA (GovActionIx x) = prettyA x

instance PrettyA (Voter c) where
  prettyA = ppString . show

instance PrettyA (Anchor era) where
  prettyA (Anchor u h) =
    ppRecord
      "Anchor"
      [ ("url", ppString (show u))
      , ("datahash", ppSafeHash h)
      ]

instance PrettyA (PParamsUpdate era) => PrettyA (GovActionState era) where
  prettyA gas@(GovActionState _ _ _ _ _ _ _ _ _ _) =
    let GovActionState {..} = gas
     in ppRecord
          "GovActionState"
          [ ("Id", prettyA gasId)
          , ("CommitteVotes", prettyA gasCommitteeVotes)
          , ("DRepVotes", prettyA gasDRepVotes)
          , ("StakePoolVotes", prettyA gasStakePoolVotes)
          , ("Deposit", prettyA gasDeposit)
          , ("Return Address", prettyA gasReturnAddr)
          , ("Action", prettyA gasAction)
          , ("Proposed In", prettyA gasProposedIn)
          , ("Expires After", prettyA gasExpiresAfter)
          , ("Children", prettyA gasChildren)
          ]

instance PrettyA (Constitution era) where
  prettyA Constitution {..} =
    ppRecord
      "Constitution"
      [ ("anchor", prettyA constitutionAnchor)
      , ("script", prettyA constitutionScript)
      ]

instance PrettyA (PParams era) => PrettyA (EnactState era) where
  prettyA ens@(EnactState _ _ _ _ _ _ _ _) =
    let EnactState {..} = ens
     in ppRecord
          "EnactState"
          [ ("Constitutional Committee", prettyA ensCommittee)
          , ("CurPParams", prettyA ensCurPParams)
          , ("PrevPParams", prettyA ensPrevPParams)
          , ("Constitution", prettyA ensConstitution)
          , ("Treasury", prettyA ensTreasury)
          , ("Withdrawals", prettyA ensWithdrawals)
          , ("PrevGovActionIds", prettyA ensPrevGovActionIds)
          , ("PrevGovActionIdsChildren", prettyA ensPrevGovActionIdsChildren)
          ]

instance PrettyA (PParamsUpdate era) => PrettyA (EnactSignal era) where
  prettyA EnactSignal {..} =
    ppRecord
      "EnactSignal"
      [ ("Gov Action Id", prettyA esGovActionId)
      , ("Gov Action", prettyA esGovAction)
      ]

instance PrettyA (PrevGovActionIds era) where
  prettyA PrevGovActionIds {..} =
    ppRecord
      "PrevGovActionIds"
      [ ("LastPParamUpdate", prettyA pgaPParamUpdate)
      , ("LastHardFork", prettyA pgaHardFork)
      , ("LastCommittee", prettyA pgaCommittee)
      , ("LastConstitution", prettyA pgaConstitution)
      ]

instance PrettyA (RatifyEnv era) where
  prettyA rs@(RatifyEnv {}) =
    let RatifyEnv {..} = rs
     in ppRecord
          "RatifyEnv"
          [ ("StakeDistr", prettyA reStakeDistr)
          , ("StakePoolDistr", prettyA reStakePoolDistr)
          , ("DRepDistr", prettyA reDRepDistr)
          , ("DRepState", prettyA reDRepState)
          , ("CurrentEpoch", prettyA reCurrentEpoch)
          , ("CommitteeState", prettyA reCommitteeState)
          ]

instance
  PrettyA (PParams era) =>
  PrettyA (RatifyState era)
  where
  prettyA rs@(RatifyState _ _ _ _) =
    let RatifyState {..} = rs
     in ppRecord
          "RatifyState"
          [ ("EnactState", prettyA rsEnactState)
          , ("Removed", prettyA rsRemoved)
          , ("Enacted", prettyA rsEnacted)
          , ("Delayed", prettyA rsDelayed)
          ]

instance
  PrettyA (PParamsUpdate era) =>
  PrettyA (RatifySignal era)
  where
  prettyA (RatifySignal s) = ppStrictSeq prettyA s

instance PrettyA (PParams era) => PrettyA (GovEnv era) where
  prettyA GovEnv {..} =
    ppRecord
      "GovEnv"
      [ ("TxId", prettyA geTxId)
      , ("Epoch", prettyA geEpoch)
      , ("PParams", prettyA gePParams)
      , ("PrevGovActionId", prettyA gePrevGovActionIds)
      ]

instance
  ( PrettyA (PParamsUpdate era)
  , PrettyA (PParams era)
  ) =>
  PrettyA (ConwayGovState era)
  where
  prettyA cg@(ConwayGovState _ _ _) =
    let ConwayGovState {..} = cg
     in ppRecord
          "ConwayGovState"
          [ ("proposals", prettyA cgProposals)
          , ("enactState", prettyA cgEnactState)
          , ("drepPulsingState", ppDRepPulsingState cgDRepPulsingState)
          ]

ppPulsingSnapshot :: PrettyA (PParamsUpdate era) => PulsingSnapshot era -> PDoc
ppPulsingSnapshot (PulsingSnapshot x y z) =
  ppRecord
    "Snapshot"
    [ ("proposals", ppStrictSeq prettyA x)
    , ("drepDistr", ppMap prettyA (ppCoin . fromCompact) y)
    , ("drepState", ppMap prettyA prettyA z)
    ]

ppDRepPulsingState :: (PrettyA (PParamsUpdate era), PrettyA (PParams era)) => DRepPulsingState era -> PDoc
ppDRepPulsingState (DRComplete x y) =
  ppRecord
    "CompleteDRepPulsingState"
    [ ("pulsingSnapshot", ppPulsingSnapshot x)
    , ("ratifyState", prettyA y)
    ]
ppDRepPulsingState pst =
  ppRecord
    "CompleteDRepPulsingState"
    [ ("pulsingSnapshot", ppPulsingSnapshot x)
    , ("ratifyState", prettyA y)
    ]
  where
    (x, y) = finishDRepPulser pst

instance
  PrettyA (PredicateFailure (EraRule "CERT" era)) =>
  PrettyA (ConwayCertsPredFailure era)
  where
  prettyA (DelegateeNotRegisteredDELEG x) =
    ppRecord
      "DelegateeNotRegisteredDELEG"
      [("KeyHash", prettyA x)]
  prettyA (WithdrawalsNotInRewardsCERTS x) =
    ppRecord
      "WithdrawalsNotInRewardsDELEGS"
      [("Missing Withdrawals", prettyA x)]
  prettyA (CertFailure x) = prettyA x

instance
  ( PrettyA (PredicateFailure (EraRule "DELEG" era))
  , PrettyA (PredicateFailure (EraRule "POOL" era))
  , PrettyA (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  PrettyA (ConwayCertPredFailure era)
  where
  prettyA = \case
    DelegFailure x ->
      ppRecord
        "ConwayDelegFailure"
        [("DELEG", prettyA x)]
    PoolFailure x ->
      ppRecord
        "ConwayPoolFailure"
        [("POOL", prettyA x)]
    GovCertFailure x ->
      ppRecord
        "ConwayGovCertFailure"
        [("GOVCERT", prettyA x)]

instance PrettyA (ConwayDelegPredFailure era) where
  prettyA = \case
    IncorrectDepositDELEG x ->
      ppRecord
        "IncorrectDepositDELEG"
        [("Coin", prettyA x)]
    StakeKeyRegisteredDELEG x ->
      ppRecord
        "StakeKeyRegisteredDELEG"
        [("Credential", prettyA x)]
    StakeKeyNotRegisteredDELEG x ->
      ppRecord
        "StakeKeyNotRegisteredDELEG"
        [("Credential", prettyA x)]
    StakeKeyHasNonZeroRewardAccountBalanceDELEG x ->
      ppRecord
        "StakeKeyHasNonZeroRewardAccountBalanceDELEG"
        [("Coin", prettyA x)]
    DRepAlreadyRegisteredForStakeKeyDELEG x ->
      ppRecord
        "DRepAlreadyRegisteredForStakeKeyDELEG"
        [("Credential", prettyA x)]

instance PrettyA (ConwayGovCertPredFailure era) where
  prettyA = const $ ppRecord "ConwayGovCertPredFailure" []
