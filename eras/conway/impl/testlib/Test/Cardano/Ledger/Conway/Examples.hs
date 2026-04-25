{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Examples (
  ledgerExamples,
  mkConwayBasedExampleTx,
  exampleConwayBasedTxBody,
  exampleAnchor,
  exampleVotingProcedures,
  exampleProposalProcedure,
  exampleProposalProcedureParameterChange,
  exampleProposalProcedureHardForkInitiation,
  exampleProposalProcedureTreasuryWithdrawals,
  exampleProposalProcedureNoConfidence,
  exampleProposalProcedureUpdateCommittee,
  exampleProposalProcedureNewConstitution,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ApplyTxError (ConwayApplyTxError), ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (
  Constitution (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Cardano.Ledger.State (StakePoolParams (sppId))
import Cardano.Ledger.TxIn (TxId (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (
  exampleBabbageBasedTxBody,
  exampleBabbageNewEpochState,
  mkBabbageBasedExampleTx,
 )
import Test.Cardano.Ledger.Conway.Era ()
import Test.Cardano.Ledger.Conway.Genesis (expectedConwayGenesis)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  exampleStakeKey,
  exampleStakePoolParams,
  mkKeyHash,
  mkScriptHash,
 )

ledgerExamples :: LedgerExamples ConwayEra
ledgerExamples =
  mkAlonzoBasedLedgerExamples
    ( ConwayApplyTxError $
        pure $
          injectFailure $
            DelegateeStakePoolNotRegisteredDELEG @ConwayEra (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    ( mkConwayBasedExampleTx
        (exampleConwayBasedTxBody exampleConwayCerts)
        (ConwaySpending $ AsIx 0)
    )
    exampleConwayGenesis

mkConwayBasedExampleTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraPlutusTxInfo 'PlutusV3 era
  ) =>
  TxBody TopTx era ->
  PlutusPurpose AsIx era ->
  Tx TopTx era
mkConwayBasedExampleTx txBody scriptPurpose =
  mkBabbageBasedExampleTx
    txBody
    scriptPurpose
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL
                .~ Map.singleton
                  (hashScript @era $ alwaysSucceeds @'PlutusV3 3)
                  (alwaysSucceeds @'PlutusV3 3)
          )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                <>~ Map.singleton PlutusV3 (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV3 3)))
        )

exampleConwayBasedTxBody ::
  forall era.
  ( ConwayEraTxBody era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , Value era ~ MaryValue
  ) =>
  StrictSeq.StrictSeq (TxCert era) ->
  TxBody TopTx era
exampleConwayBasedTxBody certs = mkConwayBasedExampleTxBody certs exampleBabbageBasedTxBody

mkConwayBasedExampleTxBody ::
  forall era.
  ( ConwayEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV3 era
  ) =>
  StrictSeq.StrictSeq (TxCert era) ->
  TxBody TopTx era ->
  TxBody TopTx era
mkConwayBasedExampleTxBody certs txBody =
  txBody
    & outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV3 3)
        ]
    & certsTxBodyL .~ certs
    & votingProceduresTxBodyL .~ exampleVotingProcedures
    & proposalProceduresTxBodyL
      .~ OSet.fromList
        [ exampleProposalProcedure
        , exampleProposalProcedureParameterChange
        , exampleProposalProcedureHardForkInitiation
        , exampleProposalProcedureTreasuryWithdrawals
        , exampleProposalProcedureNoConfidence
        , exampleProposalProcedureUpdateCommittee
        , exampleProposalProcedureNewConstitution
        ]
    & currentTreasuryValueTxBodyL .~ SJust (Coin 867530900000)
    & treasuryDonationTxBodyL .~ Coin 1000000

exampleConwayCerts :: StrictSeq.StrictSeq (ConwayTxCert era)
exampleConwayCerts =
  StrictSeq.fromList
    [ ConwayTxCertDeleg $ ConwayRegCert (KeyHashObj (mkKeyHash 2)) (SJust (Coin 2000000))
    , ConwayTxCertDeleg $ ConwayUnRegCert (KeyHashObj (mkKeyHash 2)) (SJust (Coin 2000000))
    , ConwayTxCertDeleg $
        ConwayDelegCert (KeyHashObj (mkKeyHash 3)) (DelegStake (sppId exampleStakePoolParams))
    , ConwayTxCertDeleg $
        ConwayDelegCert (KeyHashObj (mkKeyHash 4)) (DelegVote (DRepKeyHash (mkKeyHash 5)))
    , ConwayTxCertDeleg $
        ConwayDelegCert
          (KeyHashObj (mkKeyHash 6))
          (DelegStakeVote (sppId exampleStakePoolParams) (DRepKeyHash (mkKeyHash 7)))
    , ConwayTxCertDeleg $
        ConwayRegDelegCert
          (KeyHashObj (mkKeyHash 8))
          (DelegStake (sppId exampleStakePoolParams))
          (Coin 2000000)
    , ConwayTxCertPool $ RegPool exampleStakePoolParams
    , ConwayTxCertPool $ RetirePool (sppId exampleStakePoolParams) (EpochNo 2)
    , ConwayTxCertGov $ ConwayRegDRep (KeyHashObj (mkKeyHash 9)) (Coin 500000) (SJust exampleAnchor)
    , ConwayTxCertGov $ ConwayUnRegDRep (KeyHashObj (mkKeyHash 9)) (Coin 500000)
    , ConwayTxCertGov $ ConwayUpdateDRep (KeyHashObj (mkKeyHash 9)) (SJust exampleAnchor)
    , ConwayTxCertGov $ ConwayAuthCommitteeHotKey (KeyHashObj (mkKeyHash 10)) (KeyHashObj (mkKeyHash 11))
    , ConwayTxCertGov $ ConwayResignCommitteeColdKey (KeyHashObj (mkKeyHash 10)) (SJust exampleAnchor)
    ]

exampleAnchor :: Anchor
exampleAnchor =
  Anchor
    { anchorUrl = fromJust $ textToUrl 99 "https://example.com"
    , anchorDataHash = mkDummySafeHash 0
    }

exampleVotingProcedures :: VotingProcedures era
exampleVotingProcedures =
  VotingProcedures $
    Map.fromList
      [
        ( StakePoolVoter (mkKeyHash 1)
        , Map.singleton exampleGovActionId (VotingProcedure VoteYes $ SJust exampleAnchor)
        )
      ,
        ( DRepVoter (KeyHashObj (mkKeyHash 2))
        , Map.singleton exampleGovActionId (VotingProcedure VoteNo $ SJust exampleAnchor)
        )
      ,
        ( CommitteeVoter (KeyHashObj (mkKeyHash 3))
        , Map.singleton exampleGovActionId (VotingProcedure Abstain (SJust exampleAnchor))
        )
      ]

exampleGovActionId :: GovActionId
exampleGovActionId =
  GovActionId
    { gaidTxId = TxId (mkDummySafeHash 1)
    , gaidGovActionIx = GovActionIx 0
    }

exampleProposalProcedure :: ProposalProcedure era
exampleProposalProcedure =
  ProposalProcedure
    { pProcDeposit = Coin 1000000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))
    , pProcGovAction = InfoAction
    , pProcAnchor = exampleAnchor
    }

exampleProposalProcedureParameterChange :: EraPParams era => ProposalProcedure era
exampleProposalProcedureParameterChange =
  ProposalProcedure
    { pProcDeposit = Coin 1000000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))
    , pProcGovAction =
        ParameterChange
          (SJust (GovPurposeId exampleGovActionId))
          (emptyPParamsUpdate & ppuMaxBBSizeL .~ SJust 65536)
          (SJust (mkScriptHash 1))
    , pProcAnchor = exampleAnchor
    }

exampleProposalProcedureHardForkInitiation :: ProposalProcedure era
exampleProposalProcedureHardForkInitiation =
  ProposalProcedure
    { pProcDeposit = Coin 1000000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))
    , pProcGovAction =
        HardForkInitiation
          (SJust (GovPurposeId exampleGovActionId))
          (ProtVer (natVersion @10) 0)
    , pProcAnchor = exampleAnchor
    }

exampleProposalProcedureTreasuryWithdrawals :: ProposalProcedure era
exampleProposalProcedureTreasuryWithdrawals =
  ProposalProcedure
    { pProcDeposit = Coin 1000000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))
    , pProcGovAction =
        TreasuryWithdrawals
          (Map.singleton (AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))) (Coin 1000000000))
          (SJust (mkScriptHash 2))
    , pProcAnchor = exampleAnchor
    }

exampleProposalProcedureNoConfidence :: ProposalProcedure era
exampleProposalProcedureNoConfidence =
  ProposalProcedure
    { pProcDeposit = Coin 1000000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))
    , pProcGovAction = NoConfidence (SJust (GovPurposeId exampleGovActionId))
    , pProcAnchor = exampleAnchor
    }

exampleProposalProcedureUpdateCommittee :: ProposalProcedure era
exampleProposalProcedureUpdateCommittee =
  ProposalProcedure
    { pProcDeposit = Coin 500000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 1)))
    , pProcGovAction =
        UpdateCommittee
          (SJust (GovPurposeId exampleGovActionId))
          mempty
          (Map.singleton (KeyHashObj (mkKeyHash 12)) (EpochNo 10))
          (3 %! 5)
    , pProcAnchor = exampleAnchor
    }

exampleProposalProcedureNewConstitution :: ProposalProcedure era
exampleProposalProcedureNewConstitution =
  ProposalProcedure
    { pProcDeposit = Coin 1000000000
    , pProcReturnAddr = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 0)))
    , pProcGovAction =
        NewConstitution
          (SJust (GovPurposeId exampleGovActionId))
          Constitution
            { constitutionAnchor = exampleAnchor
            , constitutionGuardrailsScriptHash = SJust (mkScriptHash 3)
            }
    , pProcAnchor = exampleAnchor
    }

exampleConwayGenesis :: ConwayGenesis
exampleConwayGenesis = expectedConwayGenesis
