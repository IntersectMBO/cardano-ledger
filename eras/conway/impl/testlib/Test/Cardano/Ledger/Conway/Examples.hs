{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Conway.Examples (
  ledgerExamples,
  exampleConwayBasedTx,
  exampleConwayBasedTopTx,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ApplyTxError (ConwayApplyTxError), ConwayEra)
import Cardano.Ledger.Conway.Core
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
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Cardano.Ledger.State (StakePoolParams (sppId))
import Cardano.Ledger.TxIn (TxId (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map
import Data.Maybe (fromJust)
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  addAlonzoToConwayExampleReqSigners,
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (
  exampleBabbageBasedTopTx,
  exampleBabbageBasedTx,
  exampleBabbageNewEpochState,
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
  addShelleyBasedTopTxExampleFee,
  addShelleyToConwayTxCerts,
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
    exampleConwayTx
    expectedConwayGenesis
  where
    exampleConwayTx =
      exampleConwayBasedTopTx
        & addShelleyBasedTopTxExampleFee
        & addShelleyToConwayTxCerts
        & addAlonzoToConwayExampleReqSigners

exampleConwayBasedTopTx ::
  forall era.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraPlutusTxInfo 'PlutusV3 era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era
exampleConwayBasedTopTx =
  exampleBabbageBasedTopTx
    & addConwayBasedTxFeatures

exampleConwayBasedTx ::
  forall era l.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraPlutusTxInfo 'PlutusV3 era
  , Value era ~ MaryValue
  , Typeable l
  ) =>
  Tx l era
exampleConwayBasedTx =
  exampleBabbageBasedTx
    & addConwayBasedTxFeatures

addConwayBasedTxFeatures ::
  forall era l.
  ( EraTx era
  , ConwayEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV3 era
  , AlonzoEraTxAuxData era
  , AlonzoEraTxWits era
  ) =>
  Tx l era ->
  Tx l era
addConwayBasedTxFeatures tx =
  tx
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL <>~ Map.fromElems hashScript [alwaysSucceeds @'PlutusV3 3]
              & rdmrsTxWitsL <>~ redeemers
          )
    & modifyTxAuxData
      ( plutusScriptsTxAuxDataL
          %~ Map.insertWith
            (<>)
            PlutusV3
            (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV3 3)))
      )
    & bodyTxL . outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV3 3)
        ]
    & bodyTxL . certsTxBodyL <>~ exampleConwayCerts
    & bodyTxL . votingProceduresTxBodyL .~ exampleVotingProcedures
    & bodyTxL . proposalProceduresTxBodyL
      <>~ OSet.fromList
        [ exampleProposalProcedure
        , exampleProposalProcedureParameterChange
        , exampleProposalProcedureHardForkInitiation
        , exampleProposalProcedureTreasuryWithdrawals
        , exampleProposalProcedureNoConfidence
        , exampleProposalProcedureUpdateCommittee
        , exampleProposalProcedureNewConstitution
        ]
    & bodyTxL . currentTreasuryValueTxBodyL .~ SJust (Coin 867530900000)
    & bodyTxL . treasuryDonationTxBodyL .~ Coin 1000000
  where
    redeemers =
      Redeemers $
        Map.fromList
          [ (VotingPurpose $ AsIx 0, (redeemerData, ExUnits 5000 5000))
          , (ProposingPurpose $ AsIx 0, (redeemerData, ExUnits 5000 5000))
          ]
    redeemerData = Data (P.Constr 1 [P.List [P.I 1], P.Map [(P.I 2, P.B "2")]])

exampleConwayCerts :: ConwayEraTxCert era => StrictSeq (TxCert era)
exampleConwayCerts =
  StrictSeq.fromList
    [ RegDepositTxCert (KeyHashObj (mkKeyHash 2)) (Coin 2000000)
    , UnRegDepositTxCert (KeyHashObj (mkKeyHash 2)) (Coin 2000000)
    , DelegTxCert (KeyHashObj (mkKeyHash 3)) (DelegStake (sppId exampleStakePoolParams))
    , DelegTxCert (KeyHashObj (mkKeyHash 4)) (DelegVote (DRepKeyHash (mkKeyHash 5)))
    , DelegTxCert
        (KeyHashObj (mkKeyHash 6))
        (DelegStakeVote (sppId exampleStakePoolParams) (DRepKeyHash (mkKeyHash 7)))
    , DelegTxCert (KeyHashObj (mkKeyHash 13)) (DelegVote (DRepScriptHash (mkScriptHash 1)))
    , DelegTxCert (KeyHashObj (mkKeyHash 14)) (DelegVote DRepAlwaysAbstain)
    , DelegTxCert (KeyHashObj (mkKeyHash 15)) (DelegVote DRepAlwaysNoConfidence)
    , RegDepositDelegTxCert
        (KeyHashObj (mkKeyHash 8))
        (DelegStake (sppId exampleStakePoolParams))
        (Coin 2000000)
    , RegDRepTxCert (KeyHashObj (mkKeyHash 9)) (Coin 500000) (SJust exampleAnchor)
    , UnRegDRepTxCert (KeyHashObj (mkKeyHash 9)) (Coin 500000)
    , UpdateDRepTxCert (KeyHashObj (mkKeyHash 9)) (SJust exampleAnchor)
    , AuthCommitteeHotKeyTxCert (KeyHashObj (mkKeyHash 10)) (KeyHashObj (mkKeyHash 11))
    , ResignCommitteeColdTxCert (KeyHashObj (mkKeyHash 10)) (SJust exampleAnchor)
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
