{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubGovSpec (spec) where

import Cardano.Ledger.Address (accountAddressNetworkIdL)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Mismatch (..),
  Network (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (
  Constitution (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  constitutionGovStateL,
  constitutionGuardrailsScriptHashL,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraGovPredFailure (..),
  DijkstraSubGovPredFailure (..),
 )
import Cardano.Ledger.Shelley.LedgerState (epochStateGovStateL, nesELL, nesEsL)
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.Val ((<->))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBGOV" $ do
  it "a proposal in a sub-transaction is accepted" $ do
    proposal <- mkProposal InfoAction
    submittedTx <- submitTx . mkTopTxWithSubTxs . pure $ proposeSubTx proposal
    case fst <$> OMap.assocList (submittedTx ^. bodyTxL . subTransactionsTxBodyL) of
      [subTxId] -> do
        gas <- getGovActionState $ GovActionId subTxId (GovActionIx 0)
        gasProposalProcedure gas `shouldBe` proposal
      _ -> assertFailure "Expected exactly one sub-transaction"

  describe "ProposalCantFollow" $ do
    it "a hardfork that cannot follow the current protocol version" $ do
      currentProtVer <- getProtVer
      nextProtVer <- genCantFollow currentProtVer
      proposal <- mkProposal $ HardForkInitiation SNothing nextProtVer
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure . ProposalCantFollow SNothing $
            Mismatch {mismatchSupplied = nextProtVer, mismatchExpected = currentProtVer}
        ]

    it "a hardfork that cannot follow its parent" $ do
      currentProtVer <- getProtVer
      let parentProtVer = minorFollow currentProtVer
      parentGovActionId <- submitGovAction $ HardForkInitiation SNothing parentProtVer
      badProtVer <- genCantFollow parentProtVer
      let parentPurposeId = SJust $ GovPurposeId parentGovActionId
      proposal <- mkProposal $ HardForkInitiation parentPurposeId badProtVer
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure . ProposalCantFollow parentPurposeId $
            Mismatch {mismatchSupplied = badProtVer, mismatchExpected = parentProtVer}
        ]

  describe "MalformedProposal" $ do
    it "an empty parameter update" $ do
      useNativeGuardrailsScript
      govAction <- mkParameterChangeGovAction SNothing emptyPParamsUpdate
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [injectFailure . DijkstraSubGovPredFailure $ MalformedProposal govAction]

    it "a zero-valued protocol parameter" $ do
      useNativeGuardrailsScript
      govAction <-
        mkParameterChangeGovAction SNothing $ emptyPParamsUpdate & ppuMaxBBSizeL .~ SJust 0
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [injectFailure . DijkstraSubGovPredFailure $ MalformedProposal govAction]

  describe "ProposalReturnAccountDoesNotExist" $
    it "an unregistered return account" $ do
      account <- unregisteredAccount
      proposal <- mkProposalWithAccountAddress InfoAction account
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist account
        ]

  describe "TreasuryWithdrawalReturnAccountsDoNotExist" $
    it "an unregistered withdrawal account" $ do
      useNativeGuardrailsScript
      account <- unregisteredAccount
      registeredAccount <- registerAccountAddress
      govAction <-
        mkTreasuryWithdrawalsGovAction
          [(account, Coin 1_000_000), (registeredAccount, Coin 1_000_000)]
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure $
            TreasuryWithdrawalReturnAccountsDoNotExist [account]
        ]

  describe "ProposalDepositIncorrect" $
    it "a deposit below the protocol parameter" $ do
      accountAddress <- registerAccountAddress
      expectedDeposit <- getsPParams ppGovActionDepositL
      anchor <- arbitrary
      let suppliedDeposit = expectedDeposit <-> Coin 1
          proposal =
            ProposalProcedure
              { pProcReturnAddr = accountAddress
              , pProcGovAction = InfoAction
              , pProcDeposit = suppliedDeposit
              , pProcAnchor = anchor
              }
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure . ProposalDepositIncorrect $
            Mismatch {mismatchSupplied = suppliedDeposit, mismatchExpected = expectedDeposit}
        ]

  describe "ProposalProcedureNetworkIdMismatch" $
    it "a return account on the wrong network" $ do
      accountAddress <- registerAccountAddress
      let onWrongNetwork = accountAddress & accountAddressNetworkIdL .~ Mainnet
      proposal <- mkProposalWithAccountAddress InfoAction onWrongNetwork
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure $
            ProposalProcedureNetworkIdMismatch onWrongNetwork Testnet
        ]

  describe "TreasuryWithdrawalsNetworkIdMismatch" $
    it "a withdrawal account on the wrong network" $ do
      useNativeGuardrailsScript
      accountAddress <- registerAccountAddress
      let onWrongNetwork = accountAddress & accountAddressNetworkIdL .~ Mainnet
      govAction <- mkTreasuryWithdrawalsGovAction [(onWrongNetwork, Coin 1_000_000)]
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure $
            TreasuryWithdrawalsNetworkIdMismatch (NES.singleton onWrongNetwork) Testnet
        ]

  describe "InvalidGuardrailsScriptHash" $
    it "a policy that is not the constitution's guardrails script" $ do
      guardrailsScriptHash <- getGovPolicy
      wrongScriptHash <- impAddNativeScript . RequireSignature =<< freshKeyHash
      let govAction =
            ParameterChange SNothing (emptyPParamsUpdate & ppuCommitteeMinSizeL .~ SJust 2) $
              SJust wrongScriptHash
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure $
            InvalidGuardrailsScriptHash (SJust wrongScriptHash) guardrailsScriptHash
        ]

  describe "ZeroTreasuryWithdrawals" $
    it "withdrawals that sum to zero" $ do
      useNativeGuardrailsScript
      govAction <- mkTreasuryWithdrawalsGovAction []
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [injectFailure . DijkstraSubGovPredFailure $ ZeroTreasuryWithdrawals govAction]

  describe "ConflictingCommitteeUpdate" $
    it "a member that is both added and removed" $ do
      committeeCredential <- KeyHashObj <$> freshKeyHash
      proposal <-
        mkUpdateCommitteeProposal
          Nothing
          (Set.singleton committeeCredential)
          [(committeeCredential, EpochInterval 1)]
          (1 %! 1)
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure . ConflictingCommitteeUpdate $
            NES.singleton committeeCredential
        ]

  describe "ExpirationEpochTooSmall" $
    it "an expiration in the current epoch" $ do
      committeeCredential <- KeyHashObj <$> freshKeyHash
      currentEpochNo <- getsNES nesELL
      proposal <-
        mkUpdateCommitteeProposal
          Nothing
          mempty
          [(committeeCredential, EpochInterval 0)]
          (0 %! 1)
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure . ExpirationEpochTooSmall $
            NEM.singleton committeeCredential currentEpochNo
        ]

  describe "InvalidPrevGovActionId" $ do
    it "a parent of the wrong purpose" $ do
      govActionId <- submitGovAction InfoAction
      proposal <- mkProposal . NoConfidence . SJust $ GovPurposeId govActionId
      submitFailingSubTx
        (proposeSubTx proposal)
        [injectFailure . DijkstraSubGovPredFailure $ InvalidPrevGovActionId proposal]

    it "a parent index that does not exist" $ do
      useNativeGuardrailsScript
      govActionId <- mkMinFeeUpdateGovAction SNothing >>= submitGovAction
      govAction <-
        mkMinFeeUpdateGovAction . SJust $ govActionId {gaidGovActionIx = GovActionIx 999}
      proposal <- mkProposal govAction
      submitFailingSubTx
        (proposeSubTx proposal)
        [injectFailure . DijkstraSubGovPredFailure $ InvalidPrevGovActionId proposal]

  describe "UnelectedCommitteeVoters" $ do
    it "a member that is proposed but not enacted" $ do
      coldCredential <- KeyHashObj <$> freshKeyHash
      void $ submitUpdateCommittee Nothing mempty [(coldCredential, EpochInterval 10)] (1 %! 2)
      hotCredential <- registerCommitteeHotKey coldCredential
      govActionId <- submitGovAction InfoAction
      submitFailingSubTx
        (voteSubTx VoteYes (CommitteeVoter hotCredential) govActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            UnelectedCommitteeVoters [hotCredential]
        ]

    it "an unknown hot credential, alongside VotersDoNotExist" $ do
      govActionId <- submitGovAction InfoAction
      hotCredential <- KeyHashObj <$> freshKeyHash
      submitFailingSubTx
        (voteSubTx VoteYes (CommitteeVoter hotCredential) govActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            UnelectedCommitteeVoters [hotCredential]
        , injectFailure . DijkstraSubGovPredFailure $
            VotersDoNotExist [CommitteeVoter hotCredential]
        ]

  describe "VotersDoNotExist" $
    it "every unknown voter is reported" $ do
      govActionId <- submitGovAction InfoAction
      drepCredential <- KeyHashObj <$> freshKeyHash
      poolId <- freshKeyHash
      let voters = [DRepVoter drepCredential, StakePoolVoter poolId]
          subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & votingProceduresTxBodyL .~ votingProceduresFor VoteYes voters govActionId
      submitFailingSubTx
        subTx
        [ injectFailure . DijkstraSubGovPredFailure $
            VotersDoNotExist [DRepVoter drepCredential, StakePoolVoter poolId]
        ]

  describe "GovActionsDoNotExist" $
    it "a vote on an unknown gov action index" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      govActionId <- submitGovAction InfoAction
      let unknownGovActionId = govActionId {gaidGovActionIx = GovActionIx 99}
      submitFailingSubTx
        (voteSubTx VoteYes (DRepVoter drep) unknownGovActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            GovActionsDoNotExist [unknownGovActionId]
        ]

  describe "VotingOnExpiredGovAction" $
    it "a vote on an action past its lifetime" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      poolId <- freshKeyHash
      registerPool poolId
      govActionId <- mkProposal InfoAction >>= submitProposal
      passNEpochs 3
      let voter = StakePoolVoter poolId
      submitFailingSubTx
        (voteSubTx VoteYes voter govActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            VotingOnExpiredGovAction [(voter, govActionId)]
        ]

  describe "DisallowedVoters" $ do
    it "a committee vote on NoConfidence" $ do
      hotCredential <- NE.head <$> registerInitialCommittee
      govActionId <- submitGovAction $ NoConfidence SNothing
      let voter = CommitteeVoter hotCredential
      submitFailingSubTx
        (voteSubTx VoteYes voter govActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            DisallowedVoters [(voter, govActionId)]
        ]

    it "a committee vote on UpdateCommittee" $ do
      hotCredential <- NE.head <$> registerInitialCommittee
      newMember <- KeyHashObj <$> freshKeyHash
      govActionId <-
        submitUpdateCommittee Nothing mempty [(newMember, EpochInterval 10)] (1 %! 2)
      let voter = CommitteeVoter hotCredential
      submitFailingSubTx
        (voteSubTx VoteYes voter govActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            DisallowedVoters [(voter, govActionId)]
        ]

    it "a stake pool vote on NewConstitution" $ do
      (poolId, _, _) <- setupPoolWithStake $ Coin 42_000_000
      anchor <- arbitrary
      govActionId <- submitGovAction . NewConstitution SNothing $ Constitution anchor SNothing
      let voter = StakePoolVoter poolId
      submitFailingSubTx
        (voteSubTx VoteYes voter govActionId)
        [ injectFailure . DijkstraSubGovPredFailure $
            DisallowedVoters [(voter, govActionId)]
        ]

  describe "Accepted at the boundary" $ do
    it "a proposal carrying the constitution's guardrails script" $ do
      useNativeGuardrailsScript
      govAction <- mkMinFeeUpdateGovAction SNothing
      proposal <- mkProposal govAction
      submitTx_ . mkTopTxWithSubTxs . pure $ proposeSubTx proposal

    it "an expiration epoch one after the current one" $ do
      committeeCredential <- KeyHashObj <$> freshKeyHash
      proposal <-
        mkUpdateCommitteeProposal
          Nothing
          mempty
          [(committeeCredential, EpochInterval 1)]
          (0 %! 1)
      submitTx_ . mkTopTxWithSubTxs . pure $ proposeSubTx proposal

  describe "Composite tests" $ do
    it "failures of several proposals, in the order of the body" $ do
      firstAccount <- unregisteredAccount
      secondAccount <- unregisteredAccount
      firstProposal <- mkProposalWithAccountAddress InfoAction firstAccount
      secondProposal <- mkProposalWithAccountAddress InfoAction secondAccount
      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody & proposalProceduresTxBodyL .~ [firstProposal, secondProposal]
      submitFailingSubTx
        subTx
        [ injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist firstAccount
        , injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist secondAccount
        ]

    it "two failures of one committee update, in the order the rule checks them" $ do
      committeeCredential <- KeyHashObj <$> freshKeyHash
      currentEpochNo <- getsNES nesELL
      proposal <-
        mkUpdateCommitteeProposal
          Nothing
          (Set.singleton committeeCredential)
          [(committeeCredential, EpochInterval 0)]
          (1 %! 1)
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure . ConflictingCommitteeUpdate $
            NES.singleton committeeCredential
        , injectFailure . DijkstraSubGovPredFailure . ExpirationEpochTooSmall $
            NEM.singleton committeeCredential currentEpochNo
        ]

    it "seven failures of one proposal, in the order the rule checks them" $ do
      guardrailsScriptHash <- getGovPolicy
      wrongScriptHash <- impAddNativeScript . RequireSignature =<< freshKeyHash
      expectedDeposit <- getsPParams ppGovActionDepositL
      account <- unregisteredAccount
      anchor <- arbitrary
      let onWrongNetwork = account & accountAddressNetworkIdL .~ Mainnet
          suppliedDeposit = expectedDeposit <-> Coin 1
          govAction =
            TreasuryWithdrawals (Map.singleton onWrongNetwork mempty) (SJust wrongScriptHash)
          proposal =
            ProposalProcedure
              { pProcReturnAddr = onWrongNetwork
              , pProcGovAction = govAction
              , pProcDeposit = suppliedDeposit
              , pProcAnchor = anchor
              }
      submitFailingSubTx
        (proposeSubTx proposal)
        [ injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist onWrongNetwork
        , injectFailure . DijkstraSubGovPredFailure $
            TreasuryWithdrawalReturnAccountsDoNotExist [onWrongNetwork]
        , injectFailure . DijkstraSubGovPredFailure . ProposalDepositIncorrect $
            Mismatch {mismatchSupplied = suppliedDeposit, mismatchExpected = expectedDeposit}
        , injectFailure . DijkstraSubGovPredFailure $
            ProposalProcedureNetworkIdMismatch onWrongNetwork Testnet
        , injectFailure . DijkstraSubGovPredFailure $
            TreasuryWithdrawalsNetworkIdMismatch (NES.singleton onWrongNetwork) Testnet
        , injectFailure . DijkstraSubGovPredFailure $
            InvalidGuardrailsScriptHash (SJust wrongScriptHash) guardrailsScriptHash
        , injectFailure . DijkstraSubGovPredFailure $ ZeroTreasuryWithdrawals govAction
        ]

    it "failures of several sub-transactions, in sub-transaction order" $ do
      firstAccount <- unregisteredAccount
      secondAccount <- unregisteredAccount
      firstProposal <- mkProposalWithAccountAddress InfoAction firstAccount
      secondProposal <- mkProposalWithAccountAddress InfoAction secondAccount
      submitFailingTx
        (mkTopTxWithSubTxs [proposeSubTx firstProposal, proposeSubTx secondProposal])
        [ injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist firstAccount
        , injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist secondAccount
        ]

    it "the top-level failure precedes the same failure from a sub-transaction" $ do
      account <- unregisteredAccount
      proposal <- mkProposalWithAccountAddress InfoAction account
      submitFailingTx
        ( mkTopTxWithSubTxs [proposeSubTx proposal]
            & bodyTxL . proposalProceduresTxBodyL .~ [proposal]
        )
        [ injectFailure $ ProposalReturnAccountDoesNotExist account
        , injectFailure . DijkstraSubGovPredFailure $
            ProposalReturnAccountDoesNotExist account
        ]

  describe "Unreachable in Dijkstra" $ do
    it "DisallowedProposalDuringBootstrap does not fire for a non-bootstrap proposal" $ do
      anchor <- arbitrary
      proposal <- mkProposal . NewConstitution SNothing $ Constitution anchor SNothing
      submitTx_ . mkTopTxWithSubTxs . pure $ proposeSubTx proposal

    it "DisallowedVotesDuringBootstrap does not fire for a DRep vote" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      anchor <- arbitrary
      govActionId <- submitGovAction . NewConstitution SNothing $ Constitution anchor SNothing
      submitTx_ . mkTopTxWithSubTxs . pure $ voteSubTx VoteYes (DRepVoter drep) govActionId
      expectVote govActionId (DRepVoter drep) VoteYes

  describe "Sub-transaction semantics" $ do
    it "a top-level proposal and a sub-transaction proposal get distinct ids" $ do
      topProposal <- mkProposal InfoAction
      subProposal <- mkProposal InfoAction
      (subTx, subGovActionId) <- proposeSubTxWithStableId subProposal
      submittedTx <-
        submitTx $
          mkTopTxWithSubTxs [subTx] & bodyTxL . proposalProceduresTxBodyL .~ [topProposal]
      subGas <- getGovActionState subGovActionId
      topGas <- getGovActionState $ GovActionId (txIdTx submittedTx) (GovActionIx 0)
      gasProposalProcedure subGas `shouldBe` subProposal
      gasProposalProcedure topGas `shouldBe` topProposal
      gaidTxId subGovActionId `shouldNotBe` txIdTx submittedTx

    it "each sub-transaction numbers its own proposals from zero" $ do
      firstProposal <- mkProposal InfoAction
      secondProposal <- mkProposal InfoAction
      (firstSubTx, firstGovActionId) <- proposeSubTxWithStableId firstProposal
      (secondSubTx, secondGovActionId) <- proposeSubTxWithStableId secondProposal
      submitTx_ $ mkTopTxWithSubTxs [firstSubTx, secondSubTx]
      gaidGovActionIx firstGovActionId `shouldBe` GovActionIx 0
      gaidGovActionIx secondGovActionId `shouldBe` GovActionIx 0
      gaidTxId firstGovActionId `shouldNotBe` gaidTxId secondGovActionId
      firstGas <- getGovActionState firstGovActionId
      secondGas <- getGovActionState secondGovActionId
      gasProposalProcedure firstGas `shouldBe` firstProposal
      gasProposalProcedure secondGas `shouldBe` secondProposal

    it "each proposal in a sub-transaction gets the next index" $ do
      firstProposal <- mkProposal InfoAction
      secondProposal <- mkProposal InfoAction
      txIn <- freshFundedTxIn
      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & proposalProceduresTxBodyL .~ [firstProposal, secondProposal]
          subTxId = txIdTx subTx
      submitTx_ $ mkTopTxWithSubTxs [subTx]
      firstGas <- getGovActionState $ GovActionId subTxId (GovActionIx 0)
      secondGas <- getGovActionState $ GovActionId subTxId (GovActionIx 1)
      gasProposalProcedure firstGas `shouldBe` firstProposal
      gasProposalProcedure secondGas `shouldBe` secondProposal

    it "a sub-transaction votes on a proposal made by an earlier sibling" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      proposal <- mkProposal InfoAction
      (proposingSubTx, govActionId) <- proposeSubTxWithStableId proposal
      submitTx_ $
        mkTopTxWithSubTxs [proposingSubTx, voteSubTx VoteYes (DRepVoter drep) govActionId]
      expectVote govActionId (DRepVoter drep) VoteYes

    it "a sub-transaction cannot vote on a proposal made by a later sibling" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      proposal <- mkProposal InfoAction
      (proposingSubTx, govActionId) <- proposeSubTxWithStableId proposal
      submitFailingTx
        (mkTopTxWithSubTxs [voteSubTx VoteYes (DRepVoter drep) govActionId, proposingSubTx])
        [injectFailure . DijkstraSubGovPredFailure $ GovActionsDoNotExist [govActionId]]

    it "the top level votes on a proposal made by a sub-transaction" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      proposal <- mkProposal InfoAction
      (proposingSubTx, govActionId) <- proposeSubTxWithStableId proposal
      let votes = votingProceduresFor VoteYes [DRepVoter drep] govActionId
      submitTx_ $
        mkTopTxWithSubTxs [proposingSubTx] & bodyTxL . votingProceduresTxBodyL .~ votes
      expectVote govActionId (DRepVoter drep) VoteYes

    it "a vote in a sub-transaction replaces one cast by an earlier sibling" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      govActionId <- submitGovAction InfoAction
      submitTx_ $
        mkTopTxWithSubTxs
          [ voteSubTx VoteYes (DRepVoter drep) govActionId
          , voteSubTx VoteNo (DRepVoter drep) govActionId
          ]
      expectVote govActionId (DRepVoter drep) VoteNo

    it "a sub-transaction proposal is the parent of a later sibling's proposal" $ do
      useNativeGuardrailsScript
      parentAction <- mkMinFeeUpdateGovAction SNothing
      parentProposal <- mkProposal parentAction
      (parentSubTx, parentGovActionId) <- proposeSubTxWithStableId parentProposal
      childAction <- mkMinFeeUpdateGovAction $ SJust parentGovActionId
      childProposal <- mkProposal childAction
      submitTx_ $ mkTopTxWithSubTxs [parentSubTx, proposeSubTx childProposal]
      void $ getGovActionState parentGovActionId

    it "a committee member votes in a sub-transaction" $ do
      hotCredential <- NE.head <$> registerInitialCommittee
      govActionId <- submitGovAction InfoAction
      let voter = CommitteeVoter hotCredential
      submitTx_ . mkTopTxWithSubTxs . pure $ voteSubTx VoteYes voter govActionId
      expectVote govActionId voter VoteYes

    it "a stake pool votes in a sub-transaction" $ do
      (poolId, _, _) <- setupPoolWithStake $ Coin 42_000_000
      govActionId <- submitGovAction InfoAction
      let voter = StakePoolVoter poolId
      submitTx_ . mkTopTxWithSubTxs . pure $ voteSubTx VoteYes voter govActionId
      expectVote govActionId voter VoteYes

    it "a sub-transaction votes with a DRep it registers in that same sub-transaction" $ do
      govActionId <- submitGovAction InfoAction
      drepCredential <- KeyHashObj <$> freshKeyHash
      deposit <- getsPParams ppDRepDepositL
      txIn <- freshFundedTxIn
      submitTx_ . mkTopTxWithSubTxs . pure $
        voteSubTx VoteYes (DRepVoter drepCredential) govActionId
          & bodyTxL . inputsTxBodyL .~ [txIn]
          & bodyTxL . certsTxBodyL .~ [RegDRepTxCert drepCredential deposit SNothing]
      expectVote govActionId (DRepVoter drepCredential) VoteYes

    it "a DRep unregistered in a sub-transaction loses the votes it has cast" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      govActionId <- submitGovAction InfoAction
      submitVote_ VoteYes (DRepVoter drep) govActionId
      expectVote govActionId (DRepVoter drep) VoteYes
      deposit <- getsPParams ppDRepDepositL
      submitTx_ . mkTopTxWithSubTxs . pure . mkBasicTx $
        mkBasicTxBody & certsTxBodyL .~ [UnRegDRepTxCert drep deposit]
      cleanedGas <- getGovActionState govActionId
      Map.lookup drep (gasDRepVotes cleanedGas) `shouldBe` Nothing

    it "a sub-transaction proposes and votes in the same body" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      existingGovActionId <- submitGovAction InfoAction
      proposal <- mkProposal InfoAction
      txIn <- freshFundedTxIn
      let votes = votingProceduresFor VoteYes [DRepVoter drep] existingGovActionId
          subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & proposalProceduresTxBodyL .~ [proposal]
                & votingProceduresTxBodyL .~ votes
      submitTx_ $ mkTopTxWithSubTxs [subTx]
      newGas <- getGovActionState $ GovActionId (txIdTx subTx) (GovActionIx 0)
      gasProposalProcedure newGas `shouldBe` proposal
      expectVote existingGovActionId (DRepVoter drep) VoteYes

  describe "A phase-2 invalid top level transaction" $
    it "raises no SUBGOV failure" $ do
      account <- unregisteredAccount
      proposal <- mkProposalWithAccountAddress InfoAction account
      topTx <- phase2InvalidTxWithSubTxs [proposeSubTx proposal]
      withNoFixup $ submitTx_ topTx

-- | Expect the given voter to have cast the given vote on a governance action.
expectVote :: (HasCallStack, DijkstraEraImp era) => GovActionId -> Voter -> Vote -> ImpTestM era ()
expectVote govActionId voter vote = do
  gas <- getGovActionState govActionId
  let castVote = case voter of
        CommitteeVoter hotCredential -> Map.lookup hotCredential $ gasCommitteeVotes gas
        DRepVoter drepCredential -> Map.lookup drepCredential $ gasDRepVotes gas
        StakePoolVoter poolId -> Map.lookup poolId $ gasStakePoolVotes gas
  castVote `shouldBe` Just vote

-- | Voting procedures in which each of the given voters casts the same vote
-- on one governance action.
votingProceduresFor :: Vote -> [Voter] -> GovActionId -> VotingProcedures era
votingProceduresFor vote voters govActionId =
  VotingProcedures . Map.fromList $
    [(voter, Map.singleton govActionId (VotingProcedure vote SNothing)) | voter <- voters]

-- | A sub-transaction that submits a single proposal.
proposeSubTx :: DijkstraEraImp era => ProposalProcedure era -> Tx SubTx era
proposeSubTx proposal = mkBasicTx $ mkBasicTxBody & proposalProceduresTxBodyL .~ [proposal]

-- | A sub-transaction that submits a single proposal, paired with the
-- `GovActionId` that proposal will be assigned.
--
-- The sub-transaction is given an input so that fixup leaves its body alone.
-- Fixup supplies an input to any sub-transaction that has none, which would
-- change the sub-transaction id and with it the ids of its proposals.
proposeSubTxWithStableId ::
  DijkstraEraImp era =>
  ProposalProcedure era ->
  ImpTestM era (Tx SubTx era, GovActionId)
proposeSubTxWithStableId proposal = do
  txIn <- freshFundedTxIn
  let subTx = proposeSubTx proposal & bodyTxL . inputsTxBodyL .~ [txIn]
  pure (subTx, GovActionId (txIdTx subTx) (GovActionIx 0))

-- | Point the constitution at a native guardrails script.
--
-- A proposal carrying a guardrails policy must witness that script, and fixup
-- can witness a native script inside a sub-transaction but not the Plutus
-- script the genesis constitution carries. A sub-transaction can therefore
-- only submit a `ParameterChange` or `TreasuryWithdrawals` once this has run.
useNativeGuardrailsScript :: DijkstraEraImp era => ImpTestM era ()
useNativeGuardrailsScript = do
  scriptHash <- impAddNativeScript . RequireSignature =<< freshKeyHash
  modifyNES $
    nesEsL . epochStateGovStateL . constitutionGovStateL . constitutionGuardrailsScriptHashL
      .~ SJust scriptHash
