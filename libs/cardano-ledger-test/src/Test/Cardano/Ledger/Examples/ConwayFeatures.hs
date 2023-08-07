{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.ConwayFeatures (conwayFeatures)
where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Network (..),
  StrictMaybe (..),
  mkTxIx,
  natVersion,
  textToUrl,
 )
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (ConwayEraTxBody)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  coerceKeyRole,
  hashKey,
 )
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.Shelley.API (
  Hash,
  PoolDistr (..),
  ProtVer (..),
  VerKeyVRF,
  hashVerKeyVRF,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.CostModel (freeV1V2CostModels)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Examples.BabbageFeatures (
  InitOutputs (..),
  KeyPairRole (..),
  TestCaseData (..),
  txFromTestCaseData,
  utxoFromTestCaseData,
 )
import Test.Cardano.Ledger.Examples.STSTestUtils (
  mkGenesisTxIn,
  runEPOCH,
  runLEDGER,
  trustMeP,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxOutField (..),
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkKeyPair',
  mkVRFKeyPair,
 )
import Test.Cardano.Protocol.Crypto.VRF (VRFKeyPair (..))
import Test.Tasty
import Test.Tasty.HUnit

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Test.Cardano.Ledger.Generic.PrettyCore ()
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE

stakeKeyHash :: forall era. Era era => Proof era -> KeyHash 'Staking (EraCrypto era)
stakeKeyHash _pf = hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 2)

stakePoolKeys :: forall era. Era era => Proof era -> KeyPair 'StakePool (EraCrypto era)
stakePoolKeys _pf = mkKeyPair' @(EraCrypto era) (RawSeed 0 0 0 0 20)

stakePoolKeyHash :: forall era. Era era => Proof era -> KeyHash 'StakePool (EraCrypto era)
stakePoolKeyHash pf = hashKey . vKey $ stakePoolKeys pf

keys1 :: forall era. Era era => Proof era -> KeyPair 'Payment (EraCrypto era)
keys1 _pf = mkKeyPair' @(EraCrypto era) (RawSeed 1 3 1 1 1)

addrKeys1 :: forall era. Era era => Proof era -> Addr (EraCrypto era)
addrKeys1 pf = Addr Testnet pCred sCred
  where
    pCred = KeyHashObj . hashKey . vKey $ keys1 pf
    sCred = StakeRefBase . KeyHashObj . coerceKeyRole . hashKey . vKey $ keys1 pf

keys2 :: forall era. Era era => Proof era -> KeyPair 'Payment (EraCrypto era)
keys2 _pf = mkKeyPair' @(EraCrypto era) (RawSeed 2 2 2 2 2)

addrKeys2 :: forall era. Era era => Proof era -> Addr (EraCrypto era)
addrKeys2 pf = Addr Testnet pCred sCred
  where
    pCred = KeyHashObj . hashKey . vKey $ keys2 pf
    sCred = StakeRefBase . KeyHashObj . coerceKeyRole . hashKey . vKey $ keys2 pf

vrfKeyHash :: forall c. Crypto c => Hash c (VerKeyVRF c)
vrfKeyHash = hashVerKeyVRF . vrfVerKey . mkVRFKeyPair @c $ RawSeed 0 0 0 0 0

someTxIn :: (CH.HashAlgorithm (HASH c), HasCallStack) => TxIn c
someTxIn = mkGenesisTxIn 1

proposedConstitution :: forall era. Scriptic era => Constitution era
proposedConstitution = Constitution (SLE.mkDummySafeHash Proxy 1) SNothing

newConstitutionProposal :: forall era. Scriptic era => Proof era -> ProposalProcedure era
newConstitutionProposal pf =
  ProposalProcedure
    (Coin proposalDeposit)
    (RewardAcnt Mainnet (KeyHashObj (stakeKeyHash pf)))
    (NewConstitution (proposedConstitution @era))
    (Anchor (fromJust $ textToUrl "new.constitution.com") (SLE.mkDummySafeHash Proxy 1))

anotherConstitutionProposal :: forall era. Scriptic era => Proof era -> ProposalProcedure era
anotherConstitutionProposal pf =
  ProposalProcedure
    (Coin proposalDeposit)
    (RewardAcnt Mainnet (KeyHashObj (stakeKeyHash pf)))
    (NewConstitution (Constitution (SLE.mkDummySafeHash Proxy 2) SNothing))
    (Anchor (fromJust $ textToUrl "another.constitution.com") (SLE.mkDummySafeHash Proxy 2))

voteYes :: forall era. Scriptic era => Proof era -> GovernanceActionId (EraCrypto era) -> VotingProcedures era
voteYes pf govActionId =
  VotingProcedures $
    Map.fromList
      [ (StakePoolVoter (stakePoolKeyHash pf), Map.fromList [(govActionId, VotingProcedure VoteYes SNothing)])
      ]

govActionState :: ProposalProcedure era -> GovernanceActionState era
govActionState ProposalProcedure {..} =
  GovernanceActionState
    mempty
    mempty
    mempty
    pProcDeposit
    pProcReturnAddr
    pProcGovernanceAction
    (EpochNo 0)

govActionStateWithVote :: ProposalProcedure era -> KeyHash 'StakePool (EraCrypto era) -> Vote -> GovernanceActionState era
govActionStateWithVote ProposalProcedure {..} kh v =
  GovernanceActionState
    mempty
    mempty
    (Map.fromList [(kh, v)])
    pProcDeposit
    pProcReturnAddr
    pProcGovernanceAction
    (EpochNo 0)

-- | Value for the actual threshold, plus a small epsilon for GT (>) relation
spoThreshold :: Rational
spoThreshold = 51 % 100 + 1 % 100000000000

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls freeV1V2CostModels
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @7) 0
  , CollateralPercentage 1
  , AdaPerUTxOByte (CoinPerByte (Coin 5))
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs

fee :: Integer
fee = 5

proposalDeposit :: Integer
proposalDeposit = 10

expectRight :: Show a => String -> Either a b -> b
expectRight msg = either (error msg . show) id

proposal :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
proposal pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' []
          , Outputs'
              [ newTxOut pf [Address (addrKeys2 pf), Amount (inject $ Coin (15000 - 10000 - fee - proposalDeposit))] -- 4985
              , newTxOut pf [Address (addrKeys1 pf), Amount (inject $ Coin 10000)]
              ]
          , Txfee (Coin fee)
          , GovernanceProcs (GovernanceProcedures (VotingProcedures mempty) (Seq.fromList [newConstitutionProposal pf]))
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (addrKeys1 pf)
                  , Amount (inject $ Coin 15000)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [KeyPairPayment (keys1 pf)]
    , otherWitsFields = []
    }

secondProposal :: forall era. (Scriptic era, EraTxBody era) => Proof era -> GovernanceActionId (EraCrypto era) -> TestCaseData era
secondProposal pf govActionId =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [TxIn (gaidTxId govActionId) (mkTxIx 1)]
          , Collateral' []
          , Outputs'
              [ newTxOut pf [Address (addrKeys2 pf), Amount (inject $ Coin (10000 - 7000 - fee - proposalDeposit))]
              , newTxOut pf [Address (addrKeys1 pf), Amount (inject $ Coin 7000)]
              ]
          , Txfee (Coin fee)
          , GovernanceProcs (GovernanceProcedures (VotingProcedures mempty) (Seq.fromList [anotherConstitutionProposal pf]))
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = []
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [KeyPairPayment (keys1 pf)]
    , otherWitsFields = []
    }

vote :: forall era. (Scriptic era, EraTxBody era) => Proof era -> GovernanceActionId (EraCrypto era) -> TestCaseData era
vote pf govActionId =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [TxIn (gaidTxId govActionId) (mkTxIx 0)]
          , Outputs'
              [ newTxOut pf [Address (addrKeys1 pf), Amount (inject $ Coin (4985 - 1995 - fee))]
              , newTxOut pf [Address (addrKeys2 pf), Amount (inject $ Coin 1995)]
              ]
          , Txfee (Coin fee)
          , GovernanceProcs (GovernanceProcedures (voteYes pf govActionId) Seq.empty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = []
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [KeyPairPayment (keys2 pf), KeyPairStakePool (stakePoolKeys pf)]
    , otherWitsFields = []
    }

testGovernance ::
  forall era.
  ( State (EraRule "LEDGER" era) ~ LedgerState era
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "EPOCH" era))
  , Scriptic era
  , GoodCrypto (EraCrypto era)
  , EraTx era
  , ConwayEraTxBody era
  , EraGovernance era
  , GovernanceState era ~ ConwayGovernance era
  ) =>
  Proof era ->
  Assertion
testGovernance pf = do
  let
    (utxo0, _) = utxoFromTestCaseData pf (proposal pf)
    initialGovernance = def :: ConwayGovernance era
    initialLedgerState = LedgerState (smartUTxOState (pp pf) utxo0 (Coin 0) (Coin 0) initialGovernance) def

    proposalTx = txFromTestCaseData pf (proposal pf)

    govActionId = GovernanceActionId (txid (proposalTx ^. bodyTxL)) (GovernanceActionIx 0)
    expectedGovState0 = ConwayGovState $ Map.fromList [(govActionId, govActionState (newConstitutionProposal pf))]
    expectedGovernance0 = ConwayGovernance expectedGovState0 (initialGovernance ^. cgRatifyL)

    eitherLedgerState0 = runLEDGER (LEDGER pf) initialLedgerState (pp pf) (trustMeP pf True proposalTx)
    ledgerState0@(LedgerState (UTxOState _ _ _ govState0 _) _) =
      expectRight "Error running LEDGER when proposing: " eitherLedgerState0

  assertEqual "govState after proposal" govState0 expectedGovernance0

  let
    voteTx = txFromTestCaseData pf (vote pf govActionId)
    gas = govActionStateWithVote (newConstitutionProposal pf) (stakePoolKeyHash pf) VoteYes
    expectedGovState1 = ConwayGovState $ Map.fromList [(govActionId, gas)]
    expectedGovernance1 = ConwayGovernance expectedGovState1 (initialGovernance ^. cgRatifyL)
    eitherLedgerState1 = runLEDGER (LEDGER pf) ledgerState0 (pp pf) (trustMeP pf True voteTx)
    ledgerState1@(LedgerState (UTxOState _ _ _ govState1 _) _) =
      expectRight "Error running LEDGER when voting: " eitherLedgerState1

  assertEqual "govState after vote" govState1 expectedGovernance1

  let
    epochState0 =
      (def :: EpochState era)
        & curPParamsEpochStateL .~ pp pf
        & esLStateL .~ ledgerState1
    poolDistr =
      PoolDistr
        ( Map.fromList
            [
              ( stakePoolKeyHash pf
              , IndividualPoolStake
                  spoThreshold
                  (vrfKeyHash @(EraCrypto era))
              )
            ]
        )
    eitherEpochState1 = runEPOCH (EPOCH pf) epochState0 (EpochNo 2) poolDistr
    epochState1 = expectRight "Error running runEPOCH: " eitherEpochState1
    ledgerState2 = epochState1 ^. esLStateL
    constitution = (ensConstitution . rsEnactState) (epochState1 ^. esLStateL . lsUTxOStateL . utxosGovernanceL . cgRatifyL)
  assertEqual "constitution after enactment" constitution (proposedConstitution @era)

  let
    secondProposalTx = txFromTestCaseData pf (secondProposal pf govActionId)
    secondGovActionId = GovernanceActionId (txid (secondProposalTx ^. bodyTxL)) (GovernanceActionIx 0)
    expectedGovState2 = ConwayGovState $ Map.fromList [(secondGovActionId, govActionState (anotherConstitutionProposal pf))]
    expectedGovernance2 = ConwayGovernance expectedGovState2 (ledgerState2 ^. lsUTxOStateL . utxosGovernanceL . cgRatifyL)
    eitherLedgerState3 = runLEDGER (LEDGER pf) ledgerState2 (pp pf) (trustMeP pf True secondProposalTx)
    ledgerState3@(LedgerState (UTxOState _ _ _ govState2 _) _) =
      expectRight "Error running LEDGER when proposing:" eitherLedgerState3

  assertEqual "govState after second proposal" govState2 expectedGovernance2

  let
    epochState2 = epochState1 & esLStateL .~ ledgerState3
    eitherEpochState2 = runEPOCH (EPOCH pf) epochState2 (EpochNo 2) poolDistr
    epochState3 = expectRight "Error running runEPOCH: " eitherEpochState2
    constitution1 = (ensConstitution . rsEnactState) (epochState3 ^. esLStateL . lsUTxOStateL . utxosGovernanceL . cgRatifyL)
  assertEqual "constitution after enactment after no votes" constitution1 (proposedConstitution @era)

  case toList $ rsFuture (epochState3 ^. esLStateL . lsUTxOStateL . utxosGovernanceL . cgRatifyL) of
    [(gId, gas')] ->
      assertEqual
        "un-enacted govAction is recorded in rsFuture"
        (gId, gas')
        (secondGovActionId, govActionState (anotherConstitutionProposal pf))
    x -> error $ "Unexpected `rsFuture` after runEPOCH: " ++ show x

conwayFeatures :: TestTree
conwayFeatures =
  testGroup
    "Governance examples"
    [testCase "governance" $ testGovernance (Conway Mock)]
