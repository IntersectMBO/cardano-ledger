{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Gen.Proposals (
  genProposals,
) where

import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.Alonzo.PParams (ppuCostModelsL)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Network (..),
  ProtVer (..),
  StrictMaybe (..),
  addEpochInterval,
  binOpEpochNo,
  pvMajor,
  succVersion,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  CoinPerByte (..),
  PParams,
  PParamsUpdate,
  emptyPParamsUpdate,
  ppCommitteeMaxTermLengthL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
  ppProtocolVersionL,
  ppuCoinsPerUTxOByteL,
  ppuCollateralPercentageL,
  ppuCommitteeMaxTermLengthL,
  ppuGovActionLifetimeL,
  ppuMaxBBSizeL,
  ppuMaxBHSizeL,
  ppuMaxTxSizeL,
  ppuMaxValSizeL,
  ppuNOptL,
  ppuPoolDepositCompactL,
 )
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionState (..),
  GovPurposeId (..),
  GovRelation,
  ProposalProcedure (..),
  Proposals,
  Vote,
  fromPrevGovActionIds,
  grCommitteeL,
  grConstitutionL,
  grHardForkL,
  grPParamUpdateL,
  isCommitteeVotingAllowed,
  isDRepVotingAllowed,
  isStakePoolVotingAllowed,
  pGraphL,
  pGraphNodesL,
  pRootsL,
  prRootL,
  proposalsAddAction,
  proposalsLookupId,
 )
import Cardano.Ledger.Conway.PParams (
  ppuDRepDepositCompactL,
  ppuGovActionDepositCompactL,
 )
import Cardano.Ledger.Conway.State (
  CommitteeState (..),
  ConwayCertState,
  accountsL,
  accountsMapL,
  authorizedHotCommitteeCredentials,
  certDStateL,
  certPStateL,
  certVStateL,
  psStakePoolsL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Control.Monad.State.Strict (MonadState)
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Gen (freshCredential)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (HasKeyPairs)

-- The 'ConwayCertState' supplies four pieces of registration state read by
-- this generator: registered staking credentials (for return addresses and
-- withdrawal targets), the 'CommitteeState' (cold creds for committee
-- removals and hot creds for committee voters), registered DReps (for DRep
-- voters), and registered stake pools (for pool voters).
genProposals ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  PParams ConwayEra ->
  EpochNo ->
  StrictMaybe ScriptHash ->
  ConwayCertState ConwayEra ->
  GovRelation StrictMaybe ->
  m (Proposals ConwayEra)
genProposals pp curEpoch policy certState pgais = do
  n <- choose (0 :: Int, 30)
  go n (def & pRootsL .~ fromPrevGovActionIds pgais)
  where
    go !remaining !ps
      | remaining <= 0 = pure ps
      | otherwise = do
          ps' <- addOne ps
          go (remaining - 1) ps'
    addOne ps = do
      gas <- genGovActionState pp curEpoch policy certState ps
      case proposalsAddAction gas ps of
        Just ps' -> pure ps'
        Nothing ->
          error
            "Test.Cardano.Ledger.Conway.Gen.Proposals.genProposals: \
            \pickParent selects from the current forest, so parents are always valid"

genGovActionState ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  PParams ConwayEra ->
  EpochNo ->
  StrictMaybe ScriptHash ->
  ConwayCertState ConwayEra ->
  Proposals ConwayEra ->
  m (GovActionState ConwayEra)
genGovActionState pp curEpoch policy certState ps = do
  gasId <- arbitrary
  gasProposalProcedure <- genProposalProcedure pp curEpoch policy certState ps
  let action = pProcGovAction gasProposalProcedure
  (gasCommitteeVotes, gasDRepVotes, gasStakePoolVotes) <-
    genVoters certState curEpoch action
  let gasExpiresAfter = addEpochInterval curEpoch (pp ^. ppGovActionLifetimeL)
      gasProposedIn = curEpoch
  pure GovActionState {..}

genProposalProcedure ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  PParams ConwayEra ->
  EpochNo ->
  StrictMaybe ScriptHash ->
  ConwayCertState ConwayEra ->
  Proposals ConwayEra ->
  m (ProposalProcedure ConwayEra)
genProposalProcedure pp curEpoch policy certState ps = do
  let pProcDeposit = pp ^. ppGovActionDepositL
  pProcReturnAddr <- genReturnAccountAddress certState
  pProcGovAction <- genGovActionFor pp curEpoch policy certState ps
  pProcAnchor <- arbitrary
  pure ProposalProcedure {..}

genGovActionFor ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  PParams ConwayEra ->
  EpochNo ->
  StrictMaybe ScriptHash ->
  ConwayCertState ConwayEra ->
  Proposals ConwayEra ->
  m (GovAction ConwayEra)
genGovActionFor pp curEpoch policy certState ps =
  oneof
    [ genParameterChangeAction
    , genHardForkInitiationAction
    , genTreasuryWithdrawalsAction
    , NoConfidence <$> pickParent grCommitteeL ps
    , genUpdateCommitteeAction
    , NewConstitution <$> pickParent grConstitutionL ps <*> arbitrary
    , pure InfoAction
    ]
  where
    genParameterChangeAction = do
      parent <- pickParent grPParamUpdateL ps
      ppu <- genWellFormedPParamsUpdate
      pure $ ParameterChange parent ppu policy

    genHardForkInitiationAction = do
      parent <- pickParent grHardForkL ps
      let parentProtVer = lookupParentProtVer parent
          maxMajor = succVersionOrCurrent (pp ^. ppProtocolVersionL)
      newProtVer <- genFollowingProtVer maxMajor parentProtVer
      pure $ HardForkInitiation parent newProtVer
      where
        lookupParentProtVer SNothing = pp ^. ppProtocolVersionL
        lookupParentProtVer (SJust (GovPurposeId gaid)) =
          case proposalsLookupId gaid ps of
            Just gas
              | HardForkInitiation _ pv <- pProcGovAction (gasProposalProcedure gas) -> pv
            _ -> pp ^. ppProtocolVersionL

        -- The chain-wide cap on a hard-fork's major version: 'succVersion' of
        -- the current major, or the current major itself if already at
        -- 'maxBound'. Copied (not shared) from
        -- "Test.Cardano.Ledger.Constrained.Conway.Gov" to avoid a dependency
        -- on @cardano-ledger-test@.
        succVersionOrCurrent pv = fromMaybe (pvMajor pv) (succVersion (pvMajor pv))

        -- Pick a 'ProtVer' that satisfies 'pvCanFollow' against the given
        -- parent /and/ stays within the chain-wide cap: the GOV rule only
        -- accepts hard-fork proposals whose major is at most 'succ' of the
        -- current PParams major. Once a parent has already reached
        -- 'maxMajor', children may only bump the minor.
        genFollowingProtVer maxMajor (ProtVer parentMajor parentMinor) =
          case succVersion parentMajor of
            Just nextMajor
              | nextMajor <= maxMajor ->
                  oneof
                    [ pure $ ProtVer nextMajor 0
                    , pure $ ProtVer parentMajor (parentMinor + 1)
                    ]
            _ -> pure $ ProtVer parentMajor (parentMinor + 1)

    genTreasuryWithdrawalsAction = do
      withdrawals <- genTreasuryWithdrawalsMap
      pure $ TreasuryWithdrawals withdrawals policy
      where
        -- Non-empty withdrawals map with positive 'Coin' values. Keys are
        -- drawn with the same 90/10 registered/fresh split as
        -- 'genReturnAccountAddress'.
        genTreasuryWithdrawalsMap = do
          entries <- listOf1 $ do
            addr <- genReturnAccountAddress certState
            amount <- Coin . getPositive <$> arbitrary
            pure (addr, amount)
          pure $ Map.fromList entries

    genUpdateCommitteeAction = do
      parent <- pickParent grCommitteeL ps
      let currentCommittee =
            Map.keysSet . csCommitteeCreds $
              certState ^. certVStateL . vsCommitteeStateL
          maxTerm = pp ^. ppCommitteeMaxTermLengthL
          EpochInterval maxTermWord = maxTerm
          lowerEpoch = binOpEpochNo (+) curEpoch (EpochNo 1)
          upperEpoch = addEpochInterval curEpoch maxTerm
      membersToRemove <- uniformSubSet Nothing currentCommittee
      additions <-
        if maxTermWord == 0
          then pure []
          else listOf $ do
            cred <- freshCredential
            expiry <- uniformRM (lowerEpoch, upperEpoch)
            pure (cred, expiry)
      let membersToAdd = Map.fromList additions
          removalsCleaned = membersToRemove `Set.difference` Map.keysSet membersToAdd
      quorum <- arbitrary
      pure $ UpdateCommittee parent removalsCleaned membersToAdd quorum

-- | Pick a parent for an action of the given purpose: either the original root,
-- or any node of that purpose already added to the in-progress forest.
pickParent ::
  MonadGen m =>
  (forall f. Lens' (GovRelation f) (f (GovPurposeId p))) ->
  Proposals ConwayEra ->
  m (StrictMaybe (GovPurposeId p))
pickParent govRelL ps =
  elements $
    (ps ^. pRootsL . govRelL . prRootL)
      : fmap SJust (Map.keys (ps ^. pGraphL . govRelL . pGraphNodesL))

-- | Generate a non-empty 'PParamsUpdate' whose every constrained field is
-- non-zero (mirrors 'ppuWellFormed' but with the bootstrap-phase and pre-v11
-- carve-outs removed, so the constraint applies at every protocol version).
-- 'ppuCostModelsL' is cleared because the cost-model generator is too slow.
genWellFormedPParamsUpdate ::
  forall m. MonadGen m => m (PParamsUpdate ConwayEra)
genWellFormedPParamsUpdate = do
  ppu0 <- (& ppuCostModelsL .~ SNothing) <$> arbitrary `suchThat` notEmpty
  pure ppu0
    >>= bump ppuMaxBBSizeL 0 posW
    >>= bump ppuMaxTxSizeL 0 posW
    >>= bump ppuMaxBHSizeL 0 posW
    >>= bump ppuMaxValSizeL 0 posW
    >>= bump ppuCollateralPercentageL 0 posW
    >>= bump ppuNOptL 0 posW
    >>= bump ppuCommitteeMaxTermLengthL (EpochInterval 0) posEI
    >>= bump ppuGovActionLifetimeL (EpochInterval 0) posEI
    >>= bump ppuPoolDepositCompactL (CompactCoin 0) posCC
    >>= bump ppuGovActionDepositCompactL (CompactCoin 0) posCC
    >>= bump ppuDRepDepositCompactL (CompactCoin 0) posCC
    >>= bump ppuCoinsPerUTxOByteL (CoinPerByte (CompactCoin 0)) posCPB
  where
    -- Reject only the (mathematically possible, practically unreachable) all-
    -- SNothing record after clearing cost models. Rejection rate ≈ 2^-29, so
    -- this isn't the wasteful `suchThat` we want to avoid.
    notEmpty ppu = (ppu & ppuCostModelsL .~ SNothing) /= emptyPParamsUpdate

    bump ::
      Eq t =>
      Lens' (PParamsUpdate ConwayEra) (StrictMaybe t) ->
      t ->
      m t ->
      PParamsUpdate ConwayEra ->
      m (PParamsUpdate ConwayEra)
    bump l z gen ppu
      | ppu ^. l == SJust z = (\v -> ppu & l .~ SJust v) <$> gen
      | otherwise = pure ppu

    posW :: forall t. (Num t, Ord t, Arbitrary t) => m t
    posW = getPositive <$> arbitrary
    posEI = EpochInterval <$> posW
    posCC = CompactCoin <$> posW
    posCPB = CoinPerByte <$> posCC

-- | 90% draw a registered account credential, 10% a fresh credential; always
-- wrap as an 'AccountAddress' on the 'Testnet' network.
genReturnAccountAddress ::
  (HasKeyPairs s, MonadState s m, MonadGen m, HasStatefulGen g m) =>
  ConwayCertState ConwayEra ->
  m AccountAddress
genReturnAccountAddress certState = do
  cred <- genStakingCredential
  pure $ AccountAddress Testnet (AccountId cred)
  where
    genStakingCredential =
      frequency
        [ (9, fromAccounts)
        , (1, freshCredential)
        ]
    accountsMap = certState ^. certDStateL . accountsL . accountsMapL
    fromAccounts =
      case Map.keys accountsMap of
        [] -> freshCredential
        keys -> elements keys

-- | Generate eligible voter maps for the given action. For each voter type we
-- draw a (possibly empty) subset of the voters that are both registered in the
-- supplied 'ConwayCertState' and allowed to vote on the action.
genVoters ::
  (HasStatefulGen g m, MonadGen m) =>
  ConwayCertState ConwayEra ->
  EpochNo ->
  GovAction ConwayEra ->
  m
    ( Map (Credential HotCommitteeRole) Vote
    , Map (Credential DRepRole) Vote
    , Map (KeyHash StakePool) Vote
    )
genVoters certState curEpoch action = do
  let vState = certState ^. certVStateL
      pState = certState ^. certPStateL
      committeeState = vState ^. vsCommitteeStateL
      committeeCreds = authorizedHotCommitteeCredentials committeeState
      dRepCreds = Map.keysSet (vState ^. vsDRepsL)
      poolIds = Map.keysSet (pState ^. psStakePoolsL)
      committeeEligible
        | isCommitteeVotingAllowed curEpoch committeeState action = committeeCreds
        | otherwise = Set.empty
      dRepEligible
        | isDRepVotingAllowed action = dRepCreds
        | otherwise = Set.empty
      poolEligible
        | isStakePoolVotingAllowed action = poolIds
        | otherwise = Set.empty
  committeeVotes <- subsetVotes committeeEligible
  dRepVotes <- subsetVotes dRepEligible
  poolVotes <- subsetVotes poolEligible
  pure (committeeVotes, dRepVotes, poolVotes)
  where
    subsetVotes ::
      (HasStatefulGen g m, MonadGen m, Ord k) =>
      Set k ->
      m (Map k Vote)
    subsetVotes voters = do
      chosen <- uniformSubSet Nothing voters
      assignments <- traverse (\v -> (,) v <$> arbitrary) (Set.toList chosen)
      pure $ Map.fromList assignments
