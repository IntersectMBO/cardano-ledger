{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Certs (
  ConwayCERTS,
  ConwayCertsPredFailure (..),
  ConwayCertsEvent (..),
  CertsEnv (..),
  updateDormantDRepExpiry,
) where

import Cardano.Ledger.BaseTypes (
  EpochNo (EpochNo),
  Globals (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe,
  binOpEpochNo,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (VState, certDStateL, certVStateL, vsDRepsL, vsNumDormantEpochsL)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayCERT, ConwayCERTS, ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Committee,
  GovActionPurpose (..),
  GovActionState,
  GovPurposeId,
  Voter (DRepVoter),
  VotingProcedures (unVotingProcedures),
 )
import Cardano.Ledger.Conway.Rules.Cert (CertEnv (CertEnv), ConwayCertEvent, ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure, updateDRepExpiry)
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Shelley.API (
  CertState (..),
  Coin,
  RewardAccount,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyPoolPredFailure,
  drainWithdrawals,
  validateZeroRewards,
 )
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  validateTrans,
 )
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Sequence (Seq (..))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data CertsEnv era = CertsEnv
  { certsTx :: !(Tx era)
  , certsPParams :: !(PParams era)
  , certsSlotNo :: !SlotNo
  , certsCurrentEpoch :: !EpochNo
  , certsCurrentCommittee :: StrictMaybe (Committee era)
  , certsCommitteeProposals :: Map.Map (GovPurposeId 'CommitteePurpose era) (GovActionState era)
  }

data ConwayCertsPredFailure era
  = -- | Withdrawals that are missing or do not withdrawal the entire amount
    WithdrawalsNotInRewardsCERTS
      !(Map.Map (RewardAccount (EraCrypto era)) Coin)
  | -- | CERT rule subtransition Failures
    CertFailure !(PredicateFailure (EraRule "CERT" era))
  deriving (Generic)

type instance EraRuleFailure "CERTS" (ConwayEra c) = ConwayCertsPredFailure (ConwayEra c)

type instance EraRuleEvent "CERTS" (ConwayEra c) = ConwayCertsEvent (ConwayEra c)

instance InjectRuleFailure "CERTS" ConwayCertsPredFailure (ConwayEra c)

instance InjectRuleFailure "CERTS" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = CertFailure

instance InjectRuleFailure "CERTS" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = CertFailure . injectFailure

deriving stock instance
  Eq (PredicateFailure (EraRule "CERT" era)) =>
  Eq (ConwayCertsPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "CERT" era)) =>
  Show (ConwayCertsPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "CERT" era)) =>
  NoThunks (ConwayCertsPredFailure era)

instance
  NFData (PredicateFailure (EraRule "CERT" era)) =>
  NFData (ConwayCertsPredFailure era)

newtype ConwayCertsEvent era = CertEvent (Event (EraRule "CERT" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "CERT" era)) => Eq (ConwayCertsEvent era)

instance NFData (Event (EraRule "CERT" era)) => NFData (ConwayCertsEvent era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "CERT" era))
  ) =>
  EncCBOR (ConwayCertsPredFailure era)
  where
  encCBOR =
    encode . \case
      WithdrawalsNotInRewardsCERTS rs -> Sum (WithdrawalsNotInRewardsCERTS @era) 0 !> To rs
      CertFailure x -> Sum (CertFailure @era) 1 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "CERT" era))
  ) =>
  DecCBOR (ConwayCertsPredFailure era)
  where
  decCBOR = decode $ Summands "ConwayGovPredFailure" $ \case
    0 -> SumD WithdrawalsNotInRewardsCERTS <! From
    1 -> SumD CertFailure <! From
    k -> Invalid k

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , State (EraRule "CERT" era) ~ CertState era
  , Signal (EraRule "CERT" era) ~ TxCert era
  , Environment (EraRule "CERT" era) ~ CertEnv era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  ) =>
  STS (ConwayCERTS era)
  where
  type State (ConwayCERTS era) = CertState era
  type Signal (ConwayCERTS era) = Seq (TxCert era)
  type Environment (ConwayCERTS era) = CertsEnv era
  type BaseM (ConwayCERTS era) = ShelleyBase
  type
    PredicateFailure (ConwayCERTS era) =
      ConwayCertsPredFailure era
  type Event (ConwayCERTS era) = ConwayCertsEvent era

  transitionRules = [conwayCertsTransition @era]

conwayCertsTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  , Environment (EraRule "CERT" era) ~ CertEnv era
  , Signal (EraRule "CERT" era) ~ TxCert era
  ) =>
  TransitionRule (ConwayCERTS era)
conwayCertsTransition = do
  TRC
    ( env@(CertsEnv tx pp slot currentEpoch committee committeeProposals)
      , certState
      , certificates
      ) <-
    judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let drepActivity = pp ^. ppDRepActivityL
      -- If there is a new governance proposal to vote on in this transaction,
      -- AND the number of dormant-epochs recorded is greater than zero, we bump
      -- the expiry for all DReps by the number of dormant epochs, and reset the
      -- counter to zero.
      -- It does not matter that this rule (CERTS) is called _before_ the GOV rule
      -- in LEDGER, even though we cannot validate any governance proposal here,
      -- since the entire transaction will fail if the proposal is not accepted in
      -- GOV, and so will this expiry bump done here. It will be discarded.
      let certState' =
            let hasProposals = not . OSet.null $ tx ^. bodyTxL . proposalProceduresTxBodyL
             in if hasProposals
                  then certState & certVStateL %~ updateDormantDRepExpiry currentEpoch
                  else certState

      -- Update DRep expiry for all DReps that are voting in this transaction.
      -- This will execute in mutual-exclusion to the previous updates to DRep expiry,
      -- because if there are no proposals to vote on , there will be no votes either.
      let numDormantEpochs = certState' ^. certVStateL . vsNumDormantEpochsL
          updateVSDReps vsDReps =
            Map.foldlWithKey'
              ( \dreps voter _ -> case voter of
                  DRepVoter cred ->
                    Map.adjust (updateDRepExpiry drepActivity currentEpoch numDormantEpochs) cred dreps
                  _ -> dreps
              )
              vsDReps
              (unVotingProcedures $ tx ^. bodyTxL . votingProceduresTxBodyL)

      -- Final CertState with updates to DRep expiry based on new proposals and votes on existing proposals
      let certStateWithDRepExpiryUpdated = certState' & certVStateL . vsDRepsL %~ updateVSDReps
          dState = certStateWithDRepExpiryUpdated ^. certDStateL
          withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL

      -- Validate withdrawals and rewards and drain withdrawals
      validateTrans WithdrawalsNotInRewardsCERTS $ validateZeroRewards dState withdrawals network

      pure $ certStateWithDRepExpiryUpdated & certDStateL .~ drainWithdrawals dState withdrawals
    gamma :|> txCert -> do
      certState' <-
        trans @(ConwayCERTS era) $ TRC (env, certState, gamma)
      trans @(EraRule "CERT" era) $
        TRC (CertEnv slot pp currentEpoch committee committeeProposals, certState', txCert)

instance
  ( Era era
  , STS (ConwayCERT era)
  , BaseM (EraRule "CERT" era) ~ ShelleyBase
  , Event (EraRule "CERT" era) ~ ConwayCertEvent era
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  ) =>
  Embed (ConwayCERT era) (ConwayCERTS era)
  where
  wrapFailed = CertFailure
  wrapEvent = CertEvent

-- | Update dormant expiry for all DReps that are active.
-- And also reset the `numDormantEpochs` counter.
updateDormantDRepExpiry ::
  -- | Current Epoch
  EpochNo ->
  VState era ->
  VState era
updateDormantDRepExpiry currentEpoch vState =
  if numDormantEpochs == EpochNo 0
    then vState
    else
      vState
        & vsNumDormantEpochsL .~ EpochNo 0
        & vsDRepsL %~ Map.map updateExpiry
  where
    numDormantEpochs = vState ^. vsNumDormantEpochsL
    updateExpiry =
      drepExpiryL
        %~ \currentExpiry ->
          let actualExpiry = binOpEpochNo (+) numDormantEpochs currentExpiry
           in if actualExpiry < currentEpoch
                then currentExpiry
                else actualExpiry
