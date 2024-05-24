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
  addEpochInterval,
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
import Cardano.Ledger.CertState (certDStateL, certVStateL, vsDRepsL, vsNumDormantEpochsL)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayCERT, ConwayCERTS, ConwayEra)
import Cardano.Ledger.Conway.Governance (Voter (DRepVoter), VotingProcedures (unVotingProcedures))
import Cardano.Ledger.Conway.Rules.Cert (CertEnv (CertEnv), ConwayCertEvent, ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.DRep (DRepState, drepExpiryL)
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
    ( env@(CertsEnv tx pp slot currentEpoch)
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
                numDormantEpochs = certState ^. certVStateL . vsNumDormantEpochsL
             in if hasProposals && numDormantEpochs /= EpochNo 0
                  then
                    certState
                      & certVStateL . vsDRepsL %~ updateDormantDRepExpiry currentEpoch numDormantEpochs
                      & certVStateL . vsNumDormantEpochsL .~ EpochNo 0
                  else certState

      -- Update DRep expiry for all DReps that are voting in this transaction.
      -- This will execute in mutual-exclusion to the previous updates to DRep expiry,
      -- because if there are no proposals to vote on , there will be no votes either.
      let updatedVSDReps =
            Map.foldlWithKey'
              ( \dreps voter _ -> case voter of
                  DRepVoter cred -> Map.adjust (drepExpiryL .~ addEpochInterval currentEpoch drepActivity) cred dreps
                  _ -> dreps
              )
              (certState' ^. certVStateL . vsDRepsL)
              (unVotingProcedures $ tx ^. bodyTxL . votingProceduresTxBodyL)
          certStateWithDRepExpiryUpdated = certState' & certVStateL . vsDRepsL .~ updatedVSDReps
          dState = certStateWithDRepExpiryUpdated ^. certDStateL
          withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL

      -- Validate withdrawals and rewards and drain withdrawals
      validateTrans WithdrawalsNotInRewardsCERTS $ validateZeroRewards dState withdrawals network

      pure $ certStateWithDRepExpiryUpdated & certDStateL .~ drainWithdrawals dState withdrawals
    gamma :|> txCert -> do
      certState' <-
        trans @(ConwayCERTS era) $ TRC (env, certState, gamma)
      trans @(EraRule "CERT" era) $
        TRC (CertEnv slot pp currentEpoch, certState', txCert)

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
updateDormantDRepExpiry ::
  Functor f =>
  -- | Current Epoch
  EpochNo ->
  -- | Number of dormant epochs
  EpochNo ->
  f (DRepState c) ->
  f (DRepState c)
updateDormantDRepExpiry currentEpoch numDormantEpochs drepState =
  if numDormantEpochs == EpochNo 0
    then drepState
    else
      drepState
        <&> drepExpiryL
          %~ ( \e ->
                if e < currentEpoch
                  then e
                  else binOpEpochNo (+) numDormantEpochs e
             )
