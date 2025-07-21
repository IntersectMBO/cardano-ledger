{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
) where
import Cardano.Ledger.Coin
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.State
import Data.Map.Strict (Map)
import Data.Set (Set)
data LedgerExamples era = LedgerExamples
  { -- tx
    leTx :: Tx era
  , leApplyTxError :: ApplyTxError era
  , -- protocol parameters
    lePParams :: PParams era
  , leProposedPPUpdates :: ProposedPPUpdates era
  , -- Ledger state
    leNewEpochState :: NewEpochState era
  , lePoolDistr :: PoolDistr
  , -- rewards and delegation
    leRewardsCredentials :: Set (Either Coin (Credential 'Staking))
  , leNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking))
        (Map (KeyHash 'StakePool) Coin)
  , leTranslationContext :: TranslationContext era
  , leShelleyGenesis :: ShelleyGenesis
  }

deriving instance
  ( EraTx era
  , Eq (PParams era)
  , Eq (PParamsUpdate era)
  , EraGov era
  , Eq (Tx era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (StashedAVVMAddresses era)
  , Eq (TranslationContext era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (LedgerExamples era)
