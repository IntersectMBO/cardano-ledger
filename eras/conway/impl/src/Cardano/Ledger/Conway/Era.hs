{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Conway.Era (
  ConwayEra,
  BBODY,
  CERT,
  DELEG,
  GOVCERT,
  CERTS,
  GOV,
  HARDFORK,
  MEMPOOL,
  NEWEPOCH,
  EPOCH,
  ENACT,
  UTXO,
  UTXOS,
  UTXOW,
  TICKF,
  LEDGER,
  RATIFY,
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
  hardforkConwayDELEGIncorrectDepositsAndRefunds,
  hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule,
) where

import Cardano.Ledger.BaseTypes (ProtVer (pvMajor), natVersion)
import Cardano.Ledger.Core
import Cardano.Ledger.Internal.Era (ConwayEra)
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Shelley.Rules as Shelley

-- =====================================================

instance EraTxLevel ConwayEra where
  type STxLevel l ConwayEra = STxTopLevel l ConwayEra

type instance Value ConwayEra = MaryValue

-------------------------------------------------------------------------------
-- Deprecated rules
-------------------------------------------------------------------------------

type instance EraRule "UPEC" ConwayEra = VoidEraRule "UPEC" ConwayEra

type instance EraRuleFailure "UPEC" ConwayEra = VoidEraRule "UPEC" ConwayEra

type instance EraRuleEvent "UPEC" ConwayEra = VoidEraRule "UPEC" ConwayEra

type instance EraRule "NEWPP" ConwayEra = VoidEraRule "NEWPP" ConwayEra

type instance EraRuleFailure "NEWPP" ConwayEra = VoidEraRule "NEWPP" ConwayEra

type instance EraRuleEvent "NEWPP" ConwayEra = VoidEraRule "NEWPP" ConwayEra

type instance EraRule "PPUP" ConwayEra = VoidEraRule "PPUP" ConwayEra

type instance EraRuleFailure "PPUP" ConwayEra = VoidEraRule "PPUP" ConwayEra

type instance EraRuleEvent "PPUP" ConwayEra = VoidEraRule "PPUP" ConwayEra

type instance EraRule "MIR" ConwayEra = VoidEraRule "MIR" ConwayEra

type instance EraRuleFailure "MIR" ConwayEra = VoidEraRule "MIR" ConwayEra

type instance EraRuleEvent "MIR" ConwayEra = VoidEraRule "MIR" ConwayEra

type instance EraRule "DELEGS" ConwayEra = VoidEraRule "DELEGS" ConwayEra

type instance EraRuleFailure "DELEGS" ConwayEra = VoidEraRule "DELEGS" ConwayEra

type instance EraRuleEvent "DELEGS" ConwayEra = VoidEraRule "DELEGS" ConwayEra

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data GOV era

type instance EraRule "GOV" ConwayEra = GOV ConwayEra

data NEWEPOCH era

type instance EraRule "NEWEPOCH" ConwayEra = NEWEPOCH ConwayEra

data EPOCH era

type instance EraRule "EPOCH" ConwayEra = EPOCH ConwayEra

data ENACT era

type instance EraRule "ENACT" ConwayEra = ENACT ConwayEra

data UTXOS era

type instance EraRule "UTXOS" ConwayEra = UTXOS ConwayEra

data LEDGER era

type instance EraRule "LEDGER" ConwayEra = LEDGER ConwayEra

data TICKF era

type instance EraRule "TICKF" ConwayEra = TICKF ConwayEra

data RATIFY era

type instance EraRule "RATIFY" ConwayEra = RATIFY ConwayEra

data CERTS era

type instance EraRule "CERTS" ConwayEra = CERTS ConwayEra

data CERT era

type instance EraRule "CERT" ConwayEra = CERT ConwayEra

data DELEG era

type instance EraRule "DELEG" ConwayEra = DELEG ConwayEra

data GOVCERT era

type instance EraRule "GOVCERT" ConwayEra = GOVCERT ConwayEra

data UTXOW era

type instance EraRule "UTXOW" ConwayEra = UTXOW ConwayEra

data UTXO era

type instance EraRule "UTXO" ConwayEra = UTXO ConwayEra

data BBODY era

type instance EraRule "BBODY" ConwayEra = BBODY ConwayEra

data MEMPOOL era

type instance EraRule "MEMPOOL" ConwayEra = MEMPOOL ConwayEra

data HARDFORK era

type instance EraRule "HARDFORK" ConwayEra = HARDFORK ConwayEra

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" ConwayEra = API.LEDGERS ConwayEra

type instance EraRule "POOLREAP" ConwayEra = API.POOLREAP ConwayEra

type instance EraRule "RUPD" ConwayEra = Shelley.RUPD ConwayEra

type instance EraRule "SNAP" ConwayEra = Shelley.SNAP ConwayEra

type instance EraRule "TICK" ConwayEra = Shelley.TICK ConwayEra

type instance EraRule "POOL" ConwayEra = Shelley.POOL ConwayEra

-- | Bootstrap phase
hardforkConwayBootstrapPhase :: ProtVer -> Bool
hardforkConwayBootstrapPhase pv = pvMajor pv == natVersion @9

-- | Starting with protocol version 11, we do not allow unelected committee
-- members to submit votes.
hardforkConwayDisallowUnelectedCommitteeFromVoting :: ProtVer -> Bool
hardforkConwayDisallowUnelectedCommitteeFromVoting pv = pvMajor pv > natVersion @10

-- | Starting with protocol version 11, we report incorrect deposit and refunds better
hardforkConwayDELEGIncorrectDepositsAndRefunds :: ProtVer -> Bool
hardforkConwayDELEGIncorrectDepositsAndRefunds pv = pvMajor pv > natVersion @10

-- | Starting with protocol version 11, we move a predicate check and updates
-- related to DRep dormancy and expiry from CERTS to LEDGER since that is a
-- more suitable place for them.
--
-- 1. updates to drep expiry for all voting dreps
-- 2. drep dormancy tracking updates
-- 3. withdrawals draining - (split into two)
--
-- NOTE: In addition, we split the predicate check for withdrawals into two to
-- make it better: both invalid withdrawals (submitted in the wrong network or
-- with missing account addresss) and incomplete withdrawals were being reported
-- with WithdrawalsNotInRewardsCERTS but now ConwayWithdrawalsMissingAccounts and
-- ConwayIncompleteWithdrawals are the new predicate failures we use to report
-- the two separate cases in LEDGER
hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule :: ProtVer -> Bool
hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv = pvMajor pv > natVersion @10
