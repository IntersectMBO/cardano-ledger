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
  ConwayBBODY,
  ConwayCERT,
  ConwayDELEG,
  ConwayGOVCERT,
  ConwayCERTS,
  ConwayGOV,
  ConwayHARDFORK,
  ConwayMEMPOOL,
  ConwayNEWEPOCH,
  ConwayEPOCH,
  ConwayENACT,
  ConwayUTXO,
  ConwayUTXOS,
  ConwayUTXOW,
  ConwayTICKF,
  ConwayLEDGER,
  ConwayRATIFY,
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
import Cardano.Ledger.Shelley.Rules (
  ShelleyPOOL,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
 )

-- =====================================================

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

data ConwayGOV era

type instance EraRule "GOV" ConwayEra = ConwayGOV ConwayEra

data ConwayNEWEPOCH era

type instance EraRule "NEWEPOCH" ConwayEra = ConwayNEWEPOCH ConwayEra

data ConwayEPOCH era

type instance EraRule "EPOCH" ConwayEra = ConwayEPOCH ConwayEra

data ConwayENACT era

type instance EraRule "ENACT" ConwayEra = ConwayENACT ConwayEra

data ConwayUTXOS era

type instance EraRule "UTXOS" ConwayEra = ConwayUTXOS ConwayEra

data ConwayLEDGER era

type instance EraRule "LEDGER" ConwayEra = ConwayLEDGER ConwayEra

data ConwayTICKF era

type instance EraRule "TICKF" ConwayEra = ConwayTICKF ConwayEra

data ConwayRATIFY era

type instance EraRule "RATIFY" ConwayEra = ConwayRATIFY ConwayEra

data ConwayCERTS era

type instance EraRule "CERTS" ConwayEra = ConwayCERTS ConwayEra

data ConwayCERT era

type instance EraRule "CERT" ConwayEra = ConwayCERT ConwayEra

data ConwayDELEG era

type instance EraRule "DELEG" ConwayEra = ConwayDELEG ConwayEra

data ConwayGOVCERT era

type instance EraRule "GOVCERT" ConwayEra = ConwayGOVCERT ConwayEra

data ConwayUTXOW era

type instance EraRule "UTXOW" ConwayEra = ConwayUTXOW ConwayEra

data ConwayUTXO era

type instance EraRule "UTXO" ConwayEra = ConwayUTXO ConwayEra

data ConwayBBODY era

type instance EraRule "BBODY" ConwayEra = ConwayBBODY ConwayEra

data ConwayMEMPOOL era

type instance EraRule "MEMPOOL" ConwayEra = ConwayMEMPOOL ConwayEra

data ConwayHARDFORK era

type instance EraRule "HARDFORK" ConwayEra = ConwayHARDFORK ConwayEra

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" ConwayEra = API.ShelleyLEDGERS ConwayEra

type instance EraRule "POOLREAP" ConwayEra = API.ShelleyPOOLREAP ConwayEra

type instance EraRule "RUPD" ConwayEra = ShelleyRUPD ConwayEra

type instance EraRule "SNAP" ConwayEra = ShelleySNAP ConwayEra

type instance EraRule "TICK" ConwayEra = ShelleyTICK ConwayEra

type instance EraRule "POOL" ConwayEra = ShelleyPOOL ConwayEra

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
-- with missing reward accounts) and incomplete withdrawals were being reported
-- with WithdrawalsNotInRewardsCERTS but now ConwayWithdrawalsMissingAccounts and
-- ConwayIncompleteWithdrawals are the new predicate failures we use to report
-- the two separate cases in LEDGER
hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule :: ProtVer -> Bool
hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv = pvMajor pv > natVersion @10
